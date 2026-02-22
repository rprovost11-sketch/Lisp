import io
import os
import sys
import datetime
import time
from abc import ABC, abstractmethod
from typing import Any

import pythonslisp.Parser as Parser

### Utility Functions
### =================
def retrieveFileList( dirname ) -> list[str]:
   "Returns a list of all the filenames in the specified directory."
   testFileList = os.listdir( dirname )
   testFileList.sort()
   testFileList = [ f'{dirname}/{testFileName}' for testFileName in testFileList
                    if os.path.isfile( f'{dirname}/{testFileName}' ) ]
   return testFileList

def columnize( lst: list[str], displaywidth: int=80, file=None, itemColor=None ) -> None:
   """Display a list of strings as a compact set of columns.

   Each column is only as wide as necessary.
   Columns are separated by two spaces.
   If itemColor is an ANSI escape string, each item is wrapped in that color.
   """
   RESET = '\033[0m'
   size = len(lst)
   if size == 1:
      item = lst[0]
      print( f'{itemColor}{item}{RESET}' if itemColor else item, file=file )
      return
   # Try every row count from 1 upwards
   for nrows in range(1, len(lst)):
      ncols = (size+nrows-1) // nrows
      colwidths = []
      totwidth = -2
      for col in range(ncols):
         colwidth = 0
         for row in range(nrows):
            i = row + nrows*col
            if i >= size:
               break
            x = lst[i]
            colwidth = max(colwidth, len(x))
         colwidths.append(colwidth)
         totwidth += colwidth + 2
         if totwidth > displaywidth:
            break
      if totwidth <= displaywidth:
         break
   else:
      nrows = len(lst)
      ncols = 1
      colwidths = [0]
   for row in range(nrows):
      texts = []
      for col in range(ncols):
         i = row + nrows*col
         if i >= size:
            x = ""
         else:
            x = lst[i]
         texts.append(x)
      while texts and not texts[-1]:
         del texts[-1]
      for col in range(len(texts)):
         content = texts[col]
         padded  = content.ljust(colwidths[col])
         if itemColor and content:
            padding     = padded[len(content):]
            texts[col]  = f'{itemColor}{content}{RESET}{padding}'
         else:
            texts[col]  = padded
      print(str("  ".join(texts)), file=file )

def write_multiFile( outputString: str, *fileList ):
   """Write output to multiple output streams using print.
   fileList is a python list containing output file objects.
   An output file object of None prints directly to the screen."""
   for fileStream in fileList:
      print( outputString, end='\n', flush=True, file=fileStream )

### The Listener Implementation
### ===========================
class Interpreter( ABC ):
   '''Interpreter interface expected by the Listener class.'''
   @abstractmethod
   def reboot( self, outStrm=None ) -> None:
      '''Reboot the interpreter.'''
      pass

   @abstractmethod
   def eval( self, source: str, outStrm=None ) -> str:
      '''Evaluate an expression string of the target language and return a
      string representing the return value.'''
      pass
   
   @abstractmethod
   def evalFile( self, filename: str, outStrm=None ) -> None:
      pass


class ListenerCommandError( Exception ):
   pass

class Listener( object ):
   '''Listener environment for interpreted languages needing a repl.  Has
   a read-eval-print-loop and listener commands for session logging, as well
   as testing and rebooting the intepreter.  Partly ripped off from Python's
   cmd module.'''
   def __init__( self, anInterpreter: Interpreter, testdir: str='', **kwargs ) -> None:
      useColor   = sys.stdout.isatty()
      BOLD_WHITE = '\033[1;97m' if useColor else ''
      DIM        = '\033[2m'    if useColor else ''
      RESET      = '\033[0m'    if useColor else ''
      print( f'{BOLD_WHITE}{{language}} {{version}} by {{author}}{RESET}'.format(**kwargs) )
      print( f'{DIM}Project home {{project}}{RESET}'.format(**kwargs) )
      print( )
      print( f'{DIM}- Initializing Listener{RESET}', flush=True )
      self._interp          = anInterpreter
      self._testdir         = testdir
      self._logFile: Any    = None
      self._instrumenting = False
      self._cmd_reboot( [ ] )

   def readEvalPrintLoop( self ) -> None:
      '''Execute a read-eval-print-loop.  Handles all exceptions internally.'''
      inputExprLineList: list[str] = [ ]

      while True:
         # Read input from the user
         try:
            if len(inputExprLineList) == 0:
               lineInput = self._prompt( '>>> ' )
            else:
               lineInput = self._prompt( '... ' )
         except EOFError:
            break

         if (lineInput == '') and (len(inputExprLineList) != 0):
            inputExprStr = '\n'.join( inputExprLineList ).strip()
            try:
               if (inputExprStr != '') and (inputExprStr[0] == ']'):
                  self._runListenerCommand( inputExprStr )
               else:
                  start = time.perf_counter( )
                  resultStr,parseTime,evalTime = self._interp.eval_instrumented( inputExprStr )
                  totalTime  = time.perf_counter( ) - start
                  self._writeResult( resultStr )
                  if self._instrumenting:
                     print( f'-------------  Parse time:              {parseTime:15.8f} sec' )
                     print( f'-------------  Eval time:               {evalTime:15.8f} sec' )
                     print( f'-------------     Total execution time: {totalTime:15.8f} sec' )

            except StopIteration:
               break

            except (Parser.ParseError, ListenerCommandError) as ex:
               self._writeErrorMsg( ex.args[-1] if ex.args else str(ex) )

            except Exception as ex:   # Unknowns raised by the interpreter
               self._writeErrorMsg( ex.args[-1] if ex.args else str(ex) )
               #exceptInfo = sys.exc_info( )
               #sys.excepthook( *exceptInfo )

            self._writeLn( )
            inputExprLineList = [ ]

         else:
            if lineInput != '':
               inputExprLineList.append( lineInput )

   def sessionLog_restore( self, filename: str, verbosity: int=0 ) -> None:
      '''Read in and restore/execute a session log.
      Raises if an evaluation error occurs.'''
      inputText = None
      try:
         with open( filename, 'r') as file:
            inputText = file.read( )
      except FileNotFoundError:
         print( f'File not found: "{filename}".' )
         return

      for exprNum,exprPackage in enumerate(Listener._parseLog(inputText)):
         exprStr,outputStr,retValStr,errMsgStr = exprPackage
         if verbosity > 0:
            exprLines = exprStr.splitlines()
            for lineNum, line in enumerate(exprLines):
               if lineNum == 0:
                  print( f'\n>>> {line}' )
               else:
                  print( f'... {line}')

         resultStr = self._interp.eval( exprStr )
         if verbosity == 3:
            print( f'\n==> {resultStr}' )

   def sessionLog_test( self, filename: str, verbosity: int=3 ) -> str:
      '''Test the interpreter by comparing evaluation results to a session log.
      Returns if an exception occurs.'''
      inputText = None
      try:
         with open( filename, 'r') as file:
            inputText = file.read( )
      except FileNotFoundError:
         print( f'File not found: "{filename}".' )
         return

      print( f'   Test file: {filename}... ', end='' )
      if verbosity >= 3:
         print()

      numPassed = 0
      exprNum = -1
      for exprNum,exprPackage in enumerate(Listener._parseLog(inputText)):
         exprStr,expectedOutputStr,expectedRetValStr,expectedErrStr = exprPackage
         if verbosity == 2:
            print( f'{str(exprNum+1).rjust(8)}.' )
         elif verbosity == 3:
            print( f'{str(exprNum+1).rjust(8)}> {exprStr}' )
         
         expectedErrStr = expectedErrStr.rstrip()

         # Perform the test and collect the various outputs
         outputStream = io.StringIO( )
         actualRetValStr = ''
         actualErrorStr = ''
         
         try:
            actualRetValStr = self._interp.eval( exprStr, outStrm=outputStream )
         except Parser.ParseError as ex:
            actualErrorStr = ex.args[-1] if ex.args else str(ex)
         except Exception as ex:   # Unknowns raised by the interpreter
            actualErrorStr = ex.args[-1] if ex.args else str(ex)
         
         actualOutputStr = outputStream.getvalue().strip()

         # Compute the results
         retVal_passed = actualRetValStr == expectedRetValStr
         outVal_passed = actualOutputStr == expectedOutputStr
         errVal_passed = actualErrorStr == expectedErrStr

         # Tally findings
         passFail = 'Failed!'
         if retVal_passed and outVal_passed and errVal_passed:
            passFail = 'PASSED!'
            numPassed += 1

         # Report Results
         if passFail == 'Failed!':
            if not retVal_passed:
               print( f'         RETURN: expected [{expectedRetValStr}] got [{actualRetValStr}]' )
            if not outVal_passed:
               print( f'         OUTPUT: expected [{expectedOutputStr}] got [{actualOutputStr}]' )
            if not errVal_passed:
               print( f'         ERROR:  expected [{expectedErrStr}] got [{actualErrorStr}]' )
         print( f'         {passFail}\n' )

      # Summarize results for test file
      numTests = exprNum + 1
      numFailed = numTests - numPassed
      resultMessage = ''
      if numFailed == 0:
         resultMessage = f'{numTests:4d} TESTS PASSED!'
      else:
         resultMessage = f'({numFailed}/{numTests}) Failed.'

      print( resultMessage )
      return resultMessage, numTests

   def _runListenerCommand( self, listenerCommand: str ) -> None:
      cmdParts  = listenerCommand[1:].split( ' ' )
      cmd,*args = cmdParts

      func = getattr(self, f'_cmd_{cmd}', None)
      if func is None:
         raise ListenerCommandError( f'Unknown listener command "{cmd}"' )
      func(args)

   def _cmd_close( self, args: list[str] ) -> None:
      '''Usage:  close
      Close the current logging session.
      '''
      if len(args) != 0:
         raise ListenerCommandError( self._cmd_close.__doc__ )

      if self._logFile is None:
         raise ListenerCommandError( "Not currently logging." )

      self._writeLn( '>>> ;;;;;;  Logging ended.' )
      self._writeLn( '... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;' )
      self._writeLn( '... 0')
      self._writeLn( '' )
      self._writeLn( '==> 0')
      self._logFile.close( )
      self._logFile = None

   def _cmd_continue( self, args: list[str] ) -> None:
      '''Usage:  continue <filename> [V|v]
      Read and execute a log file.  Keep the log file open to
      continue a logging session where you left off.  V reads
      the file verbosely.
      '''
      if self._logFile:
         raise ListenerCommandError(
            "A log file is already open and logging.  Only one log file can be open at a time." )

      numArgs = len(args)
      if numArgs not in ( 1, 2 ):
         raise ListenerCommandError( self._cmd_continue.__doc__ )

      self._cmd_readlog( args )

      filename = args[0]
      try:
         self._logFile = open( filename, 'a' )
      except OSError:
         raise ListenerCommandError( 'Unable to open file for append.' )

      self._writeLn( '>>> ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;' )
      self._writeLn( '... ;;;;;;  Continuing Log ( {0} ): {1}'.format( datetime.datetime.now().isoformat(), filename ) )
      self._writeLn( '... 0')
      self._writeLn( '' )
      self._writeLn( '==> 0')

   def _cmd_exit( self, args: list[str] ) -> None:
      '''Usage:  exit
      Exit the listener.
      '''
      if len(args) != 0:
         raise ListenerCommandError( self._cmd_exit.__doc__ )

      if self._logFile is not None:
         self._cmd_close( [] )

      print( 'Bye.' )
      raise StopIteration( )

   def _cmd_help( self, args: list[str] ) -> None:
      '''Usage: help [<command>]
      List all available commands, or detailed help for a specific command.
      '''
      if len(args) > 0:
         arg = args[0]
         try:
            doc = getattr(self, f'_cmd_{arg}').__doc__
            if doc:
               print(doc)
         except AttributeError:
            raise ListenerCommandError( f"*** No help on {arg}." )
      else:
         useColor   = sys.stdout.isatty()
         BOLD_WHITE = '\033[1;97m' if useColor else ''
         CYAN       = '\033[96m'   if useColor else ''
         RESET      = '\033[0m'    if useColor else ''
         header = "Listener Commands"
         names = dir(self.__class__)
         names.sort()
         cmds = [ name[5:] for name in names if name.startswith('_cmd_') ]
         print( )
         print( f'{BOLD_WHITE}{header}{RESET}' )
         print( f'{BOLD_WHITE}{"=" * len(header)}{RESET}' )
         columnize(cmds, 69, itemColor=CYAN or None)
         print()
         print( "Type ']help <command>' for help on a command." )
         print( "Type ']<command> [ <arg> ]' to execute a command." )

   def _cmd_instrument( self, args: list[str] ) -> None:
      '''Usage:  instrument
      Toggle on or off instrumenting in the repl.  Note that performance
      characteristics are only reported on screen.  If logging they are not
      recorded in the log file.
      '''
      if len(args) != 0:
         raise ListenerCommandError( self._cmd_instrument.__doc__ )
      
      self._instrumenting = not self._instrumenting       # toggle the state
      stateStr   = 'ON' if self._instrumenting else 'OFF'
      useColor   = sys.stdout.isatty()
      GREEN      = '\033[92m' if useColor else ''
      YELLOW     = '\033[93m' if useColor else ''
      RESET      = '\033[0m'  if useColor else ''
      stateColor = GREEN if self._instrumenting else YELLOW
      print( f'Instrumenting is now {stateColor}{stateStr}{RESET}.' )

   def _cmd_log( self, args: list[str] ) -> None:
      '''Usage:  log <filename>
      Begin a new logging session.
      '''
      if len(args) != 1:
         raise ListenerCommandError( self._cmd_log.__doc__ )

      filename = args[0]
      if self._logFile is not None:
         raise ListenerCommandError( 'Already logging.  Can\'t open more than one log file at a time.\n' )

      try:
         self._logFile = open( filename, 'w' )
      except OSError:
         raise ListenerCommandError( 'Unable to open file for writing.' )

      self._writeLn( '>>> ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;' )
      self._writeLn( '... ;;;;;;  Starting Log ( {0} ): {1}'.format( datetime.datetime.now().isoformat(), filename ) )
      self._writeLn( '... 0')
      self._writeLn( '' )
      self._writeLn( '==> 0')

   def _cmd_readlog( self, args: list[str] ) -> None:
      '''Usage:  readlog <filename> [v|v]
      Read and execute a log file.  V is for verbose.
      '''
      if len(args) not in ( 1, 2 ):
         raise ListenerCommandError( self._cmd_readlog.__doc__ )

      verbosity: int=0
      if len(args) == 2:
         if args[1].upper() == 'V':
            verbosity=3

      filename: str = args[0]
      self.sessionLog_restore( filename, verbosity=verbosity )
      useColor = sys.stdout.isatty()
      GREEN    = '\033[92m' if useColor else ''
      RESET    = '\033[0m'  if useColor else ''
      print( f'{GREEN}Log file read successfully:{RESET} {filename}' )

   def _cmd_readsrc( self, args: list[str] ) -> None:
      '''Usage:  readsrc <filename>
      Read and execute a source file.
      '''
      if len(args) != 1:
         raise ListenerCommandError( self._cmd_readsrc.__doc__ )

      filename: str = args[0].strip()
      try:
         self._interp.evalFile( filename )
      except FileNotFoundError as ex:
         print( f'File not found: "{filename}".' )
         return
      
      useColor = sys.stdout.isatty()
      GREEN    = '\033[92m' if useColor else ''
      RESET    = '\033[0m'  if useColor else ''
      print( f'{GREEN}Source file read successfully:{RESET} {filename}' )

   def _cmd_reboot( self, args: list[str] ) -> None:
      '''Usage: reboot
      Reset the interpreter.
      '''
      if len(args) > 0:
         raise ListenerCommandError( self._cmd_reboot.__doc__ )

      if self._logFile:
         raise ListenerCommandError( 'Please close the log before rebooting.' )

      useColor   = sys.stdout.isatty()
      BOLD_GREEN = '\033[1;92m' if useColor else ''
      CYAN       = '\033[96m'   if useColor else ''
      DIM        = '\033[2m'    if useColor else ''
      RESET      = '\033[0m'    if useColor else ''
      print( f'{DIM}- Initializing interpreter{RESET}' )
      print( f'{DIM}- Loading Runtime library{RESET}' )
      self._interp.reboot( )                     # boot/Reboot the interpreter
      print( )
      print( f'Enter \'{CYAN}]help{RESET}\' for listener commands.' )
      print( 'Enter any expression to have it evaluated by the interpreter.')
      print( f'{BOLD_GREEN}Welcome!{RESET}' )
      print( )

   def _cmd_trace( self, args: list[str] ) -> None:
      '''Usage:  trace
      Toggle global function tracing on or off.  When on, every call to a
      user-defined function is reported with its arguments and return value.
      Use (trace fn) and (untrace fn) in Lisp to trace specific functions only.
      '''
      if len(args) != 0:
         raise ListenerCommandError( self._cmd_trace.__doc__ )

      from pythonslisp.LispInterpreter import LispInterpreter
      LispInterpreter._trace_global = not LispInterpreter._trace_global
      LispInterpreter._set_trace_hook()

      useColor   = sys.stdout.isatty()
      GREEN      = '\033[92m' if useColor else ''
      YELLOW     = '\033[93m' if useColor else ''
      RESET      = '\033[0m'  if useColor else ''
      state      = LispInterpreter._trace_global
      stateColor = GREEN if state else YELLOW
      stateStr   = 'ON' if state else 'OFF'
      print( f'Tracing is now {stateColor}{stateStr}{RESET}.' )

   def _cmd_test( self, args: list[str] ) -> None:
      '''Usage:  test [<filename>]
      
      Test the interpreter using a log file.
      Read and execute a log file.  Compare the output, return value, and error
      messages to the those in the log file.  If no test file is specified the
      listener will run the full standard suite of tests in the testing
      directory.
      
      Note: Testing reboots the interpreter before every test set and at the
      end of testing to insure a clean environment.  Any work you've done
      will be lost.
      '''
      numArgs = len(args)
      if numArgs > 1:
         raise ListenerCommandError( self._cmd_test.__doc__ )

      if self._logFile:
         raise ListenerCommandError( "Please discontinue logging before running any tests." )

      # Collect the test filenames into a list
      if numArgs == 1:
         filenameList = args
      else:
         filenameList = retrieveFileList( self._testdir )

      # Create runs directory and output file
      runsDir = os.path.join(self._testdir, 'runs')
      os.makedirs(runsDir, exist_ok=True)
      timestamp = datetime.datetime.now().strftime('%Y-%m-%d-%H%M%S')
      runFilename = os.path.join(runsDir, f'test-{timestamp}.run')
      runFile = open(runFilename, 'w')

      outStrm = io.StringIO()
      totalTestsRun = 0

      # Conduct the testing — redirect stdout to file
      testSummaryList: list[tuple[str, str]] = [ ]
      savedStdout = sys.stdout
      sys.stdout = runFile
      try:
         for filename in filenameList:
            self._interp.reboot( outStrm=outStrm )
            testResultMsg, numTestsRunThisFile = self.sessionLog_test( filename, verbosity=3 )
            testSummaryList.append( (filename, testResultMsg) )
            totalTestsRun += numTestsRunThisFile
      finally:
         sys.stdout = savedStdout
      self._interp.reboot( outStrm=outStrm )

      # Summarize Test Results — print to both screen and file
      useColor   = sys.stdout.isatty()
      BOLD_WHITE = '\033[1;97m' if useColor else ''
      GREEN      = '\033[92m'   if useColor else ''
      RED        = '\033[91m'   if useColor else ''
      RESET      = '\033[0m'    if useColor else ''

      reportLines = [
         '\n\nTest Report',
         '===========',
      ]
      for filename, testSummary in testSummaryList:
         reportLines.append( f'{os.path.basename(filename):40} {testSummary}' )
      reportLines.append( '' )
      reportLines.append( f'Total test files: {len(filenameList)}.' )
      reportLines.append( f'Total test cases: {totalTestsRun}.' )

      for line in reportLines:
         print( line, file=runFile )   # always plain in the run file
         if line in ('\n\nTest Report', '==========='):
            print( f'{BOLD_WHITE}{line}{RESET}' )
         elif 'TESTS PASSED!' in line:
            print( f'{GREEN}{line}{RESET}' )
         elif 'Failed.' in line and not line.startswith('Total'):
            print( f'{RED}{line}{RESET}' )
         else:
            print( line )

      runFile.close()
      print( f'\nTest output: {runFilename}' )

   def _writeLn( self, value: str='', file=None ) -> None:
      if self._logFile:
         write_multiFile( value, file, self._logFile )
      else:
         write_multiFile( value, file )

      #print( value, flush=True )
      #if self._logFile:
         #print( value, file=self._logFile, flush=True )

   def _writeResult( self, resultStr: str ) -> None:
      useColor     = sys.stdout.isatty()
      BRIGHT_GREEN = '\033[92m'  if useColor else ''
      BOLD_WHITE   = '\033[1;97m' if useColor else ''
      RESET        = '\033[0m'    if useColor else ''
      plainLine    = f'\n==> {resultStr}'
      colorLine    = f'\n{BRIGHT_GREEN}==>{RESET} {BOLD_WHITE}{resultStr}{RESET}'
      print( colorLine if useColor else plainLine, flush=True )
      if self._logFile:
         print( plainLine, file=self._logFile, flush=True )

   def _writeErrorMsg( self, errMsg: str, file=None ):
      RED   = '\033[91m'
      RESET = '\033[0m'
      useColor = sys.stdout.isatty()
      errMsgLinesOfText = errMsg.splitlines()
      for errMsgLine in errMsgLinesOfText:
         plainLine = f'%%% {errMsgLine}'
         colorLine = f'{RED}{plainLine}{RESET}' if useColor else plainLine
         print( colorLine, end='\n', flush=True, file=file )
         if self._logFile:
            print( plainLine, end='\n', flush=True, file=self._logFile )

   def _prompt( self, prompt: str='' ) -> str:
      inputStr: str = input( prompt ).strip()
      if self._logFile and (len(inputStr) != 0) and (inputStr[0] != ']'):
         self._logFile.write( f'{prompt}{inputStr}' )
      return inputStr

   @staticmethod
   def _parseLog( inputText: str ) -> list[tuple[str, str, str, str]]:
      lineIter = iter(inputText.splitlines(keepends=True))
      parsedLog: list[Any] = [ ]
      eof = False

      try:
         line = next(lineIter)
      except StopIteration:
         return parsedLog

      while not eof:
         expr = ''
         output = ''
         retVal = ''
         errMsg = ''

         # Skip to the begenning of an interaction prompt
         try:
            while not line.startswith( '>>> ' ):
               line = next(lineIter)

            # Parse Expression
            # string variable *line* begins with '>>> '
            if line.startswith( '>>> ' ):
               expr = line[ 4: ]
               line = next(lineIter)
               while line.startswith( '...' ):
                  expr += line[ 4: ]
                  line = next(lineIter)

            # Parse Output from the evaluation (such as write statements)
            while not line.startswith( ('==> ','... ','>>> ', '%%% ') ) and line.rstrip() != '==>':
               # Parse written output
               output += line
               line = next(lineIter)

            # Parse Return Value
            if line.startswith( '==> ' ) or line.rstrip() == '==>':
               retVal = line[ 4: ] if len(line) > 4 else ''
               line = next(lineIter)
               while not line.startswith( ('==> ','... ','>>> ','%%% ') ) and line.rstrip() != '==>':
                  if line.startswith( ';' ):
                     expr += line
                  else:
                     retVal += line
                  line = next(lineIter)

            if line.startswith( '%%% '):
               errMsg = line[4:]
               line = next(lineIter)
               while line.startswith( '%%% ' ):
                  errMsg += line[4:]
                  line = next(lineIter)

         except StopIteration:
            eof = True

         if expr != '':
            parsedLog.append( (expr,output.rstrip(),retVal.rstrip(),errMsg) )

      return parsedLog


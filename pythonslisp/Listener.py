import io
import os
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
   testFileList = [ f'{dirname}/{testFileName}' for testFileName in testFileList ]
   return testFileList

def columnize( lst: list[str], displaywidth: int=80 ) -> None:
   """Display a list of strings as a compact set of columns.

   Each column is only as wide as necessary.
   Columns are separated by two spaces.
   """
   size = len(lst)
   if size == 1:
      print(str(lst[0]))
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
         texts[col] = texts[col].ljust(colwidths[col])
      print(str("  ".join(texts)))

### The Listener Implementation
### ===========================
class Interpreter( ABC ):
   '''Interpreter interface expected by the Listener class.'''
   @abstractmethod
   def reboot( self ):
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
   def __init__( self, message, *args, **kwargs ):
      super().__init__( message )

class Listener( object ):
   '''Listener environment for interpreted languages.  Has a read-eval-print
   loop and listener commands for session logging, as well as testing and
   rebooting the intepreter.  Partly ripped off from Python's cmd module.'''
   def __init__( self, anInterpreter: Interpreter, testdir: str='', **kwargs ) -> None:
      print( '{language} {version} by {author}'.format(**kwargs) )
      print( 'Project home {project}'.format(**kwargs) )
      print( '- Initialing Listener', flush=True )
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
                  self._writeLn( f'\n==> {resultStr}' )
                  if self._instrumenting:
                     print( f'-------------  Parse time:              {parseTime:15.8f} sec' )
                     print( f'-------------  Eval time:               {evalTime:15.8f} sec' )
                     print( f'-------------     Total execution time: {totalTime:15.8f} sec' )

            except StopIteration:
               break

            except (Parser.ParseError, ListenerCommandError) as ex:
               self._writeErrorMsg( ex.args[-1] )

            except Exception as ex:   # Unknowns raised by the interpreter
               self._writeErrorMsg( ex.args[-1] )
               #exceptInfo = sys.exc_info( )
               #sys.excepthook( *exceptInfo )

            self._writeLn( )
            inputExprLineList = [ ]

         else:
            if lineInput != '':
               inputExprLineList.append( lineInput )

   def sessionLog_restore( self, filename: str, verbosity: int=0 ) -> None:
      '''Read in and restore/execute a session log.  Returns if an exception occurs.'''
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
            actualErrorStr = ex.args[-1]
         except Exception as ex:   # Unknowns raised by the interpreter
            actualErrorStr = ex.args[-1]
         
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
         print( f'         {passFail}\n' )

      # Summarize results for test file
      numTests = exprNum + 1
      numFailed = numTests - numPassed
      resultMessage = ''
      if numFailed == 0:
         resultMessage = 'ALL TESTS PASSED!'
      else:
         resultMessage = f'({numFailed}/{numTests}) Failed.'

      print( resultMessage )
      return resultMessage

   def _runListenerCommand( self, listenerCommand: str ) -> None:
      cmdParts  = listenerCommand[1:].split( ' ' )
      cmd,*args = cmdParts

      try:
         func = getattr(self, f'_cmd_{cmd}')
         func(args)
      except AttributeError:
         raise ListenerCommandError( f'Unknown listener command "{cmd}"' )

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
         self._cmd_close( [''] )

      print( 'Bye.' )
      raise StopIteration( )

   def _cmd_help( self, args: list[str] ) -> None:
      '''Usage: help [<command>]
      List all available commands, or detailed help for a specific command.
      '''
      if len(args) > 0:
         arg = args[0]
         try:
            doc=getattr(self, f'_cmd_{arg}').__doc__
            if doc:
               self._writeErrorMsg(doc)
         except AttributeError:
            pass
         raise ListenerCommandError( f"*** No help on {arg}." )
      else:
         header = "Listener Commands"
         names = dir(self.__class__)
         names.sort()
         cmds = [ name[5:] for name in names if name.startswith('_cmd_') ]
         print( )
         print(header)
         print('=' * len(header))
         columnize(cmds, 69)
         print()
         print( "Type ']help <command>' for help on a command." )
         print( "Type ']<command> [ <args> ]' to execute a command." )

   def _cmd_instrument( self, args: list[str] ) -> None:
      '''Usage:  instrument
      Toggle on or off instrumenting in the repl.  Note that performance
      characteristics are only reported on screen.  If logging they are not
      recorded in the log file.
      '''
      if len(args) != 0:
         raise ListenerCommandError( self._cmd_instrument.__doc__ )
      
      self._instrumenting = not self._instrumenting       # toggle the state
      stateStr = 'ON' if self._instrumenting else 'OFF'
      print( f'Instrumenting is now {stateStr}.' )

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
      print( f'Log file read successfully: {filename}' )

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
      
      print( f'Source file read successfully: {filename}' )

   def _cmd_reboot( self, args: list[str] ) -> None:
      '''Usage: reboot
      Reset the interpreter.
      '''
      if len(args) > 0:
         raise ListenerCommandError( self._cmd_reboot.__doc__ )

      if self._logFile:
         raise ListenerCommandError( 'Please close the log before rebooting.' )

      print( '- Initializing interpreter' )
      print( '- Loading Runtime library' )
      self._interp.reboot( )                     # boot/Reboot the interpreter
      print( 'Enter \']help\' for listener commands.' )
      print( 'Enter any expression to have it evaluated by the interpreter.')
      print( 'Welcome!' )

   def _cmd_test( self, args: list[str] ) -> None:
      '''Usage:  test [<filename>]
      Test the interpreter using a log file.
      Read and execute a log file.  Compare the output, return value, and error
      messages to the those in the log file.  If no test file is specified the
      listener will run the full standard suite of tests.
      Note: It's reccomended that you reboot the interpreter using ]reboot both
      before and after running tests.
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

      # Conduct the testing
      testSummaryList: list[tuple[str, str]] = [ ]
      for filename in filenameList:
         testResultMsg = self.sessionLog_test( filename, verbosity=3 )
         testSummaryList.append( (filename, testResultMsg) )

      # Summarize Test Results
      print( '\n\nTest Report' )
      print( '===========')
      for filename, testSummary in testSummaryList:
         print( f'{filename:55} {testSummary}' )

      print( '\nWARNING: Testing leaves the interpreter in an unknown state. ' +
             'It\'s reccomended that you reboot the interpreter using the listener command ]reboot.' )

   def _writeLn( self, value: str='', file=None ) -> None:
      print( value, file=file, flush=True )
      if self._logFile:
         self._logFile.write( value + '\n' )

   def _writeErrorMsg( self, errMsg: str, file=None ):
      errMsgLinesOfText = errMsg.splitlines()
      for errMsgLine in errMsgLinesOfText:
         self._writeLn( f'%%% {errMsgLine}', file=file )

   def _prompt( self, prompt: str='' ) -> str:
      inputStr: str = input( prompt ).strip()
      if self._logFile and (len(inputStr) != 0) and (inputStr[0] != ']'):
         self._logFile.write( f'{prompt}{inputStr}\n' )
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
               while not eof and line.startswith( '...' ):
                  expr += line[ 4: ]
                  line = next(lineIter)

            # Parse Output from the evaluation (such as write statements)
            while not line.startswith( ('==> ','... ','>>> ', '%%% ') ):
               # Parse written output
               output += line
               line = next(lineIter)

            # Parse Return Value
            if line.startswith( '==> ' ):
               retVal = line[ 4: ]
               line = next(lineIter)
               while not eof and not line.startswith( ('==> ','... ','>>> ','%%% ') ):
                  retVal += line
                  line = next(lineIter)

            if line.startswith( '%%% '):
               errMsg = line[4:]
               line = next(lineIter)
               while not eof and line.startswith( '%%% ' ):
                  errMsg += line[4:]
                  line = next(lineIter)

         except StopIteration:
            eof = True

         if expr != '':
            parsedLog.append( (expr,output.rstrip(),retVal.rstrip(),errMsg) )

      return parsedLog


from __future__ import annotations
import io
import os
import sys
import datetime
import atexit
from abc import ABC, abstractmethod
from typing import Any

import pythonslisp.ParserBase as ParserBase
from pythonslisp.Utils import columnize, retrieveFileList, writeln_multiFile, paren_state


### The Listener Implementation
### ===========================
class InterpreterBase( ABC ):
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
   def eval_instrumented( self, source: str, outStrm=None ) -> tuple(str, float, float):
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

   # readline state - mirrors Python's former module-level globals
   _HIST_FILE  = os.path.expanduser('~/.lisp_history')
   _rl         = None
   _historyMax = 500

   def __init__( self, anInterpreter: InterpreterBase, testdir: str='', **kwargs ) -> None:
      useColor   = sys.stdout.isatty()
      BOLD_WHITE = '\033[1;97m' if useColor else ''
      BOLD_GREEN = '\033[1;92m' if useColor else ''
      CYAN       = '\033[96m'   if useColor else ''
      DIM        = '\033[2m'    if useColor else ''
      RESET      = '\033[0m'    if useColor else ''
      print( f'{BOLD_WHITE}{{language}} {{version}} by {{author}}{RESET}'.format(**kwargs) )
      print( f'{DIM}Project home {{project}}{RESET}'.format(**kwargs) )
      print( )
      print( f'{DIM}- Interpreter Initialized{RESET}', flush=True )      
      print( f'{DIM}- Runtime Library Loaded{RESET}', flush=True )      
      print( f'{DIM}- Listener Initialized{RESET}', flush=True )
      self._interp        = anInterpreter
      self._testdir       = testdir
      self._logFile: Any  = None
      self._instrumenting = False
      print( )
      Listener.printWelcomeBanner( )
      print( )
      if not Listener._rl:
         if sys.platform == 'win32':
            try:
               import pythonslisp.readline_win as _rl_mod
               Listener._rl = _rl_mod
               Listener._rl.read_history_file(Listener._HIST_FILE)
               Listener._rl.set_history_length(Listener._historyMax)
               atexit.register(Listener._rl.write_history_file, Listener._HIST_FILE)
            except ImportError:
               pass
         else:
            try:
               import readline as _rl_mod
               Listener._rl = _rl_mod
               try:
                  Listener._rl.read_history_file(Listener._HIST_FILE)
               except FileNotFoundError:
                  pass
               Listener._rl.set_history_length(Listener._historyMax)
               Listener._rl.set_auto_history(False)
               atexit.register(Listener._rl.write_history_file, Listener._HIST_FILE)
            except ImportError:
               pass

      if hasattr( self._interp, 'set_nested_repl' ):
         self._interp.set_nested_repl( self._run_nested_repl )
      if hasattr( self._interp, 'set_dribble_fns' ):
         self._interp.set_dribble_fns( self._dribble_start, self._dribble_stop )

   def _dribble_start( self, filename: str ) -> str:
      """Open a dribble (session logging) file.  Returns the pathname."""
      import datetime as _dt
      if self._logFile is not None:
         self._logFile.close()
         self._logFile = None
      self._logFile = open( filename, 'w' )
      self._logFile.write( f';;; Dribble started {_dt.datetime.now().isoformat()}\n' )
      self._logFile.write( f';;; {filename}\n\n' )
      return filename

   def _dribble_stop( self ) -> str | None:
      """Close the dribble file.  Returns the pathname or None."""
      import datetime as _dt
      if self._logFile is None:
         return None
      self._logFile.write( f'\n;;; Dribble stopped {_dt.datetime.now().isoformat()}\n' )
      name = getattr( self._logFile, 'name', None )
      self._logFile.close()
      self._logFile = None
      return name

   def readEvalPrintLoop( self ) -> None:
      '''Execute a read-eval-print-loop.  Handles all exceptions internally.'''
      inputExprLineList: list[str] = [ ]

      while True:
         # Read input from the user
         try:
            if len(inputExprLineList) == 0:
               lineInput = self._prompt( '>>> ' )
            else:
               indent    = Listener._compute_indent( inputExprLineList )
               lineInput = self._prompt( '... ', prefill=indent )
         except EOFError:
            break
         except KeyboardInterrupt:
            print( )
            inputExprLineList = [ ]
            continue

         submit = False
         if lineInput == '':
            if inputExprLineList:
               submit = True
         else:
            # Super-bracket: trailing ] closes all open parens.
            # Lines that start with ] followed by text are listener commands - skip.
            if lineInput.endswith( ']' ) and not (lineInput.startswith( ']' ) and len(lineInput) > 1):
               tentative    = lineInput[:-1]
               combined     = '\n'.join( inputExprLineList + ([tentative] if tentative else []) )
               sb_depth, sb_in_string = Listener._paren_state( combined )
               if sb_depth > 0 and not sb_in_string:
                  lineInput = tentative + ')' * sb_depth
               elif lineInput == ']' and sb_depth == 0 and not sb_in_string:
                  continue
            # Log the (possibly expanded) line
            if self._logFile and lineInput and lineInput[0] != ']':
               log_prompt = '>>> ' if len(inputExprLineList) == 0 else '... '
               self._logFile.write( f'{log_prompt}{lineInput}' )
            inputExprLineList.append( lineInput )
            depth, _ = Listener._paren_state( '\n'.join(inputExprLineList) )
            if depth == 0:
               submit = True

         if submit:
            inputExprStr = '\n'.join( inputExprLineList ).strip()
            if self._rl and inputExprStr:
               self._rl.add_history(inputExprStr)
            try:
               if (inputExprStr != '') and (inputExprStr[0] == ']'):
                  self._runListenerCommand( inputExprStr )
               else:
                  if self._instrumenting:
                     rawVal, metrics = self._interp.rawEval_instrumented( inputExprStr )
                     self._writeResult( rawVal )
                     for name, value in metrics.items():
                        if name != 'total':
                           print( f'-------------  {name.capitalize()+" time:": <24} {value:15.8f} sec' )
                     print( f'-------------     {"Total time:": <21} {metrics["total"]:15.8f} sec' )
                  else:
                     rawVal = self._interp.rawEval( inputExprStr )
                     self._writeResult( rawVal )

            except StopIteration:
               break

            except (ParserBase.ParseError, ListenerCommandError) as ex:
               self._writeErrorMsg( ex.args[-1] if ex.args else str(ex) )

            except KeyboardInterrupt:
               self._writeErrorMsg( 'Interrupted.' )

            except Exception as ex:   # Unknowns raised by the interpreter
               self._writeErrorMsg( str(ex) if ex.args else '' )

            self._writeLn( )
            inputExprLineList = [ ]

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
            print( f'{str(exprNum+1).rjust(8)}.', flush=True)
         elif verbosity == 3:
            print( f'{str(exprNum+1).rjust(8)}> {exprStr}', flush=True)
         
         expectedErrStr = expectedErrStr.rstrip()

         # Perform the test and collect the various outputs
         outputStream = io.StringIO( )
         actualRetValStr = ''
         actualErrorStr = ''
         
         try:
            actualRetValStr = self._interp.eval( exprStr, outStrm=outputStream )
         except ParserBase.ParseError as ex:
            actualErrorStr = ex.args[-1] if ex.args else str(ex)
         except Exception as ex:   # Unknowns raised by the interpreter
            actualErrorStr = str(ex) if ex.args else ''
         
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
      cmd,*args = listenerCommand[1:].split( ' ' )
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

   def _cmd_resume( self, args: list[str] ) -> None:
      '''Usage:  resume <filename> [V|v]
      Read and execute a log file.  Keep the log file open to
      resume a logging session where you left off.  V reads
      the file verbosely.
      '''
      if self._logFile:
         raise ListenerCommandError(
            "A log file is already open and logging.  Only one log file can be open at a time." )

      numArgs = len(args)
      if numArgs not in ( 1, 2 ):
         raise ListenerCommandError( self._cmd_resume.__doc__ )

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
         print( "Type ']<command> <arg1> <arg2> ...' to execute a command." )

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

   def _cmd_lhistory( self, args: list[str] ) -> None:
      '''Usage:  lhistory [<n>]
      Get or set the maximum readline history size.
      '''
      if len(args) > 1:
         raise ListenerCommandError( self._cmd_lhistory.__doc__ )
      if len(args) == 0:
         print( f'Current history size: {Listener._historyMax}' )
      else:
         try:
            n = int(args[0])
         except ValueError:
            raise ListenerCommandError( self._cmd_lhistory.__doc__ )
         if n < 1:
            raise ListenerCommandError( 'History size must be a positive integer.' )
         Listener._historyMax = n
         if self._rl:
            self._rl.set_history_length(n)
         print( f'New history size: {n}' )

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

   def _cmd_quit( self, args: list[str] ) -> None:
      '''Usage:  quit
      Exit the listener (same as exit).
      '''
      self._cmd_exit( args )

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
      Listener.printWelcomeBanner( )
      print( )

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

      useColor   = sys.stdout.isatty()
      BOLD_WHITE = '\033[1;97m' if useColor else ''
      GREEN      = '\033[92m'   if useColor else ''
      RED        = '\033[91m'   if useColor else ''
      RESET      = '\033[0m'    if useColor else ''

      # Print header to screen before testing begins
      print( f'\n{BOLD_WHITE}Test Report{RESET}', flush=True )
      print( f'{BOLD_WHITE}==========={RESET}', flush=True )

      # Conduct the testing - redirect stdout to file
      testSummaryList: list[tuple[str, str]] = [ ]
      savedStdout = sys.stdout
      sys.stdout = runFile
      try:
         for filename in filenameList:
            self._interp.reboot( outStrm=outStrm )
            # Do not merge these two print statements.
            print( f'{os.path.basename(filename):40} ', end='', flush=True, file=savedStdout )
            testResultMsg, numTestsRunThisFile = self.sessionLog_test( filename, verbosity=3 )
            resultColor = GREEN if 'TESTS PASSED!' in testResultMsg else RED
            print( f'{resultColor}{testResultMsg}{RESET}', file=savedStdout, flush=True )
            testSummaryList.append( (filename, testResultMsg) )
            totalTestsRun += numTestsRunThisFile
      finally:
         sys.stdout = savedStdout
      self._interp.reboot( outStrm=outStrm )

      # Write full report to run file only
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
         print( line, file=runFile )

      runFile.close()

      # Print totals and run file path to screen
      print( f'\nTotal test files: {len(filenameList)}.' )
      print( f'Total test cases: {totalTestsRun}.' )
      print( f'\nTest output: {runFilename}' )

   def _cmd_traces( self, args: list[str] ) -> None:
      '''Usage:  traces [on|off]
      Enable or disable call-stack tracing on errors from file-loaded code.
      When on, errors show a call stack (source line + caret per frame) but
      the evaluator runs ~20-30% slower.  When off, full speed is restored.
      With no argument, shows the current state.
      '''
      useColor   = sys.stdout.isatty()
      GREEN      = '\033[92m' if useColor else ''
      YELLOW     = '\033[93m' if useColor else ''
      RESET      = '\033[0m'  if useColor else ''

      if len(args) == 0:
         from pythonslisp.Evaluator import _stack_traces_enabled as state
         stateColor = GREEN if state else YELLOW
         stateStr   = 'ON' if state else 'OFF'
         print( f'Stack-trace mode is {stateColor}{stateStr}{RESET}.' )
         return

      if len(args) != 1 or args[0].lower() not in ('on', 'off'):
         raise ListenerCommandError( self._cmd_traces.__doc__ )

      enabled = args[0].lower() == 'on'
      self._interp.set_tracing( enabled )
      stateColor = GREEN if enabled else YELLOW
      stateStr   = 'ON' if enabled else 'OFF'
      print( f'Stack-trace mode is now {stateColor}{stateStr}{RESET}.' )

   def _writeLn( self, value: str='', file=None, flush=False ) -> None:
      if self._logFile:
         writeln_multiFile( value, file, self._logFile, flush=flush )
      else:
         writeln_multiFile( value, file, flush=flush )

   def _writeResult( self, rawVal ) -> None:
      from pythonslisp.AST import LMultipleValues, prettyPrintSExpr
      useColor     = sys.stdout.isatty()
      BRIGHT_GREEN = '\033[92m'  if useColor else ''
      BOLD_WHITE   = '\033[1;97m' if useColor else ''
      RESET        = '\033[0m'    if useColor else ''
      if type(rawVal) is LMultipleValues:
         for v in rawVal.values:
            valStr    = prettyPrintSExpr( v ).strip()
            plainLine = f'\n==> {valStr}'
            colorLine = f'\n{BRIGHT_GREEN}==>{RESET} {BOLD_WHITE}{valStr}{RESET}'
            self._writeLn( colorLine if useColor else plainLine, file=None, flush=True )
      else:
         resultStr = prettyPrintSExpr( rawVal ).strip()
         plainLine = f'\n==> {resultStr}'
         colorLine = f'\n{BRIGHT_GREEN}==>{RESET} {BOLD_WHITE}{resultStr}{RESET}'
         self._writeLn( colorLine if useColor else plainLine, file=None, flush=True )

   def _writeErrorMsg( self, errMsg: str, file=None ) -> None:
      RED   = '\033[91m'
      RESET = '\033[0m'
      useColor = sys.stdout.isatty()
      errMsgLinesOfText = errMsg.splitlines()
      for errMsgLine in errMsgLinesOfText:
         plainLine = f'%%% {errMsgLine}'
         colorLine = f'{RED}{plainLine}{RESET}'
         self._writeLn( colorLine if useColor else plainLine, file=None, flush=True )

   def _prompt( self, prompt: str='', prefill: str='' ) -> str:
      if sys.platform == 'win32' and self._rl and sys.stdin.isatty():
         inputStr = self._rl.input_line( prompt, continuation_prompt='... ', prefill=prefill ).rstrip()
      else:
         if prefill and self._rl and hasattr( self._rl, 'set_startup_hook' ):
            self._rl.set_startup_hook( lambda: self._rl.insert_text( prefill ) )
            try:
               inputStr = input( prompt ).rstrip()
            finally:
               self._rl.set_startup_hook( None )
         else:
            inputStr = input( prompt ).rstrip()
      return inputStr

   @staticmethod
   def _paren_state( text: str ) -> tuple[int, bool]:
      return paren_state( text )

   def _run_nested_repl( self, env ) -> Any:
      """Run a nested debug REPL with brk>>> prompts.
      Returns the value passed to ]cont (or NIL).
      Raises LRuntimeError on ]abort or EOF."""
      from pythonslisp.AST import L_NIL, prettyPrintSExpr
      from pythonslisp.Exceptions import LRuntimeError
      ctx            = self._interp._ctx
      inputExprLines = []

      while True:
         try:
            if not inputExprLines:
               lineInput = self._prompt( 'brk>>> ' )
            else:
               indent    = Listener._compute_indent( inputExprLines )
               lineInput = self._prompt( 'brk... ', prefill=indent )
         except EOFError:
            print()
            raise LRuntimeError( 'break: end of input in debug REPL' )
         except KeyboardInterrupt:
            print()
            inputExprLines = []
            continue

         # ]cont and ]abort commands
         if not inputExprLines and lineInput.startswith( ']' ):
            parts = lineInput[1:].split( None, 1 )
            cmd   = parts[0] if parts else ''
            rest  = parts[1].strip() if len(parts) > 1 else ''
            if cmd == 'cont':
               if rest:
                  try:
                     ast    = ctx.parse( rest )
                     result = L_NIL
                     for form in ast[1:]:
                        form   = ctx.expand( env, form )
                        ctx.analyze( env, form )
                        result = ctx.lEval( env, form )
                     return result
                  except Exception as ex:
                     print( f'%%% {ex}' )
                     continue
               return L_NIL
            elif cmd == 'abort':
               raise LRuntimeError( 'Aborted from (break).' )
            else:
               print( f"Unknown command '{cmd}'.  Use ]cont or ]abort." )
               continue

         # Super-bracket
         if lineInput.endswith( ']' ) and not (lineInput.startswith( ']' ) and len(lineInput) > 1):
            tentative = lineInput[:-1]
            combined  = '\n'.join( inputExprLines + ([tentative] if tentative else []) )
            sb_depth, sb_in_str = paren_state( combined )
            if sb_depth > 0 and not sb_in_str:
               lineInput = tentative + ')' * sb_depth
            elif lineInput == ']' and sb_depth == 0 and not sb_in_str:
               continue

         if lineInput == '' and not inputExprLines:
            continue

         inputExprLines.append( lineInput )
         depth, _ = paren_state( '\n'.join( inputExprLines ) )

         if lineInput == '' or depth == 0:
            src            = '\n'.join( inputExprLines ).strip()
            inputExprLines = []
            if not src:
               continue
            try:
               ast    = ctx.parse( src )
               result = L_NIL
               for form in ast[1:]:
                  form   = ctx.expand( env, form )
                  ctx.analyze( env, form )
                  result = ctx.lEval( env, form )
               print( f'\n==> {prettyPrintSExpr( result )}\n' )
            except Exception as ex:
               print( f'%%% {ex}\n' )

   @staticmethod
   def _compute_indent( lines: list ) -> str:
      """Return whitespace to auto-indent the next continuation line.
      Indents 3 spaces per unclosed paren depth."""
      depth     = 0
      in_string = False
      escape    = False
      for line in lines:
         for ch in line:
            if escape:
               escape = False
            elif in_string:
               if ch == '\\':
                  escape = True
               elif ch == '"':
                  in_string = False
            else:
               if ch == '"':
                  in_string = True
               elif ch == ';':
                  break
               elif ch == '(':
                  depth += 1
               elif ch == ')':
                  if depth > 0:
                     depth -= 1
      return ' ' * (depth * 3)

   @staticmethod
   def printWelcomeBanner( ):
      useColor   = sys.stdout.isatty()
      BOLD_GREEN = '\033[1;92m' if useColor else ''
      CYAN       = '\033[96m'   if useColor else ''
      RESET      = '\033[0m'    if useColor else ''
      print( 'Enter any expression to have it evaluated by the interpreter.' )
      print( f'Evaluate \'{CYAN}(help){RESET}\' for online help.' )
      print( f'Evaluate \'{CYAN}(help "listener"){RESET}\' for Listener features.' )
      print( f'{BOLD_GREEN}Welcome! to the Python\'s Lisp Listener{RESET}' )
   
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


import ltk.Parser as Parser

import io
import os
import sys
import datetime
import time
from abc import ABC, abstractmethod
from typing import List, Tuple, Any

class ListenerCommandError( Exception ):
   def __init__( self, message, *args, **kwargs ):
      super().__init__( message )

class Interpreter( ABC ):
   '''Interpreter interface used by Listener.
   To use the Listener class, the execution environment must be encapsulated
   behind the following interface.'''
   @abstractmethod
   def reboot( self ):
      '''Reboot the interpreter.'''
      pass

   @abstractmethod
   def eval( self, anExprStr: str, file=None ):
      '''Evaluate an expression string of the target language and return a
      string expr representing the result of the evaluation.

      Currently, Listener only understands how to deal with eval() that returns
      strings.  Future incarnations of Listener may recognize other return value
      types.

      The caller can supply streams to use in place of stdin, stdout and stderr.

      EXCEPTIONS:
         Implementation should bundle errors and exceptions such that only
         two kinds of exceptions leave
      '''
      pass


class Listener( object ):
   '''A generic Listener environment for dynamic languages.
   Heavily ripped-off from Python's own cmd library.'''
   prompt0 = '>>> '
   prompt1 = '... '
   ruler = '='

   def __init__( self, anInterpreter: Interpreter, libdir: str='', testdir: str='', **kwargs ) -> None:
      super().__init__( )

      self._interp          = anInterpreter
      self._logFile: Any    = None
      self._testdir         = testdir
      self._libdir          = libdir
      self._exceptInfo: Any = None
      self.writeLn( '{language:s} {version:s}'.format(**kwargs) )
      self.writeLn( '- Execution environment initialized.' )
      self.do_reboot( [ ] )

   def writeLn( self, value: str='', file=None ) -> None:
      print( value, file=file )
      if self._logFile:
         self._logFile.write( value + '\n' )

   def writeErrorMsg( self, errMsg: str, file=None ):
      errMsgLinesOfText = errMsg.splitlines()
      for errMsgLine in errMsgLinesOfText:
         self.writeLn( f'%%% {errMsgLine}', file=file )

   def prompt( self, prompt: str='' ) -> str:
      inputStr: str = input( prompt ).lstrip()
      if self._logFile and ((len(inputStr) == 0) or (inputStr[0] != ']')):
         self._logFile.write( f'{prompt}{inputStr}\n' )

      return inputStr

   def do_reboot( self, args: List[str] ) -> None:
      '''Usage: reboot
      Reset the interpreter.
      '''
      if len(args) > 0:
         raise ListenerCommandError( self.do_reboot.__doc__ )

      if self._logFile:
         raise ListenerCommandError( 'Please close the log before exiting or rebooting.' )

      self._interp.reboot( )
      print( '- Runtime environment reinitialized.' )

      # Read in the libraries
      filenameList = self.retrieveFileList( self._libdir )
      for filename in filenameList:
         self._sessionLog_restore( filename )
      print( '- Runtime libraries loaded.' )
      print( 'Listener started.' )
      print( 'Enter any expression to have it evaluated by the interpreter.')
      print( 'Enter \']help\' for listener commands.' )
      print( 'Welcome!' )

   def do_log( self, args: List[str] ) -> None:
      '''Usage:  log <filename>
      Begin a new logging session.
      '''
      if len(args) != 1:
         raise ListenerCommandError( self.do_log.__doc__ )

      filename = args[0]
      if self._logFile is not None:
         raise ListenerCommandError( 'Already logging.  Can\'t open more than one log file at a time.\n' )

      try:
         self._logFile = open( filename, 'w' )
      except OSError:
         raise ListenerCommandError( 'Unable to open file for writing.' )

      self.writeLn( '>>> ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;' )
      self.writeLn( '... ;;;;;;  Starting Log ( {0} ): {1}'.format( datetime.datetime.now().isoformat(), filename ) )
      self.writeLn( '... 0')
      self.writeLn( '' )
      self.writeLn( '==> 0')

   def do_read( self, args: List[str] ) -> None:
      '''Usage:  read <filename> [v|v]
      Read and execute a log file.  V is for verbose.
      '''
      if len(args) not in ( 1, 2 ):
         raise ListenerCommandError( self.do_read.__doc__ )

      verbosity: int=0
      if len(args) == 2:
         if args[1].upper() == 'V':
            verbosity=3

      filename: str = args[0]
      self._sessionLog_restore( filename, verbosity=verbosity )
      print( f'Log file read successfully: {filename}' )

   def do_test( self, args: List[str] ) -> None:
      '''Usage:  test <filename>
      Test the interpreter using a log file.
      Read and execute a log file;
      comparing the return value to the log file return value.
      If no test file is specified the listener will run the full standard set
      of tests for the interpreter.
      '''
      numArgs = len(args)
      if numArgs not in ( 0, 1 ):
         raise ListenerCommandError( self.do_test.__doc__ )

      if self._logFile:
         raise ListenerCommandError( "Please discontinue logging before running any tests." )

      # Collect the test filenames into a list
      if numArgs == 1:
         filenameList = args
      else:
         filenameList = self.retrieveFileList( self._testdir )

      # Conduct the testing
      testSummaryList: List[Tuple[str, str]] = [ ]
      for filename in filenameList:
         testResultMsg = self._sessionLog_test( filename, verbosity=3 )
         testSummaryList.append( (filename, testResultMsg) )

      # Summarize Test Results
      print( '\n\nTest Report' )
      print( '===========')
      for filename, testSummary in testSummaryList:
         print( f'{filename:45} {testSummary}' )

      print( '\nWARNING: Testing leaves the interpreter in an unknown state.' +
             'It\'s reccomended that you reboot the interpreter using the listener command ]reboot.' )

   def do_continue( self, args: List[str] ) -> None:
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
         raise ListenerCommandError( self.do_continue.__doc__ )

      self.do_read( args )

      filename = args[0]
      try:
         self._logFile = open( filename, 'a' )
      except OSError:
         raise ListenerCommandError( 'Unable to open file for append.' )

      self.writeLn( '>>> ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;' )
      self.writeLn( '... ;;;;;;  Continuing Log ( {0} ): {1}'.format( datetime.datetime.now().isoformat(), filename ) )
      self.writeLn( '... 0')
      self.writeLn( '' )
      self.writeLn( '==> 0')

   def do_close( self, args: List[str] ) -> None:
      '''Usage:  close
      Close the current logging session.
      '''
      if len(args) != 0:
         raise ListenerCommandError( self.do_close.__doc__ )

      if self._logFile is None:
         raise ListenerCommandError( "Not currently logging." )

      self.writeLn( '>>> ;;;;;;  Logging ended.' )
      self.writeLn( '... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;' )
      self.writeLn( '... 0')
      self.writeLn( '' )
      self.writeLn( '==> 0')

      self._logFile.close( )

      self._logFile = None

   def do_dump( self, args: List[str] ) -> None:
      '''Usage:  dump
      Dump a stack trace of the most recent error.
      '''
      if len(args) != 0:
         raise ListenerCommandError( self.do_dump.__doc__ )

      if self._exceptInfo is None:
         raise ListenerCommandError( 'No exception information available.\n' )

      sys.excepthook( *self._exceptInfo )

   def do_exit( self, args: List[str] ) -> None:
      '''Usage:  exit
      Exit the interpreter and listener.
      '''
      if self._logFile is not None:
         raise ListenerCommandError( "Logging must be stopped before you can exit." )

      if len(args) != 0:
         raise ListenerCommandError( self.do_exit.__doc__ )

      self.writeLn( 'Bye.' )
      raise StopIteration( )

   def do_help(self, args: List[str] ) -> None:
      '''Usage: help [<command>]
      List all available commands, or detailed help for a specific command.
      '''
      if len(args) > 0:
         arg = args[0]
         # XXX check arg syntax
         try:
            doc=getattr(self, f'do_{arg}').__doc__
            if doc:
               raise ListenerCommandError(str(doc))
         except AttributeError:
            pass
         raise ListenerCommandError( f"*** No help on {arg}." )
      else:
         names = dir(self.__class__)
         names.sort()
         cmds_doc = []
         # There can be duplicates if routines overridden
         prevname = ''
         for name in names:
            if name[:3] == 'do_':
               if name == prevname:
                  continue
               prevname = name
               cmd=name[3:]
               cmds_doc.append(cmd)
         print( )
         self.print_topics( cmds_doc, 15, 80 )

   def print_topics( self, cmds: List[str], cmdlen: int, maxcol: int ) -> None:
      header = "Documented commands (type help <topic>):"
      if cmds:
         print(str(header))
         if self.ruler:
            print(str(self.ruler * len(header)))
         self.columnize(cmds, maxcol-1)
         print()

   def columnize(self, list, displaywidth: int=80) -> None:
      """Display a list of strings as a compact set of columns.

      Each column is only as wide as necessary.
      Columns are separated by two spaces (one was not legible enough).
      """
      if not list:
         print("<empty>")
         return

      nonstrings = [i for i in range(len(list))
                    if not isinstance(list[i], str)]
      if nonstrings:
         raise TypeError("list[i] not a string for i in %s"
                         % ", ".join(map(str, nonstrings)))
      size = len(list)
      if size == 1:
         print(str(list[0]))
         return
      # Try every row count from 1 upwards
      for nrows in range(1, len(list)):
         ncols = (size+nrows-1) // nrows
         colwidths = []
         totwidth = -2
         for col in range(ncols):
            colwidth = 0
            for row in range(nrows):
               i = row + nrows*col
               if i >= size:
                  break
               x = list[i]
               colwidth = max(colwidth, len(x))
            colwidths.append(colwidth)
            totwidth += colwidth + 2
            if totwidth > displaywidth:
               break
         if totwidth <= displaywidth:
            break
      else:
         nrows = len(list)
         ncols = 1
         colwidths = [0]
      for row in range(nrows):
         texts = []
         for col in range(ncols):
            i = row + nrows*col
            if i >= size:
               x = ""
            else:
               x = list[i]
            texts.append(x)
         while texts and not texts[-1]:
            del texts[-1]
         for col in range(len(texts)):
            texts[col] = texts[col].ljust(colwidths[col])
         print(str("  ".join(texts)))

   def doCommand( self, listenerCommand: str ) -> None:
      cmdParts  = listenerCommand[1:].split( ' ' )
      cmd,*args = cmdParts

      try:
         func = getattr(self, f'do_{cmd}')
         func(args)
      except AttributeError:
         raise ListenerCommandError( f'Unknown command "{listenerCommand}"' )

   def readEvalPrintLoop( self ) -> None:
      inputExprLineList: List[str] = [ ]

      keepLooping = True
      while keepLooping:
         if len(inputExprLineList) == 0:
            lineInput = self.prompt( '>>> ' ).strip()
         else:
            lineInput = self.prompt( '... ' ).strip()

         if (lineInput == '') and (len(inputExprLineList) != 0):
            inputExprStr = ''.join( inputExprLineList ).strip()
            try:
               if inputExprStr[0] == ']':
                  self.doCommand( inputExprStr )
               else:
                  start = time.perf_counter( )
                  resultStr = self._interp.eval( inputExprStr )
                  cost  = time.perf_counter( ) - start
                  self.writeLn( f'\n==> {resultStr}' )
                  print( f'-------------  Total execution time:  {cost:15.5f} sec' )

            except StopIteration:
               keepLooping = False

            except Parser.ParseError as ex:
               self._exceptInfo = sys.exc_info( )
               self.writeErrorMsg( ex.generateVerboseErrorString() )

            except Exception as ex:
               self._exceptInfo = sys.exc_info( )
               self.writeErrorMsg( ex.args[-1] )

            self.writeLn( )

            inputExprLineList = [ ]

         else:
            inputExprLineList.append( lineInput + '\n' )

   def _sessionLog_restore( self, filename: str, verbosity: int=0 ) -> None:
      inputText = None
      with open( filename, 'r') as file:
         inputText = file.read( )

      if inputText is None:
         raise ListenerCommandError( f'Unable to read file {filename}.\n' )

      for exprNum,exprPackage in enumerate(self.parseLog(inputText)):
         exprStr,outputStr,retValStr,errMsgStr = exprPackage
         if verbosity == 0:
            self._interp.eval( exprStr )
         else:
            exprLines = exprStr.splitlines()
            for lineNum, line in enumerate(exprLines):
               if lineNum == 0:
                  print( f'\n>>> {line}' )
               else:
                  print( f'... {line}')

            resultStr = self._interp.eval( exprStr )
            print( f'\n==> {resultStr}' )

   def _sessionLog_test( self, filename: str, verbosity: int=3 ) -> str:
      inputText = None
      with open( filename, 'r') as file:
         inputText = file.read( )

      if inputText is None:
         raise ListenerCommandError( 'Unable to read file {filename}.\n' )

      print( f'   Test file: {filename}... ', end='' )

      if verbosity >= 3:
         print()

      numPassed = 0
      exprNum = -1
      actualRetValStr = ''
      actualOutputStr = ''
      actualErrorStr = ''
      for exprNum,exprPackage in enumerate(self.parseLog(inputText)):
         exprStr,expectedOutputStr,expectedRetValStr,expectedErrStr = exprPackage
         if verbosity == 2:
            print( f'{str(exprNum).rjust(8)}.' )
         elif verbosity == 3:
            print( f'{str(exprNum).rjust(8)}> {exprStr}' )

         # Perform the test and collect the various outputs
         errorStream = io.StringIO( )
         outputStream = io.StringIO( )

         try:
            actualRetValStr = self._interp.eval( exprStr, outputStream )
         except Parser.ParseError as ex:
            self.writeErrorMsg( ex.generateVerboseErrorString(), file=errorStream )
         except Exception as ex:
            self.writeErrorMsg( ex.args[-1], file=errorStream )

         actualOutputStr = outputStream.getvalue().strip()
         actualErrorStr = errorStream.getvalue().strip()

         # Assess the results
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


      numTests = exprNum + 1
      numFailed = numTests - numPassed
      resultMessage = ''
      if numFailed == 0:
         resultMessage = 'ALL TESTS PASSED!'
      else:
         resultMessage = f'({numFailed}/{numTests}) Failed.'

      print( resultMessage )
      return resultMessage

   def parseLog( self, inputText: str ) -> List[Tuple[str, str, str, str]]:
      stream = Parser.LineScanner( inputText )
      parsedLog = [ ]
      eof = False

      while not eof:
         expr = ''
         output = ''
         retVal = ''
         errMsg = ''

         # Skip to the begenning of an interaction prompt
         try:
            line = stream.peekLine()
            while not line.startswith( '>>> ' ):
               stream.consumeLine()
               line = stream.peekLine()

            # Parse Expression
            # string variable *line* begins with '>>> '
            if line.startswith( '>>> ' ):
               expr = line[ 4: ]
               stream.consumeLine()
               line = stream.peekLine()
               while not eof and line.startswith( '...' ):
                  expr += line[ 4: ]
                  stream.consumeLine()
                  line = stream.peekLine()

            # Parse Output from the evaluation (such as write statements)
            while not line.startswith( ('==> ','... ','>>> ', '%%% ') ):
               # Parse written output
               if output is None:
                  output = ''
               output += line
               stream.consumeLine()
               line = stream.peekLine()

            # Parse Return Value
            if line.startswith( '==> ' ):
               retVal = line[ 4: ]
               stream.consumeLine()
               line = stream.peekLine()
               while not eof and not line.startswith( ('==> ','... ','>>> ','%%% ') ):
                  retVal += line
                  stream.consumeLine()
                  line = stream.peekLine()

            if line.startswith( '%%% '):
               errMsg = line[4:]
               stream.consumeLine()
               line = stream.peekLine()
               while not eof and line.startswith( '%%% ' ):
                  errMsg += line[4:]
                  stream.consumeLine()
                  line = stream.peekLine()

         except StopIteration:
            eof = True

         if expr != '':
            parsedLog.append( (expr,output.rstrip(),retVal.rstrip(),errMsg) )

      return parsedLog

   def retrieveFileList( self, dirname: str='' ) -> List[str]:
      "Returns a list of all the filenames in the specified directory."
      testFileList = os.listdir( dirname )
      testFileList.sort()
      testFileList = [ f'{dirname}/{testFileName}' for testFileName in testFileList ]
      return testFileList

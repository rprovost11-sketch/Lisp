import os
import sys
import tempfile
from pathlib import Path
from typing import Any, Callable
from _io import TextIOWrapper

from pythonslisp.Environment import Environment
from pythonslisp.LispAST import LSymbol, LCallable, LPrimitive, LFunction, LMacro, prettyPrint, prettyPrintSExpr
from pythonslisp.LispAST import L_T, L_NIL
from pythonslisp.LispContext import LispContext
from pythonslisp.LispExceptions import LispRuntimeError, LispRuntimeFuncError
from pythonslisp.LispParser import ParseError
from pythonslisp.Utils import columnize


HELP_DIR = Path(__file__).parent.parent / 'help'


def register(primitive, parseLispString: Callable) -> None:

   def _decode_escapes( s: str ) -> str:
      """Interpret standard escape sequences without corrupting non-ASCII text."""
      return ( s.replace('\\\\', '\x00')
                .replace('\\n', '\n')
                .replace('\\t', '\t')
                .replace('\\"', '"')
                .replace('\x00', '\\') )

   def lwrite( outStrm, *values, end='' ):
      if not values:
         return []
      for value in values:
         valueStr = prettyPrintSExpr( value )
         valueStr = _decode_escapes( valueStr )
         print( valueStr, end='', file=outStrm )
      if end:
         print( end=end, file=outStrm )
      return values[-1]

   def luwrite( outStrm, *values, end='' ):
      if not values:
         return []
      for value in values:
         valueStr = prettyPrint( value )
         valueStr = _decode_escapes( valueStr )
         print( valueStr, end='', file=outStrm )
      if end:
         print( end=end, file=outStrm )
      return values[-1]

   def printHelpListings( outStrm, env: Environment ) -> None:
      primitivesList = []
      functionsList  = []
      macrosList     = []
      topicsList     = [ f'"{f.stem}"' for f in sorted(HELP_DIR.glob('*.txt')) ] if HELP_DIR.exists() else []
      outFile  = outStrm or sys.stdout
      useColor = hasattr(outFile, 'isatty') and outFile.isatty()

      BOLD_WHITE = '\033[1;97m' if useColor else ''
      CYAN       = '\033[96m'   if useColor else ''
      GREEN      = '\033[92m'   if useColor else ''
      MAGENTA    = '\033[95m'   if useColor else ''
      YELLOW     = '\033[93m'   if useColor else ''
      RESET      = '\033[0m'    if useColor else ''

      def hdr( text ):
         ul = '=' * len(text)
         print( f'{BOLD_WHITE}{text}{RESET}', file=outStrm )
         print( f'{BOLD_WHITE}{ul}{RESET}',   file=outStrm )

      for symbolStr in env.getGlobalEnv().localSymbols():
         if symbolStr.startswith('%') or symbolStr.endswith('-INTERNAL'):
            continue
         obj = env.lookupGlobal(symbolStr)
         if isinstance(obj, LPrimitive):
            primitivesList.append(symbolStr)
         elif isinstance(obj, LFunction):
            functionsList.append(symbolStr)
         elif isinstance(obj, LMacro):
            macrosList.append(symbolStr)

      hdr( "Predefined Symbols" )
      print( "E  NIL  PI  T", file=outStrm )
      print( file=outStrm )
      hdr( "Primitives" )
      columnize( primitivesList, 78, file=outStrm, itemColor=CYAN or None )
      print( file=outStrm )
      hdr( "Functions" )
      columnize( functionsList, 78, file=outStrm, itemColor=GREEN or None )
      print( file=outStrm )
      hdr( "Macros" )
      columnize( macrosList, 78, file=outStrm, itemColor=MAGENTA or None )
      print( file=outStrm )
      hdr( "TOPICS" )
      columnize( topicsList, 78, file=outStrm, itemColor=YELLOW or None )
      print( file=outStrm )
      print( "Type '(help callable)' for available documentation on a callable.", file=outStrm )
      print( "Type '(help \"topic\")' for available documentation on the named topic.", file=outStrm )

   # -----------------------------------------------------------------------

   @primitive( 'open-read', 'filename &optional encoding', 
               min_args=1, max_args=2, arity_msg='1 or 2 arguments expected.' )
   def LP_openRead( ctx: LispContext, env: Environment, *args):
      """Opens and returns a text stream for reading.  encoding may be \"utf-8\"."""
      fileName, *encoding = args
      if not isinstance( fileName, str):
         raise LispRuntimeFuncError( LP_openRead, "1st argument expected to be a filename string." )
      
      try:
         if len(encoding) == 1:
            encoding = encoding[0]
            return open( fileName, 'r', encoding=encoding )
         else:
            return open( fileName, 'r' )
      except FileNotFoundError:
         raise LispRuntimeFuncError( LP_openRead, f'File not found "{fileName}".' )
   
   @primitive( 'open-write', 'filename &optional encoding', 
               min_args=1, max_args=2, arity_msg='1 or 2 arguments expected.' )
   def LP_openWrite( ctx: LispContext, env: Environment, *args):
      """Opens and returns a text stream for writing.  encoding may be \"utf-8\"."""
      fileName, *encoding = args
      if not isinstance( fileName, str):
         raise LispRuntimeFuncError( LP_openWrite, "1st argument expected to be a filename string." )
      try:
         if len(encoding) == 1:
            encoding = encoding[0]
            return open( fileName, 'w', encoding=encoding )
         else:
            return open( fileName, 'w' )
      except FileNotFoundError:
         raise LispRuntimeFuncError( LP_openWrite, f'File not found "{fileName}".' )
   
   @primitive( 'open-append', 'filename &optional encoding', 
               min_args=1, max_args=2, arity_msg='1 or 2 arguments expected.' )
   def LP_openAppend( ctx: LispContext, env: Environment, *args):
      """Opens and returns a text stream for appending.  encoding may be \"utf-8\"."""
      fileName, *encoding = args
      if not isinstance( fileName, str):
         raise LispRuntimeFuncError( LP_openAppend, "1st argument expected to be a filename string." )
      try:
         if len(encoding) == 1:
            encoding = encoding[0]
            return open( fileName, 'a', encoding=encoding )
         else:
            return open( fileName, 'a' )
      except FileNotFoundError:
         raise LispRuntimeFuncError( LP_openAppend, f'File not found "{fileName}".' )

   @primitive( 'close', 'stream',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_close( ctx: LispContext, env: Environment, *args ) -> Any:
      """Closes a stream and returns t."""
      stream = args[0]
      if not isinstance(stream, TextIOWrapper ):
         raise LispRuntimeFuncError( LP_close, 'Argument expected to be a stream.' )
      stream.close( )
      return L_T

   @primitive( 'flush', '&optional stream',
               min_args=0, max_args=1, arity_msg='1 optional argument expected.' )
   def LP_flush( ctx: LispContext, env: Environment, *args ) -> Any:
      """Flushes a stream and returns t."""
      if len(args) == 1:
         stream = args[0]
         if not isinstance(stream, TextIOWrapper ):
            raise LispRuntimeFuncError( LP_flush, 'Argument expected to be a stream.' )
         stream.flush( )
      else:
         sys.stdout.flush()
      return L_T

   @primitive( 'closed', 'stream',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_closed( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if the stream is closed, nil otherwise."""
      stream = args[0]
      if not isinstance(stream, TextIOWrapper ):
         raise LispRuntimeFuncError( LP_closed, 'Argument expected to be a stream.' )
      return L_T if stream.closed else L_NIL

   @primitive( 'isatty', 'stream',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_isatty( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if the stream is a tty, nil otherwise."""
      stream = args[0]
      if not isinstance(stream, TextIOWrapper ):
         raise LispRuntimeFuncError( LP_isatty, 'Argument expected to be a stream.' )
      return L_T if stream.isatty() else L_NIL

   @primitive( 'readable', 'stream',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_readable( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if the stream is readable, nil otherwise."""
      stream = args[0]
      if not isinstance(stream, TextIOWrapper ):
         raise LispRuntimeFuncError( LP_readable, 'Argument expected to be a stream.' )
      return L_T if stream.readable() else L_NIL

   @primitive( 'writable', 'stream',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_writable( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if the stream is writable, nil otherwise."""
      stream = args[0]
      if not isinstance(stream, TextIOWrapper ):
         raise LispRuntimeFuncError( LP_writable, 'Argument expected to be a stream.' )
      return L_T if stream.writable() else L_NIL

   @primitive( 'tmpdir', '',
               min_args=0, max_args=0, arity_msg='0 arguments expected.' )
   def LP_tmpdir( ctx: LispContext, env: Environment, *args ) -> str:
      """Returns the system temporary directory as a string."""
      return tempfile.gettempdir()

   @primitive( 'path-join', 'path1 path2 ...',
               min_args=1, arity_msg='1 or more arguments expected.' )
   def LP_path_join( ctx: LispContext, env: Environment, *args ) -> str:
      """Joins path components using the OS path separator.  Returns the result as a string."""
      for i, arg in enumerate(args):
         if not isinstance(arg, str):
            raise LispRuntimeFuncError( LP_path_join, f'Argument {i+1} expected to be a string.' )
      return os.path.join(*args)

   @primitive( 'writef', 'formatString &optional dictOrList stream',
               min_args=1, max_args=3, arity_msg='1 to 3 arguments expected.' )
   def LP_writef( ctx: LispContext, env: Environment, *args ) -> str:
      """Writes formatted text.  Returns the string that is written.
Takes a Python format string and an optional map or list of values.
If no second argument is given, the format string is output unchanged.
Returns the output string."""
      formatString = args[0]
      if not isinstance( formatString, str ):
         raise LispRuntimeFuncError( LP_writef, "1st argument expected to be a format string." )

      numArgs = len(args)
      if numArgs == 1:
         dictOrList = None
         stream = ctx.outStrm
         formattedStr = formatString
      elif numArgs == 2:
         otherArg = args[1]
         if isinstance( otherArg, (list, dict)):
            dictOrList = otherArg
            stream = ctx.outStrm
         elif isinstance(otherArg, TextIOWrapper):
            dictOrList = None
            stream = otherArg
            if not stream.writable():
               raise LispRuntimeFuncError( LP_writef, 'Stream is not writable.' )
         else:
            raise LispRuntimeFuncError( LP_writef, "2nd argument expected to be a list, map or stream." )
      else: # numArgs == 3
         dictOrList, stream = args[1:]
         if not isinstance(dictOrList, (list, dict)):
            raise LispRuntimeFuncError( LP_writef, '2nd argument expected to be a list or map.' )
         if not isinstance(stream, TextIOWrapper):
            raise LispRuntimeFuncError( LP_writef, '3rd argument expected to be a stream.' )
         if not stream.writable():
            raise LispRuntimeFuncError( LP_writef, 'Stream is not writable.' )
      
      try:
         if dictOrList is None:
            formattedStr = formatString
         elif isinstance( dictOrList, list ):
            formattedStr = formatString.format( *dictOrList )
         elif isinstance( dictOrList, dict ):
            formattedStr = formatString.format( **dictOrList )
         else:
            raise LispRuntimeFuncError( LP_writef, "2nd argument expected to be a list or map." )
      except (IndexError, KeyError, ValueError) as e:
         raise LispRuntimeFuncError( LP_writef, f"Format error: {e}" )

      outputStr = _decode_escapes( formattedStr )
      print( outputStr, end='', file=stream )
      return outputStr

   @primitive( 'write!', 'obj1 obj2 ... &optional stream',
               min_args=1, arity_msg='1 or more arguments expected.' )
   def LP_write( ctx: LispContext, env: Environment, *args ) -> Any:
      """Sequentially prettyPrints in programmer readable text the objects listed.
Returns the last value printed."""
      if isinstance( args[-1], TextIOWrapper):
         stream = args[-1]
         args = args[0:-1]
         if not stream.writable():
            raise LispRuntimeFuncError( LP_write, 'Stream is not writable.' )
      else:
         stream = ctx.outStrm
      return lwrite( stream, *args, end='' )

   @primitive( 'writeLn!', 'obj1 obj2 ... &optional stream',
               min_args=1, arity_msg='1 or more arguments expected.' )
   def LP_writeln( ctx: LispContext, env: Environment, *args ) -> Any:
      """Sequentially prettyPrints in programmer readable text the objects listed.
Terminates the output with a newline character.  Returns the last value printed."""
      if isinstance(args[-1], TextIOWrapper):
         stream = args[-1]
         args = args[0:-1]
         if not stream.writable():
            raise LispRuntimeFuncError( LP_writeln, 'Stream is not writable.' )
      else:
         stream = ctx.outStrm
      return lwrite( stream, *args, end='\n' )

   @primitive( 'uwrite!', 'obj1 obj2 ... &optional stream',
               min_args=1, arity_msg='1 or more arguments expected.' )
   def LP_uwrite( ctx: LispContext, env: Environment, *args ) -> Any:
      """Sequentially prettyPrints in user readable text the objects listed.  Returns the last value printed."""
      if isinstance(args[-1], TextIOWrapper):
         stream = args[-1]
         args = args[0:-1]
         if not stream.writable():
            raise LispRuntimeFuncError( LP_uwrite, 'Stream is not writable.' )
      else:
         stream = ctx.outStrm
      return luwrite( stream, *args, end='' )

   @primitive( 'uwriteLn!', 'obj1 obj2 ... &optional stream',
               min_args=1, arity_msg='1 or more arguments expected.' )
   def LP_uwriteln( ctx: LispContext, env: Environment, *args ) -> Any:
      """Sequentially prettyPrints in user readable text the objects listed.
Terminates the output with a newline character.  Returns the last value printed."""
      if isinstance(args[-1], TextIOWrapper):
         stream = args[-1]
         args = args[0:-1]
         if not stream.writable():
            raise LispRuntimeFuncError( LP_uwriteln, 'Stream is not writable.' )
      else:
         stream = ctx.outStrm
      return luwrite( stream, *args, end='\n' )

   @primitive( 'terpri', '&optional stream',
               min_args=0, max_args=1, arity_msg='0 or 1 argument expected.' )
   def LP_terpri( ctx: LispContext, env: Environment, *args ) -> Any:
      """Outputs a newline character.  Returns NIL."""
      if len(args) == 0:
         stream = ctx.outStrm
      else:
         stream = args[0]
         if not isinstance(stream, TextIOWrapper):
            raise LispRuntimeFuncError( LP_terpri, "Optional argument expected to be a stream." )
         if not stream.writable():
            raise LispRuntimeFuncError( LP_terpri, 'Stream is not writable.' )
      print( end='\n', file=stream )
      return L_NIL

   @primitive( 'readall', 'stream',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_readall( ctx: LispContext, env: Environment, *args ) -> Any:
      """Reads and returns the entire contents of a readable stream as a single string."""
      stream = args[0]
      if not isinstance(stream, TextIOWrapper):
         raise LispRuntimeFuncError( LP_readall, 'Argument expected to be a stream.' )
      if not stream.readable():
         raise LispRuntimeFuncError( LP_readall, 'Stream is not readable.' )
      return stream.read()

   @primitive( 'readLn!', '&optional stream',
               min_args=0, max_args=1, arity_msg='1 optional argument expected.' )
   def LP_readln( ctx: LispContext, env: Environment, *args ) -> Any:
      """Reads and returns text input from standard input or stream.  For console input
This function blocks while it waits for the input return key to be pressed at the end
of text entry."""
      if len(args) == 0:
         return input()
      else:
         stream = args[0]
         if not isinstance(stream, TextIOWrapper):
            raise LispRuntimeFuncError( LP_readln, 'Optional argument expected to be a stream.' )
         if not stream.readable():
            raise LispRuntimeFuncError( LP_readln, 'Stream is not readable.' )
         return stream.readline(-1)

   @primitive( 'save', 'filename obj1 obj2 ...',
               min_args=1, arity_msg='1 or more arguments expected.' )
   def LP_save( ctx: LispContext, env: Environment, *args ) -> Any:
      """Saves python object to a text file."""
      filename, *objs = args
      if not isinstance(filename, str):
         raise LispRuntimeFuncError( LP_save, '1st argument expected to be a filename.' )
      with open( filename, 'w', encoding='utf-8' ) as st:
         lines = [ f'{prettyPrintSExpr(obj)}\n' for obj in objs ]
         st.writelines( lines )
      return L_NIL

   @primitive( 'load', 'fileName',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_load( ctx: LispContext, env: Environment, *args ) -> Any:
      """Loads a lisp source file.  Returns a progn of the parsed contents of the file."""
      filename = args[0]
      if not isinstance(filename, str):
         raise LispRuntimeFuncError( LP_load, 'Argument expected to be a filename.' )
      try:
         with open( filename, 'r', encoding='utf-8' ) as f:
            content = f.read()
      except FileNotFoundError:
         raise LispRuntimeFuncError( LP_load, f'File not found "{filename}".' )
      return parseLispString( content )   # (progn form1 form2 ...)

   @primitive( 'error', 'formatString &optional dictOrList',
               min_args=1, max_args=2, arity_msg='1 or 2 arguments expected.' )
   def LP_error( ctx: LispContext, env: Environment, *args ) -> Any:
      """Signals a runtime error with the given message string.
The format string may optionally be followed by a list or map of values,
in which case the message is formatted using Python str.format() before
being raised.  With no second argument the format string is used as-is."""
      formatString = args[0]
      if not isinstance( formatString, str ):
         raise LispRuntimeFuncError( LP_error, 'Argument 1 must be a String.' )
      if len(args) == 1:
         raise LispRuntimeError( formatString )
      dictOrList = args[1]
      try:
         if isinstance( dictOrList, list ):
            message = formatString.format( *dictOrList )
         elif isinstance( dictOrList, dict ):
            message = formatString.format( **dictOrList )
         else:
            raise LispRuntimeFuncError( LP_error, '2nd argument expected to be a list or map.' )
      except (IndexError, KeyError, ValueError) as e:
         raise LispRuntimeFuncError( LP_error, f'Format error: {e}' )
      raise LispRuntimeError( message )

   @primitive( 'parse', 'string',
               min_args=1, max_args=1, arity_msg='1 string argument expected.' )
   def LP_parse( ctx: LispContext, env: Environment, *args ) -> Any:
      """Parses the string as a Lisp sexpression and returns the resulting expression tree."""
      theExprStr = args[0]
      if not isinstance(theExprStr, str):
         raise LispRuntimeFuncError( LP_parse, 'Argument expected to be a string.' )
      return parseLispString( theExprStr )

   @primitive( 'python', 'string',
               min_args=1, max_args=1, arity_msg='1 string argument expected by python.' )
   def LP_python( ctx: LispContext, env: Environment, *args ) -> Any:
      """Executes some python code from Lisp."""
      thePythonCode = args[0]
      if not isinstance(thePythonCode, str):
         raise LispRuntimeFuncError( LP_python, 'Argument expected to be a string.' )
      theReturnVal = eval( thePythonCode, globals(), locals() )
      return theReturnVal

   @primitive( 'recursion-limit', '&optional newLimit',
               min_args=0, max_args=1, arity_msg='Only one optional arg is allowed.' )
   def LP_recursionlimit( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns or sets the system recursion limit.  The higher the integer
argument the deeper the recursion will be allowed to go.  If setting,
returns newLimit upon success."""
      if len(args) == 0:
         return sys.getrecursionlimit()
      try:
         newLimit = int(args[0])
         sys.setrecursionlimit(newLimit)
         return newLimit
      except (TypeError, ValueError):
         raise LispRuntimeFuncError( LP_recursionlimit, 'Argument must be an integer.' )

   @primitive( 'help', '&optional callableSymbol' )
   def LP_help( ctx: LispContext, env: Environment, *args ) -> Any:
      """Prints a set of tables for all the globally defined symbols and
topics currently available in Python's Lisp. Or prints the usage and
documentation for a specific callable (primitive, function or macro) or topic.

Type '(help callable)' for available documentation on a callable.
Type '(help "topic")' for available documentation on the named topic."""
      numArgs = len(args)
      if numArgs > 1:
         raise LispRuntimeFuncError( LP_help, f'Too many arguments.  Received {numArgs}' )

      elif numArgs == 0:
         printHelpListings( ctx.outStrm, env )
         return L_T

      # numArgs == 1
      arg = args[0]
      if isinstance(arg, str):
         topicName = arg.upper()
         topicFile = HELP_DIR / f'{topicName}.txt'
         if topicFile.exists():
            print( topicFile.read_text( encoding='utf-8' ), file=ctx.outStrm )
         else:
            print( f'Unknown topic: "{topicName}"', file=ctx.outStrm )
         return L_T

      if not isinstance(arg, LCallable):
         raise LispRuntimeFuncError( LP_help, 'First argument expected to be a callable.' )
      callableObj = arg

      outStrm  = ctx.outStrm
      outFile  = outStrm or sys.stdout
      useColor = hasattr(outFile, 'isatty') and outFile.isatty()
      BOLD_WHITE = '\033[1;97m' if useColor else ''
      CYAN       = '\033[96m'   if useColor else ''
      RESET      = '\033[0m'    if useColor else ''

      print( f'   {BOLD_WHITE}USAGE:{RESET} {CYAN}{callableObj.usageString()}{RESET}', file=outStrm )
      print( file=outStrm )
      if callableObj.docString != '':
         valueStr = prettyPrint( callableObj.docString )
         valueStr = _decode_escapes( valueStr )
         print( valueStr, file=outStrm )

      return L_T

   @primitive( 'define-help-topic', 'name-string text-string',
               min_args=2, max_args=2, arity_msg='2 arguments expected.' )
   def LP_define_help_topic( ctx: LispContext, env: Environment, *args ) -> Any:
      """Defines a new help topic by writing a text file to the help directory.
The topic is immediately available via (help \"name\").  Returns the topic name
as a symbol.  An existing topic with the same name is overwritten."""
      name, text = args
      if not isinstance( name, str ):
         raise LispRuntimeFuncError( LP_define_help_topic, 'Argument 1 must be a string (topic name).' )
      if not isinstance( text, str ):
         raise LispRuntimeFuncError( LP_define_help_topic, 'Argument 2 must be a string (topic text).' )
      HELP_DIR.mkdir( exist_ok=True )
      topicFile = HELP_DIR / f'{name.upper()}.txt'
      topicFile.write_text( text, encoding='utf-8' )
      return LSymbol( name )

   @primitive( 'undefine-help-topic', 'name-string',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_undefine_help_topic( ctx: LispContext, env: Environment, *args ) -> Any:
      """Removes a help topic by deleting its file from the help directory.
Returns T if the topic existed and was removed, NIL if the topic was not found."""
      name = args[0]
      if not isinstance( name, str ):
         raise LispRuntimeFuncError( LP_undefine_help_topic, 'Argument 1 must be a string (topic name).' )
      topicFile = HELP_DIR / f'{name.upper()}.txt'
      if topicFile.exists():
         topicFile.unlink()
         return L_T
      return L_NIL

import sys
from pathlib import Path
from typing import Any, Callable

from pythonslisp.Environment import Environment
from pythonslisp.LispAST import LSymbol, LCallable, LPrimitive, LFunction, LMacro, prettyPrint, prettyPrintSExpr
from pythonslisp.LispAST import L_T, L_NIL
from pythonslisp.LispExceptions import LispRuntimeError, LispRuntimeFuncError
from pythonslisp.LispInterpreter import LispInterpreter
from pythonslisp.LispParser import ParseError
from pythonslisp.Listener import columnize


HELP_DIR = Path(__file__).parent.parent / 'help'


def register(primitive, parseLispString: Callable) -> None:

   def _decode_escapes( s: str ) -> str:
      """Interpret standard escape sequences without corrupting non-ASCII text."""
      return ( s.replace('\\\\', '\x00')
                .replace('\\n', '\n')
                .replace('\\t', '\t')
                .replace('\\"', '"')
                .replace('\x00', '\\') )

   def lwrite( *values, end='' ):
      if not values:
         return []
      for value in values:
         valueStr = prettyPrintSExpr( value )
         valueStr = _decode_escapes( valueStr )
         print( valueStr, end='', file=LispInterpreter.outStrm )
      if end:
         print( end=end, file=LispInterpreter.outStrm )
      return values[-1]

   def luwrite( *values, end='' ):
      if not values:
         return []
      for value in values:
         valueStr = prettyPrint( value )
         valueStr = _decode_escapes( valueStr )
         print( valueStr, end='', file=LispInterpreter.outStrm )
      if end:
         print( end=end, file=LispInterpreter.outStrm )
      return values[-1]

   def printHelpListings( env: Environment ) -> None:
      primitivesList = []
      functionsList  = []
      macrosList     = []
      topicsList     = [ f'"{f.stem}"' for f in sorted(HELP_DIR.glob('*.txt')) ] if HELP_DIR.exists() else []
      outStrm  = LispInterpreter.outStrm
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
      print( "Type '(help <callable>)' for available documentation on a callable.", file=outStrm )
      print( "Type '(help \"topic\")' for available documentation on the named topic.", file=outStrm )

   # -----------------------------------------------------------------------

   @primitive( 'writef', '<formatString> <MapOrList>' )
   def LP_writef( env: Environment, *args ) -> str:
      """Writes formatted text.  Returns the string that is written.
Takes a python f-string and a map or list of values for the f-string.
Returns the formatted output string."""
      try:
         formatString, mapOrList = args
      except ValueError:
         raise LispRuntimeFuncError( LP_writef, "2 arguments expected." )

      if not isinstance( formatString, str ):
         raise LispRuntimeFuncError( LP_writef, "1st argument expected to be a format string." )

      try:
         if isinstance( mapOrList, list ):
            formattedStr = formatString.format( *mapOrList )
         elif isinstance( mapOrList, dict ):
            formattedStr = formatString.format( **mapOrList )
         else:
            raise LispRuntimeFuncError( LP_writef, "2nd argument expected to be a list or map." )
      except (IndexError, KeyError, ValueError) as e:
         raise LispRuntimeFuncError( LP_writef, f"Format error: {e}" )

      outputStr = _decode_escapes( formattedStr )
      print( outputStr, end='', file=LispInterpreter.outStrm )
      return outputStr

   @primitive( 'write!', '<obj1> <obj2> ...' )
   def LP_write( env: Environment, *args ) -> Any:
      """Sequentially prettyPrints in programmer readable text the objects listed.
Returns the last value printed."""
      if len(args) == 0:
         raise LispRuntimeFuncError( LP_write, '1 or more arguments expected.' )
      return lwrite( *args, end='' )

   @primitive( 'writeLn!', '<obj1> <obj2> ...' )
   def LP_writeln( env: Environment, *args ) -> Any:
      """Sequentially prettyPrints in programmer readable text the objects listed.
Terminates the output with a newline character.  Returns the last value printed."""
      if len(args) == 0:
         raise LispRuntimeFuncError( LP_writeln, '1 or more arguments expected.' )
      return lwrite( *args, end='\n' )

   @primitive( 'uwrite!', '<obj1> <obj2> ...' )
   def LP_uwrite( env: Environment, *args ) -> Any:
      """Sequentially prettyPrints in user readable text the objects listed.  Returns the last value printed."""
      if len(args) == 0:
         raise LispRuntimeFuncError( LP_uwrite, '1 or more arguments expected.' )
      return luwrite( *args, end='' )

   @primitive( 'uwriteLn!', '<obj1> <obj2> ...' )
   def LP_uwriteln( env: Environment, *args ) -> Any:
      """Sequentially prettyPrints in user readable text the objects listed.
Terminates the output with a newline character.  Returns the last value printed."""
      if len(args) == 0:
         raise LispRuntimeFuncError( LP_uwriteln, '1 or more arguments expected.' )
      return luwrite( *args, end='\n' )

   @primitive( 'terpri', '' )
   def LP_terpri( env: Environment, *args ) -> Any:
      """Outputs a newline character.  Returns NIL."""
      if len(args) > 0:
         raise LispRuntimeFuncError( LP_terpri, '0 arguments expected.' )
      print( end='\n', file=LispInterpreter.outStrm )
      return L_NIL

   @primitive( 'readLn!', '' )
   def LP_readln( env: Environment, *args ) -> Any:
      """Reads and returns text input from standard input.  This function blocks
while it waits for the input return key to be pressed at the end of text entry."""
      if len(args) > 0:
         raise LispRuntimeFuncError( LP_readln, '0 arguments expected.' )
      return input()

   @primitive( 'error', '<message-string>' )
   def LP_error( env: Environment, *args ) -> Any:
      """Signals a runtime error with the given message string."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_error, '1 argument expected.' )
      if not isinstance( args[0], str ):
         raise LispRuntimeFuncError( LP_error, 'Argument 1 must be a String.' )
      raise LispRuntimeError( args[0] )

   @primitive( 'parse', '<string>' )
   def LP_parse( env: Environment, *args ) -> Any:
      """Parses the string as a Lisp sexpression and returns the resulting expression tree."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_parse, '1 string argument expected.' )
      theExprStr = args[0]
      if not isinstance(theExprStr, str):
         raise LispRuntimeFuncError( LP_parse, 'Argument expected to be a string.' )
      return parseLispString( theExprStr )

   @primitive( 'python', '<string>' )
   def LP_python( env: Environment, *args ) -> Any:
      """Executes some python code from Lisp."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_python, '1 string argument expected by python.' )
      thePythonCode = args[0]
      if not isinstance(thePythonCode, str):
         raise LispRuntimeFuncError( LP_python, 'Argument expected to be a string.' )
      theReturnVal = eval( thePythonCode, globals(), locals() )
      return theReturnVal

   @primitive( 'recursion-limit', '&optional <newLimit>' )
   def LP_recursionlimit( env: Environment, *args ) -> Any:
      """Returns or sets the system recursion limit.  The higher the integer
argument the deeper the recursion will be allowed to go.  If setting,
returns newLimit upon success."""
      numArgs = len(args)
      if numArgs == 0:
         return sys.getrecursionlimit()
      elif numArgs == 1:
         try:
            newLimit = int(args[0])
            sys.setrecursionlimit(newLimit)
            return newLimit
         except (TypeError, ValueError):
            raise LispRuntimeFuncError( LP_recursionlimit, 'Argument must be an integer.' )
      else:
         raise LispRuntimeFuncError( LP_recursionlimit, 'Only one optional arg is allowed.' )

   @primitive( 'help', '&optional callableSymbol' )
   def LP_help( env: Environment, *args ) -> Any:
      """Prints a set of tables for all the globally defined symbols and
topics currently available in Python's Lisp. Or prints the usage and
documentation for a specific callable (primitive, function or macro) or topic.

Type '(help <callable>)' for available documentation on a callable.
Type '(help "topic")' for available documentation on the named topic."""
      numArgs = len(args)
      if numArgs > 1:
         raise LispRuntimeFuncError( LP_help, f'Too many arguments.  Received {numArgs}' )

      elif numArgs == 0:
         printHelpListings( env )
         return L_T

      # numArgs == 1
      arg = args[0]
      if isinstance(arg, str):
         topicName = arg.upper()
         topicFile = HELP_DIR / f'{topicName}.txt'
         if topicFile.exists():
            print( topicFile.read_text( encoding='utf-8' ), file=LispInterpreter.outStrm )
         else:
            print( f'Unknown topic: "{topicName}"', file=LispInterpreter.outStrm )
         return L_T

      if not isinstance(arg, LCallable):
         raise LispRuntimeFuncError( LP_help, 'First argument expected to be a callable.' )
      callableObj = arg

      outStrm  = LispInterpreter.outStrm
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

   @primitive( 'define-help-topic', '<name-string> <text-string>' )
   def LP_define_help_topic( env: Environment, *args ) -> Any:
      """Defines a new help topic by writing a text file to the help directory.
The topic is immediately available via (help \"name\").  Returns the topic name
as a symbol.  An existing topic with the same name is overwritten."""
      if len(args) != 2:
         raise LispRuntimeFuncError( LP_define_help_topic, '2 arguments expected.' )
      name, text = args
      if not isinstance( name, str ):
         raise LispRuntimeFuncError( LP_define_help_topic, 'Argument 1 must be a string (topic name).' )
      if not isinstance( text, str ):
         raise LispRuntimeFuncError( LP_define_help_topic, 'Argument 2 must be a string (topic text).' )
      HELP_DIR.mkdir( exist_ok=True )
      topicFile = HELP_DIR / f'{name.upper()}.txt'
      topicFile.write_text( text, encoding='utf-8' )
      return LSymbol( name )

   @primitive( 'undefine-help-topic', '<name-string>' )
   def LP_undefine_help_topic( env: Environment, *args ) -> Any:
      """Removes a help topic by deleting its file from the help directory.
Returns T if the topic existed and was removed, NIL if the topic was not found."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_undefine_help_topic, '1 argument expected.' )
      name = args[0]
      if not isinstance( name, str ):
         raise LispRuntimeFuncError( LP_undefine_help_topic, 'Argument 1 must be a string (topic name).' )
      topicFile = HELP_DIR / f'{name.upper()}.txt'
      if topicFile.exists():
         topicFile.unlink()
         return L_T
      return L_NIL

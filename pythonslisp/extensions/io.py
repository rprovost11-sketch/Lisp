from __future__ import annotations
import io
import os
import sys
import tempfile
from pathlib import Path
from typing import Any
from io import IOBase, StringIO

from pythonslisp.Environment import Environment
from pythonslisp.AST import LSymbol, LCallable, LPrimitive, LFunction, LMacro, prettyPrint, prettyPrintSExpr, got_str
from pythonslisp.AST import L_T, L_NIL
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimeError, LRuntimePrimError
from pythonslisp.Parser import ParseError
from pythonslisp.ltk.Utils import columnize
from pythonslisp.extensions import LambdaListMode, primitive
from pythonslisp.ltk.Highlighter import render_markdown

# Platform-specific readline module (same logic as Listener)
_rl = None
if sys.platform == 'win32':
   try:
      import pythonslisp.ltk.readline_win as _rl
   except ImportError:
      pass
else:
   try:
      import readline as _rl
   except ImportError:
      pass


HELP_DIR = Path(__file__).parent.parent / 'help'


def lwrite( outStrm, *values, end='' ):
   if not values:
      return []
   for value in values:
      valueStr = prettyPrintSExpr( value )
      print( valueStr, end='', file=outStrm )
   if end:
      print( end=end, file=outStrm )
   return values[-1]

def luwrite( outStrm, *values, end='' ):
   if not values:
      return []
   for value in values:
      valueStr = prettyPrint( value )
      print( valueStr, end='', file=outStrm )
   if end:
      print( end=end, file=outStrm )
   return values[-1]

def _get_output_stream( ctx: Context, env: Environment ) -> Any:
   """Return the current default output stream.
If *standard-output* has been locally rebound (e.g. inside a let form)
to something other than its global value, that stream is used, enabling
output capture via (let ((*standard-output* s)) ...).  Otherwise
ctx.outStrm is used, preserving the test runner's ability to redirect
output at the Python level."""
   try:
      local_val  = env.lookup( '*STANDARD-OUTPUT*' )
      global_val = env.lookupGlobalWithDefault( '*STANDARD-OUTPUT*', None )
      if local_val is not global_val and isinstance( local_val, IOBase ) and local_val.writable():
         return local_val
   except Exception:
      pass
   return ctx.outStrm

def is_struct_descriptor( obj ) -> bool:
   return isinstance(obj, dict) and obj.get(LSymbol('STRUCT-TYPE')) == LSymbol('%STRUCT-DESCRIPTOR%')

def print_struct_help( descriptor, outStrm ) -> None:
   name    = descriptor.get(LSymbol('NAME'), LSymbol('?'))
   docstr  = descriptor.get(LSymbol('DOCSTRING'), L_NIL)
   fields  = descriptor.get(LSymbol('FIELDS'), [])
   nameStr = name.name if isinstance(name, LSymbol) else str(name)
   nameLow = nameStr.lower()
   field_names = [ f[0].name.lower()
                   for f in fields
                   if isinstance(f, list) and f and isinstance(f[0], LSymbol) ]
   print( f'STRUCT  {nameStr}', file=outStrm )
   if isinstance(docstr, str) and docstr:
      print( file=outStrm )
      print( f'   {docstr}', file=outStrm )
   print( file=outStrm )
   print( 'Fields:', file=outStrm )
   if isinstance(fields, list) and fields:
      for fspec in fields:
         if isinstance(fspec, list) and fspec and isinstance(fspec[0], LSymbol):
            fname    = fspec[0].name
            fdefault = prettyPrintSExpr(fspec[1]) if len(fspec) > 1 else 'NIL'
            print( f'   {fname:<20} default: {fdefault}', file=outStrm )
   else:
      print( '   (none)', file=outStrm )
   print( file=outStrm )
   key_args = ' '.join(field_names)
   print( f'Constructor: (make-{nameLow} &key {key_args})', file=outStrm )
   print( f'Predicate:   ({nameLow}-p obj)', file=outStrm )
   if field_names:
      accessors = '  '.join(f'{nameLow}-{n}' for n in field_names)
      print( f'Accessors:   {accessors}', file=outStrm )
   print( f'Copier:      (copy-{nameLow} inst)', file=outStrm )

def printHelpListings( outStrm, env, find: str | None = None ) -> None:
   # Create bins to sort symbols into
   variablesList = []
   primitivesList = []
   functionsList  = []
   macrosList     = []
   typesList    = []
   findUpper = find.upper() if find is not None else None
   if HELP_DIR.exists():
      from collections import defaultdict
      raw_groups: dict[str, set] = defaultdict(set)
      for f in list(HELP_DIR.glob('**/*.txt')) + list(HELP_DIR.glob('**/*.md')):
         rel   = f.relative_to(HELP_DIR)
         group = rel.parts[0] if len(rel.parts) > 1 else ''
         raw_groups[group].add(f.stem.upper())
      if findUpper is not None:
         topic_groups = {g: sorted(s for s in stems if findUpper in s)
                         for g, stems in raw_groups.items()}
         topic_groups = {g: v for g, v in topic_groups.items() if v}
      else:
         topic_groups = {g: sorted(stems) for g, stems in raw_groups.items()}
   else:
      topic_groups = {}
   outFile  = outStrm or sys.stdout
   useColor = hasattr(outFile, 'isatty') and outFile.isatty()

   # Define the color strings
   BOLD_WHITE = '\033[1;97m' if useColor else ''
   CYAN       = '\033[96m'   if useColor else ''
   GREEN      = '\033[92m'   if useColor else ''
   MAGENTA    = '\033[95m'   if useColor else ''
   YELLOW     = '\033[93m'   if useColor else ''
   RESET      = '\033[0m'    if useColor else ''

   # Print a table header
   def hdr( text ):
      ul = '=' * len(text)
      print( f'{BOLD_WHITE}{text}{RESET}', file=outStrm )
      print( f'{BOLD_WHITE}{ul}{RESET}',   file=outStrm )

   # Bin the global symbols into the various lists, filtering by substring if requested
   for symbolStr in env.getGlobalEnv().localSymbols():
      if symbolStr.startswith('%') or symbolStr.endswith('-INTERNAL'):
         continue
      if findUpper is not None and findUpper not in symbolStr:
         continue
      obj = env.lookupGlobal(symbolStr)
      if isinstance(obj, LPrimitive):
         primitivesList.append(symbolStr)
      elif isinstance(obj, LFunction):
         functionsList.append(symbolStr)
      elif isinstance(obj, LMacro):
         macrosList.append(symbolStr)
      elif is_struct_descriptor(obj):
         typesList.append(symbolStr)
      else:
         variablesList.append(symbolStr)

   # Print out the table
   hdr( "Predefined Symbols" )
   columnize( variablesList, 78, file=outStrm )
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
   hdr( "Types" )
   columnize( typesList, 78, file=outStrm, itemColor=YELLOW or None )
   print( file=outStrm )
   hdr( "TOPICS" )
   def subhdr( text: str ) -> None:
      ul = '-' * len(text)
      print( f'{BOLD_WHITE}{text}{RESET}', file=outStrm )
      print( f'{BOLD_WHITE}{ul}{RESET}',   file=outStrm )
   for group in sorted(topic_groups):
      label = group.title() if group else 'General'
      items = [f'"{s}"' for s in topic_groups[group]]
      print( file=outStrm )
      subhdr( label )
      columnize( items, 78, file=outStrm, itemColor=YELLOW or None )
   print( file=outStrm )
   print( "Type '(help callable)' for available documentation on a callable.", file=outStrm )
   print( "Type '(help \"topic\")' for available documentation on the named topic.", file=outStrm )
   print( "Type '(help \"substring\" :substring t)' to search all names by substring.", file=outStrm )


@primitive( 'open',
            '(filespec &key (direction :input) (if-exists :supersede) (if-does-not-exist :error))',
            mode=LambdaListMode.FULL_BINDING )
def LP_open( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Opens and returns a stream connected to a file.
:direction :input (default) opens for reading; :output opens for writing.
:if-exists controls behaviour when an output file already exists:
  :supersede (default) - truncate and overwrite
  :append              - append to existing content
  :error               - signal an error
  nil                  - return nil without opening
:if-does-not-exist controls behaviour when the file is absent:
  :error (default) - signal an error
  nil              - return nil without opening"""
   filespec  = env.lookup( 'FILESPEC' )
   direction = env.lookup( 'DIRECTION' )
   if_exists = env.lookup( 'IF-EXISTS' )
   if_dne    = env.lookup( 'IF-DOES-NOT-EXIST' )
   if not isinstance( filespec, str ):
      raise LRuntimePrimError( LP_open, f'Invalid argument 1. STRING FILE PATH expected{got_str(filespec)}.' )
   if direction == LSymbol(':INPUT'):
      if isinstance( if_dne, list ) and not os.path.exists( filespec ):
         return L_NIL
      try:
         return open( filespec, 'r' )
      except FileNotFoundError:
         raise LRuntimePrimError( LP_open, f'File not found "{filespec}".' )
   elif direction == LSymbol(':OUTPUT'):
      if isinstance( if_exists, list ):           # nil
         if os.path.exists( filespec ):
            return L_NIL
         mode_str = 'w'
      elif if_exists == LSymbol(':SUPERSEDE'):
         mode_str = 'w'
      elif if_exists == LSymbol(':APPEND'):
         mode_str = 'a'
      elif if_exists == LSymbol(':ERROR'):
         if os.path.exists( filespec ):
            raise LRuntimePrimError( LP_open, f'File already exists "{filespec}".' )
         mode_str = 'w'
      else:
         raise LRuntimePrimError( LP_open,
            ':if-exists must be :supersede, :append, :error, or nil.' )
      if isinstance( if_dne, list ) and not os.path.exists( filespec ):
         return L_NIL
      try:
         return open( filespec, mode_str )
      except (FileNotFoundError, OSError):
         raise LRuntimePrimError( LP_open, f'Cannot open file "{filespec}".' )
   else:
      raise LRuntimePrimError( LP_open, ':direction must be :input or :output.' )

@primitive( 'make-string-output-stream', '()' )
def LP_make_string_output_stream( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Creates and returns a new string output stream for writing.  Use
get-output-stream-string to retrieve and clear the accumulated content."""
   return StringIO()

@primitive( 'make-string-input-stream', '(string &optional (start 0) end)',
            mode=LambdaListMode.FULL_BINDING )
def LP_make_string_input_stream( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Creates and returns a readable string stream backed by string.
Optionally constrained to the substring from start (default 0) up to
but not including end (default: entire string)."""
   s     = env.lookup( 'STRING' )
   start = env.lookup( 'START' )
   end   = env.lookup( 'END' )
   if not isinstance( s, str ):
      raise LRuntimePrimError( LP_make_string_input_stream,
                                  f'Invalid argument 1. STRING expected{got_str(s)}.' )
   start_py = start if isinstance( start, int ) else 0
   end_py   = end   if isinstance( end, int )   else None
   return StringIO( s[start_py:end_py] )

@primitive( 'get-output-stream-string', '(string-stream)' )
def LP_get_output_stream_string( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the string accumulated in string-stream since it was created or
since the last call to get-output-stream-string, then clears the buffer.
The stream remains open and writable.  (CL semantics.)"""
   stream = args[0]
   if not isinstance( stream, StringIO ):
      raise LRuntimePrimError( LP_get_output_stream_string,
                                  f'Invalid argument 1. STRING OUTPUT STREAM expected{got_str(stream)}.' )
   if stream.closed:
      raise LRuntimePrimError( LP_get_output_stream_string,
                                  'String stream is closed.' )
   content = stream.getvalue()
   stream.seek( 0 )
   stream.truncate( 0 )
   return content

@primitive( 'close', '(stream &key (abort nil))', mode=LambdaListMode.FULL_BINDING )
def LP_close( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Closes a stream and returns T.
The :abort keyword argument is accepted for CL compatibility but is ignored
in this implementation (flushing on close cannot be suppressed)."""
   stream = env.lookup( 'STREAM' )
   if not isinstance(stream, IOBase):
      raise LRuntimePrimError( LP_close, f'Invalid argument 1. STREAM expected{got_str(stream)}.' )
   stream.close()
   return L_T

@primitive( 'flush', '(&optional stream)' )
def LP_flush( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Flushes a stream and returns t."""
   if len(args) == 1:
      stream = args[0]
      if not isinstance(stream, IOBase):
         raise LRuntimePrimError( LP_flush, f'Invalid argument 1. STREAM expected{got_str(stream)}.' )
      stream.flush( )
   else:
      sys.stdout.flush()
   return L_T

@primitive( 'open-stream-p', '(stream)' )
def LP_open_stream_p( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns T if the stream is open, NIL if it is closed."""
   stream = args[0]
   if not isinstance(stream, IOBase):
      raise LRuntimePrimError( LP_open_stream_p, f'Invalid argument 1. STREAM expected{got_str(stream)}.' )
   return L_NIL if stream.closed else L_T

@primitive( 'interactive-stream-p', '(stream)' )
def LP_interactive_stream_p( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns T if the stream is interactive (connected to a terminal), NIL otherwise."""
   stream = args[0]
   if not isinstance(stream, IOBase):
      raise LRuntimePrimError( LP_interactive_stream_p, f'Invalid argument 1. STREAM expected{got_str(stream)}.' )
   return L_T if stream.isatty() else L_NIL

@primitive( 'input-stream-p', '(stream)' )
def LP_input_stream_p( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns T if the stream can be read from, NIL otherwise."""
   stream = args[0]
   if not isinstance(stream, IOBase):
      raise LRuntimePrimError( LP_input_stream_p, f'Invalid argument 1. STREAM expected{got_str(stream)}.' )
   return L_T if stream.readable() else L_NIL

@primitive( 'output-stream-p', '(stream)' )
def LP_output_stream_p( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns T if the stream can be written to, NIL otherwise."""
   stream = args[0]
   if not isinstance(stream, IOBase):
      raise LRuntimePrimError( LP_output_stream_p, f'Invalid argument 1. STREAM expected{got_str(stream)}.' )
   return L_T if stream.writable() else L_NIL

@primitive( 'stdin', '()' )
def LP_stdin( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the standard input stream (sys.stdin)."""
   if isinstance( sys.stdin, IOBase ):
      return sys.stdin
   if sys.__stdin__ is not None:
      return sys.__stdin__
   return sys.stdin

@primitive( 'stdout', '()' )
def LP_stdout( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the standard output stream (sys.stdout)."""
   if isinstance( sys.stdout, IOBase ):
      return sys.stdout
   if sys.__stdout__ is not None:
      return sys.__stdout__
   return sys.stdout

@primitive( 'stderr', '()' )
def LP_stderr( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the standard error stream (sys.stderr)."""
   if isinstance( sys.stderr, IOBase ):
      return sys.stderr
   if sys.__stderr__ is not None:
      return sys.__stderr__
   return sys.stderr

@primitive( 'tmpdir', '()' )
def LP_tmpdir( ctx: Context, env: Environment, args: list[Any] ) -> str:
   """Returns the system temporary directory as a string."""
   return tempfile.gettempdir()

@primitive( 'path-join', '(path-segment &rest more-segments)' )
def LP_path_join( ctx: Context, env: Environment, args: list[Any] ) -> str:
   """Joins path-segments using the OS path separator.  Returns the result as a string."""
   for i, arg in enumerate(args):
      if not isinstance(arg, str):
         raise LRuntimePrimError( LP_path_join, f'Invalid argument {i+1}. STRING expected{got_str(arg)}.' )
   return os.path.join(*args)

@primitive( 'writef', '(formatString &optional dictOrList stream)' )
def LP_writef( ctx: Context, env: Environment, args: list[Any] ) -> str:
   """Writes formatted text.  Returns the string that is written.
Takes a Python format string and an optional map or list of values.
If no second argument is given, the format string is output unchanged.
Returns the output string."""
   formatString = args[0]
   if not isinstance( formatString, str ):
      raise LRuntimePrimError( LP_writef, f'Invalid argument 1. FORMAT STRING expected{got_str(formatString)}.' )

   numArgs = len(args)
   if numArgs == 1:
      dictOrList = None
      stream = _get_output_stream( ctx, env )
      formattedStr = formatString
   elif numArgs == 2:
      otherArg = args[1]
      if isinstance( otherArg, (list, dict)):
         dictOrList = otherArg
         stream = _get_output_stream( ctx, env )
      elif isinstance(otherArg, IOBase):
         dictOrList = None
         stream = otherArg
         if not stream.writable():
            raise LRuntimePrimError( LP_writef, 'Stream is not writable.' )
      else:
         raise LRuntimePrimError( LP_writef, f'Invalid argument 2. LIST, DICT, or STREAM expected{got_str(otherArg)}.' )
   else: # numArgs == 3
      dictOrList, stream = args[1:]
      if not isinstance(dictOrList, (list, dict)):
         raise LRuntimePrimError( LP_writef, f'Invalid argument 2. LIST or DICT expected{got_str(dictOrList)}.' )
      if not isinstance(stream, IOBase):
         raise LRuntimePrimError( LP_writef, f'Invalid argument 3. STREAM expected{got_str(stream)}.' )
      if not stream.writable():
         raise LRuntimePrimError( LP_writef, 'Stream is not writable.' )

   try:
      if dictOrList is None:
         formattedStr = formatString
      elif isinstance( dictOrList, list ):
         formattedStr = formatString.format( *dictOrList )
      elif isinstance( dictOrList, dict ):
         strDict = { (k.name if isinstance(k, LSymbol) else k): v for k, v in dictOrList.items() }
         formattedStr = formatString.format( **strDict )
      else:
         raise LRuntimePrimError( LP_writef, 'Invalid argument 2. LIST or DICT expected.' )
   except (IndexError, KeyError, ValueError) as e:
      raise LRuntimePrimError( LP_writef, f"Format error: {e}" )

   print( formattedStr, end='', file=stream )
   return formattedStr

@primitive( 'write!', '(&optional stream &rest objects)' )
def LP_write( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Sequentially prettyPrints in programmer readable text the objects listed.
Returns the last value printed.  The optional first argument is a stream to which
the output is written.  If stream is omitted, output goes to stdout."""
   if args and isinstance( args[0], IOBase):
      stream = args[0]
      args = args[1:]
      if not stream.writable():
         raise LRuntimePrimError( LP_write, 'Stream is not writable.' )
   else:
      stream = _get_output_stream( ctx, env )
   return lwrite( stream, *args, end='' )

@primitive( 'write-line', '(&optional stream &rest objects)' )
def LP_write_line( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Sequentially prettyPrints in programmer readable text the objects listed.
Terminates the output with a newline character.  The optional first argument is
a stream to which the output is written.  If stream is omitted, output goes to stdout.
Returns the last value printed."""
   if args and isinstance( args[0], IOBase):
      stream = args[0]
      args = args[1:]
      if not stream.writable():
         raise LRuntimePrimError( LP_write_line, 'Stream is not writable.' )
   else:
      stream = _get_output_stream( ctx, env )
   return lwrite( stream, *args, end='\n' )

@primitive( 'uwrite!', '(&optional stream &rest objects)' )
def LP_uwrite( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Sequentially prettyPrints in user readable text the objects listed.
The optional first argument is a stream to which the output is written.
If stream is omitted, output goes to stdout.  Returns the last value printed."""
   if args and isinstance( args[0], IOBase):
      stream = args[0]
      args = args[1:]
      if not stream.writable():
         raise LRuntimePrimError( LP_uwrite, 'Stream is not writable.' )
   else:
      stream = _get_output_stream( ctx, env )
   return luwrite( stream, *args, end='' )

@primitive( 'uwrite-line', '(&optional stream &rest objects)' )
def LP_uwrite_line( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Sequentially prettyPrints in user readable text the objects listed.
Terminates the output with a newline character.  The optional first argument is
a stream to which the output is written.  If stream is omitted, output goes to stdout.
Returns the last value printed."""
   if args and isinstance( args[0], IOBase):
      stream = args[0]
      args = args[1:]
      if not stream.writable():
         raise LRuntimePrimError( LP_uwrite_line, 'Stream is not writable.' )
   else:
      stream = _get_output_stream( ctx, env )
   return luwrite( stream, *args, end='\n' )

@primitive( 'terpri', '(&optional stream)' )
def LP_terpri( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Outputs a newline character.  Returns NIL."""
   if len(args) == 0:
      stream = _get_output_stream( ctx, env )
   else:
      stream = args[0]
      if not isinstance(stream, IOBase):
         raise LRuntimePrimError( LP_terpri, f'Invalid argument 1. STREAM expected{got_str(stream)}.' )
      if not stream.writable():
         raise LRuntimePrimError( LP_terpri, 'Stream is not writable.' )
   print( end='\n', file=stream )
   return L_NIL

@primitive( 'readall', '(stream)' )
def LP_readall( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Reads and returns the entire contents of a readable stream as a single string."""
   stream = args[0]
   if not isinstance(stream, IOBase):
      raise LRuntimePrimError( LP_readall, f'Invalid argument 1. STREAM expected{got_str(stream)}.' )
   if not stream.readable():
      raise LRuntimePrimError( LP_readall, 'Stream is not readable.' )
   return stream.read()

@primitive( 'read-line', '(&optional stream (eof-error-p t) eof-value recursive-p)' )
def LP_read_line( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Reads one line of text from stream (default: standard input) and
returns it as a string, without the trailing newline character.
At end of file: signals an error if eof-error-p is T (default), otherwise
returns eof-value (default NIL).  recursive-p is accepted but ignored."""
   stream       = None
   eof_error_p  = True
   eof_value    = L_NIL
   if len( args ) >= 1 and args[0] is not L_NIL:
      stream = args[0]
      if not isinstance( stream, IOBase ):
         raise LRuntimePrimError( LP_read_line, f'Invalid argument 1. STREAM expected{got_str(stream)}.' )
   if len( args ) >= 2:
      eof_error_p = args[1] is not L_NIL
   if len( args ) >= 3:
      eof_value = args[2]
   if stream is None:
      try:
         return input( )
      except EOFError:
         if eof_error_p:
            raise LRuntimePrimError( LP_read_line, 'End of file on standard input.' )
         return eof_value
   if not stream.readable( ):
      raise LRuntimePrimError( LP_read_line, 'Stream is not readable.' )
   line = stream.readline( )
   if line == '':
      if eof_error_p:
         raise LRuntimePrimError( LP_read_line, 'End of file.' )
      return eof_value
   return line[:-1] if line.endswith( '\n' ) else line

@primitive( 'read-char', '(&optional stream (eof-error-p t) eof-value recursive-p)' )
def LP_read_char( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Reads and returns the next character from stream (default: standard input)
as a one-character string.
At end of file: signals an error if eof-error-p is T (default), otherwise
returns eof-value (default NIL).  recursive-p is accepted but ignored."""
   stream       = None
   eof_error_p  = True
   eof_value    = L_NIL
   if len( args ) >= 1 and args[0] is not L_NIL:
      stream = args[0]
      if not isinstance( stream, IOBase ):
         raise LRuntimePrimError( LP_read_char, f'Invalid argument 1. STREAM expected{got_str(stream)}.' )
   if len( args ) >= 2:
      eof_error_p = args[1] is not L_NIL
   if len( args ) >= 3:
      eof_value = args[2]
   if stream is None:
      ch = sys.stdin.read( 1 )
      if ch == '':
         if eof_error_p:
            raise LRuntimePrimError( LP_read_char, 'End of file on standard input.' )
         return eof_value
      return ch
   if not stream.readable( ):
      raise LRuntimePrimError( LP_read_char, 'Stream is not readable.' )
   ch = stream.read( 1 )
   if ch == '':
      if eof_error_p:
         raise LRuntimePrimError( LP_read_char, 'End of file.' )
      return eof_value
   return ch

@primitive( 'read', '(&optional stream (eof-error-p t) eof-value recursive-p)' )
def LP_read( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Reads and returns one s-expression from stream (default: standard input),
without evaluating it.  At end of file, signals an error if eof-error-p is T
(default), otherwise returns eof-value (default NIL).
recursive-p is accepted but ignored.
Seekable streams (files, string streams) are supported via tell/seek.
Non-seekable streams (stdin) are read line by line until a complete expression
is accumulated."""
   stream       = None
   eof_error_p  = True
   eof_value    = L_NIL
   if len( args ) >= 1 and args[0] is not L_NIL:
      stream = args[0]
      if not isinstance( stream, IOBase ):
         raise LRuntimePrimError( LP_read, f'Invalid argument 1. STREAM expected{got_str(stream)}.' )
   if len( args ) >= 2:
      eof_error_p = args[1] is not L_NIL
   if len( args ) >= 3:
      eof_value = args[2]
   if stream is None:
      stream = sys.stdin
   if not stream.readable( ):
      raise LRuntimePrimError( LP_read, 'Stream is not readable.' )
   if stream.seekable( ):
      pos     = stream.tell( )
      content = stream.read( )
      if not content:
         if eof_error_p:
            raise LRuntimePrimError( LP_read, 'End of file.' )
         return eof_value
      try:
         ast, chars_consumed = ctx.parseOne( content )
      except ParseError as exc:
         raise LRuntimePrimError( LP_read, f'Parse error: {exc}.' )
      stream.seek( pos + chars_consumed )
      return ast
   else:
      accumulated = ''
      while True:
         line = stream.readline( )
         if line == '':
            if not accumulated:
               if eof_error_p:
                  raise LRuntimePrimError( LP_read, 'End of file.' )
               return eof_value
            break
         accumulated += line
         try:
            ast, _ = ctx.parseOne( accumulated )
            return ast
         except ParseError:
            continue
      try:
         ast, _ = ctx.parseOne( accumulated )
         return ast
      except ParseError as exc:
         raise LRuntimePrimError( LP_read, f'Parse error: {exc}.' )

@primitive( 'save', '(filename &rest objects)' )
def LP_save( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Saves python object to a text file."""
   filename, *objs = args
   if not isinstance(filename, str):
      raise LRuntimePrimError( LP_save, f'Invalid argument 1. STRING FILE PATH expected{got_str(filename)}.' )
   with open( filename, 'w', encoding='utf-8' ) as st:
      lines = [ f'{prettyPrintSExpr(obj)}\n' for obj in objs ]
      st.writelines( lines )
   return L_NIL

@primitive( 'load', '(fileName)' )
def LP_load( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Loads a lisp source file.  Returns a progn of the parsed contents of the file."""
   filename = args[0]
   if not isinstance(filename, str):
      raise LRuntimePrimError( LP_load, f'Invalid argument 1. STRING FILE PATH expected{got_str(filename)}.' )
   try:
      with open( filename, 'r', encoding='utf-8' ) as f:
         content = f.read()
   except FileNotFoundError:
      raise LRuntimePrimError( LP_load, f'File not found "{filename}".' )
   return ctx.parse( content )   # (progn form1 form2 ...)

@primitive( 'error', '(formatString &optional dictOrList)' )
def LP_error( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Signals a runtime error with the given message string.
The format string may optionally be followed by a list or map of values,
in which case the message is formatted using Python str.format() before
being raised.  With no second argument the format string is used as-is."""
   formatString = args[0]
   if not isinstance( formatString, str ):
      raise LRuntimePrimError( LP_error, f'Invalid argument 1. STRING expected{got_str(formatString)}.' )
   if len(args) == 1:
      raise LRuntimeError( formatString )
   dictOrList = args[1]
   try:
      if isinstance( dictOrList, list ):
         message = formatString.format( *dictOrList )
      elif isinstance( dictOrList, dict ):
         strDict = { (k.name if isinstance(k, LSymbol) else k): v for k, v in dictOrList.items() }
         message = formatString.format( **strDict )
      else:
         raise LRuntimePrimError( LP_error, f'Invalid argument 2. LIST or DICT expected{got_str(dictOrList)}.' )
   except (IndexError, KeyError, ValueError) as e:
      raise LRuntimePrimError( LP_error, f'Format error: {e}' )
   raise LRuntimeError( message )

@primitive( 'parse', '(string)' )
def LP_parse( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Parses the string as a Lisp sexpression and returns the resulting expression tree."""
   theExprStr = args[0]
   if not isinstance(theExprStr, str):
      raise LRuntimePrimError( LP_parse, f'Invalid argument 1. STRING expected{got_str(theExprStr)}.' )
   return ctx.parse( theExprStr )

@primitive( 'python', '(string)' )
def LP_python( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Executes some python code from Lisp."""
   thePythonCode = args[0]
   if not isinstance(thePythonCode, str):
      raise LRuntimePrimError( LP_python, f'Invalid argument 1. STRING expected{got_str(thePythonCode)}.' )
   theReturnVal = eval( thePythonCode, globals(), locals() )
   return theReturnVal

@primitive( 'help', '(&optional target &key (substring nil))',
            mode=LambdaListMode.FULL_BINDING )
def LP_help( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Prints a set of tables for all the globally defined symbols and
topics currently available in Python's Lisp. Or prints the usage and
documentation for a specific callable (primitive, function or macro) or topic.

Type '(help callable)' for available documentation on a callable.
Type '(help "topic")' for available documentation on the named topic.
Type '(help "substring" :substring t)' to search all names by substring."""
   target    = env.lookup( 'TARGET' )
   substring = env.lookup( 'SUBSTRING' )

   if substring is not L_NIL:
      if not isinstance( target, str ):
         raise LRuntimePrimError( LP_help, f':substring t requires a string target{got_str(target)}.' )
      printHelpListings( ctx.outStrm, env, find=target )
      return L_T

   if isinstance( target, list ) and not target:   # NIL - no target given
      printHelpListings( ctx.outStrm, env )
      return L_T

   pos_arg = target

   # Positional arg: show specific callable or topic
   if isinstance(pos_arg, str):
      topicName = pos_arg.upper()
      topicMd  = next(HELP_DIR.glob(f'**/{topicName}.md'),  None)
      topicTxt = next(HELP_DIR.glob(f'**/{topicName}.txt'), None)
      if topicMd is not None:
         outFile   = ctx.outStrm or sys.stdout
         use_color = ( hasattr(outFile, 'isatty')
                       and outFile.isatty()
                       and os.environ.get('NO_COLOR') is None
                       and os.environ.get('TERM') != 'dumb' )
         content  = topicMd.read_text( encoding='utf-8' )
         rendered = render_markdown( content, use_color=use_color )
         print( rendered, file=ctx.outStrm )
      elif topicTxt is not None:
         print( topicTxt.read_text( encoding='utf-8' ), file=ctx.outStrm )
      else:
         print( f'Unknown topic: "{topicName}"', file=ctx.outStrm )
      return L_T

   # If passed a symbol, resolve it to its global value
   if isinstance(pos_arg, LSymbol):
      try:
         pos_arg = env.lookupGlobal(pos_arg.name)
      except KeyError:
         raise LRuntimePrimError( LP_help, f'Unbound variable: {pos_arg.name}.' )

   if is_struct_descriptor(pos_arg):
      print_struct_help( pos_arg, ctx.outStrm )
      return L_T

   if not isinstance(pos_arg, LCallable):
      raise LRuntimePrimError( LP_help, 'First argument expected to be a callable or struct type.' )
   callableObj = pos_arg

   outStrm  = ctx.outStrm
   outFile  = outStrm or sys.stdout
   useColor = hasattr(outFile, 'isatty') and outFile.isatty()
   CYAN  = '\033[96m' if useColor else ''
   RESET = '\033[0m'  if useColor else ''

   print( f'{callableObj.typeLabel()}  |  args: pre-evaluated', file=outStrm )
   print( file=outStrm )
   print( f'   {CYAN}Usage: {callableObj.callForm()}{RESET}', file=outStrm )
   print( file=outStrm )
   if callableObj.docString != '':
      valueStr = prettyPrint( callableObj.docString )
      print( valueStr, file=outStrm )

   return L_T

@primitive( 'define-help-topic', '(name-string text-string &key (category ""))',
            mode=LambdaListMode.FULL_BINDING )
def LP_define_help_topic( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Defines a new help topic by writing a text file to the help directory.
The topic is immediately available via (help \"name\").  Returns the topic name
as a symbol.  An existing topic with the same name is overwritten.
:category names a subdirectory within the help directory (created if needed)."""
   name     = env.lookup( 'NAME-STRING' )
   text     = env.lookup( 'TEXT-STRING' )
   category = env.lookup( 'CATEGORY' )
   if not isinstance( name, str ):
      raise LRuntimePrimError( LP_define_help_topic, f'Invalid argument 1. STRING expected{got_str(name)}.' )
   if not isinstance( text, str ):
      raise LRuntimePrimError( LP_define_help_topic, f'Invalid argument 2. STRING expected{got_str(text)}.' )
   if not isinstance( category, str ):
      raise LRuntimePrimError( LP_define_help_topic, f'Invalid keyword :category. STRING expected{got_str(category)}.' )
   target_dir = HELP_DIR / category if category else HELP_DIR
   target_dir.mkdir( parents=True, exist_ok=True )
   topicFile  = target_dir / f'{name.upper()}.txt'
   topicFile.write_text( text, encoding='utf-8' )
   return LSymbol( name )

@primitive( 'undefine-help-topic', '(name-string)' )
def LP_undefine_help_topic( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Removes a help topic by deleting its file from the help directory.
Returns T if the topic existed and was removed, NIL if the topic was not found."""
   name = args[0]
   if not isinstance( name, str ):
      raise LRuntimePrimError( LP_undefine_help_topic, f'Invalid argument 1. STRING expected{got_str(name)}.' )
   topicFile = next(HELP_DIR.glob(f'**/{name.upper()}.txt'), None)
   if topicFile is not None:
      topicFile.unlink()
      return L_T
   return L_NIL

@primitive( 'directory-files', '(dir &optional extension)' )
def LP_directory_files( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a sorted list of full file paths in dir.
With an optional extension string (e.g. \".log\"), only files with that
extension are returned.  Directories are excluded.  Returns NIL if dir
does not exist or is empty."""
   dirPath = args[0]
   if not isinstance( dirPath, str ):
      raise LRuntimePrimError( LP_directory_files, f'Invalid argument 1. STRING expected{got_str(dirPath)}.' )
   ext = None
   if len(args) == 2:
      ext = args[1]
      if not isinstance( ext, str ):
         raise LRuntimePrimError( LP_directory_files, f'Invalid argument 2. STRING expected{got_str(ext)}.' )
   if not os.path.isdir( dirPath ):
      return L_NIL
   entries = sorted( os.listdir( dirPath ) )
   result = []
   for name in entries:
      fullPath = os.path.join( dirPath, name )
      if not os.path.isfile( fullPath ):
         continue
      if ext is not None and not name.endswith( ext ):
         continue
      result.append( fullPath )
   return result

@primitive( 'make-directory', '(path)' )
def LP_make_directory( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Creates the directory at path, including any missing parent directories.
Does nothing if the directory already exists.  Returns the path string."""
   path = args[0]
   if not isinstance( path, str ):
      raise LRuntimePrimError( LP_make_directory, f'Invalid argument 1. STRING expected{got_str(path)}.' )
   os.makedirs( path, exist_ok=True )
   return path

@primitive( 'file-basename', '(path)' )
def LP_file_basename( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the final component of a file path (the filename without
the directory prefix).  E.g. (file-basename \"/foo/bar/baz.log\") => \"baz.log\"."""
   path = args[0]
   if not isinstance( path, str ):
      raise LRuntimePrimError( LP_file_basename, f'Invalid argument 1. STRING expected{got_str(path)}.' )
   return os.path.basename( path )

@primitive( 'readline-add-history', '(string)' )
def LP_readline_add_history( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Adds string to the readline input history.
Has no effect if readline is not available.  Returns the string."""
   s = args[0]
   if not isinstance( s, str ):
      raise LRuntimePrimError( LP_readline_add_history, f'Invalid argument 1. STRING expected{got_str(s)}.' )
   if _rl is not None:
      _rl.add_history( s )
   return s

@primitive( 'readline-set-history-length', '(n)' )
def LP_readline_set_history_length( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Sets the maximum number of entries kept in readline history.
Has no effect if readline is not available.  Returns n."""
   n = args[0]
   if not isinstance( n, int ) or isinstance( n, bool ):
      raise LRuntimePrimError( LP_readline_set_history_length, f'Invalid argument 1. INTEGER expected{got_str(n)}.' )
   if n < 1:
      raise LRuntimePrimError( LP_readline_set_history_length, 'Invalid argument 1. Positive INTEGER expected.' )
   if _rl is not None:
      _rl.set_history_length( n )
   return n

@primitive( 'readline-read-history-file', '(&optional path)' )
def LP_readline_read_history_file( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Loads readline history from path (default: ~/.lisp_history).
Has no effect if readline is not available.  Returns T on success, NIL if
the file does not exist."""
   path = args[0] if args else os.path.expanduser( '~/.lisp_history' )
   if not isinstance( path, str ):
      raise LRuntimePrimError( LP_readline_read_history_file, f'Invalid argument 1. STRING PATH expected{got_str(path)}.' )
   if _rl is None:
      return L_NIL
   try:
      _rl.read_history_file( path )
      return L_T
   except FileNotFoundError:
      return L_NIL

@primitive( 'readline-write-history-file', '(&optional path)' )
def LP_readline_write_history_file( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Saves readline history to path (default: ~/.lisp_history).
Has no effect if readline is not available.  Returns T on success."""
   path = args[0] if args else os.path.expanduser( '~/.lisp_history' )
   if not isinstance( path, str ):
      raise LRuntimePrimError( LP_readline_write_history_file, f'Invalid argument 1. STRING PATH expected{got_str(path)}.' )
   if _rl is None:
      return L_NIL
   _rl.write_history_file( path )
   return L_T

@primitive( 'columnize', '(list width &optional stream)' )
def LP_columnize( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Prints the elements of list as a compact multi-column layout fitting
within width characters.  Columns are separated by two spaces; each column
is only as wide as its widest item.  An optional stream argument selects
the output destination (default: current output stream).
All list elements must be strings.  Returns NIL."""
   lst = args[0]
   width = args[1]
   outFile = args[2] if len(args) == 3 else ctx.outStrm
   if not isinstance( lst, list ):
      raise LRuntimePrimError( LP_columnize, f'Invalid argument 1. LIST expected{got_str(lst)}.' )
   if not isinstance( width, int ) or isinstance( width, bool ):
      raise LRuntimePrimError( LP_columnize, f'Invalid argument 2. INTEGER expected{got_str(width)}.' )
   for i, item in enumerate( lst ):
      if not isinstance( item, str ):
         raise LRuntimePrimError( LP_columnize, f'Invalid element {i+1}. STRING expected{got_str(item)}.' )
   if not lst:
      return L_NIL
   columnize( lst, width, file=outFile )
   return L_NIL

from __future__ import annotations
from typing import Any, Callable

from pythonslisp.Environment import Environment
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimeUsageError
from pythonslisp.extensions import LambdaListMode, primitive
from pythonslisp.extensions.sequences import _validate_bounds
from pythonslisp.AST import LSymbol, prettyPrint, prettyPrintSExpr, got_str


@primitive( 'string-upcase', '(string)' )
def LP_string_upcase( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a copy of string with all characters converted to uppercase."""
   if not isinstance( args[0], str ):
      raise LRuntimeUsageError( LP_string_upcase, f'Invalid argument 1. STRING expected{got_str(args[0])}.' )
   return args[0].upper()

@primitive( 'string-downcase', '(string)' )
def LP_string_downcase( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a copy of string with all characters converted to lowercase."""
   if not isinstance( args[0], str ):
      raise LRuntimeUsageError( LP_string_downcase, f'Invalid argument 1. STRING expected{got_str(args[0])}.' )
   return args[0].lower()

@primitive( 'string-trim', '(char-bag string)' )
def LP_string_trim( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Removes leading and trailing characters in char-bag from string."""
   charBag, s = args[0], args[1]
   if not isinstance( charBag, str ):
      raise LRuntimeUsageError( LP_string_trim, f'Invalid argument 1. STRING expected{got_str(charBag)}.' )
   if not isinstance( s, str ):
      raise LRuntimeUsageError( LP_string_trim, f'Invalid argument 2. STRING expected{got_str(s)}.' )
   return s.strip( charBag )

@primitive( 'string-left-trim', '(char-bag string)' )
def LP_string_left_trim( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Removes leading characters in char-bag from string."""
   charBag, s = args[0], args[1]
   if not isinstance( charBag, str ):
      raise LRuntimeUsageError( LP_string_left_trim, f'Invalid argument 1. STRING expected{got_str(charBag)}.' )
   if not isinstance( s, str ):
      raise LRuntimeUsageError( LP_string_left_trim, f'Invalid argument 2. STRING expected{got_str(s)}.' )
   return s.lstrip( charBag )

@primitive( 'string-right-trim', '(char-bag string)' )
def LP_string_right_trim( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Removes trailing characters in char-bag from string."""
   charBag, s = args[0], args[1]
   if not isinstance( charBag, str ):
      raise LRuntimeUsageError( LP_string_right_trim, f'Invalid argument 1. STRING expected{got_str(charBag)}.' )
   if not isinstance( s, str ):
      raise LRuntimeUsageError( LP_string_right_trim, f'Invalid argument 2. STRING expected{got_str(s)}.' )
   return s.rstrip( charBag )

@primitive( 'char-code', '(char)' )
def LP_char_code( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the integer character code of a single-character string."""
   if not isinstance( args[0], str ) or len( args[0] ) != 1:
      raise LRuntimeUsageError( LP_char_code, f'Invalid argument 1. SINGLE-CHARACTER STRING expected{got_str(args[0])}.' )
   return ord( args[0] )

@primitive( 'code-char', '(integer)' )
def LP_code_char( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the single-character string corresponding to an integer character code."""
   if not isinstance( args[0], int ):
      raise LRuntimeUsageError( LP_code_char, f'Invalid argument 1. INTEGER expected{got_str(args[0])}.' )
   return chr( args[0] )

def _cl_capitalize( s: str ) -> str:
   """CL word-boundary capitalize: first alpha/digit in a word → upper,
rest → lower.  Non-alphanumeric characters end the current word."""
   result   = []
   in_word  = False
   for c in s:
      if c.isalpha() or c.isdigit():
         result.append( c.upper() if not in_word else c.lower() )
         in_word = True
      else:
         result.append( c )
         in_word = False
   return ''.join( result )

@primitive( 'string-capitalize', '(string &key (start 0) (end nil))',
            mode=LambdaListMode.FULL_BINDING )
def LP_string_capitalize( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a copy of string with CL word-boundary capitalization applied.
Optional :start and :end bound the region affected; text outside is unchanged."""
   s       = env.lookup( 'STRING' )
   start   = env.lookup( 'START' )
   end     = env.lookup( 'END' )
   if not isinstance( s, str ):
      raise LRuntimeUsageError( LP_string_capitalize, f'Invalid argument 1. STRING expected{got_str(s)}.' )
   start_n, end_n = _validate_bounds( start, end, len(s), LP_string_capitalize )
   prefix   = s[:start_n]
   middle   = s[start_n:end_n]
   suffix   = s[end_n:]
   return prefix + _cl_capitalize( middle ) + suffix

@primitive( 'string-join', '(separator list)' )
def LP_string_join( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Joins the elements of list into a single string, separated by separator.
Each element is converted via prettyPrintSExpr, except strings which are
used as-is (without surrounding quotes)."""
   if not isinstance( args[0], str ):
      raise LRuntimeUsageError( LP_string_join, f'Invalid argument 1. STRING expected{got_str(args[0])}.' )
   if not isinstance( args[1], list ):
      raise LRuntimeUsageError( LP_string_join, f'Invalid argument 2. LIST expected{got_str(args[1])}.' )
   parts = [ (elt if isinstance( elt, str ) else prettyPrint( elt ))
             for elt in args[1] ]
   return args[0].join( parts )

@primitive( 'string-split', '(string &optional separator)' )
def LP_string_split( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Splits string into a list of substrings.
With no separator, splits on runs of whitespace (empty strings omitted).
With a separator string, splits at every occurrence of that separator."""
   s = args[0]
   if not isinstance( s, str ):
      raise LRuntimeUsageError( LP_string_split, f'Invalid argument 1. STRING expected{got_str(s)}.' )
   if len(args) == 1:
      return s.split()
   sep = args[1]
   if not isinstance( sep, str ):
      raise LRuntimeUsageError( LP_string_split, f'Invalid argument 2. STRING expected{got_str(sep)}.' )
   return s.split( sep )

@primitive( 'string-lines', '(string)' )
def LP_string_lines( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Splits string into a list of lines.  Line endings (\\n, \\r\\n, \\r) are
stripped from each element.  An empty string returns NIL."""
   s = args[0]
   if not isinstance( s, str ):
      raise LRuntimeUsageError( LP_string_lines, f'Invalid argument 1. STRING expected{got_str(s)}.' )
   return s.splitlines()


# ── Type Conversions ────────────────────────────────────────────────────

def _filter_keyword_pairs( lst: list, declared_keys: set = None ) -> list:
   """Strip keyword/value pairs from a &rest list that also has &key params.
   In CL, &rest captures all args including keyword pairs; callers must filter.
   When declared_keys is provided, only strips pairs whose keyword name (sans colon,
   uppercased) is in declared_keys - leaving undeclared keyword symbols as data."""
   result = []
   i = 0
   while i < len(lst):
      if isinstance(lst[i], LSymbol) and lst[i].isKeyword():
         key = lst[i].name[1:]   # strip leading colon; already uppercased
         if declared_keys is None or key in declared_keys:
            i += 2               # skip this declared key-value pair
         else:
            result.append(lst[i])
            i += 1
      else:
         result.append(lst[i])
         i += 1
   return result

@primitive( 'string', '(&rest objects &key (sep ""))', mode=LambdaListMode.FULL_BINDING, min_args=1 )
def LP_string( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """PrettyPrints each argument into programmer readable form and returns the
results joined by :sep (default \"\").  With :sep, acts like string-join in
programmer mode: (string 1 2 3 :sep \", \") => \"1, 2, 3\"."""
   objects = env.lookup( 'OBJECTS' )
   sep     = env.lookup( 'SEP' )
   filtered = _filter_keyword_pairs( objects, {'SEP'} )
   return sep.join( prettyPrintSExpr(o) for o in filtered )

@primitive( 'ustring', '(&rest objects &key (sep ""))', mode=LambdaListMode.FULL_BINDING, min_args=1 )
def LP_ustring( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """PrettyPrints each argument into user readable form and returns the
results joined by :sep (default \"\").  With :sep, acts like string-join in
user mode: (ustring \"a\" \"b\" :sep \", \") => \"a, b\"."""
   objects = env.lookup( 'OBJECTS' )
   sep     = env.lookup( 'SEP' )
   filtered = _filter_keyword_pairs( objects, {'SEP'} )
   return sep.join( prettyPrint(o) for o in filtered )

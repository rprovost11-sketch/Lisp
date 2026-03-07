from typing import Any, Callable

from pythonslisp.ltk.EnvironmentBase import EnvironmentBase
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimePrimError
from pythonslisp.extensions import LambdaListMode
from pythonslisp.extensions.sequences import _validate_bounds


def register(lispFunction) -> None:

   @lispFunction( 'string-upcase', '(string)' )
   def LP_string_upcase( ctx: Context, env: Environment, *args ) -> Any:
      """Returns a copy of string with all characters converted to uppercase."""
      if not isinstance( args[0], str ):
         raise LRuntimePrimError( LP_string_upcase, 'Argument 1 must be a String.' )
      return args[0].upper()

   @lispFunction( 'string-downcase', '(string)' )
   def LP_string_downcase( ctx: Context, env: Environment, *args ) -> Any:
      """Returns a copy of string with all characters converted to lowercase."""
      if not isinstance( args[0], str ):
         raise LRuntimePrimError( LP_string_downcase, 'Argument 1 must be a String.' )
      return args[0].lower()

   @lispFunction( 'string-trim', '(char-bag string)' )
   def LP_string_trim( ctx: Context, env: Environment, *args ) -> Any:
      """Removes leading and trailing characters in char-bag from string."""
      charBag, s = args[0], args[1]
      if not isinstance( charBag, str ):
         raise LRuntimePrimError( LP_string_trim, 'Argument 1 (char-bag) must be a String.' )
      if not isinstance( s, str ):
         raise LRuntimePrimError( LP_string_trim, 'Argument 2 must be a String.' )
      return s.strip( charBag )

   @lispFunction( 'string-left-trim', '(char-bag string)' )
   def LP_string_left_trim( ctx: Context, env: Environment, *args ) -> Any:
      """Removes leading characters in char-bag from string."""
      charBag, s = args[0], args[1]
      if not isinstance( charBag, str ):
         raise LRuntimePrimError( LP_string_left_trim, 'Argument 1 (char-bag) must be a String.' )
      if not isinstance( s, str ):
         raise LRuntimePrimError( LP_string_left_trim, 'Argument 2 must be a String.' )
      return s.lstrip( charBag )

   @lispFunction( 'string-right-trim', '(char-bag string)' )
   def LP_string_right_trim( ctx: Context, env: Environment, *args ) -> Any:
      """Removes trailing characters in char-bag from string."""
      charBag, s = args[0], args[1]
      if not isinstance( charBag, str ):
         raise LRuntimePrimError( LP_string_right_trim, 'Argument 1 (char-bag) must be a String.' )
      if not isinstance( s, str ):
         raise LRuntimePrimError( LP_string_right_trim, 'Argument 2 must be a String.' )
      return s.rstrip( charBag )

   @lispFunction( 'char-code', '(char)' )
   def LP_char_code( ctx: Context, env: Environment, *args ) -> Any:
      """Returns the integer character code of a single-character string."""
      if not isinstance( args[0], str ) or len( args[0] ) != 1:
         raise LRuntimePrimError( LP_char_code, 'Argument 1 must be a single-character String.' )
      return ord( args[0] )

   @lispFunction( 'code-char', '(integer)' )
   def LP_code_char( ctx: Context, env: Environment, *args ) -> Any:
      """Returns the single-character string corresponding to an integer character code."""
      if not isinstance( args[0], int ):
         raise LRuntimePrimError( LP_code_char, 'Argument 1 must be an Integer.' )
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

   @lispFunction( 'string-capitalize', '(string &key (start 0) (end nil))',
               mode=LambdaListMode.FULL_BINDING )
   def LP_string_capitalize( ctx: Context, env: Environment, *args ) -> Any:
      """Returns a copy of string with CL word-boundary capitalization applied.
Optional :start and :end bound the region affected; text outside is unchanged."""
      s       = env.lookup( 'STRING' )
      start   = env.lookup( 'START' )
      end     = env.lookup( 'END' )
      if not isinstance( s, str ):
         raise LRuntimePrimError( LP_string_capitalize, '1st argument must be a string.' )
      start_n, end_n = _validate_bounds( start, end, len(s), LP_string_capitalize )
      prefix   = s[:start_n]
      middle   = s[start_n:end_n]
      suffix   = s[end_n:]
      return prefix + _cl_capitalize( middle ) + suffix

from typing import Any, Callable

from pythonslisp.Environment import Environment
from pythonslisp.LispContext import LispContext
from pythonslisp.LispExceptions import LispRuntimeFuncError
from pythonslisp.primitives.p_sequences import _is_nil_val, _bind_kw, _validate_bounds


def register(primitive, parseLispString: Callable) -> None:

   def _ll( s: str ) -> list:
      """Parse a lambda-list string; strip the PROGN wrapper parseLispString adds."""
      return parseLispString( s )[1]

   @primitive( 'string-upcase', '<string>',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_string_upcase( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns a copy of string with all characters converted to uppercase."""
      if not isinstance( args[0], str ):
         raise LispRuntimeFuncError( LP_string_upcase, 'Argument 1 must be a String.' )
      return args[0].upper()

   @primitive( 'string-downcase', '<string>',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_string_downcase( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns a copy of string with all characters converted to lowercase."""
      if not isinstance( args[0], str ):
         raise LispRuntimeFuncError( LP_string_downcase, 'Argument 1 must be a String.' )
      return args[0].lower()

   @primitive( 'string-trim', '<char-bag> <string>',
               min_args=2, max_args=2, arity_msg='2 arguments expected.' )
   def LP_string_trim( ctx: LispContext, env: Environment, *args ) -> Any:
      """Removes leading and trailing characters in char-bag from string."""
      charBag, s = args[0], args[1]
      if not isinstance( charBag, str ):
         raise LispRuntimeFuncError( LP_string_trim, 'Argument 1 (char-bag) must be a String.' )
      if not isinstance( s, str ):
         raise LispRuntimeFuncError( LP_string_trim, 'Argument 2 must be a String.' )
      return s.strip( charBag )

   @primitive( 'string-left-trim', '<char-bag> <string>',
               min_args=2, max_args=2, arity_msg='2 arguments expected.' )
   def LP_string_left_trim( ctx: LispContext, env: Environment, *args ) -> Any:
      """Removes leading characters in char-bag from string."""
      charBag, s = args[0], args[1]
      if not isinstance( charBag, str ):
         raise LispRuntimeFuncError( LP_string_left_trim, 'Argument 1 (char-bag) must be a String.' )
      if not isinstance( s, str ):
         raise LispRuntimeFuncError( LP_string_left_trim, 'Argument 2 must be a String.' )
      return s.lstrip( charBag )

   @primitive( 'string-right-trim', '<char-bag> <string>',
               min_args=2, max_args=2, arity_msg='2 arguments expected.' )
   def LP_string_right_trim( ctx: LispContext, env: Environment, *args ) -> Any:
      """Removes trailing characters in char-bag from string."""
      charBag, s = args[0], args[1]
      if not isinstance( charBag, str ):
         raise LispRuntimeFuncError( LP_string_right_trim, 'Argument 1 (char-bag) must be a String.' )
      if not isinstance( s, str ):
         raise LispRuntimeFuncError( LP_string_right_trim, 'Argument 2 must be a String.' )
      return s.rstrip( charBag )

   @primitive( 'char-code', '<char>',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_char_code( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns the integer character code of a single-character string."""
      if not isinstance( args[0], str ) or len( args[0] ) != 1:
         raise LispRuntimeFuncError( LP_char_code, 'Argument 1 must be a single-character String.' )
      return ord( args[0] )

   @primitive( 'code-char', '<integer>',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_code_char( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns the single-character string corresponding to an integer character code."""
      if not isinstance( args[0], int ):
         raise LispRuntimeFuncError( LP_code_char, 'Argument 1 must be an Integer.' )
      return chr( args[0] )

   _STRCAP_LL = _ll( '(string &key (start 0) (end nil))' )

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

   @primitive( 'string-capitalize', 'string &key (start 0) (end nil)',
               min_args=1, arity_msg='At least 1 argument expected.' )
   def LP_string_capitalize( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns a copy of string with CL word-boundary capitalization applied.
Optional :start and :end bound the region affected; text outside is unchanged."""
      kw      = _bind_kw( _STRCAP_LL, args, ctx, env, LP_string_capitalize )
      s       = kw.lookup( 'STRING' )
      start   = kw.lookup( 'START' )
      end     = kw.lookup( 'END' )
      if not isinstance( s, str ):
         raise LispRuntimeFuncError( LP_string_capitalize, '1st argument must be a string.' )
      start_n, end_n = _validate_bounds( start, end, len(s), LP_string_capitalize )
      prefix   = s[:start_n]
      middle   = s[start_n:end_n]
      suffix   = s[end_n:]
      return prefix + _cl_capitalize( middle ) + suffix

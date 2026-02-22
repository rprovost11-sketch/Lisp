from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.LispExceptions import LispRuntimeFuncError


def register(primitive) -> None:

   @primitive( 'string-upcase', '<string>' )
   def LP_string_upcase( env: Environment, *args ) -> Any:
      """Returns a copy of string with all characters converted to uppercase."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_string_upcase, '1 argument expected.' )
      if not isinstance( args[0], str ):
         raise LispRuntimeFuncError( LP_string_upcase, 'Argument 1 must be a String.' )
      return args[0].upper()

   @primitive( 'string-downcase', '<string>' )
   def LP_string_downcase( env: Environment, *args ) -> Any:
      """Returns a copy of string with all characters converted to lowercase."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_string_downcase, '1 argument expected.' )
      if not isinstance( args[0], str ):
         raise LispRuntimeFuncError( LP_string_downcase, 'Argument 1 must be a String.' )
      return args[0].lower()

   @primitive( 'string-trim', '<char-bag> <string>' )
   def LP_string_trim( env: Environment, *args ) -> Any:
      """Removes leading and trailing characters in char-bag from string."""
      if len(args) != 2:
         raise LispRuntimeFuncError( LP_string_trim, '2 arguments expected.' )
      charBag, s = args[0], args[1]
      if not isinstance( charBag, str ):
         raise LispRuntimeFuncError( LP_string_trim, 'Argument 1 (char-bag) must be a String.' )
      if not isinstance( s, str ):
         raise LispRuntimeFuncError( LP_string_trim, 'Argument 2 must be a String.' )
      return s.strip( charBag )

   @primitive( 'string-left-trim', '<char-bag> <string>' )
   def LP_string_left_trim( env: Environment, *args ) -> Any:
      """Removes leading characters in char-bag from string."""
      if len(args) != 2:
         raise LispRuntimeFuncError( LP_string_left_trim, '2 arguments expected.' )
      charBag, s = args[0], args[1]
      if not isinstance( charBag, str ):
         raise LispRuntimeFuncError( LP_string_left_trim, 'Argument 1 (char-bag) must be a String.' )
      if not isinstance( s, str ):
         raise LispRuntimeFuncError( LP_string_left_trim, 'Argument 2 must be a String.' )
      return s.lstrip( charBag )

   @primitive( 'string-right-trim', '<char-bag> <string>' )
   def LP_string_right_trim( env: Environment, *args ) -> Any:
      """Removes trailing characters in char-bag from string."""
      if len(args) != 2:
         raise LispRuntimeFuncError( LP_string_right_trim, '2 arguments expected.' )
      charBag, s = args[0], args[1]
      if not isinstance( charBag, str ):
         raise LispRuntimeFuncError( LP_string_right_trim, 'Argument 1 (char-bag) must be a String.' )
      if not isinstance( s, str ):
         raise LispRuntimeFuncError( LP_string_right_trim, 'Argument 2 must be a String.' )
      return s.rstrip( charBag )

   @primitive( 'char-code', '<char>' )
   def LP_char_code( env: Environment, *args ) -> Any:
      """Returns the integer character code of a single-character string."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_char_code, '1 argument expected.' )
      if not isinstance( args[0], str ) or len( args[0] ) != 1:
         raise LispRuntimeFuncError( LP_char_code, 'Argument 1 must be a single-character String.' )
      return ord( args[0] )

   @primitive( 'code-char', '<integer>' )
   def LP_code_char( env: Environment, *args ) -> Any:
      """Returns the single-character string corresponding to an integer character code."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_code_char, '1 argument expected.' )
      if not isinstance( args[0], int ):
         raise LispRuntimeFuncError( LP_code_char, 'Argument 1 must be an Integer.' )
      return chr( args[0] )

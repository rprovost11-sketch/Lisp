from fractions import Fraction
from typing import Any, Callable
from _io import TextIOWrapper


from pythonslisp.Environment import Environment
from pythonslisp.LispAST import ( LSymbol, LNUMBER, LCallable, LFunction, LMacro, LPrimitive,
                                   LContinuation, prettyPrint, prettyPrintSExpr,
                                   eql, equal, equalp )
from pythonslisp.LispAST import L_T, L_NIL
from pythonslisp.LispContext import LispContext
from pythonslisp.LispExceptions import LispRuntimeFuncError
from pythonslisp.LispParser import ParseError
from pythonslisp.primitives import LambdaListMode


def register(primitive, parseLispString: Callable) -> None:

   @primitive( 'numberp', '(sexpr)' )
   def LP_numberp( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if expr is a number otherwise nil."""
      return L_T if isinstance( args[0], LNUMBER ) else L_NIL

   @primitive( 'integerp', '(sexpr)' )
   def LP_integerp( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if expr is an integer otherwise nil."""
      return L_T if isinstance( args[0], int ) else L_NIL

   @primitive( 'rationalp', '(sexpr)' )
   def LP_rationalp( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if expr is an integer or fraction otherwise nil."""
      return L_T if isinstance( args[0], (int,Fraction) ) else L_NIL

   @primitive( 'floatp', '(sexpr)' )
   def LP_floatp( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if expr is a float otherwise nil."""
      return L_T if isinstance( args[0], float ) else L_NIL

   @primitive( 'symbolp', '(sexpr)' )
   def LP_symbolp( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if expr is a symbol otherwise nil."""
      return L_T if isinstance( args[0], LSymbol ) else L_NIL

   @primitive( 'symbol-name', '(symbol)' )
   def LP_symbol_name( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns the name of a symbol as a string."""
      if not isinstance( args[0], LSymbol ):
         raise LispRuntimeFuncError( LP_symbol_name, 'Argument 1 must be a Symbol.' )
      return args[0].strval

   @primitive( 'atom', '(sexpr)' )
   def LP_atom( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if expr is an atom (int,float,string,map or nil) otherwise nil."""
      arg = args[0]
      if isinstance(arg, list):
         return L_T if len(arg) == 0 else L_NIL
      return L_T

   @primitive( 'listp', '(sexpr)' )
   def LP_listp( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if expr is a list otherwise nil."""
      return L_T if isinstance(args[0], list) else L_NIL

   @primitive( 'dictp', '(sexpr)' )
   def LP_dictp( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if expr is a dict otherwise nil."""
      return L_T if isinstance(args[0], dict) else L_NIL

   @primitive( 'stringp', '(sexpr)' )
   def LP_stringp( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if expr is a string otherwise nil."""
      return L_T if isinstance( args[0], str ) else L_NIL

   @primitive( 'functionp', '(sexpr)' )
   def LP_functionp( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if expr is a function otherwise nil."""
      return L_T if isinstance( args[0], LFunction ) else L_NIL

   @primitive( 'macrop', '(sexpr)' )
   def LP_macrop( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if expr is a macro otherwise nil."""
      return L_T if isinstance( args[0], LMacro ) else L_NIL

   @primitive( 'consp', '(sexpr)' )
   def LP_consp( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if expr is a non-NIL list (cons cell), otherwise nil."""
      a = args[0]
      return L_T if (isinstance(a, list) and len(a) > 0) else L_NIL

   @primitive( 'streamp', '(sexpr)' )
   def LP_streamp( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if expr is a stream otherwise nil."""
      return L_T if isinstance(args[0], TextIOWrapper) else L_NIL

   @primitive( 'type-of', '(sexpr)' )
   def LP_typeof( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns the type of its argument as a symbol (CL type-of conventions)."""
      arg = args[0]
      if isinstance( arg, list ):
         return LSymbol('NULL') if len(arg) == 0 else LSymbol('CONS')
      elif isinstance( arg, int ):
         return LSymbol('INTEGER')
      elif isinstance( arg, float ):
         return LSymbol('FLOAT')
      elif isinstance( arg, Fraction ):
         return LSymbol('RATIO')
      elif isinstance( arg, str ):
         return LSymbol('STRING')
      elif isinstance( arg, LSymbol ):
         return LSymbol('SYMBOL')
      elif isinstance( arg, dict ):
         struct_type = arg.get('STRUCT-TYPE')
         return struct_type if struct_type is not None else LSymbol('DICT')
      elif isinstance( arg, LFunction ):
         return LSymbol('FUNCTION')
      elif isinstance( arg, LMacro ):
         return LSymbol('MACRO')
      elif isinstance( arg, LPrimitive ):
         return LSymbol('PRIMITIVE')
      elif isinstance( arg, LContinuation ):
         return LSymbol('CONTINUATION')
      elif isinstance( arg, TextIOWrapper ):
         return LSymbol('STREAM')
      else:
         return LSymbol('T')

   @primitive( 'not', '(object)' )
   def LP_not( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if the argument is nil otherwise returns nil."""
      arg1 = args[0]
      return L_T if (isinstance(arg1,list) and (len(arg1)==0)) else L_NIL

   @primitive( 'eq', '(a b)' )
   def LP_eq( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if the two values are the same object (CL eq semantics).
Symbols with the same name are always eq.  All other types use object
identity.  Note: small integers and interned strings may be identical
in CPython due to implementation-level caching."""
      arg1, arg2 = args
      if isinstance(arg1, LSymbol) and isinstance(arg2, LSymbol):
         return L_T if (arg1.strval == arg2.strval) else L_NIL
      return L_T if (arg1 is arg2) else L_NIL

   @primitive( 'eql', '(a b)' )
   def LP_eql( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if a and b are eql: symbols with the same name; numbers of the
same type with the same value (so 1 and 1.0 are not eql); or any other objects
that are the same (identical) object."""
      return L_T if eql(args[0], args[1]) else L_NIL

   @primitive( 'equal', '(a b)' )
   def LP_equalCL( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if a and b are structurally equal.  Recursively compares lists
element by element and strings by content.  Uses eql at the leaves so numbers
must be the same type: (equal 1 1.0) is nil."""
      return L_T if equal(args[0], args[1]) else L_NIL

   @primitive( 'equalp', '(a b)' )
   def LP_equalp( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if a and b are equalp.  Like equal but case-insensitive for
strings and type-insensitive for numbers: (equalp 1 1.0) is t,
(equalp \"ABC\" \"abc\") is t."""
      return L_T if equalp(args[0], args[1]) else L_NIL

   @primitive( '=', '(expr1 expr2 ...)',
               mode=LambdaListMode.DOC_ONLY, min_args=2 )
   def LP_equal( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if the two exprs are the same value otherwise nil."""

      prior = None
      for mbr in args:
         if prior is not None:
            if not( prior == mbr ):
               return L_NIL
         prior = mbr

      return L_T

   @primitive( '/=', '(expr1 expr2 ...)',
               mode=LambdaListMode.DOC_ONLY, min_args=2 )
   def LP_notEqual( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if no two arguments are numerically equal, otherwise nil.
CL semantics: all pairwise combinations are checked, not just adjacent pairs.
(/= 1 2 1) is NIL because the 1st and 3rd arguments are equal."""

      for i in range(len(args)):
         for j in range(i + 1, len(args)):
            if args[i] == args[j]:
               return L_NIL

      return L_T

   @primitive( '<', '(expr1 expr2 ...)',
               mode=LambdaListMode.DOC_ONLY, min_args=2 )
   def LP_less( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if the arguments are in ascending order."""

      prior = None
      try:
         for mbr in args:
            if prior is not None:
               if not( prior < mbr ):
                  return L_NIL
            prior = mbr
      except TypeError:
         raise LispRuntimeFuncError( LP_less, 'Invalid argument.  Arguments are not comparable.' )

      return L_T

   @primitive( '<=', '(expr1 expr2 ...)',
               mode=LambdaListMode.DOC_ONLY, min_args=2 )
   def LP_lessOrEqual( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if the adjacent arguments are less-than-or-equal otherwise nil."""

      prior = None
      try:
         for mbr in args:
            if prior is not None:
               if not( prior <= mbr ):
                  return L_NIL
            prior = mbr
      except TypeError:
         raise LispRuntimeFuncError( LP_lessOrEqual, 'Invalid argument.  Arguments are not comparable.' )

      return L_T

   @primitive( '>', '(expr1 expr2 ...)',
               mode=LambdaListMode.DOC_ONLY, min_args=2 )
   def LP_greater( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if the arguments are in descending order otherwise nil."""

      prior = None
      try:
         for mbr in args:
            if prior is not None:
               if not( prior > mbr ):
                  return L_NIL
            prior = mbr
      except TypeError:
         raise LispRuntimeFuncError( LP_greater, 'Invalid argument.  Arguments are not comparable.' )

      return L_T

   @primitive( '>=', '(expr1 expr2 ...)',
               mode=LambdaListMode.DOC_ONLY, min_args=2 )
   def LP_greaterOrEqual( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if the adjacent arguments are greater-than-or-equal otherwise nil."""

      prior = None
      try:
         for mbr in args:
            if prior is not None:
               if not( prior >= mbr ):
                  return L_NIL
            prior = mbr
      except TypeError:
         raise LispRuntimeFuncError( LP_greaterOrEqual, 'Invalid argument.  Arguments are not comparable.' )

      return L_T

   @primitive( 'float', '(number)' )
   def LP_float( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns val as a float.  Val can be any number type or a string containing a valid lisp float."""
      try:
         return float(args[0])
      except (ValueError, TypeError):
         raise LispRuntimeFuncError( LP_float, 'Invalid argument.' )

   @primitive( 'integer', '(number &optional (base 10))' )
   def LP_integer( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns val as an integer.  Val can be any number type or a string containing a valid lisp integer."""
      try:
         return int(*args)
      except (TypeError, ValueError):
         raise LispRuntimeFuncError( LP_integer, 'Invalid argument.' )

   @primitive( 'rational', '(number)' )
   def LP_rational( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns its argument as a fraction.  Val can be any number or a string
containing a valid lisp number that can be expressed as a fraction."""
      try:
         return Fraction(args[0])
      except (IndexError, TypeError, ValueError):
         raise LispRuntimeFuncError( LP_rational, 'Invalid argument.' )

   @primitive( 'string', '(object &rest more-objects)' )
   def LP_string( ctx: LispContext, env: Environment, *args ) -> Any:
      """PrettyPrints into programmer readable strings each argument object and
returns the concatenation of those strings."""
      resultStrs = [ prettyPrintSExpr(sExpr) for sExpr in args ]
      return ''.join(resultStrs)

   @primitive( 'ustring', '(object &rest more-objects)' )
   def LP_ustring( ctx: LispContext, env: Environment, *args ) -> Any:
      """PrettyPrints into user readable strings each argument object and
returns the concatenation of those strings."""
      resultStrs = [ prettyPrint(sExpr) for sExpr in args ]
      return ''.join(resultStrs)

   @primitive( 'make-symbol', '(string)' )
   def LP_make_symbol( ctx: LispContext, env: Environment, *args ) -> Any:
      """Takes a string and returns a new symbol whose print string is that string."""
      arg = args[0]
      if not isinstance(arg, str):
         raise LispRuntimeFuncError( LP_make_symbol, '1st argument expected to be a string.' )
      try:
         parsed = parseLispString(arg)
      except ParseError:
         raise LispRuntimeFuncError( LP_make_symbol, f'"{arg}" is not a valid symbol name.' )
      sym = parsed[1] if isinstance(parsed, list) and len(parsed) == 2 else parsed
      if not isinstance(sym, LSymbol):
         raise LispRuntimeFuncError( LP_make_symbol, f'"{arg}" is not a valid symbol name.' )
      return sym

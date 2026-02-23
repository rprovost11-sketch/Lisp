from fractions import Fraction
from typing import Any, Callable

from pythonslisp.Environment import Environment
from pythonslisp.LispAST import ( LSymbol, LNUMBER, LCallable, LFunction, LMacro, LPrimitive,
                                   LContinuation, prettyPrint, prettyPrintSExpr )
from pythonslisp.LispAST import L_T, L_NIL
from pythonslisp.LispExceptions import LispRuntimeFuncError
from pythonslisp.LispParser import ParseError


def register(primitive, parseLispString: Callable) -> None:

   @primitive( 'numberp', '<sexpr>' )
   def LP_numberp( env: Environment, *args ) -> Any:
      """Returns t if expr is a number otherwise nil."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_numberp, '1 argument expected.' )
      return L_T if isinstance( args[0], LNUMBER ) else L_NIL

   @primitive( 'integerp', '<sexpr>' )
   def LP_integerp( env: Environment, *args ) -> Any:
      """Returns t if expr is an integer otherwise nil."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_integerp, '1 argument expected.' )
      return L_T if isinstance( args[0], int ) else L_NIL

   @primitive( 'rationalp', '<sexpr>' )
   def LP_rationalp( env: Environment, *args ) -> Any:
      """Returns t if expr is an integer or fraction otherwise nil."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_rationalp, '1 argument expected.' )
      return L_T if isinstance( args[0], (int,Fraction) ) else L_NIL

   @primitive( 'floatp', '<sexpr>' )
   def LP_floatp( env: Environment, *args ) -> Any:
      """Returns t if expr is a float otherwise nil."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_floatp, '1 argument expected.' )
      return L_T if isinstance( args[0], float ) else L_NIL

   @primitive( 'symbolp', '<sexpr>' )
   def LP_symbolp( env: Environment, *args ) -> Any:
      """Returns t if expr is a symbol otherwise nil."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_symbolp, '1 argument expected.' )
      return L_T if isinstance( args[0], LSymbol ) else L_NIL

   @primitive( 'symbol-name', '<symbol>' )
   def LP_symbol_name( env: Environment, *args ) -> Any:
      """Returns the name of a symbol as a string."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_symbol_name, '1 argument expected.' )
      if not isinstance( args[0], LSymbol ):
         raise LispRuntimeFuncError( LP_symbol_name, 'Argument 1 must be a Symbol.' )
      return args[0].strval

   @primitive( 'atom', '<sexpr>' )
   def LP_atom( env: Environment, *args ) -> Any:
      """Returns t if expr is an atom (int,float,string,map or nil) otherwise nil."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_atom, '1 argument expected.' )
      arg = args[0]
      if isinstance(arg, list):
         return L_T if len(arg) == 0 else L_NIL
      return L_T

   @primitive( 'listp', '<sexpr>' )
   def LP_listp( env: Environment, *args ) -> Any:
      """Returns t if expr is a list otherwise nil."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_listp, '1 argument expected.' )
      return L_T if isinstance(args[0], list) else L_NIL

   @primitive( 'mapp', '<sexpr>' )
   def LP_mapp( env: Environment, *args ) -> Any:
      """Returns t if expr is a map otherwise nil."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_mapp, '1 argument expected.' )
      return L_T if isinstance(args[0], dict) else L_NIL

   @primitive( 'stringp', '<sexpr>' )
   def LP_stringp( env: Environment, *args ) -> Any:
      """Returns t if expr is a string otherwise nil."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_stringp, '1 argument expected.' )
      return L_T if isinstance( args[0], str ) else L_NIL

   @primitive( 'functionp', '<sexpr>' )
   def LP_functionp( env: Environment, *args ) -> Any:
      """Returns t if expr is a function otherwise nil."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_functionp, '1 argument expected.' )
      return L_T if isinstance( args[0], LFunction ) else L_NIL

   @primitive( 'macrop', '<sexpr>' )
   def LP_macrop( env: Environment, *args ) -> Any:
      """Returns t if expr is a macro otherwise nil."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_macrop, '1 argument expected.' )
      return L_T if isinstance( args[0], LMacro ) else L_NIL

   @primitive( 'type-of', '<sexpr>' )
   def LP_typeof( env: Environment, *args ) -> Any:
      """Returns the type of its argument as a symbol (CL type-of conventions)."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_typeof, '1 argument expected.' )
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
         return struct_type if struct_type is not None else LSymbol('MAP')
      elif isinstance( arg, LFunction ):
         return LSymbol('FUNCTION')
      elif isinstance( arg, LMacro ):
         return LSymbol('MACRO')
      elif isinstance( arg, LPrimitive ):
         return LSymbol('PRIMITIVE')
      elif isinstance( arg, LContinuation ):
         return LSymbol('CONTINUATION')
      else:
         return LSymbol('T')

   @primitive( 'not', '<boolean>' )
   def LP_not( env: Environment, *args ) -> Any:
      """Returns t if the argument is nil otherwise returns nil."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_not, '1 argument expected.' )
      arg1 = args[0]
      return L_T if (isinstance(arg1,list) and (len(arg1)==0)) else L_NIL

   @primitive( 'eql', '<a> <b>' )
   def LP_eql( env: Environment, *args ) -> Any:
      """Returns t if a and b are eql: symbols with the same name; numbers of the
same type with the same value (so 1 and 1.0 are not eql); or any other objects
that are the same (identical) object."""
      if len(args) != 2:
         raise LispRuntimeFuncError( LP_eql, '2 arguments expected.' )
      a, b = args
      if isinstance(a, LSymbol) and isinstance(b, LSymbol):
         return L_T if a.strval == b.strval else L_NIL
      if type(a) is type(b) and isinstance(a, (int, float, Fraction)):
         return L_T if a == b else L_NIL
      return L_T if a is b else L_NIL

   @primitive( 'is?', '<expr1> <expr2>' )
   def LP_is( env: Environment, *args ) -> Any:
      """Returns t if the two values are the same object otherwise nil."""
      try:
         arg1,arg2 = args
      except ValueError:
         raise LispRuntimeFuncError( LP_is, '2 arguments expected.' )

      if isinstance(arg1, (int,float,str)):
         return L_T if (arg1 == arg2) else L_NIL
      else:
         return L_T if (arg1 is arg2) else L_NIL

   @primitive( '=', '<expr1> <expr2> ...' )
   def LP_equal( env: Environment, *args ) -> Any:
      """Returns t if the two exprs are the same value otherwise nil."""
      if len(args) < 2:
         raise LispRuntimeFuncError( LP_equal, '2 or more arguments expected.' )

      prior = None
      for mbr in args:
         if prior is not None:
            if not( prior == mbr ):
               return L_NIL
         prior = mbr

      return L_T

   @primitive( '/=', '<expr1> <expr2> ...' )
   def LP_notEqual( env: Environment, *args ) -> Any:
      """Returns t if the two exprs are different values otherwise nil."""
      if len(args) < 2:
         raise LispRuntimeFuncError( LP_notEqual, '2 or more arguments expected.' )

      prior = None
      for mbr in args:
         if prior is not None:
            if not( prior != mbr ):
               return L_NIL
         prior = mbr

      return L_T

   @primitive( '<', '<expr1> <expr2> ...' )
   def LP_less( env: Environment, *args ) -> Any:
      """Returns t if the arguments are in ascending order."""
      if len(args) < 2:
         raise LispRuntimeFuncError( LP_less, '2 or more arguments expected.' )

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

   @primitive( '<=', '<expr1> <expr2> ...' )
   def LP_lessOrEqual( env: Environment, *args ) -> Any:
      """Returns t if the adjacent arguments are less-than-or-equal otherwise nil."""
      if len(args) < 2:
         raise LispRuntimeFuncError( LP_lessOrEqual, '2 or more arguments expected.' )

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

   @primitive( '>', '<expr1> <expr2> ...' )
   def LP_greater( env: Environment, *args ) -> Any:
      """Returns t if the arguments are in descending order otherwise nil."""
      if len(args) < 2:
         raise LispRuntimeFuncError( LP_greater, '2 or more arguments expected.' )

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

   @primitive( '>=', '<expr1> <expr2> ...' )
   def LP_greaterOrEqual( env: Environment, *args ) -> Any:
      """Returns t if the adjacent arguments are greater-than-or-equal otherwise nil."""
      if len(args) < 2:
         raise LispRuntimeFuncError( LP_greaterOrEqual, '2 or more arguments expected.' )

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

   @primitive( 'float', '<number>' )
   def LP_float( env: Environment, *args ) -> Any:
      """Returns val as a float.  Val can be any number type or a string containing a valid lisp float."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_float, '1 argument expected.' )

      try:
         return float(args[0])
      except (ValueError, TypeError):
         raise LispRuntimeFuncError( LP_float, 'Invalid argument.' )

   @primitive( 'integer', '<number> &optional (<base> 10)' )
   def LP_integer( env: Environment, *args ) -> Any:
      """Returns val as an integer.  Val can be any number type or a string containing a valid lisp integer."""
      numArgs = len(args)
      if (numArgs < 1) or (numArgs > 2):
         raise LispRuntimeFuncError( LP_integer, '1 or 2 arguments expected.' )

      try:
         return int(*args)
      except (TypeError, ValueError):
         raise LispRuntimeFuncError( LP_integer, 'Invalid argument.' )

   @primitive( 'rational', '<number>' )
   def LP_rational( env: Environment, *args ) -> Any:
      """Returns its argument as a fraction.  Val can be any number or a string
containing a valid lisp number that can be expressed as a fraction."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_rational, 'Exactly 1 argument expected.' )

      try:
         return Fraction(args[0])
      except (IndexError, TypeError, ValueError):
         raise LispRuntimeFuncError( LP_rational, 'Invalid argument.' )

   @primitive( 'string', '<object1> <object2> ...' )
   def LP_string( env: Environment, *args ) -> Any:
      """PrettyPrints as programmer readable strings each argument object and
concatenates the results to form a new string."""
      if len(args) == 0:
         raise LispRuntimeFuncError( LP_string, '1 or more arguments expected.' )

      resultStrs = [ prettyPrintSExpr(sExpr) for sExpr in args ]
      return ''.join(resultStrs)

   @primitive( 'ustring', '<object1> <object2> ...' )
   def LP_ustring( env: Environment, *args ) -> Any:
      """PrettyPrints as user readable strings each argument object and
concatenates the results to form a new string."""
      if len(args) == 0:
         raise LispRuntimeFuncError( LP_ustring, '1 or more arguments expected.' )

      resultStrs = [ prettyPrint(sExpr) for sExpr in args ]
      return ''.join(resultStrs)

   @primitive( 'symbol', '<string1> <string2> ...' )
   def LP_symbol( env: Environment, *args ) -> Any:
      """PrettyPrints as user readable strings each argument object and
concatenates the results to form a new string which is used to define a new
symbol object."""
      if len(args) == 0:
         raise LispRuntimeFuncError( LP_symbol, '1 or more string argument expected.' )

      strList = [ prettyPrint(arg) for arg in args ]
      symstr = ''.join(strList)
      try:
         parsed = parseLispString(symstr)
      except ParseError:
         raise LispRuntimeFuncError( LP_symbol, f'The resulting string "{symstr}" is not a valid Lisp symbol.' )
      sym = parsed[1] if isinstance(parsed, list) and len(parsed) == 2 else parsed
      if not isinstance(sym, LSymbol):
         raise LispRuntimeFuncError( LP_symbol, f'The resulting string "{symstr}" is not a valid Lisp symbol.' )
      return sym

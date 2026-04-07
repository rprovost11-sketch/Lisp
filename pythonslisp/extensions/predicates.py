from __future__ import annotations
from fractions import Fraction
from typing import Any
from io import IOBase, StringIO

from pythonslisp.Environment import Environment, ModuleEnvironment
from pythonslisp.AST import ( LSymbol, LNUMBER, LFunction, LMacro,
                               eql, equal, equalp, lisp_type_name, got_str )
from pythonslisp.AST import L_T, L_NIL
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimeUsageError, LRuntimeError
from pythonslisp.extensions import primitive


def _typep_atom( obj, tname: str ) -> bool:
   """Return True if obj belongs to the named atomic CL type specifier."""
   if tname == 'T':              return True
   if tname == 'NIL':            return False
   if tname == 'NULL':           return isinstance(obj, list) and len(obj) == 0
   if tname == 'CONS':           return isinstance(obj, list) and len(obj) > 0
   if tname == 'LIST':           return isinstance(obj, list)
   if tname == 'ATOM':           return not (isinstance(obj, list) and len(obj) > 0)
   if tname == 'BOOLEAN':
      if isinstance(obj, list) and len(obj) == 0:
         return True
      if isinstance(obj, LSymbol) and obj.name == 'T':
         return True
      return False
   if tname == 'NUMBER':           return isinstance(obj, LNUMBER)
   if tname == 'REAL':             return isinstance(obj, (int, float, Fraction))
   if tname == 'COMPLEX':          return isinstance(obj, complex)
   if tname == 'INTEGER':          return isinstance(obj, int)
   if tname == 'FLOAT':            return isinstance(obj, float)
   if tname == 'RATIO':            return isinstance(obj, Fraction)
   if tname == 'RATIONAL':         return isinstance(obj, (int, Fraction))
   if tname == 'STRING':         return isinstance(obj, str)
   if tname == 'SYMBOL':         return isinstance(obj, LSymbol)
   if tname == 'FUNCTION':       return isinstance(obj, LFunction)
   if tname == 'MACRO':          return isinstance(obj, LMacro)
   if tname == 'STREAM':         return isinstance(obj, IOBase)
   if tname == 'FILE-STREAM':    return isinstance(obj, IOBase) and not isinstance(obj, StringIO)
   if tname == 'STRING-STREAM':  return isinstance(obj, StringIO)
   if tname == 'MODULE':         return isinstance(obj, ModuleEnvironment)
   if tname == 'DICT':           return isinstance(obj, dict)
   if isinstance(obj, dict):
      struct_type = obj.get(LSymbol('STRUCT-TYPE'))
      if isinstance(struct_type, LSymbol) and struct_type.name == tname:
         return True
      struct_includes = obj.get(LSymbol('STRUCT-INCLUDES'))
      if isinstance(struct_includes, list):
         if any(isinstance(s, LSymbol) and s.name == tname for s in struct_includes):
            return True
   return False


def _check_numeric_bounds( val, bounds: list ) -> bool:
   """Check CL-style numeric bounds.
Each bound is * (unbounded), a number (inclusive), or a 1-element list (exclusive)."""
   low  = bounds[0] if len(bounds) >= 1 else LSymbol('*')
   high = bounds[1] if len(bounds) >= 2 else LSymbol('*')
   if not (isinstance(low, LSymbol) and low.name == '*'):
      if isinstance(low, list) and len(low) == 1:
         if not (val > low[0]):
            return False
      else:
         if not (val >= low):
            return False
   if not (isinstance(high, LSymbol) and high.name == '*'):
      if isinstance(high, list) and len(high) == 1:
         if not (val < high[0]):
            return False
      else:
         if not (val <= high):
            return False
   return True


def _typep( obj, spec, ctx=None, env=None ) -> bool:
   """Return True if obj matches the type specifier.
spec may be an LSymbol (atomic) or a list (compound)."""
   if isinstance(spec, LSymbol):
      return _typep_atom(obj, spec.name)
   if not (isinstance(spec, list) and len(spec) > 0):
      raise LRuntimeError('typep: type specifier must be a symbol or non-empty list.')
   head = spec[0]
   if not isinstance(head, LSymbol):
      raise LRuntimeError('typep: compound type specifier head must be a symbol.')
   op   = head.name
   rest = spec[1:]
   if op == 'OR':
      return any( _typep(obj, s, ctx, env) for s in rest )
   if op == 'AND':
      return all( _typep(obj, s, ctx, env) for s in rest )
   if op == 'NOT':
      if len(rest) != 1:
         raise LRuntimeError('typep: NOT requires exactly one type argument.')
      return not _typep(obj, rest[0], ctx, env)
   if op == 'MEMBER':
      return any( eql(obj, v) for v in rest )
   if op == 'SATISFIES':
      if len(rest) != 1 or not isinstance(rest[0], LSymbol):
         raise LRuntimeError('typep: SATISFIES requires a function-name symbol.')
      fn     = env.lookupSym(rest[0])
      result = ctx.lApply(ctx, env, fn, [obj])
      return result is not L_NIL
   if op == 'INTEGER':
      return isinstance(obj, int) and _check_numeric_bounds(obj, rest)
   if op == 'FLOAT':
      return isinstance(obj, float) and _check_numeric_bounds(obj, rest)
   if op == 'RATIONAL':
      return isinstance(obj, (int, Fraction)) and _check_numeric_bounds(obj, rest)
   if op == 'REAL':
      return isinstance(obj, (int, float, Fraction)) and _check_numeric_bounds(obj, rest)
   if op == 'NUMBER':
      return isinstance(obj, LNUMBER) and _check_numeric_bounds(obj, rest)
   raise LRuntimeError(f'typep: unknown compound type operator {op}.')


@primitive( 'modulep', '(sexpr)' )
def LP_modulep( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if expr is a module otherwise nil."""
   return L_T if isinstance( args[0], ModuleEnvironment ) else L_NIL

@primitive( 'numberp', '(sexpr)' )
def LP_numberp( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if expr is a number otherwise nil."""
   return L_T if isinstance( args[0], LNUMBER ) else L_NIL

@primitive( 'integerp', '(sexpr)' )
def LP_integerp( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if expr is an integer otherwise nil."""
   return L_T if isinstance( args[0], int ) else L_NIL

@primitive( 'rationalp', '(sexpr)' )
def LP_rationalp( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if expr is an integer or fraction otherwise nil."""
   return L_T if isinstance( args[0], (int,Fraction) ) else L_NIL

@primitive( 'complexp', '(sexpr)' )
def LP_complexp( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if expr is a complex number otherwise nil."""
   return L_T if isinstance( args[0], complex ) else L_NIL

@primitive( 'realp', '(sexpr)' )
def LP_realp( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if expr is a real number (integer, float, or ratio) otherwise nil."""
   return L_T if isinstance( args[0], (int, float, Fraction) ) else L_NIL

@primitive( 'floatp', '(sexpr)' )
def LP_floatp( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if expr is a float otherwise nil."""
   return L_T if isinstance( args[0], float ) else L_NIL

@primitive( 'symbolp', '(sexpr)' )
def LP_symbolp( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if expr is a symbol otherwise nil."""
   return L_T if isinstance( args[0], LSymbol ) else L_NIL

@primitive( 'atom', '(sexpr)' )
def LP_atom( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if expr is an atom (int,float,string,map or nil) otherwise nil."""
   arg = args[0]
   if isinstance(arg, list):
      return L_T if len(arg) == 0 else L_NIL
   return L_T

@primitive( 'listp', '(sexpr)' )
def LP_listp( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if expr is a list otherwise nil."""
   return L_T if isinstance(args[0], list) else L_NIL

@primitive( 'dictp', '(sexpr)' )
def LP_dictp( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if expr is a dict otherwise nil."""
   return L_T if isinstance(args[0], dict) else L_NIL

@primitive( 'stringp', '(sexpr)' )
def LP_stringp( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if expr is a string otherwise nil."""
   return L_T if isinstance( args[0], str ) else L_NIL

@primitive( 'functionp', '(sexpr)' )
def LP_functionp( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if expr is a function otherwise nil."""
   return L_T if isinstance( args[0], LFunction ) else L_NIL

@primitive( 'macrop', '(sexpr)' )
def LP_macrop( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if expr is a macro otherwise nil."""
   return L_T if isinstance( args[0], LMacro ) else L_NIL

@primitive( 'consp', '(sexpr)' )
def LP_consp( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if expr is a non-NIL list (cons cell), otherwise nil."""
   a = args[0]
   return L_T if (isinstance(a, list) and len(a) > 0) else L_NIL

@primitive( 'streamp', '(sexpr)' )
def LP_streamp( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if expr is a stream otherwise nil."""
   return L_T if isinstance(args[0], IOBase) else L_NIL

@primitive( 'file-stream-p', '(sexpr)' )
def LP_file_stream_p( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if expr is a file stream (opened with open), nil otherwise."""
   arg = args[0]
   return L_T if (isinstance(arg, IOBase) and not isinstance(arg, StringIO)) else L_NIL

@primitive( 'string-stream-p', '(sexpr)' )
def LP_string_stream_p( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if expr is a string stream (opened with make-string-input-stream
or open-string), nil otherwise."""
   return L_T if isinstance(args[0], StringIO) else L_NIL

@primitive( 'typep', '(object type-specifier)' )
def LP_typep( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns T if object matches the type specifier, NIL otherwise.
type-specifier may be an atomic symbol or a compound list form.
Atomic specifiers: T NIL ATOM LIST NULL CONS BOOLEAN NUMBER REAL INTEGER
  FLOAT RATIO RATIONAL STRING SYMBOL FUNCTION MACRO STREAM FILE-STREAM
  STRING-STREAM DICT, and any struct type name.
Compound specifiers: (OR ...) (AND ...) (NOT t) (MEMBER v...) (SATISFIES fn)
  (INTEGER low high) (FLOAT low high) (RATIONAL low high) (REAL low high)
  (NUMBER low high).  Bounds are * (unbounded), n (inclusive), or (n) (exclusive)."""
   obj, spec = args
   if not isinstance(spec, (LSymbol, list)):
      raise LRuntimeUsageError( LP_typep, f'Invalid argument 2. TYPE SPECIFIER expected{got_str(spec)}.' )
   try:
      return L_T if _typep(obj, spec, ctx, env) else L_NIL
   except LRuntimeError as e:
      raise LRuntimeUsageError( LP_typep, str(e).replace('typep: ', '') ) from e

@primitive( 'not', '(object)' )
def LP_not( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if the argument is nil otherwise returns nil."""
   arg1 = args[0]
   return L_T if (isinstance(arg1,list) and (len(arg1)==0)) else L_NIL

@primitive( 'eq', '(a b)' )
def LP_eq( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if the two values are the same object (CL eq semantics).
Symbols with the same name are always eq.  All other types use object
identity.  Note: small integers and interned strings may be identical
in CPython due to implementation-level caching."""
   arg1, arg2 = args
   if isinstance(arg1, LSymbol) and isinstance(arg2, LSymbol):
      return L_T if (arg1.name == arg2.name) else L_NIL
   return L_T if (arg1 is arg2) else L_NIL

@primitive( 'eql', '(a b)' )
def LP_eql( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if a and b are eql: symbols with the same name; numbers of the
same type with the same value (so 1 and 1.0 are not eql); or any other objects
that are the same (identical) object."""
   return L_T if eql(args[0], args[1]) else L_NIL

@primitive( 'equal', '(a b)' )
def LP_equal( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if a and b are structurally equal.  Recursively compares lists
element by element and strings by content.  Uses eql at the leaves so numbers
must be the same type: (equal 1 1.0) is nil."""
   return L_T if equal(args[0], args[1]) else L_NIL

@primitive( 'equalp', '(a b)' )
def LP_equalp( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if a and b are equalp.  Like equal but case-insensitive for
strings and type-insensitive for numbers: (equalp 1 1.0) is t,
(equalp \"ABC\" \"abc\") is t."""
   return L_T if equalp(args[0], args[1]) else L_NIL

@primitive( '=', '(expr1 expr2 &rest exprs)' )
def LP_isEqualTo( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if the two exprs are the same value otherwise nil."""
   prior = None
   for mbr in args:
      if prior is not None and prior != mbr:
         return L_NIL
      prior = mbr
   return L_T

@primitive( '/=', '(expr1 expr2 &rest exprs)' )
def LP_notEqual( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if no two arguments are numerically equal, otherwise nil.
CL semantics: all pairwise combinations are checked, not just adjacent pairs.
(/= 1 2 1) is NIL because the 1st and 3rd arguments are equal."""
   for i in range(len(args)):
      for j in range(i + 1, len(args)):
         if args[i] == args[j]:
            return L_NIL
   return L_T

@primitive( '<', '(expr1 expr2 &rest exprs)' )
def LP_less( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if the arguments are in ascending order."""
   prior = None
   for i, mbr in enumerate(args, 1):
      if prior is not None:
         try:
            if not( prior < mbr ):
               return L_NIL
         except TypeError:
            raise LRuntimeUsageError( LP_less, f'Invalid argument {i}. {lisp_type_name(prior)} and {lisp_type_name(mbr)} are not comparable{got_str(mbr)}.' )
      prior = mbr
   return L_T

@primitive( '<=', '(expr1 expr2 &rest exprs)' )
def LP_lessOrEqual( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if the adjacent arguments are less-than-or-equal otherwise nil."""
   prior = None
   for i, mbr in enumerate(args, 1):
      if prior is not None:
         try:
            if not( prior <= mbr ):
               return L_NIL
         except TypeError:
            raise LRuntimeUsageError( LP_lessOrEqual, f'Invalid argument {i}. {lisp_type_name(prior)} and {lisp_type_name(mbr)} are not comparable{got_str(mbr)}.' )
      prior = mbr
   return L_T

@primitive( '>', '(expr1 expr2 &rest exprs)' )
def LP_greater( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if the arguments are in descending order otherwise nil."""
   prior = None
   for i, mbr in enumerate(args, 1):
      if prior is not None:
         try:
            if not( prior > mbr ):
               return L_NIL
         except TypeError:
            raise LRuntimeUsageError( LP_greater, f'Invalid argument {i}. {lisp_type_name(prior)} and {lisp_type_name(mbr)} are not comparable{got_str(mbr)}.' )
      prior = mbr
   return L_T

@primitive( '>=', '(expr1 expr2 &rest exprs)' )
def LP_greaterOrEqual( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if the adjacent arguments are greater-than-or-equal otherwise nil."""
   prior = None
   for i, mbr in enumerate(args, 1):
      if prior is not None:
         try:
            if not( prior >= mbr ):
               return L_NIL
         except TypeError:
            raise LRuntimeUsageError( LP_greaterOrEqual, f'Invalid argument {i}. {lisp_type_name(prior)} and {lisp_type_name(mbr)} are not comparable{got_str(mbr)}.' )
      prior = mbr
   return L_T

@primitive( 'boundp', '(symbol)' )
def LP_boundp( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns T if the symbol has a value bound in the environment, NIL otherwise."""
   sym = args[0]
   if not isinstance( sym, LSymbol ):
      raise LRuntimeUsageError( LP_boundp, f'Invalid argument 1. SYMBOL expected{got_str(sym)}.' )
   try:
      env.lookupSym( sym )
      return L_T
   except KeyError:
      return L_NIL

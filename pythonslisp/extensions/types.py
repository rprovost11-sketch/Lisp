from __future__ import annotations
from fractions import Fraction
from typing import Any

from pythonslisp.Environment import Environment, ModuleEnvironment
from pythonslisp.AST import LSymbol, prettyPrint, prettyPrintSExpr, got_str, lisp_type_name
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimeUsageError
from pythonslisp.Parser import ParseError
from pythonslisp.extensions import LambdaListMode, primitive


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


@primitive( 'type-of', '(sexpr)' )
def LP_typeof( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the type of its argument as a symbol (CL type-of conventions)."""
   arg = args[0]
   if isinstance( arg, ModuleEnvironment ):
      return LSymbol('MODULE')
   return LSymbol( lisp_type_name(arg) )

@primitive( 'float', '(number)' )
def LP_float( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns val as a float.  Val can be any number type or a string containing a valid lisp float."""
   try:
      return float(args[0])
   except (ValueError, TypeError):
      raise LRuntimeUsageError( LP_float, f'Invalid argument 1. NUMBER or NUMERIC STRING expected{got_str(args[0])}.' )

@primitive( 'integer', '(number &optional (base 10))' )
def LP_integer( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns val as an integer.  Val can be any number type or a string containing a valid lisp integer."""
   try:
      return int(*args)
   except (TypeError, ValueError):
      raise LRuntimeUsageError( LP_integer, f'Invalid argument 1. NUMBER or NUMERIC STRING expected{got_str(args[0])}.' )

@primitive( 'rational', '(number)' )
def LP_rational( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns its argument as a fraction.  Val can be any number or a string
containing a valid lisp number that can be expressed as a fraction."""
   try:
      return Fraction(args[0])
   except (IndexError, TypeError, ValueError):
      raise LRuntimeUsageError( LP_rational, f'Invalid argument 1. NUMBER expected{got_str(args[0])}.' )

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

@primitive( 'make-symbol', '(string)' )
def LP_make_symbol( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Takes a string and returns a new symbol whose print string is that string."""
   arg = args[0]
   if not isinstance(arg, str):
      raise LRuntimeUsageError( LP_make_symbol, f'Invalid argument 1. STRING expected{got_str(arg)}.' )
   try:
      sym, _ = ctx.parseOne(arg)
   except ParseError:
      raise LRuntimeUsageError( LP_make_symbol, f'Invalid argument 1. Valid symbol name expected; "{arg}" is not valid.' )
   if not isinstance(sym, LSymbol):
      raise LRuntimeUsageError( LP_make_symbol, f'Invalid argument 1. Valid symbol name expected; "{arg}" is not valid.' )
   return sym

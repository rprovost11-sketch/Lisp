from __future__ import annotations
from typing import Any

from pythonslisp.ltk.EnvironmentBase import EnvironmentBase
from pythonslisp.AST import L_NIL, LMultipleValues
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimeError


def register( primitive ) -> None:

   @primitive( 'values', '(&rest vals)' )
   def LP_values( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
      """Returns zero or more values.  With one argument returns it unchanged.
With zero arguments returns an empty multiple-values object.  In any scalar
context only the primary (first) value is used; use multiple-value-bind or
nth-value to capture additional values."""
      if len(args) == 1:
         return args[0]
      return LMultipleValues( list(args) )

   @primitive( 'values-list', '(list)' )
   def LP_values_list( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
      """Converts a list to multiple values.  (values-list '(a b c)) is
equivalent to (values a b c)."""
      lst = args[0]
      if not isinstance( lst, list ):
         raise LRuntimeError( 'values-list: argument must be a list.' )
      if len(lst) == 1:
         return lst[0]
      return LMultipleValues( list(lst) )

   @primitive( 'multiple-value-list', '(values-form)', preEvalArgs=False )
   def LP_multiple_value_list( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
      """Evaluates values-form and returns all its values as a list.
A single non-multiple value is returned as a one-element list."""
      result = ctx.lEval( env, args[0] )
      if type(result) is LMultipleValues:
         return list( result.values )
      return [result]

   @primitive( 'nth-value', '(n values-form)', preEvalArgs=False )
   def LP_nth_value( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
      """Returns the nth value (0-based) produced by values-form.  Returns NIL
if n is out of range.  A non-multiple-values result is treated as a single
value: n=0 returns it, n>0 returns NIL."""
      from pythonslisp.Interpreter import Interpreter
      n_val = Interpreter._primary( ctx.lEval( env, args[0] ) )
      if not isinstance( n_val, int ) or n_val < 0:
         raise LRuntimeError( 'nth-value: first argument must be a non-negative integer.' )
      result = ctx.lEval( env, args[1] )
      if type(result) is LMultipleValues:
         return result.values[n_val] if n_val < len(result.values) else L_NIL
      return result if n_val == 0 else L_NIL

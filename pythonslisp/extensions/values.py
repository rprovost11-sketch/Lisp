from __future__ import annotations
from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.AST import L_NIL, LMultipleValues, got_str
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimePrimError
from pythonslisp.extensions import primitive


@primitive( 'values', '(&rest vals)' )
def LP_values( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns zero or more values.  With one argument returns it unchanged.
With zero arguments returns an empty multiple-values object.  In any scalar
context only the primary (first) value is used; use multiple-value-bind or
nth-value to capture additional values."""
   if len(args) == 1:
      return args[0]
   return LMultipleValues( list(args) )

@primitive( 'values-list', '(list)' )
def LP_values_list( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Converts a list to multiple values.  (values-list '(a b c)) is
equivalent to (values a b c)."""
   lst = args[0]
   if not isinstance( lst, list ):
      raise LRuntimePrimError( LP_values_list, f'Invalid argument 1. LIST expected{got_str(lst)}.' )
   if len(lst) == 1:
      return lst[0]
   return LMultipleValues( list(lst) )

@primitive( 'multiple-value-list', '(values-form)' )
def LP_multiple_value_list( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Evaluates values-form and returns all its values as a list.
A single non-multiple value is returned as a one-element list."""
   raise LRuntimePrimError( LP_multiple_value_list, 'Handled by CEK machine.' )

@primitive( 'nth-value', '(n values-form)' )
def LP_nth_value( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the nth value (0-based) produced by values-form.  Returns NIL
if n is out of range.  A non-multiple-values result is treated as a single
value: n=0 returns it, n>0 returns NIL."""
   raise LRuntimePrimError( LP_nth_value, 'Handled by CEK machine.' )

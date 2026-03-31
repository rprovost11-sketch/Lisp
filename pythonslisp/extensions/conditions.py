from __future__ import annotations
from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.AST import LSymbol, T_SYM, L_NIL, got_str
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimeError, LRuntimeUsageError, Signaled
from pythonslisp.extensions import primitive


@primitive( 'make-condition', '(type &optional message)' )
def LP_make_condition( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Creates and returns a condition object with a type symbol and optional
message string.  Condition objects are used with signal and handler-case."""
   ctype = args[0]
   if not isinstance( ctype, LSymbol ):
      raise LRuntimeUsageError( LP_make_condition, f'Invalid argument 1. SYMBOL expected{got_str(ctype)}.' )
   msg = args[1] if len(args) > 1 else ''
   if not isinstance( msg, str ):
      raise LRuntimeUsageError( LP_make_condition, f'Invalid argument 2. STRING expected{got_str(msg)}.' )
   return {'CONDITION-TYPE': ctype, 'MESSAGE': msg}

@primitive( 'signal', '(type-or-condition &optional message)' )
def LP_signal( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Signals a condition.  If the first argument is a condition object it is
signaled directly.  Otherwise the first argument must be a type symbol and an
optional message string are used to construct the condition."""
   arg = args[0]
   if isinstance( arg, dict ) and 'CONDITION-TYPE' in arg:
      raise Signaled( arg )
   if not isinstance( arg, LSymbol ):
      raise LRuntimeUsageError( LP_signal, f'Invalid argument 1. CONDITION or SYMBOL expected{got_str(arg)}.' )
   msg = args[1] if len(args) > 1 else ''
   if not isinstance( msg, str ):
      raise LRuntimeUsageError( LP_signal, f'Invalid argument 2. STRING expected{got_str(msg)}.' )
   raise Signaled( {'CONDITION-TYPE': arg, 'MESSAGE': msg} )

@primitive( 'conditionp', '(obj)' )
def LP_conditionp( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns T if obj is a condition object (created by make-condition or
signal), NIL otherwise."""
   return T_SYM if (isinstance( args[0], dict ) and 'CONDITION-TYPE' in args[0]) else L_NIL

@primitive( 'condition-type', '(condition)' )
def LP_condition_type( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the type symbol of a condition object."""
   cond = args[0]
   if not (isinstance( cond, dict ) and 'CONDITION-TYPE' in cond):
      raise LRuntimeUsageError( LP_condition_type, f'Invalid argument 1. CONDITION expected{got_str(cond)}.' )
   return cond['CONDITION-TYPE']

@primitive( 'condition-message', '(condition)' )
def LP_condition_message( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the message string of a condition object."""
   cond = args[0]
   if not (isinstance( cond, dict ) and 'CONDITION-TYPE' in cond):
      raise LRuntimeUsageError( LP_condition_message, f'Invalid argument 1. CONDITION expected{got_str(cond)}.' )
   return cond.get( 'MESSAGE', '' )

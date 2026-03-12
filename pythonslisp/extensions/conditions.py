from __future__ import annotations
from typing import Any

from pythonslisp.ltk.EnvironmentBase import EnvironmentBase
from pythonslisp.AST import LSymbol, L_T, L_NIL
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimeError, Signaled
from pythonslisp.extensions import primitive


@primitive( 'make-condition', '(type &optional message)' )
def LP_make_condition( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Creates and returns a condition object with a type symbol and optional
message string.  Condition objects are used with signal and handler-case."""
   ctype = args[0]
   if not isinstance( ctype, LSymbol ):
      raise LRuntimeError( 'make-condition: type must be a symbol.' )
   msg = args[1] if len(args) > 1 else ''
   if not isinstance( msg, str ):
      raise LRuntimeError( 'make-condition: message must be a string.' )
   return {'CONDITION-TYPE': ctype, 'MESSAGE': msg}

@primitive( 'signal', '(type-or-condition &optional message)' )
def LP_signal( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Signals a condition.  If the first argument is a condition object it is
signaled directly.  Otherwise the first argument must be a type symbol and an
optional message string are used to construct the condition."""
   arg = args[0]
   if isinstance( arg, dict ) and 'CONDITION-TYPE' in arg:
      raise Signaled( arg )
   if not isinstance( arg, LSymbol ):
      raise LRuntimeError( 'signal: first argument must be a condition object or a symbol.' )
   msg = args[1] if len(args) > 1 else ''
   if not isinstance( msg, str ):
      raise LRuntimeError( 'signal: message must be a string.' )
   raise Signaled( {'CONDITION-TYPE': arg, 'MESSAGE': msg} )

@primitive( 'conditionp', '(obj)' )
def LP_conditionp( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Returns T if obj is a condition object (created by make-condition or
signal), NIL otherwise."""
   return L_T if (isinstance( args[0], dict ) and 'CONDITION-TYPE' in args[0]) else L_NIL

@primitive( 'condition-type', '(condition)' )
def LP_condition_type( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Returns the type symbol of a condition object."""
   cond = args[0]
   if not (isinstance( cond, dict ) and 'CONDITION-TYPE' in cond):
      raise LRuntimeError( 'condition-type: argument must be a condition object.' )
   return cond['CONDITION-TYPE']

@primitive( 'condition-message', '(condition)' )
def LP_condition_message( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Returns the message string of a condition object."""
   cond = args[0]
   if not (isinstance( cond, dict ) and 'CONDITION-TYPE' in cond):
      raise LRuntimeError( 'condition-message: argument must be a condition object.' )
   return cond.get( 'MESSAGE', '' )

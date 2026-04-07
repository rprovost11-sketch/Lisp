"""Errors and conditions extension."""
from __future__ import annotations

LISP_DOCUMENTATION_TITLE = 'Errors & Conditions'
from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.AST import LSymbol, L_T, L_NIL, got_str
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimeError, LRuntimePrimError, LRuntimeUsageError, Signaled
from pythonslisp.extensions import LambdaListMode, primitive


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
   return L_T if (isinstance( args[0], dict ) and 'CONDITION-TYPE' in args[0]) else L_NIL

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


@primitive( 'error', '(formatString &optional dictOrList)' )
def LP_error( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Signals a runtime error with the given message string.
The format string may optionally be followed by a list or map of values,
in which case the message is formatted using Python str.format() before
being raised.  With no second argument the format string is used as-is."""
   formatString = args[0]
   if not isinstance( formatString, str ):
      raise LRuntimeUsageError( LP_error, f'Invalid argument 1. STRING expected{got_str(formatString)}.' )
   if len(args) == 1:
      raise LRuntimeError( formatString )
   dictOrList = args[1]
   try:
      if isinstance( dictOrList, list ):
         message = formatString.format( *dictOrList )
      elif isinstance( dictOrList, dict ):
         strDict = { (k.name if isinstance(k, LSymbol) else k): v for k, v in dictOrList.items() }
         message = formatString.format( **strDict )
      else:
         raise LRuntimeUsageError( LP_error, f'Invalid argument 2. LIST or DICT expected{got_str(dictOrList)}.' )
   except (IndexError, KeyError, ValueError) as e:
      raise LRuntimePrimError( LP_error, f'Format error: {e}')
   raise LRuntimeError( message )


@primitive( 'handler-case',
            '(form (type1 (var) body1...) (type2 (var) body2...) ...)',
            mode=LambdaListMode.DOC_ONLY, special=True )
def LP_handler_case( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Evaluates form with condition handlers established.  If a condition is
signaled that matches a clause type, the matching clause body is evaluated with
var bound to the condition object and that result is returned.  If no clause
matches the condition propagates.  Use T or ERROR as the type to match any
condition.  handler-case also catches errors raised by the error primitive,
wrapping them in a condition with type ERROR."""
   raise LRuntimeUsageError( LP_handler_case, 'Handled by CEK machine.' )


@primitive( 'warn', '(formatString &optional dictOrList)' )
def LP_warn( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Issues a warning message and returns NIL.  Does not stop execution.
The format string and optional dict/list work the same as (error).
Output goes to *error-output* if locally rebound, otherwise to standard output."""
   formatString = args[0]
   if not isinstance( formatString, str ):
      raise LRuntimeUsageError( LP_warn, f'Invalid argument 1. STRING expected{got_str(formatString)}.' )
   if len(args) == 1:
      message = formatString
   else:
      dictOrList = args[1]
      try:
         if isinstance( dictOrList, list ):
            message = formatString.format( *dictOrList )
         elif isinstance( dictOrList, dict ):
            strDict = { (k.name if isinstance(k, LSymbol) else k): v for k, v in dictOrList.items() }
            message = formatString.format( **strDict )
         else:
            raise LRuntimeUsageError( LP_warn, f'Invalid argument 2. LIST or DICT expected{got_str(dictOrList)}.' )
      except (IndexError, KeyError, ValueError) as e:
         raise LRuntimePrimError( LP_warn, f'Format error: {e}')
   from io import IOBase
   try:
      local_val  = env.lookup( '*ERROR-OUTPUT*' )
      global_val = env.lookupGlobalWithDefault( '*ERROR-OUTPUT*', None )
      if local_val is not global_val and isinstance( local_val, IOBase ) and local_val.writable():
         out = local_val
      else:
         out = ctx.outStrm
   except Exception:
      out = ctx.outStrm
   print( f'WARNING: {message}', file=out )
   return L_NIL

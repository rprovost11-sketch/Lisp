"""Errors and conditions extension."""
from __future__ import annotations

LISP_DOCUMENTATION_TITLE = 'Errors & Conditions'
from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.AST import LSymbol, L_T, L_NIL, got_str
from pythonslisp.Context import Context
from pythonslisp.Exceptions import ( LRuntimeError, LRuntimePrimError, LRuntimeUsageError,
                                      Signaled, RestartInvoked )
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


# ── Restarts ────────────────────────────────────────────────────────────

@primitive( 'restart-case',
            '(form (name1 (params) body1...) ...)',
            mode=LambdaListMode.DOC_ONLY, special=True )
def LP_restart_case( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Establishes named restarts around the evaluation of form.  If a restart
is invoked via invoke-restart, control transfers to the matching restart
clause and the clause body's value becomes the result of restart-case.

  (restart-case (risky-op)
    (use-value (v) v)
    (skip () nil))"""
   raise LRuntimeUsageError( LP_restart_case, 'Handled by CEK machine.' )


@primitive( 'handler-bind',
            '(((type handler-fn) ...) body...)',
            mode=LambdaListMode.DOC_ONLY, special=True )
def LP_handler_bind( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Binds condition handlers that run WITHOUT unwinding the stack.  The
handler function is called in the dynamic context of the signaler.  If the
handler invokes a restart, control transfers to the matching restart-case.
If the handler returns normally, the condition continues to propagate.

  (handler-bind ((error (lambda (c) (invoke-restart 'use-value 0))))
    (risky-op))"""
   raise LRuntimeUsageError( LP_handler_bind, 'Handled by CEK machine.' )


@primitive( 'invoke-restart', '(restart-name &rest args)' )
def LP_invoke_restart( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Invokes the restart designated by restart-name, transferring control to
the restart-case that established it.  Any additional arguments are passed to
the restart's parameter list."""
   name = args[0]
   if isinstance( name, LSymbol ):
      raise RestartInvoked( name.name, args[1:] )
   if isinstance( name, dict ) and 'RESTART-NAME' in name:
      raise RestartInvoked( name['RESTART-NAME'], args[1:] )
   raise LRuntimeUsageError( LP_invoke_restart,
      f'Invalid argument 1. SYMBOL or RESTART expected{got_str(name)}.' )


@primitive( 'find-restart', '(restart-name)' )
def LP_find_restart( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a restart object for restart-name if a restart with that name
is currently active, or NIL otherwise."""
   name = args[0]
   if not isinstance( name, LSymbol ):
      raise LRuntimeUsageError( LP_find_restart,
         f'Invalid argument 1. SYMBOL expected{got_str(name)}.' )
   from pythonslisp.Evaluator import RestartCaseBodyFrame
   for K in reversed( ctx._restart_stack ):
      for frame in reversed( K ):
         if isinstance( frame, RestartCaseBodyFrame ):
            params, body = frame.find_restart( name.name )
            if body is not None:
               return { 'RESTART-NAME': name.name, 'RESTART-PARAMS': params }
   return L_NIL


@primitive( 'compute-restarts', '()', max_args=0 )
def LP_compute_restarts( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a list of all restart objects currently available."""
   from pythonslisp.Evaluator import RestartCaseBodyFrame
   result = []
   seen   = set()
   for K in reversed( ctx._restart_stack ):
      for frame in reversed( K ):
         if isinstance( frame, RestartCaseBodyFrame ):
            for rname, params, body in frame.clauses:
               if rname.name not in seen:
                  seen.add( rname.name )
                  result.append( { 'RESTART-NAME': rname.name, 'RESTART-PARAMS': params } )
   return result if result else L_NIL


@primitive( 'restart-name', '(restart)' )
def LP_restart_name( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the name of a restart object as a symbol."""
   r = args[0]
   if isinstance( r, dict ) and 'RESTART-NAME' in r:
      return LSymbol( r['RESTART-NAME'] )
   raise LRuntimeUsageError( LP_restart_name,
      f'Invalid argument 1. RESTART expected{got_str(r)}.' )

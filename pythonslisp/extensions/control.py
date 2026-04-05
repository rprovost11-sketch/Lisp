from __future__ import annotations
from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.AST import LSymbol, got_str
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimeError, LRuntimePrimError, LRuntimeUsageError, Thrown
from pythonslisp.extensions import LambdaListMode, primitive


@primitive( 'let', '(( (var1 sexpr1) (var2 sexpr2) ...) &rest body)',
            mode=LambdaListMode.DOC_ONLY, special=True )
def LP_let( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Executes a list of expressions (body) in sequence in a nested scope and
returns the result of the last one.  var1,var2,... are local variables bound to
results of expressions.  Variable initializations are not evaluated in
sequence and are not evaluated in let's nested scope."""
   raise LRuntimeUsageError( LP_let, 'Evaluation handled by main eval loop.' )

@primitive( 'let*', '(( (var1 sexpr1) (var2 sexpr2) ...) &rest body)',
            mode=LambdaListMode.DOC_ONLY, special=True )
def LP_letstar( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Executes a list of expressions (body) in sequence in a nested scope and
returns the result of the last one.  var1,var2,... are local variables bound to
results of expressions.  Variable initializations are evaluated in sequence
and are evaluated in let's nested scope.  So later initializer expressions may
refer to variables already initialized."""
   raise LRuntimeUsageError( LP_letstar, 'Evaluation handled by main eval loop.' )

@primitive( 'progn', '(&rest body)', special=True )
def LP_progn( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Evaluates each expression in body in sequence.  Returns the result of
the last evaluation."""
   raise LRuntimeUsageError( LP_progn, 'Evaluation handled by main eval loop.')

@primitive( 'if', '(cond conseq &optional alt)', special=True )
def LP_if( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Evaluates the condition.  If truthy (non-nil) then conseq is
evaluated and its result returned, otherwise alt is evaluated and its result
is returned.  Or nil is returned if there is no alt."""
   raise LRuntimeUsageError( LP_if, 'Expression evaluated in main eval loop.' )

@primitive( 'cond', '((cond1 body1) (cond2 body2) ...)',
            mode=LambdaListMode.DOC_ONLY, min_args=1, special=True )
def LP_cond( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Evaluates each cond in order until one evaluates to truthy (non-nil).
Then evaluates each expr in the paired body and returns the result of the
last expr evaluated.  All remaining conds and bodys are skipped.  End the
sequence with '(t bodyn)' to have code evaluated if no other condition
is satisfied."""
   raise LRuntimeUsageError( LP_cond, 'Handled by main eval loop.' )

@primitive( 'case', '(sexpr (val1 body1) (val2 body2) ...)',
            mode=LambdaListMode.DOC_ONLY, min_args=2, special=True )
def LP_case( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Evaluates expr.  Finds the first val that equals expr's val.  Then
evaluates each expr in the paired body and returns the result of the last expr
evaluated.  All remaining cases are skipped."""
   raise LRuntimeUsageError( LP_case, 'Handled by main eval loop.' )

@primitive( 'block', '(name &rest body)', special=True )
def LP_block( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Establishes a named lexical block.  Evaluates body forms in sequence and
returns the value of the last one.  A (return-from name value) anywhere in the
dynamic extent of the block performs an immediate non-local exit, returning
value.  The name may be a symbol or NIL."""
   raise LRuntimeUsageError( LP_block, 'Handled by CEK machine.' )

@primitive( 'return-from', '(name &optional value)', special=True )
def LP_return_from( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Performs a non-local exit from the nearest enclosing (block name ...).
Returns value (default NIL) from that block.  name is not evaluated."""
   raise LRuntimeUsageError( LP_return_from, 'Handled by CEK machine.' )

@primitive( 'return', '(&optional value)', special=True )
def LP_return( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Performs a non-local exit from the nearest enclosing (block nil ...).
Returns value (default NIL) from that block.  Equivalent to (return-from nil value)."""
   raise LRuntimeUsageError( LP_return, 'Handled by CEK machine.' )

@primitive( 'throw', '(tag result)' )
def LP_throw( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Performs a non-local exit to the nearest enclosing (catch tag ...) whose
tag is eql to this tag.  Both tag and result are evaluated before throw is
invoked.  If no matching catch exists, an error is signaled."""
   tag, value = args
   raise Thrown( tag, value )

@primitive( 'catch', '(tag sexpr1 sexpr2 ...)',
            mode=LambdaListMode.DOC_ONLY, special=True )
def LP_catch( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Establishes a dynamic catch point tagged with tag (evaluated).  Evaluates
body forms in sequence.  If (throw tag value) is executed within the dynamic
extent, catch immediately returns value.  Otherwise returns the value of the
last body form, or NIL if body is empty."""
   raise LRuntimeUsageError( LP_catch, 'Handled by CEK machine.' )

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

@primitive( 'call/cc', '(procedure)', special=True )
def LP_callcc( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Calls procedure with one argument: a first-class continuation object.
Invoking the continuation with a value restores the captured computation
state and delivers that value as the result of the original call/cc expression.
Continuations are fully re-invocable: the same continuation may be called
multiple times, reinstating the saved state each time."""
   raise LRuntimeUsageError( LP_callcc, 'Handled by CEK machine.' )

@primitive( 'dynamic-wind', '(before thunk after)', special=True )
def LP_dynamic_wind( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Calls thunk with dynamic wind protection.  before is called first, then
thunk, then after - regardless of whether thunk exits normally, via a
non-local transfer (throw, return-from, continuation), or via an error.
All three arguments must be zero-argument callables.  Returns the value
of thunk.  The macro unwind-protect is built on dynamic-wind."""
   raise LRuntimeUsageError( LP_dynamic_wind, 'Handled by CEK machine.' )

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

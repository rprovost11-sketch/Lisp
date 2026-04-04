from __future__ import annotations
from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.Environment import Environment
from pythonslisp.AST import LSymbol, LCallable, LFunction, LMultipleValues, L_NIL, eql, prettyPrintSExpr
from pythonslisp.Context import Context
from pythonslisp.Exceptions import ( LRuntimeError, LRuntimeUsageError, LArgBindingError,
                                      Thrown, Signaled, ContinuationInvoked )
from pythonslisp.extensions import LambdaListMode, primitive


@primitive( 'lambda', '(lambda-list &rest body)', special=True )
def LP_lambda( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Creates and returns an unnamed lambda function.  When evaluating such a
function the body exprs are evaluated within a nested scope.  This primitive
captures the environment it is defined in to allow for closures.  The first body
expression can be a documentation string."""
   raise LRuntimeUsageError( LP_lambda, 'Handled by CEK machine.' )

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

@primitive( 'quote', '(sexpr)', special=True )
def LP_quote( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns expr without evaluating it."""
   return args[0]

@primitive( 'quasiquote', '(sexpr)', special=True )
def LP_quasiquote( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Similar to quote but allows unquote and unquote-splicing expressions within expr.
Quasiquotes may be nested; each level of unquote belongs to the nearest enclosing quasiquote."""
   raise LRuntimeUsageError( LP_quasiquote, 'Handled by CEK machine.' )

@primitive( 'unquote', '(sexpr)', special=True )
def LP_unquote( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Must occur within a quasiquote expr or it's an error."""
   raise LRuntimeUsageError( LP_unquote, 'UNQUOTE can only occur inside a QUASIQUOTE.')

@primitive( 'unquote-splicing', '(sexpr)', special=True )
def LP_unquote_splicing( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Must occur within a quasiquote expr or it's an error."""
   raise LRuntimeUsageError( LP_unquote_splicing, 'UNQUOTE-SPLICING can only occur inside a QUASIQUOTE.')

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

@primitive( 'funcall', '(callable &rest args)', mode=LambdaListMode.DOC_ONLY, min_args=1, special=True )
def LP_funcall( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Calls a function with the args listed."""
   raise LRuntimeUsageError( LP_funcall, 'Evaluation handled by main eval loop.' )

@primitive( 'eval', '(sexpr)' )
def LP_eval( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Evaluates expr in the current scope."""
   return ctx.lEval( env, args[0] )

@primitive( 'eval-for-display', '(sexpr)' )
def LP_eval_for_display( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Evaluates sexpr (a pre-parsed AST, typically from parse) and returns a
list of all result values suitable for display in a REPL.  A normal single-value
result returns a one-element list.  Multiple values return a list of all values.
An empty multiple-values object returns NIL.
Use as (eval-for-display (parse input)) in a REPL loop to correctly display
(values ...) forms without losing values to multiple-value stripping."""
   result = ctx.lEval( env, args[0] )
   if type(result) is LMultipleValues:
      return result.values if result.values else L_NIL
   return [result]

@primitive( 'raweval-for-display', '(string &optional stream)', min_args=1, max_args=2 )
def LP_raweval_for_display( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Parses string as Lisp, runs the full pipeline (macro-expand, analyze, eval),
and returns a list of all result values suitable for display in a REPL.  A normal
single-value result returns a one-element list.  Multiple values return a list of
all values.  An empty multiple-values object returns NIL.
If stream is supplied, ctx.outStrm is set to it for the duration of the call so
that all output (including from user-defined functions with captured environments)
goes to that stream.
Use as (raweval-for-display input-string) or (raweval-for-display input-string stream)."""
   source = args[0]
   if not isinstance( source, str ):
      raise LRuntimeUsageError( LP_raweval_for_display, 'Invalid argument 1. STRING expected.' )
   from io import IOBase
   _UNSET = object()   # sentinel distinct from None so we can detect "did we change ctx.outStrm?"
   stream_val = args[1] if len(args) > 1 else None
   if isinstance( stream_val, IOBase ) and stream_val.writable():
      old_outStrm = ctx.outStrm   # save old value - may be None if Python listener set it so
      ctx.outStrm = stream_val
   else:
      old_outStrm = _UNSET        # sentinel: we did NOT change ctx.outStrm
   result = L_NIL
   try:
      ast       = ctx.parse( source )   # [PROGN, form1, ...]
      top_forms = ast[1:]               # strip PROGN wrapper
      for form in top_forms:
         form   = ctx.expand( env, form )
         ctx.analyze( env, form )
         result = ctx.lEval( env, form )
   except LArgBindingError as e:
      # Convert to plain LRuntimeError so CEK's outer except-LArgBindingError
      # (in _do_apply) does not re-wrap this with "RAWEVAL-FOR-DISPLAY" as the
      # function name.  The error originated inside expand/eval, not in our own
      # argument binding.
      raise LRuntimeError( str(e) ) from e
   except LRuntimeError:
      raise
   except Signaled as e:
      _ct = prettyPrintSExpr( e.condition.get('CONDITION-TYPE', LSymbol('UNKNOWN')) )
      _cm = e.condition.get('MESSAGE', '')
      raise LRuntimeError( f'Unhandled condition {_ct}: {_cm}' if _cm else f'Unhandled condition {_ct}' ) from e
   except Thrown as e:
      raise LRuntimeError( f'throw: no catch for tag {prettyPrintSExpr(e.tag)}.' ) from e
   except ContinuationInvoked:
      raise LRuntimeError( 'Continuation invoked outside its dynamic extent.' )
   except Exception as e:
      raise LRuntimeError( str(e) ) from e
   finally:
      if old_outStrm is not _UNSET:
         ctx.outStrm = old_outStrm
   if type(result) is LMultipleValues:
      return result.values if result.values else L_NIL
   return [result]

@primitive( 'apply', '(function &rest args)',
            mode=LambdaListMode.DOC_ONLY, min_args=2, special=True )
def LP_apply( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Applies function to the args.  The last argument must be a list whose
elements are appended to any preceding individual args.  Returns the result of
that function application.  function is any callable that is not a special form."""
   raise LRuntimeUsageError( LP_apply, 'Evaluation handled by main eval loop.' )

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

@primitive( 'multiple-value-bind',
            '((var1 var2 ...) values-form &rest body)',
            mode=LambdaListMode.DOC_ONLY, special=True )
def LP_multiple_value_bind( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Evaluates values-form and binds its multiple return values to the listed
variables in a new scope.  Extra variables beyond the number of values are bound
to NIL; extra values beyond the number of variables are discarded.  Evaluates
body forms in sequence and returns the result of the last."""
   raise LRuntimeUsageError( LP_multiple_value_bind, 'Handled by CEK machine.' )

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

@primitive( 'dynamic-wind', '(before thunk after)', special=True )
def LP_dynamic_wind( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Calls thunk with dynamic wind protection.  before is called first, then
thunk, then after - regardless of whether thunk exits normally, via a
non-local transfer (throw, return-from, continuation), or via an error.
All three arguments must be zero-argument callables.  Returns the value
of thunk.  The macro unwind-protect is built on dynamic-wind."""
   raise LRuntimeUsageError( LP_dynamic_wind, 'Handled by CEK machine.' )

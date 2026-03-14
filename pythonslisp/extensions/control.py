from __future__ import annotations
from typing import Any

from pythonslisp.ltk.EnvironmentBase import EnvironmentBase
from pythonslisp.Environment import Environment
from pythonslisp.AST import LSymbol, LCallable, LFunction, LMultipleValues, L_NIL, eql
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimeError, LRuntimePrimError, Thrown, ReturnFrom, Signaled
from pythonslisp.extensions import LambdaListMode, primitive


def _block_name( obj ):
   """Extract the block name string from a BLOCK or RETURN-FROM name argument.
Accepts a symbol or NIL (the empty list) as the name."""
   if isinstance(obj, LSymbol):
      return obj.name
   if isinstance(obj, list) and not obj:   # L_NIL / empty list
      return 'NIL'
   raise LRuntimeError('block: name must be a symbol.')


@primitive( 'lambda', '(lambda-list &rest body)', preEvalArgs=False )
def LP_lambda( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Creates and returns an unnamed lambda function.  When evaluating such a
function the body exprs are evaluated within a nested scope.  This primitive
captures the environment it is defined in to allow for closures.  The first body
expression can be a documentation string."""
   funcParams, *funcBody = args
   if funcBody and isinstance(funcBody[0], str):
      docString, *funcBody = funcBody
   else:
      docString = ''
   return LFunction( LSymbol(""), funcParams, docString, funcBody, capturedEnvironment=env )

@primitive( 'let', '(( (var1 sexpr1) (var2 sexpr2) ...) &rest body)',
            mode=LambdaListMode.DOC_ONLY, preEvalArgs=False )
def LP_let( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Executes a list of expressions (body) in sequence in a nested scope and
returns the result of the last one.  var1,var2,... are local variables bound to
results of expressions.  Variable initializations are not evaluated in
sequence and are not evaluated in let's nested scope."""
   raise LRuntimePrimError( LP_let, 'Evaluation handled by main eval loop.' )

@primitive( 'let*', '(( (var1 sexpr1) (var2 sexpr2) ...) &rest body)',
            mode=LambdaListMode.DOC_ONLY, preEvalArgs=False )
def LP_letstar( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Executes a list of expressions (body) in sequence in a nested scope and
returns the result of the last one.  var1,var2,... are local variables bound to
results of expressions.  Variable initializations are evaluated in sequence
and are evaluated in let's nested scope.  So later initializer expressions may
refer to variables already initialized."""
   raise LRuntimePrimError( LP_letstar, 'Evaluation handled by main eval loop.' )

@primitive( 'progn', '(&rest body)', preEvalArgs=False )
def LP_progn( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Evaluates each expression in body in sequence.  Returns the result of
the last evaluation."""
   raise LRuntimePrimError( LP_progn, 'Evaluation handled by main eval loop.')

@primitive( 'if', '(cond conseq &optional alt)', preEvalArgs=False )
def LP_if( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Evaluates the condition.  If truthy (non-nil) then conseq is
evaluated and its result returned, otherwise alt is evaluated and its result
is returned.  Or nil is returned if there is no alt."""
   raise LRuntimePrimError( LP_if, 'Expression evaluated in main eval loop.' )

@primitive( 'cond', '((cond1 body1) (cond2 body2) ...)',
            mode=LambdaListMode.DOC_ONLY, preEvalArgs=False, min_args=1 )
def LP_cond( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Evaluates each cond in order until one evaluates to truthy (non-nil).
Then evaluates each expr in the paired body and returns the result of the
last expr evaluated.  All remaining conds and bodys are skipped.  End the
sequence with '(t bodyn)' to have code evaluated if no other condition
is satisfied."""
   raise LRuntimePrimError( LP_cond, 'Handled by main eval loop.' )

@primitive( 'case', '(sexpr (val1 body1) (val2 body2) ...)',
            mode=LambdaListMode.DOC_ONLY, preEvalArgs=False, min_args=2 )
def LP_case( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Evaluates expr.  Finds the first val that equals expr's val.  Then
evaluates each expr in the paired body and returns the result of the last expr
evaluated.  All remaining cases are skipped."""
   raise LRuntimePrimError( LP_case, 'Handled by main eval loop.' )

@primitive( 'quote', '(sexpr)', preEvalArgs=False )
def LP_quote( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Returns expr without evaluating it."""
   return args[0]

@primitive( 'quasiquote', '(sexpr)', preEvalArgs=False )
def LP_quasiquote( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Similar to quote but allows unquote and unquote-splicing expressions within expr.
Quasiquotes may be nested; each level of unquote belongs to the nearest enclosing quasiquote."""
   result = ctx.lQuasiquoteExpand( ctx, env, args[0] )
   if ( isinstance(result, list) and
        len(result) > 0 and
        result[0] == LSymbol('UNQUOTE-SPLICING') ):
      raise LRuntimeError( "Ill-placed ,@ (UNQUOTE-SPLICING): splice requires a list context." )
   return result

@primitive( 'unquote', '(sexpr)', preEvalArgs=False )
def LP_unquote( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Must occur within a quasiquote expr or it's an error."""
   raise LRuntimePrimError( LP_unquote, 'UNQUOTE can only occur inside a QUASIQUOTE.')

@primitive( 'unquote-splicing', '(sexpr)', preEvalArgs=False )
def LP_unquote_splicing( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Must occur within a quasiquote expr or it's an error."""
   raise LRuntimePrimError( LP_unquote_splicing, 'UNQUOTE-SPLICING can only occur inside a QUASIQUOTE.')

@primitive( 'block', '(name &rest body)', preEvalArgs=False )
def LP_block( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Establishes a named lexical block.  Evaluates body forms in sequence and
returns the value of the last one.  A (return-from name value) anywhere in the
dynamic extent of the block performs an immediate non-local exit, returning
value.  The name may be a symbol or NIL."""
   blockNameStr = _block_name(args[0])   # analyzer guarantees: symbol or NIL
   body = args[1:]
   if len(body) == 0:
      return L_NIL
   try:
      for sexpr in body[:-1]:
         ctx.lEval( env, sexpr )
      return ctx.lEval( env, body[-1] )
   except ReturnFrom as e:
      if e.name == blockNameStr:
         return e.value
      raise

@primitive( 'return-from', '(name &optional value)', preEvalArgs=False )
def LP_return_from( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Performs a non-local exit from the nearest enclosing (block name ...).
Returns value (default NIL) from that block.  name is not evaluated."""
   blockNameStr = _block_name(args[0])   # analyzer guarantees: symbol or NIL
   value = ctx.lEval( env, args[1] ) if len(args) == 2 else L_NIL
   raise ReturnFrom( blockNameStr, value )

@primitive( 'return', '(&optional value)', preEvalArgs=False )
def LP_return( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Performs a non-local exit from the nearest enclosing (block nil ...).
Returns value (default NIL) from that block.  Equivalent to (return-from nil value)."""
   value = ctx.lEval( env, args[0] ) if len(args) == 1 else L_NIL
   raise ReturnFrom( 'NIL', value )

@primitive( 'funcall', '(callable &rest args)', mode=LambdaListMode.DOC_ONLY, preEvalArgs=False, min_args=1 )
def LP_funcall( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Calls a function with the args listed."""
   raise LRuntimePrimError( LP_funcall, 'Evaluation handled by main eval loop.' )

@primitive( 'eval', '(sexpr)' )
def LP_eval( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Evaluates expr in the current scope."""
   return ctx.lEval( env, args[0] )

@primitive( 'apply', '(function &rest args)',
            mode=LambdaListMode.DOC_ONLY, preEvalArgs=False, min_args=2 )
def LP_apply( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Applies function to the args.  The last argument must be a list whose
elements are appended to any preceding individual args.  Returns the result of
that function application.  function is any callable that is not a special form."""
   raise LRuntimePrimError( LP_apply, 'Evaluation handled by main eval loop.' )

@primitive( 'throw', '(tag result)' )
def LP_throw( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Performs a non-local exit to the nearest enclosing (catch tag ...) whose
tag is eql to this tag.  Both tag and result are evaluated before throw is
invoked.  If no matching catch exists, an error is signaled."""
   tag, value = args
   raise Thrown( tag, value )

@primitive( 'catch', '(tag sexpr1 sexpr2 ...)',
            mode=LambdaListMode.DOC_ONLY, preEvalArgs=False )
def LP_catch( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Establishes a dynamic catch point tagged with tag (evaluated).  Evaluates
body forms in sequence.  If (throw tag value) is executed within the dynamic
extent, catch immediately returns value.  Otherwise returns the value of the
last body form, or NIL if body is empty."""
   tag  = ctx.lEval( env, args[0] )
   body = args[1:]
   try:
      result = L_NIL
      for sexpr in body:
         result = ctx.lEval( env, sexpr )
      return result
   except Thrown as e:
      if eql( e.tag, tag ):
         return e.value
      raise

@primitive( 'multiple-value-bind',
            '((var1 var2 ...) values-form &rest body)',
            mode=LambdaListMode.DOC_ONLY, preEvalArgs=False )
def LP_multiple_value_bind( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Evaluates values-form and binds its multiple return values to the listed
variables in a new scope.  Extra variables beyond the number of values are bound
to NIL; extra values beyond the number of variables are discarded.  Evaluates
body forms in sequence and returns the result of the last."""
   var_list, values_form, *body = args
   result = ctx.lEval( env, values_form )
   if type(result) is LMultipleValues:
      vals = result.values
   else:
      vals = [result]
   new_env = Environment( env )
   for i, var in enumerate( var_list ):
      new_env.bindLocal( var.name, vals[i] if i < len(vals) else L_NIL )
   if len(body) == 0:
      return L_NIL
   for sexpr in body[:-1]:
      ctx.lEval( new_env, sexpr )
   return ctx.lEval( new_env, body[-1] )

@primitive( 'handler-case',
            '(form (type1 (var) body1...) (type2 (var) body2...) ...)',
            mode=LambdaListMode.DOC_ONLY, preEvalArgs=False )
def LP_handler_case( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
   """Evaluates form with condition handlers established.  If a condition is
signaled that matches a clause type, the matching clause body is evaluated with
var bound to the condition object and that result is returned.  If no clause
matches the condition propagates.  Use T or ERROR as the type to match any
condition.  handler-case also catches errors raised by the error primitive,
wrapping them in a condition with type ERROR."""
   if len(args) == 0:
      raise LRuntimeError( 'handler-case: requires a protected form.' )
   form    = args[0]
   clauses = args[1:]
   parsed  = []
   for clause in clauses:
      if not isinstance( clause, list ) or len(clause) < 2:
         raise LRuntimeError( 'handler-case: malformed clause.' )
      ctype   = clause[0]
      var_lst = clause[1]
      body    = clause[2:]
      var     = var_lst[0] if isinstance( var_lst, list ) and len(var_lst) > 0 else None
      parsed.append( (ctype, var, body) )

   def _find_handler( type_name: str ):
      for ctype, var, body in parsed:
         if isinstance( ctype, LSymbol ):
            if ctype.name in ('T', 'ERROR'):
               return var, body
            if ctype.name == type_name:
               return var, body
      return None, None

   def _run_handler( var, body, cond ):
      new_env = Environment( env )
      if var is not None:
         new_env.bindLocal( var.name, cond )
      result = L_NIL
      for sexpr in body:
         result = ctx.lEval( new_env, sexpr )
      return result

   try:
      return ctx.lEval( env, form )
   except Signaled as e:
      cond      = e.condition
      ctype_sym = cond.get( 'CONDITION-TYPE', LSymbol('ERROR') )
      type_name = ctype_sym.name if isinstance( ctype_sym, LSymbol ) else 'ERROR'
      var, body = _find_handler( type_name )
      if var is None and body is None:
         raise
      return _run_handler( var, body, cond )
   except LRuntimeError as e:
      var, body = _find_handler( 'ERROR' )
      if var is None and body is None:
         raise
      cond = {'CONDITION-TYPE': LSymbol('ERROR'), 'MESSAGE': str(e)}
      return _run_handler( var, body, cond )

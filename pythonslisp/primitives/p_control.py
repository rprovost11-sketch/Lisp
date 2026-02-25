from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.LispAST import LSymbol, LCallable, LFunction, L_NIL
from pythonslisp.LispContext import LispContext
from pythonslisp.LispExceptions import LispRuntimeFuncError, Thrown, ReturnFrom


def register(primitive) -> None:

   @primitive( 'lambda', '<lambda-list> <sexpr1> <sexpr2> ...', specialForm=True,
               min_args=1, arity_msg='1 or more arguments expected.' )
   def LP_lambda( ctx: LispContext, env: Environment, *args ) -> Any:
      """Creates and returns an unnamed lambda function.  When evaluating such a
function the body (the exprs) are evaluated within a nested scope.  This
primitive captures the environment it is defined in to allow for closures.
The first body expression can be a documentation string."""
      funcParams, *funcBody = args
      if funcBody and isinstance(funcBody[0], str):
         docString, *funcBody = funcBody
      else:
         docString = ''
      return LFunction( LSymbol(""), funcParams, docString, funcBody, capturedEnvironment=env )

   @primitive( 'let', '( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <sexpr1> <sexpr2> ...)', specialForm=True )
   def LP_let( ctx: LispContext, env: Environment, *args ) -> Any:
      """Executes a list of expressions in sequence in a nested scope and returns
the result of the last one.  var1,var2,... are local variables bound to
results of expressions.  Variable initializations are not evaluated in
sequence and are not evaluated in let's nested scope."""
      raise LispRuntimeFuncError( LP_let, 'Evaluation handled by main eval loop.' )

   @primitive( 'let*', '( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <sexpr1> <sexpr2> ...)', specialForm=True )
   def LP_letstar( ctx: LispContext, env: Environment, *args ) -> Any:
      """Executes a list of expressions in sequence in a nested scope and returns
the result of the last one.  var1,var2,... are local variables bound to
results of expressions.  Variable initializations are evaluated in sequence
and are evaluated in let's nested scope.  So later initializer expressions may
refer to variables already initialized."""
      raise LispRuntimeFuncError( LP_letstar, 'Evaluation handled by main eval loop.' )

   @primitive( 'progn', '<sexpr1> <sexpr2> ...', specialForm=True )
   def LP_progn( ctx: LispContext, env: Environment, *args ) -> Any:
      """Evaluates each sexpression in sequence.  Returns the result of the last evaluation."""
      raise LispRuntimeFuncError( LP_progn, 'Evaluation handled by main eval loop.')

   @primitive( 'if', '<cond> <conseq> &optional <alt>', specialForm=True )
   def LP_if( ctx: LispContext, env: Environment, *args ) -> Any:
      """Evaluates the condition.  If truthy (non-nil) then consequence is
evaluated and its result returned, otherwise alt is evaluated and its result
is returned."""
      raise LispRuntimeFuncError( LP_if, 'Expression evaluated in main eval loop.' )

   @primitive( 'cond', '(<cond1> <body1>) (<cond2> <body2>) ...', specialForm=True,
               min_args=1, arity_msg='1 or more argument expected.' )
   def LP_cond( ctx: LispContext, env: Environment, *args ) -> Any:
      """Evaluates each cond in order until one evaluates to truthy (non-nil).
Then evaluates each expr in the paired body and returns the result of the
last expr evaluated.  All remaining conds and bodys are skipped.  End the
sequence with '(t <bodyn>)' to have code evaluated if no other condition
is satisfied."""
      raise LispRuntimeFuncError( LP_cond, 'Handled by main eval loop.' )

   @primitive( 'case', '<sexpr> (<val1> <body1>) (<val2> <body2>) ...', specialForm=True,
               min_args=2, arity_msg='2 or more arguments expected.' )
   def LP_case( ctx: LispContext, env: Environment, *args ) -> Any:
      """Evaluates expr.  Finds the first val that equals expr's val.  Then
evaluates each expr in body and returns the result of the last expr evaluated.
All remaining cases are skipped."""
      raise LispRuntimeFuncError( LP_case, 'Handled by main eval loop.' )

   @primitive( 'quote', '<sexpr>', specialForm=True,
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_quote( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns expr without evaluating it."""
      return args[0]

   @primitive( 'backquote', '<sexpr>', specialForm=True,
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_backquote( ctx: LispContext, env: Environment, *args ) -> Any:
      """Similar to quote but allows comma and comma-at expressions within expr.
Backquotes may be nested; each level of comma belongs to the nearest enclosing backquote."""
      return ctx.lBackquoteExpand( env, args[0] )

   @primitive( 'comma', '<sexpr>', specialForm=True )
   def LP_comma( ctx: LispContext, env: Environment, *args ) -> Any:
      """Must occur within a backquote expr or it's an error."""
      raise LispRuntimeFuncError( LP_comma, 'COMMA can only occur inside a BACKQUOTE.')

   @primitive( 'comma-at', '<sexpr>', specialForm=True )
   def LP_comma_at( ctx: LispContext, env: Environment, *args ) -> Any:
      """Must occur within a backquote expr or it's an error."""
      raise LispRuntimeFuncError( LP_comma_at, 'COMMA-AT can only occur inside a BACKQUOTE.')

   @primitive( 'while', '<cond> <sexpr1> <sexpr2> ...', specialForm=True )
   def LP_while( ctx: LispContext, env: Environment, *args ) -> Any:
      """Perform a loop over the body sexprs.  Before each iteration conditionExpr
is evaluated.  If it evaluates as truthy (non-nil) the exprs are evaluated
in sequence.  However if conditionExpr evaluates to nil, the loop terminates
and returns the result of the last expr evaluated."""
      raise LispRuntimeFuncError( LP_while, 'Evaluation handled by macro.' )

   @primitive( 'dotimes', '(<var> <countExpr>) <sexpr1> <sexpr2> ...', specialForm=True )
   def LP_dotimes( ctx: LispContext, env: Environment, *args ) -> Any:
      """Performs a loop over a body of exprs countExpr times.  Before each iteration
the loop variable is set to the next loop count number (starting with 0 for
the first loop).  The value of the last sexpr evaluated is returned."""
      raise LispRuntimeFuncError( LP_dotimes, 'Evaluation handled by macro.' )

   @primitive( 'foreach', '<variable> <list> <sexpr1> <sexpr2> ...', specialForm=True )
   def LP_foreach( ctx: LispContext, env: Environment, *args ) -> Any:
      """Perform a loop over the elements of list.  On each iteration var is set
to the next element in the list, then the expr's are evaluated in order.
Returns the result of the very last evaluation."""
      raise LispRuntimeFuncError( LP_foreach, 'Evaluation handled by macro.' )

   @primitive( 'dolist', '(<variable> <list>) <sexpr1> <sexpr2> ...', specialForm=True )
   def LP_dolist( ctx: LispContext, env: Environment, *args ) -> Any:
      """Iterate over the elements of list, binding variable to each in turn.
Returns the result of the last body expression, or NIL if the list is empty."""
      raise LispRuntimeFuncError( LP_dolist, 'Evaluation handled by macro.' )

   @primitive( 'block', '<name> <sexpr1> <sexpr2> ...', specialForm=True )
   def LP_block( ctx: LispContext, env: Environment, *args ) -> Any:
      """Establishes a named lexical block.  Evaluates body forms in sequence and
returns the value of the last one.  A (return-from name value) anywhere in the
dynamic extent of the block performs an immediate non-local exit, returning value."""
      blockName = args[0]   # analyzer guarantees: LSymbol
      body = args[1:]
      if len(body) == 0:
         return L_NIL
      try:
         for sexpr in body[:-1]:
            ctx.lEval( env, sexpr )
         return ctx.lEval( env, body[-1] )
      except ReturnFrom as e:
         if e.name == blockName.strval:
            return e.value
         raise

   @primitive( 'return-from', '<name> &optional <value>', specialForm=True )
   def LP_return_from( ctx: LispContext, env: Environment, *args ) -> Any:
      """Performs a non-local exit from the nearest enclosing (block name ...).
Returns value (default NIL) from that block.  name is not evaluated."""
      blockName = args[0]   # analyzer guarantees: LSymbol
      value = ctx.lEval( env, args[1] ) if len(args) == 2 else L_NIL
      raise ReturnFrom( blockName.strval, value )

   @primitive( 'funcall', '<fnNameSymbol> <arg1> <arg2> ...',
               min_args=1, arity_msg='1 or more arguments expected' )
   def LP_funcall( ctx: LispContext, env: Environment, *args ) -> Any:
      """Calls a function with the args listed."""
      return ctx.lApply( env, args[0], args[1:] )

   @primitive( 'eval', '<sexpr>',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_eval( ctx: LispContext, env: Environment, *args ) -> Any:
      """Evaluates expr in the current scope."""
      return ctx.lEval( env, args[0] )

   @primitive( 'apply', '<function> &rest <args> <argsList>',
               min_args=2, arity_msg='At least 2 arguments expected to apply.' )
   def LP_apply( ctx: LispContext, env: Environment, *args ) -> Any:
      """Inserts arg1,arg2,... into the front of listOfMoreArgs, then applies the
function the the whole list of args.  Returns the result of that function
application.

function is any callable that is not a special form."""

      listArg = args[-1]
      if not isinstance(listArg, list):
         raise LispRuntimeFuncError( LP_apply, "Last argument expected to be a list." )

      primary = args[0]
      if isinstance(primary, LCallable):
         fnObj = primary
      elif isinstance(primary, LSymbol):
         try:
            fnObj = env.lookup( primary.strval )
         except KeyError:
            raise LispRuntimeFuncError( LP_apply, f'First argument "{primary}" expected to be the name of a callable.')
      else:
         raise LispRuntimeFuncError( LP_apply, "First argument expected to be a symbol.")

      if fnObj.specialForm:
         raise LispRuntimeFuncError( LP_apply, "First argument may not be a special form." )

      fnArgs = list( args[1:-1] )
      fnArgs.extend( listArg )

      return ctx.lApply( env, fnObj, fnArgs )

   @primitive( 'and', '&rest <forms>', specialForm=True )
   def LP_and( ctx: LispContext, env: Environment, *args ) -> Any:
      """Evaluates forms left to right.  Returns nil at the first nil form.
Returns the value of the last form if all are truthy.  (and) returns t.
Short-circuits: stops evaluating upon encountering the first nil."""
      raise LispRuntimeFuncError( LP_and, 'Evaluation handled by macro.' )

   @primitive( 'or', '&rest <forms>', specialForm=True )
   def LP_or( ctx: LispContext, env: Environment, *args ) -> Any:
      """Evaluates forms left to right.  Returns the first truthy value found.
Returns nil if all forms are nil.  (or) returns nil.
Short-circuits: stops evaluating upon encountering the first truthy value."""
      raise LispRuntimeFuncError( LP_or, 'Evaluation handled by macro.' )

   @primitive( 'throw', '<tag> <result>',
               min_args=2, max_args=2, arity_msg='2 arguments expected.' )
   def LP_throw( ctx: LispContext, env: Environment, *args ) -> Any:
      """Performs a non-local exit to the nearest enclosing (catch tag ...) whose
tag is eql to this tag.  Both tag and result are evaluated before throw is
invoked.  If no matching catch exists, an error is signaled."""
      tag, value = args
      raise Thrown( tag, value )

   @primitive( 'catch', '<tag> <sexpr1> <sexpr2> ...', specialForm=True )
   def LP_catch( ctx: LispContext, env: Environment, *args ) -> Any:
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
         if ctx.lEql( e.tag, tag ):
            return e.value
         raise

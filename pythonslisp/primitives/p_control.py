from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.LispAST import LSymbol, LFunction, LMacro, LCallable
from pythonslisp.LispAST import L_T, L_NIL
from pythonslisp.LispExceptions import LispRuntimeFuncError
from pythonslisp.LispInterpreter import LispInterpreter


def register(primitive) -> None:

   @primitive( 'lambda', '<lambda-list> <sexpr1> <sexpr2> ...', specialForm=True )
   def LP_lambda( env: Environment, *args ) -> Any:
      """Creates and returns an unnamed lambda function.  When evaluating such a
function the body (the exprs) are evaluated within a nested scope.  This
primitive captures the environment it is defined in to allow for closures.
The first body expression can be a documentation string."""
      try:
         funcParams, *funcBody = args
      except ValueError:
         raise LispRuntimeFuncError( LP_lambda, '2 arguments expected.' )

      if len(funcBody) < 1:
         raise LispRuntimeFuncError( LP_lambda, 'At least one body expression expected.' )

      if isinstance(funcBody[0], str):
         docString = funcBody[0]
         funcBody = funcBody[1:]
      else:
         docString = ''

      if len(funcBody) < 1:
         raise LispRuntimeFuncError( LP_lambda, 'At least one body expression expected after docstring.' )

      return LFunction( LSymbol(""), funcParams, docString, funcBody, capturedEnvironment=env )

   @primitive( 'let', '( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <sexpr1> <sexpr2> ...)', specialForm=True )
   def LP_let( env: Environment, *args ) -> Any:
      """Executes a list of expressions in sequence in a nested scope and returns
the result of the last one.  var1,var2,... are local variables bound to
results of expressions.  Variable initializations are not evaluated in
sequence and are not evaluated in let's nested scope."""
      try:
         vardefs, *body = args
      except ValueError:
         raise LispRuntimeFuncError( LP_let, '2 or more arguments expected.' )

      if not isinstance(vardefs,  list):
         raise LispRuntimeFuncError( LP_let, 'The first argument to let expected to be a list of variable initializations.' )

      # Evaluate the var def initial value exprs in the outer scope.
      initDict = { }
      for varSpec in vardefs:
         if isinstance(varSpec, LSymbol):
            varName  = varSpec
            initForm = list()
         elif isinstance(varSpec, list):
            varSpecLen = len(varSpec)
            if varSpecLen == 1:
               varName  = varSpec[0]
               initForm = list()
            elif varSpecLen == 2:
               varName, initForm = varSpec
            else:
               raise LispRuntimeFuncError( LP_let, 'Variable initializer spec expected to be 1 or 2 elements long.' )

            if not isinstance(varName, LSymbol):
               raise LispRuntimeFuncError( LP_let, 'First element of a variable initializer pair expected to be a symbol.' )
         else:
            raise LispRuntimeFuncError( LP_let, 'Variable initializer spec expected to be a symbol or a list.' )

         initDict[varName.strval] = LispInterpreter._lEval(env, initForm)

      # Open the new scope
      env = Environment( env, initialBindings=initDict )

      # Evaluate each body sexpr in the new env/scope
      lastResult = L_NIL
      for sexpr in body:
         lastResult = LispInterpreter._lEval( env, sexpr )
      return lastResult

   @primitive( 'let*', '( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <sexpr1> <sexpr2> ...)', specialForm=True )
   def LP_letstar( env: Environment, *args ) -> Any:
      """Executes a list of expressions in sequence in a nested scope and returns
the result of the last one.  var1,var2,... are local variables bound to
results of expressions.  Variable initializations are evaluated in sequence
and are evaluated in let's nested scope.  So later initializer expressions may
refer to variables already initialized."""
      try:
         vardefs, *body = args
      except ValueError:
         raise LispRuntimeFuncError( LP_letstar, '2 or more arguments expected.' )

      if not isinstance(vardefs,  list):
         raise LispRuntimeFuncError( LP_letstar, 'The first argument to let expected to be a list of variable initializations.' )

      # Open the new scope
      env = Environment( env )

      for varSpec in vardefs:
         if isinstance(varSpec, LSymbol):
            varName  = varSpec
            initForm = list()
         elif isinstance(varSpec, list):
            varSpecLen = len(varSpec)
            if varSpecLen == 1:
               varName  = varSpec[0]
               initForm = list()
            elif varSpecLen == 2:
               varName, initForm = varSpec
            else:
               raise LispRuntimeFuncError( LP_letstar, 'Variable initializer spec expected to be 1 or 2 elements long.' )

            if not isinstance(varName, LSymbol):
               raise LispRuntimeFuncError( LP_letstar, 'First element of a variable initializer pair expected to be a symbol.' )
         else:
            raise LispRuntimeFuncError( LP_letstar, 'Variable initializer spec expected to be a symbol or a list.' )

         env.bindLocal( varName.strval, LispInterpreter._lEval(env, initForm) )

      # Evaluate each body sexpr in the new env/scope.
      lastResult = L_NIL
      for sexpr in body:
         lastResult = LispInterpreter._lEval( env, sexpr )
      return lastResult

   @primitive( 'progn', '<sexpr1> <sexpr2> ...', specialForm=True )
   def LP_progn( env: Environment, *args ) -> Any:
      """Evaluates each sexpression in sequence.  Returns the result of the last evaluation."""
      lastResult = L_NIL
      for expr in args:
         lastResult = LispInterpreter._lEval( env, expr )
      return lastResult

   @primitive( 'if', '<cond> <conseq> &optional <alt>', specialForm=True )
   def LP_if( env: Environment, *args ) -> Any:
      """Evaluates the condition.  If truthy (non-nil) then consequence is
evaluated and its result returned, otherwise alt is evaluated and its result
is returned."""
      numArgs = len(args)
      if not(2 <= numArgs <= 3):
         raise LispRuntimeFuncError( LP_if, '2 or 3 arguments expected.' )

      condValue = LispInterpreter._lEval( env, args[0] )
      if LispInterpreter._lTrue(condValue):
         return LispInterpreter._lEval( env, args[1])
      elif numArgs == 3:
         return LispInterpreter._lEval( env, args[2])
      else:
         return L_NIL

   @primitive( 'cond', '(<cond1> <body1>) (<cond2> <body2>) ...', specialForm=True )
   def LP_cond( env: Environment, *args ) -> Any:
      """Evaluates each cond in order until one evaluates to truthy (non-nil).
Then evaluates each expr in the paired body and returns the result of the
last expr evaluated.  All remaining conds and bodys are skipped.  End the
sequence with '(t <bodyn>)' to have code evaluated if no other condition
is satisfied."""
      if len(args) < 1:
         raise LispRuntimeFuncError( LP_cond, '1 or more argument expected.' )
      caseList = args

      for caseNum,case in enumerate(caseList):
         try:
            testExpr, *body = case
         except (ValueError, TypeError):
            raise LispRuntimeFuncError( LP_cond, f"Entry {caseNum+1} does not contain a (<cond:expr> <body:expr>) pair." )

         if len(body) < 1:
            raise LispRuntimeFuncError( LP_cond, f'Entry {caseNum+1} expects at least one body expression.' )

         if LispInterpreter._lTrue(LispInterpreter._lEval(env,testExpr)):
            latestResult = L_NIL
            for sexpr in body:
               latestResult = LispInterpreter._lEval( env, sexpr )
            return latestResult

      return L_NIL

   @primitive( 'case', '<sexpr> (<val1> <body1>) (<val2> <body2>) ...', specialForm=True )
   def LP_case( env: Environment, *args ) -> Any:
      """Evaluates expr.  Finds the first val that equals expr's val.  Then
evaluates each expr in body and returns the result of the last expr evaluated.
All remaining cases are skipped."""
      try:
         expr, *caseList = args
      except ValueError:
         raise LispRuntimeFuncError( LP_case, '2 or more arguments expected.' )
      exprVal = LispInterpreter._lEval( env, expr )

      if len(caseList) < 1:
         raise LispRuntimeFuncError( LP_case, 'At least one case expected.' )

      for caseNum,case in enumerate(caseList):
         try:
            caseVal, *body = case
         except (ValueError, TypeError):
            raise LispRuntimeFuncError( LP_case, f'Entry {caseNum+1} does not contain a (<val> <body>) pair.' )

         if len(body) < 1:
            raise LispRuntimeFuncError( LP_case, "Case body expected." )

         if LispInterpreter._lEval(env,caseVal) == exprVal:
            latestResult = None
            for sexpr in body:
               latestResult = LispInterpreter._lEval( env, sexpr )
            return latestResult

      return L_NIL

   @primitive( 'quote', '<sexpr>', specialForm=True )
   def LP_quote( env: Environment, *args ) -> Any:
      """Returns expr without evaluating it."""
      if (len(args) != 1):
         raise LispRuntimeFuncError( LP_quote, '1 argument expected.' )
      return args[0]

   @primitive( 'backquote', '<sexpr>', specialForm=True )
   def LP_backquote( env: Environment, *args ) -> Any:
      """Similar to quote but allows comma and comma-at expressions within expr.
Backquotes may be nested; each level of comma belongs to the nearest enclosing backquote."""
      if (len(args) != 1):
         raise LispRuntimeFuncError( LP_backquote, '1 argument expected.' )
      sExpr = args[0]
      return LispInterpreter._lbackquoteExpand( env, sExpr )

   @primitive( 'comma', '<sexpr>', specialForm=True )
   def LP_comma( env: Environment, *args ) -> Any:
      """Must occur within a backquote expr or it's an error."""
      raise LispRuntimeFuncError( LP_comma, 'COMMA can only occur inside a BACKQUOTE.')

   @primitive( 'comma-at', '<sexpr>', specialForm=True )
   def LP_comma_at( env: Environment, *args ) -> Any:
      """Must occur within a backquote expr or it's an error."""
      raise LispRuntimeFuncError( LP_comma_at, 'COMMA-AT can only occur inside a BACKQUOTE.')

   @primitive( 'while', '<cond> <sexpr1> <sexpr2> ...', specialForm=True )
   def LP_while( env: Environment, *args ) -> Any:
      """Perform a loop over the body sexprs.  Before each iteration conditionExpr
is evaluated.  If it evaluates as truthy (non-nil) the exprs are evaluated
in sequence.  However if conditionExpr evaluates to nil, the loop terminates
and returns the result of the last expr evaluated."""
      try:
         conditionExpr, *body = args
      except ValueError:
         raise LispRuntimeFuncError( LP_while, '2 arguments expected.' )

      if len(body) < 1:
         raise LispRuntimeFuncError( LP_while, 'At least one sexpr expected for the body.' )

      latestResult = L_NIL
      condResult = LispInterpreter._lEval(env, conditionExpr)
      while LispInterpreter._lTrue( condResult ):
         for expr in body:
            latestResult = LispInterpreter._lEval( env, expr )
         condResult = LispInterpreter._lEval(env, conditionExpr )
      return latestResult

   @primitive( 'dotimes', '(<var> <countExpr>) <sexpr1> <sexpr2> ...', specialForm=True )
   def LP_dotimes( env: Environment, *args ) -> Any:
      """Performs a loop over a body of exprs countExpr times.  Before each iteration
the loop variable is set to the next loop count number (starting with 0 for
the first loop).  The value of the last sexpr evaluated is returned."""
      try:
         loopControl, *body = args
      except ValueError:
         raise LispRuntimeFuncError( LP_dotimes, '2 or more arguments expected.' )

      if not isinstance(loopControl, list):
         raise LispRuntimeFuncError( LP_dotimes, 'Argument 1 expected to be a list.' )

      try:
         variable, countExpr = loopControl
      except ValueError:
         raise LispRuntimeFuncError( LP_dotimes, 'Argument 1 expected to contain two elements.' )

      if not isinstance( variable, LSymbol ):
         raise LispRuntimeFuncError( LP_dotimes, 'Argument 1 of the control list expected to be a symbol.' )

      count = LispInterpreter._lEval( env, countExpr )

      if not isinstance( count, int ):
         raise LispRuntimeFuncError( LP_dotimes, 'Argument 2 of the control list expected to be an integer' )

      if len(body) < 1:
         raise LispRuntimeFuncError( LP_dotimes, 'At least one sexpr expected for the loop body.' )

      latestResult = L_NIL
      loopEnv = Environment( env )
      for iterCount in range(count):
         loopEnv.bindLocal( variable.strval, iterCount )
         for sexpr in body:
            latestResult = LispInterpreter._lEval( loopEnv, sexpr )
      return latestResult

   @primitive( 'foreach', '<variable> <list> <sexpr1> <sexpr2> ...', specialForm=True )
   def LP_foreach( env: Environment, *args ) -> Any:
      """Perform a loop over the elements of list.  On each iteration var is set
to the next element in the list, then the expr's are evaluated in order.
Returns the result of the very last evaluation."""
      try:
         varSymbol, anExpr, *body = args
      except ValueError:
         raise LispRuntimeFuncError( LP_foreach, "3 or more arguments expected." )

      if not isinstance( varSymbol, LSymbol ):
         raise LispRuntimeFuncError( LP_foreach, "Argument 1 expected to be a symbol." )

      if len(body) < 1:
         raise LispRuntimeFuncError( LP_foreach, 'At least one sexpr expected for the loop body.' )

      alist = LispInterpreter._lEval( env, anExpr )
      if not isinstance(alist, list):
         raise LispRuntimeFuncError( LP_foreach, "Argument 2 expected to evaluate to a list." )

      latestResult = L_NIL
      loopEnv = Environment( env )
      for element in alist:
         loopEnv.bindLocal( varSymbol.strval, element )
         for sexpr in body:
            latestResult = LispInterpreter._lEval( loopEnv, sexpr )
      return latestResult

   @primitive( 'dolist', '(<variable> <list>) <sexpr1> <sexpr2> ...', specialForm=True )
   def LP_dolist( env: Environment, *args ) -> Any:
      """Iterate over the elements of list, binding variable to each in turn.
Returns the result of the last body expression, or NIL if the list is empty."""
      if len(args) < 1:
         raise LispRuntimeFuncError( LP_dolist, '2 or more arguments expected.' )

      controlSpec = args[0]
      body        = args[1:]

      if not isinstance( controlSpec, list ) or len(controlSpec) != 2:
         raise LispRuntimeFuncError( LP_dolist, 'Argument 1 must be a (variable list) control spec.' )

      varSymbol, listExpr = controlSpec
      if not isinstance( varSymbol, LSymbol ):
         raise LispRuntimeFuncError( LP_dolist, 'Control spec variable must be a Symbol.' )

      alist = LispInterpreter._lEval( env, listExpr )
      if not isinstance( alist, list ):
         raise LispRuntimeFuncError( LP_dolist, 'Control spec list must evaluate to a List.' )

      latestResult = L_NIL
      loopEnv = Environment( env )
      for element in alist:
         loopEnv.bindLocal( varSymbol.strval, element )
         for sexpr in body:
            latestResult = LispInterpreter._lEval( loopEnv, sexpr )
      return latestResult

   @primitive( 'funcall', '<fnNameSymbol> <arg1> <arg2> ...' )
   def LP_funcall( env: Environment, *args ) -> Any:
      """Calls a function with the args listed."""
      if len(args) == 0:
         raise LispRuntimeFuncError( LP_funcall, "1 or more arguments expected" )

      return LispInterpreter._lApply( env, args[0], args[1:] )

   @primitive( 'eval', '<sexpr>' )
   def LP_eval( env: Environment, *args ) -> Any:
      """Evaluates expr in the current scope."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_eval, '1 argument expected.' )
      expr = args[0]
      return LispInterpreter._lEval( env, expr )

   @primitive( 'apply', '<function> &rest <args> <argsList>' )
   def LP_apply( env: Environment, *args ) -> Any:
      """Inserts arg1,arg2,... into the front of listOfMoreArgs, then applies the
function the the whole list of args.  Returns the result of that function
application.

function is any callable that is not a special form."""
      if len(args) < 2:
         raise LispRuntimeFuncError( LP_apply, "At least 2 arguments expected to apply." )

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

      return LispInterpreter._lApply( env, fnObj, fnArgs )

   @primitive( 'and', '<boolean1> <boolean2> ...', specialForm=True )
   def LP_and( env: Environment, *args ) -> Any:
      """Returns t if all arguments are truthy (non-nil).
Short-circuits: stops evaluating arguments upon encountering the first nil."""
      if len(args) < 2:
         raise LispRuntimeFuncError( LP_and, '2 or more arguments expected.' )

      for arg in args:
         if not LispInterpreter._lTrue(LispInterpreter._lEval(env, arg)):
            return L_NIL

      return L_T

   @primitive( 'or', '<boolean1> <boolean2> ...', specialForm=True )
   def LP_or( env: Environment, *args ) -> Any:
      """Returns t if at least one argument is truthy (non-nil).
Short-circuits:  stops evaluating upon encountering the first truthy value."""
      if len(args) < 2:
         raise LispRuntimeFuncError( LP_or, '2 or more arguments expected.' )

      for arg in args:
         if LispInterpreter._lTrue(LispInterpreter._lEval(env, arg)):
            return L_T

      return L_NIL

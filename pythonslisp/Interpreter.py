import time
from fractions import Fraction
from pathlib import Path
from typing import Any, Sequence

from pythonslisp.Parser import Parser
from pythonslisp.ltk.Listener import InterpreterBase
from pythonslisp.ltk.Utils import retrieveFileList
from pythonslisp.ltk.EnvironmentBase import EnvironmentBase
from pythonslisp.AST import ( LSymbol, L_T, L_NIL,
                                  LCallable, LFunction, LPrimitive, LMacro, LContinuation,
                                  prettyPrintSExpr )
from pythonslisp.Exceptions import ( LRuntimeError, LRuntimePrimError,
                                         LArgBindingError, ContinuationInvoked,
                                         ReturnFrom, Thrown )
from pythonslisp.Environment import Environment
from pythonslisp.Expander import Expander
from pythonslisp.Analyzer import Analyzer
from pythonslisp.Tracer import Tracer
from pythonslisp.Context import Context
from pythonslisp.primitives import constructPrimitives


class Interpreter( InterpreterBase ):
   DEFAULT_LIB_DIR = Path(__file__).parent / 'lib'

   def __init__( self, runtimeLibraryDir: (Path|str) = DEFAULT_LIB_DIR ) -> None:
      self._libDir = runtimeLibraryDir
      self._parser: Parser = Parser( )
      self.tracer: Tracer = Tracer()
      self._setf_registry: dict[str, str] = {}   # accessor-name → field-dict-key
      self._ctx: Context = None               # initialized in reboot()

   def reboot( self, outStrm=None ) -> None:
      # Reset tracing state so stale traced-function names don't linger
      self.tracer.reset()
      self._setf_registry.clear()

      # Create the GLOBAL environment and load in the primitives
      primitiveDict: dict[str, Any] = constructPrimitives( self._parser.parse )
      self._ctx = self._makeContext( outStrm )
      self._env: Environment = Environment( parent=None, initialBindings=primitiveDict,
                                                    evalFn=self._ctx.lEval )

      # Load in the runtime library
      if self._libDir:
         filenameList = retrieveFileList( self._libDir )
         for filename in filenameList:
            self.evalFile( filename, outStrm )

      # Load system startup script (always, from package directory)
      startup_path = Path(__file__).parent / 'startup.lisp'
      if startup_path.exists():
         self.evalFile( str(startup_path), outStrm )

      # Load user startup script if present (~/.pythonslisp_rc)
      user_startup = Path.home() / '.pythonslisp_rc'
      if user_startup.exists():
         self.evalFile( str(user_startup), outStrm )

   def eval( self, source: str, outStrm=None ) -> str:
      returnVal = self.rawEval( source, outStrm=outStrm )
      return prettyPrintSExpr( returnVal ).strip()

   def eval_instrumented( self, source: str, outStrm=None ) -> str:
      returnVal,parseTime,execTime = self.rawEval_instrumented( source, outStrm=outStrm )
      return prettyPrintSExpr( returnVal ).strip(), parseTime, execTime

   def evalFile( self, filename: str, outStrm=None ) -> str:
      self.rawEvalFile( filename, outStrm=outStrm )

   def rawEval( self, source: str, outStrm=None ) -> Any:
      self._ctx.outStrm = outStrm
      ctx = self._ctx
      try:
         ast = self._parser.parse( source )   # (progn form1 form2 ...)
         top_level_forms = ast[1:]            # strip progn wrapper
         returnVal = L_NIL
         for form in top_level_forms:
            form = Expander.expand( ctx, self._env, form )
            Analyzer.analyze( self._env, form )
            returnVal = Interpreter._lEval( ctx, self._env, form )
      except ContinuationInvoked:
         raise LRuntimeError( 'Continuation invoked outside its dynamic extent.' )
      except Thrown as e:
         raise LRuntimeError( f'throw: no catch for tag {prettyPrintSExpr(e.tag)}.' )
      except ReturnFrom as e:
         raise LRuntimeError( f'return-from: no block named {e.name} is currently active.' )
      return returnVal

   def rawEval_instrumented( self, source: str, outStrm=None ) -> Any:
      self._ctx.outStrm = outStrm
      ctx = self._ctx
      try:
         parseStartTime = time.perf_counter()
         ast = self._parser.parse( source )
         parseTime = time.perf_counter() - parseStartTime

         startTime = time.perf_counter()
         top_level_forms = ast[1:]
         returnVal = L_NIL
         for form in top_level_forms:
            form = Expander.expand( ctx, self._env, form )
            Analyzer.analyze( self._env, form )
            returnVal = Interpreter._lEval( ctx, self._env, form )
         evalTime = time.perf_counter() - startTime
      except ContinuationInvoked:
         raise LRuntimeError( 'Continuation invoked outside its dynamic extent.' )
      except Thrown as e:
         raise LRuntimeError( f'throw: no catch for tag {prettyPrintSExpr(e.tag)}.' )
      except ReturnFrom as e:
         raise LRuntimeError( f'return-from: no block named {e.name} is currently active.' )
      return returnVal, parseTime, evalTime

   def rawEvalFile( self, filename: str, outStrm=None ) -> Any:
      self._ctx.outStrm = outStrm
      ctx = self._ctx
      try:
         ast = self._parser.parseFile( filename )   # (progn form1 form2 ...)
         top_level_forms = ast[1:]                  # strip progn wrapper
         returnVal = L_NIL
         for form in top_level_forms:
            form = Expander.expand( ctx, self._env, form )
            Analyzer.analyze( self._env, form )
            returnVal = Interpreter._lEval( ctx, self._env, form )
      except ContinuationInvoked:
         raise LRuntimeError( 'Continuation invoked outside its dynamic extent.' )
      except Thrown as e:
         raise LRuntimeError( f'throw: no catch for tag {prettyPrintSExpr(e.tag)}.' )
      except ReturnFrom as e:
         raise LRuntimeError( f'return-from: no block named {e.name} is currently active.' )
      return returnVal

   def _makeContext( self, outStrm ) -> Context:
      ctx = Context( outStrm, self.tracer, self._setf_registry )
      ctx.lEval            = lambda env, sexpr: Interpreter._lEval( ctx, env, sexpr )
      ctx.lApply           = lambda env, fn, a: Interpreter._lApply( ctx, env, fn, a )
      ctx.lBackquoteExpand = lambda env, expr, depth=1: Interpreter._lbackquoteExpand( ctx, env, expr, depth )
      return ctx

   @staticmethod
   def _lTrue( sExpr: Any ) -> bool:
      if isinstance(sExpr, list):
         return len(sExpr) != 0
      return True

   @staticmethod
   def _lEval( ctx: Context, env: Environment, sExprAST: Any ) -> Any:
      '''This is a recursive tree-walk evaluator.  It's the interpreter's main
      evaluation function.  Pass it any s-expression in the form of an AST;
      eval will return the result.'''
      while True:
         if isinstance(sExprAST, LSymbol):
            try:
               return env.lookup( sExprAST.strval )
            except KeyError:
               if sExprAST.isKeyArg():
                  return sExprAST
               raise LRuntimeError( f'Unbound Variable: {sExprAST.strval}.' )
         elif not isinstance(sExprAST, list):  # atom or map
            return sExprAST

         # sExpr is a list expression - function call

         if len(sExprAST) == 0:
            return L_NIL  # An empty list always evaluates to an empty list

         # Primary ought to evaluate to a callable (LPrimitive, LFunction or LMacro)
         primary, *args = sExprAST
         function        = None    # set by FUNCALL, APPLY, or the else-branch
         args_pre_evaled = False   # True when FUNCALL/APPLY have already evaluated args
         if primary == 'IF':
            condValue = Interpreter._lEval( ctx, env, args[0] )
            sExprAST = args[1] if Interpreter._lTrue(condValue) else args[2]

         elif primary == 'LET':
            vardefs, *body = args

            # Evaluate the var def initial value exprs in the outer scope.
            initDict = { }
            for varSpec in vardefs:
               if isinstance(varSpec, LSymbol):
                  varName  = varSpec
                  initForm = list()
               else:  # list — structure guaranteed valid by analyzer
                  if len(varSpec) == 1:
                     varName  = varSpec[0]
                     initForm = list()
                  else:
                     varName, initForm = varSpec
               initDict[varName.strval] = Interpreter._lEval(ctx, env, initForm)

            # Open the new scope
            env = Environment( env, initialBindings=initDict )

            # Evaluate each body sexpr in the new env/scope
            if len(body) == 0:
               return L_NIL
            else:
               for i in range(len(body) - 1):
                  Interpreter._lEval( ctx, env, body[i] )
               sExprAST = body[-1]

         elif primary == 'LET*':
            vardefs, *body = args

            # Open the new scope
            env = Environment( env )

            for varSpec in vardefs:
               if isinstance(varSpec, LSymbol):
                  varName  = varSpec
                  initForm = list()
               else:  # list — structure guaranteed valid by analyzer
                  if len(varSpec) == 1:
                     varName  = varSpec[0]
                     initForm = list()
                  else:
                     varName, initForm = varSpec
               env.bindLocal( varName.strval, Interpreter._lEval(ctx, env, initForm) )

            # Evaluate each body sexpr in the new env/scope.
            if len(body) == 0:
               return L_NIL
            else:
               for i in range(len(body) - 1):
                  Interpreter._lEval( ctx, env, body[i] )
               sExprAST = body[-1]

         elif primary == 'PROGN':
            if len(args) == 0:
               return L_NIL
            else:
               for i in range(len(args) - 1):
                  Interpreter._lEval( ctx, env, args[i] )
               sExprAST = args[-1]

         elif primary == 'SETQ':
            rval = L_NIL
            while len(args) > 0:
               lval,rval,*args = args
               rval = Interpreter._lEval(ctx, env, rval)
               if isinstance(rval, (LMacro, LFunction)) and (rval.name == ''):
                  rval.name = lval.strval
               sym = lval.strval   # lval is a LSymbol — guaranteed by analyzer
               theSymTab = env.findDef( sym )
               if theSymTab:
                  theSymTab.bindLocal( sym, rval )
               else:
                  env.bindGlobal( sym, rval )
            return rval

         elif primary == 'COND':
            sExprAST = L_NIL
            for clause in args:
               testExpr = clause[0]      # analyzer guarantees: list, len >= 2
               if Interpreter._lTrue( Interpreter._lEval(ctx, env, testExpr) ):
                  body     = clause[1:]
                  if len(body) == 0:
                     return L_NIL
                  else:
                     for i in range(len(body) - 1):
                        Interpreter._lEval( ctx, env, body[i] )
                     sExprAST = body[-1]
                  break

         elif primary == 'CASE':
            keyVal   = Interpreter._lEval( ctx, env, args[0] )
            sExprAST = L_NIL
            for i in range(1, len(args)):
               clause  = args[i]
               caseVal = clause[0]       # analyzer guarantees: list, len >= 2; key is NOT evaluated (CL)
               if isinstance(caseVal, LSymbol) and caseVal.strval in ('T', 'OTHERWISE'):
                  matched = True
               elif isinstance(caseVal, list):
                  matched = keyVal in caseVal    # list of atoms: match any
               else:
                  matched = (caseVal == keyVal)
               if matched:
                  body = clause[1:]
                  if len(body) == 0:
                     return L_NIL
                  else:
                     for j in range(len(body) - 1):
                        Interpreter._lEval( ctx, env, body[j] )
                     sExprAST = body[-1]
                  break

         elif primary == 'FUNCALL':
            if len(args) < 1:
               raise LRuntimeError( 'Error binding arguments in call to function "FUNCALL".\nToo few positional arguments.' )
            function = Interpreter._lEval( ctx, env, args[0] )
            if not isinstance( function, LCallable ):
               raise LRuntimeError( 'FUNCALL: first argument must evaluate to a callable.' )
            if isinstance( function, LContinuation ):
               if len(args) != 2:
                  raise LRuntimeError( f'Continuation expects exactly 1 argument, got {len(args) - 1}.' )
               raise ContinuationInvoked( function.token, Interpreter._lEval(ctx, env, args[1]) )
            if isinstance( function, LMacro ):
               raise LRuntimeError( f'FUNCALL: cannot call macro "{function.name}".' )
            if not function.preEvalArgs:
               raise LRuntimeError( 'FUNCALL: first argument may not be a special form.' )
            args            = [ Interpreter._lEval( ctx, env, a ) for a in args[1:] ]
            args_pre_evaled = True

         elif primary == 'APPLY':
            if len(args) < 2:
               raise LRuntimeError( 'Error binding arguments in call to function "APPLY".\nToo few positional arguments.' )
            fn_val = Interpreter._lEval( ctx, env, args[0] )
            if isinstance( fn_val, LCallable ):
               function = fn_val
            elif isinstance( fn_val, LSymbol ):
               try:
                  function = env.lookup( fn_val.strval )
               except KeyError:
                  raise LRuntimeError( f'APPLY: "{fn_val}" is not bound to a callable.' )
               if not isinstance( function, LCallable ):
                  raise LRuntimeError( f'APPLY: "{fn_val}" is not bound to a callable.' )
            else:
               raise LRuntimeError( 'APPLY: first argument must evaluate to a callable or symbol.' )
            evaled   = [ Interpreter._lEval( ctx, env, a ) for a in args[1:] ]
            list_arg = evaled[-1]
            if not isinstance( list_arg, list ):
               raise LRuntimeError( 'APPLY: last argument must be a list.' )
            if isinstance( function, LContinuation ):
               spread = list( evaled[:-1] ) + list( list_arg )
               if len(spread) != 1:
                  raise LRuntimeError( f'Continuation expects exactly 1 argument, got {len(spread)}.' )
               raise ContinuationInvoked( function.token, spread[0] )
            if isinstance( function, LMacro ):
               raise LRuntimeError( f'APPLY: cannot apply macro "{function.name}".' )
            if not function.preEvalArgs:
               raise LRuntimeError( 'APPLY: first argument may not be a special form.' )
            args            = list( evaled[:-1] ) + list( list_arg )
            args_pre_evaled = True

         else:
            function = Interpreter._lEval( ctx, env, primary )
            if not isinstance( function, LCallable ):
               raise LRuntimeError( f'Badly formed list expression \'{primary}\'.  The first element should evaluate to a callable.' )

            # Continuation invocation: bypass tracing, raise immediately
            if isinstance( function, LContinuation ):
               if len(args) != 1:
                  raise LRuntimeError( f'Continuation expects exactly 1 argument, got {len(args)}.' )
               value = Interpreter._lEval( ctx, env, args[0] )
               raise ContinuationInvoked( function.token, value )

            # Inline macro expansion: handles macros called directly inside _lEval
            # (e.g. from within another macro's body, which bypasses the top-level expander)
            if isinstance( function, LMacro ):
               sExprAST = Expander._expandMacroCall( ctx, env, function, args )
               continue

         # Shared function dispatch: reached from FUNCALL, APPLY, and the else-branch.
         if function is not None:
            # Tracing
            tracer  = ctx.tracer
            printed = False
            if tracer.isActive():
               depth   = tracer.getMaxTraceDepth()
               printed = tracer.trace( 'enter', function, args, depth, ctx.outStrm )
               if printed:
                  tracer.setMaxTraceDepth( depth + 1 )

            # Pre-evaluate args unless FUNCALL/APPLY already did so.
            if not args_pre_evaled and function.preEvalArgs:
               args = [ Interpreter._lEval(ctx, env, arg) for arg in args ]

            try:
               if isinstance( function, LPrimitive ):
                  if function.lambdaListAST is not None:
                     kw_env = Environment( env, evalFn=ctx.lEval )
                     kw_env.bindArguments( function.lambdaListAST, args )
                     result = function.pythonFn( ctx, kw_env, *args )
                  else:
                     result = function.pythonFn( ctx, env, *args )
               else:
                  env = Environment( function.capturedEnvironment, evalFn=ctx.lEval )
                  env.bindArguments( function.lambdaListAST, args )
                  if printed:
                     # Traced: evaluate recursively so the exit trace fires at the right time.
                     result = L_NIL
                     for sexpr in function.bodyAST:
                        result = Interpreter._lEval( ctx, env, sexpr )
                     # Fall through to the traced-exit block below.
                  else:
                     # Untraced: TCO — update env/sExprAST and continue the loop.
                     body = function.bodyAST
                     if len(body) == 0:
                        sExprAST = L_NIL
                     else:
                        for i in range(len(body) - 1):
                           Interpreter._lEval( ctx, env, body[i] )
                        sExprAST = body[-1]
                     continue   # TCO: next iteration of the while loop
            except LArgBindingError as ex:
               if printed:
                  tracer.setMaxTraceDepth( depth )
               errorMsg = ex.args[-1]
               fnName = function.name
               if fnName == '':
                  raise LRuntimeError( f'Error binding arguments in call to "(lambda ...)".\n{errorMsg}')
               else:
                  raise LRuntimeError( f'Error binding arguments in call to function "{fnName}".\n{errorMsg}')

            # Reached for LPrimitive and traced LFunction (not the TCO path).
            if printed:
               tracer.setMaxTraceDepth( depth )
               tracer.trace( 'exit', function, result, depth, ctx.outStrm )
            return result

   @staticmethod
   def _lApply( ctx: Context, env: Environment, function: LCallable, args: Sequence ) -> Any:
      # Continuation invocation: raise immediately, no tracing
      if isinstance( function, LContinuation ):
         if len(args) != 1:
            raise LRuntimeError( f'Continuation expects exactly 1 argument, got {len(args)}.' )
         raise ContinuationInvoked( function.token, args[0] )

      tracer  = ctx.tracer
      printed = False
      if tracer.isActive():
         depth   = tracer.getMaxTraceDepth()
         printed = tracer.trace( 'enter', function, args, depth, ctx.outStrm )
         if printed:
            tracer.setMaxTraceDepth( depth + 1 )
      try:
         if isinstance( function, LPrimitive ):
            if function.lambdaListAST is not None:
               kw_env = Environment( env, evalFn=ctx.lEval )
               kw_env.bindArguments( function.lambdaListAST, args )
               result = function.pythonFn( ctx, kw_env, *args )
            else:
               result = function.pythonFn( ctx, env, *args )
         elif isinstance( function, LFunction ):
            env = Environment( function.capturedEnvironment, evalFn=ctx.lEval ) # Open a new scope on the function's captured env to support closures.

            # store the arguments as locals
            env.bindArguments( function.lambdaListAST, args )

            # evaluate the body expressions.
            result = L_NIL
            for sexpr in function.bodyAST:
               result = Interpreter._lEval( ctx, env, sexpr )
         else:   # LMacro — should have been expanded before reaching here
            raise LRuntimeError( f'Macro "{function.name}" was not expanded before evaluation.' )
      except LArgBindingError as ex:
         errorMsg = ex.args[-1]
         fnName = function.name
         if fnName == '':
            raise LRuntimeError( f'Error binding arguments in call to "(lambda ...)".\n{errorMsg}')
         else:
            raise LRuntimeError( f'Error binding arguments in call to function "{fnName}".\n{errorMsg}')
      finally:
         if printed:
            tracer.setMaxTraceDepth( depth )   # restore depth on both normal exit and exception
      if printed:
         tracer.trace( 'exit', function, result, depth, ctx.outStrm )
      return result

   @staticmethod
   def _lbackquoteExpand( ctx: Context, env: Environment, expr: Any, depth: int = 1 ) -> Any:
      '''Expand a backquote expression, tracking nesting depth.
      depth=1 is the innermost (active) backquote level.  Commas and
      splices are only evaluated at depth 1; at deeper levels they are
      preserved as template structure for the inner backquote.'''
      if not isinstance(expr, list):
         return expr       # atoms/symbols pass through unchanged at any depth

      if len(expr) == 0:
         return expr

      primary = expr[0]

      if primary == 'BACKQUOTE':
         # Nested backquote — increase depth, process content, rewrap
         inner = Interpreter._lbackquoteExpand( ctx, env, expr[1], depth + 1 )
         return [ LSymbol('BACKQUOTE'), inner ]

      if primary == 'COMMA':
         if depth == 1:
            return Interpreter._lEval( ctx, env, expr[1] )
         else:
            inner = Interpreter._lbackquoteExpand( ctx, env, expr[1], depth - 1 )
            return [ LSymbol('COMMA'), inner ]

      if primary == 'COMMA-AT':
         if depth == 1:
            result = Interpreter._lEval( ctx, env, expr[1] )
            if not isinstance( result, list ):
               raise LRuntimePrimError( env.lookup('COMMA-AT'), 'Argument 1 must evaluate to a List.' )
            return [ LSymbol('COMMA-AT'), result ]
         else:
            inner = Interpreter._lbackquoteExpand( ctx, env, expr[1], depth - 1 )
            return [ LSymbol('COMMA-AT'), inner ]

      # Regular list — process each element at the same depth.
      # Splice COMMA-AT sentinels only at depth 1.
      resultList: list[Any] = [ ]
      for listElt in expr:
         resultListElt = Interpreter._lbackquoteExpand( ctx, env, listElt, depth )
         if ( depth == 1 and
              isinstance(resultListElt, list) and
              len(resultListElt) > 0 and
              resultListElt[0] == 'COMMA-AT' ):
            for elt in resultListElt[1]:
               resultList.append( elt )
         else:
            resultList.append( resultListElt )
      return resultList



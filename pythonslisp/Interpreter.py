import time
from fractions import Fraction
from pathlib import Path
from typing import Any, Sequence

from pythonslisp.Parser import Parser
from pythonslisp.ltk.Listener import InterpreterBase
from pythonslisp.ltk.Utils import retrieveFileList
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
   DEFAULT_LIB_DIR        = Path(__file__).parent / 'lib'
   _SPECIAL_OPERATOR_SET  = frozenset({'LET', 'LET*', 'PROGN', 'SETQ',
                                       'COND', 'CASE', 'FUNCALL', 'APPLY'})

   def __init__( self, runtimeLibraryDir: (Path|str) = DEFAULT_LIB_DIR ) -> None:
      self._libDir = runtimeLibraryDir
      self._parser: Parser = Parser( )
      self._tracer: Tracer = Tracer()
      self._setf_registry: dict[str, str] = {}   # accessor-name → field-dict-key
      self._ctx: Context = None               # initialized in reboot()

   def reboot( self, outStrm=None ) -> None:
      # Reset tracing state so stale traced-function names don't linger
      self._tracer.reset()
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
      ctx = Context( outStrm, self._tracer, self._setf_registry )
      ctx.lEval            = lambda env, sexpr: Interpreter._lEval( ctx, env, sexpr )
      ctx.lApply           = Interpreter._lApply
      ctx.lBackquoteExpand = Interpreter._lbackquoteExpand
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
      _eval                 = Interpreter._lEval
      _true                 = Interpreter._lTrue
      _SPECIAL_OPERATOR_SET = Interpreter._SPECIAL_OPERATOR_SET
      tracer                = ctx.tracer
      while True:
         if type(sExprAST) is LSymbol:
            try:
               return env.lookup( sExprAST.name )
            except KeyError:
               if sExprAST.isKeyArg():
                  return sExprAST
               raise LRuntimeError( f'Unbound Variable: {sExprAST.name}.' )
         elif not isinstance(sExprAST, list):  # atom or map
            return sExprAST

         # sExpr is a list expression - function call

         if len(sExprAST) == 0:
            return L_NIL  # An empty list always evaluates to an empty list

         # Primary ought to evaluate to a callable (LPrimitive, LFunction or LMacro)
         head = sExprAST[0]
         headStr = head.name if type(head) is LSymbol else None
         argsPreEvaled = False

         if headStr == 'IF':
            condValue = _eval( ctx, env, sExprAST[1] )
            sExprAST = sExprAST[2] if _true(condValue) else sExprAST[3]
            continue

         elif headStr == 'QUOTE':
            return sExprAST[1]

         # Common function-call path: frozenset guard skips special-operator comparisons
         elif headStr not in _SPECIAL_OPERATOR_SET:
            args     = sExprAST[1:]
            function = _eval( ctx, env, head )
            if not isinstance( function, LCallable ):
               raise LRuntimeError( f'Badly formed list expression \'{head}\'.  The first element should evaluate to a callable.' )
            # Continuation invocation: bypass tracing, raise immediately
            if type( function ) is LContinuation:
               if len(args) != 1:
                  raise LRuntimeError( f'Continuation expects exactly 1 argument, got {len(args)}.' )
               value = _eval( ctx, env, args[0] )
               raise ContinuationInvoked( function.token, value )
            # Inline macro expansion fallback.
            # Normally all macros are expanded by the pre-evaluation expander before
            # _lEval ever sees the code.  But two paths enter _lEval without going
            # through that expander:
            #   1. _expandMacroCall evaluates a macro's body directly with _lEval to
            #      produce its expansion.  If that body itself calls another macro
            #      (e.g. a macro body that uses `first` or `when`), _lEval will
            #      encounter the inner macro unexpanded and must handle it here.
            #   2. The `eval` primitive evaluates a user-constructed s-expression that
            #      was never run through the expander.
            # In both cases, we expand the macro inline and re-enter the loop.
            if type( function ) is LMacro:
               sExprAST = Expander._expandMacroCall( ctx, env, function, args )
               continue

         elif headStr == 'LET':
            args           = sExprAST[1:]
            vardefs, *body = args
            # Evaluate the var def initial value exprs in the outer scope.
            initDict = { }
            for varSpec in vardefs:
               if type(varSpec) is LSymbol:
                  varName  = varSpec
                  initForm = L_NIL
               else:  # list — structure guaranteed valid by analyzer
                  if len(varSpec) == 1:
                     varName  = varSpec[0]
                     initForm = L_NIL
                  else:
                     varName, initForm = varSpec
               initDict[varName.name] = _eval(ctx, env, initForm)
            # Open the new scope
            env = Environment( env, initialBindings=initDict )
            # Evaluate each body sexpr in the new env/scope
            if len(body) == 0:
               return L_NIL
            for bodyIdx in range(len(body) - 1):
               _eval( ctx, env, body[bodyIdx] )
            sExprAST = body[-1]
            continue

         elif headStr == 'LET*':
            args           = sExprAST[1:]
            vardefs, *body = args
            # Open the new scope
            env = Environment( env )
            for varSpec in vardefs:
               if type(varSpec) is LSymbol:
                  varName  = varSpec
                  initForm = L_NIL
               else:  # list — structure guaranteed valid by analyzer
                  if len(varSpec) == 1:
                     varName  = varSpec[0]
                     initForm = L_NIL
                  else:
                     varName, initForm = varSpec
               env.bindLocal( varName.name, _eval(ctx, env, initForm) )
            # Evaluate each body sexpr in the new env/scope.
            if len(body) == 0:
               return L_NIL
            for bodyIdx in range(len(body) - 1):
               _eval( ctx, env, body[bodyIdx] )
            sExprAST = body[-1]
            continue

         elif headStr == 'PROGN':
            exprLen = len(sExprAST)
            if exprLen == 1:
               return L_NIL
            for exprIdx in range(1, exprLen - 1):
               _eval( ctx, env, sExprAST[exprIdx] )
            sExprAST = sExprAST[-1]
            continue

         elif headStr == 'SETQ':
            rval = L_NIL
            exprIdx    = 1
            exprLen = len(sExprAST)
            while exprIdx < exprLen:
               lval = sExprAST[exprIdx]
               rval = _eval( ctx, env, sExprAST[exprIdx + 1] )
               if (type(rval) is LMacro or type(rval) is LFunction) and (rval.name == ''):
                  rval.name = lval.name
               env.bind( lval.name, rval )
               exprIdx += 2
            return rval

         elif headStr == 'COND':
            args = sExprAST[1:]
            for clause in args:
               testExpr = clause[0]      # analyzer guarantees: list, len >= 2
               if _true( _eval(ctx, env, testExpr) ):
                  body = clause[1:]
                  if len(body) == 0:
                     return L_NIL
                  for bodyIdx in range(len(body) - 1):
                     _eval( ctx, env, body[bodyIdx] )
                  sExprAST = body[-1]
                  break
            else:
               return L_NIL
            continue

         elif headStr == 'CASE':
            args   = sExprAST[1:]
            keyVal = _eval( ctx, env, args[0] )
            for argsIdx in range(1, len(args)):
               clause  = args[argsIdx]
               caseVal = clause[0]       # analyzer guarantees: list, len >= 2; key is NOT evaluated (CL)
               if type(caseVal) is LSymbol and caseVal.name in ('T', 'OTHERWISE'):
                  matched = True
               elif isinstance(caseVal, list):
                  matched = keyVal in caseVal    # list of atoms: match any
               else:
                  matched = (caseVal == keyVal)
               if matched:
                  body = clause[1:]
                  if len(body) == 0:
                     return L_NIL
                  for bodyIdx in range(len(body) - 1):
                     _eval( ctx, env, body[bodyIdx] )
                  sExprAST = body[-1]
                  break
            else:
               return L_NIL
            continue

         elif headStr == 'FUNCALL':
            args     = sExprAST[1:]
            if len(args) < 1:
               raise LRuntimeError( 'Error binding arguments in call to function "FUNCALL".\nToo few positional arguments.' )
            function = _eval( ctx, env, args[0] )
            if not isinstance( function, LCallable ):
               raise LRuntimeError( 'FUNCALL: first argument must evaluate to a callable.' )
            if type( function ) is LContinuation:
               if len(args) != 2:
                  raise LRuntimeError( f'Continuation expects exactly 1 argument, got {len(args) - 1}.' )
               raise ContinuationInvoked( function.token, _eval(ctx, env, args[1]) )
            if type( function ) is LMacro:
               raise LRuntimeError( f'FUNCALL: cannot call macro "{function.name}".' )
            if not function.preEvalArgs:
               raise LRuntimeError( 'FUNCALL: first argument may not be a special form.' )
            args          = [ _eval( ctx, env, a ) for a in args[1:] ]
            argsPreEvaled = True

         elif headStr == 'APPLY':
            args = sExprAST[1:]
            if len(args) < 2:
               raise LRuntimeError( 'Error binding arguments in call to function "APPLY".\nToo few positional arguments.' )
            fn_val = _eval( ctx, env, args[0] )
            if isinstance( fn_val, LCallable ):
               function = fn_val
            elif type( fn_val ) is LSymbol:
               try:
                  function = env.lookup( fn_val.name )
               except KeyError:
                  raise LRuntimeError( f'APPLY: "{fn_val}" is not bound to a callable.' )
               if not isinstance( function, LCallable ):
                  raise LRuntimeError( f'APPLY: "{fn_val}" is not bound to a callable.' )
            else:
               raise LRuntimeError( 'APPLY: first argument must evaluate to a callable or symbol.' )
            evaled   = [ _eval( ctx, env, a ) for a in args[1:] ]
            list_arg = evaled.pop()
            if not isinstance( list_arg, list ):
               raise LRuntimeError( 'APPLY: last argument must be a list.' )
            if type( function ) is LContinuation:
               spread = evaled + list_arg
               if len(spread) != 1:
                  raise LRuntimeError( f'Continuation expects exactly 1 argument, got {len(spread)}.' )
               raise ContinuationInvoked( function.token, spread[0] )
            if type( function ) is LMacro:
               raise LRuntimeError( f'APPLY: cannot apply macro "{function.name}".' )
            if not function.preEvalArgs:
               raise LRuntimeError( 'APPLY: first argument may not be a special form.' )
            evaled.extend( list_arg )
            args          = evaled
            argsPreEvaled = True

         # Shared function dispatch: reached from FUNCALL, APPLY, and the common function-call path.
         # Tracing
         printed = False
         if tracer.isActive():
            depth   = tracer.getMaxTraceDepth()
            printed = tracer.trace( 'enter', function, args, depth, ctx.outStrm )
            if printed:
               tracer.setMaxTraceDepth( depth + 1 )

         # Pre-evaluate args unless FUNCALL/APPLY already did so.
         if not argsPreEvaled and function.preEvalArgs:
            args = [ _eval(ctx, env, arg) for arg in args ]

         try:
            if type( function ) is LPrimitive:
               if function.lambdaListAST:
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
                     result = _eval( ctx, env, sexpr )
                  # Fall through to the traced-exit block below.
               else:
                  # Untraced: TCO — update env/sExprAST and continue the loop.
                  body = function.bodyAST
                  bodyLen = len(body)
                  if bodyLen == 0:
                     sExprAST = L_NIL
                  else:
                     for bodyIdx in range(bodyLen - 1):
                        _eval( ctx, env, body[bodyIdx] )
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
            if function.lambdaListAST:
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

      head = expr[0]

      if head == 'BACKQUOTE':
         # Nested backquote — increase depth, process content, rewrap
         inner = Interpreter._lbackquoteExpand( ctx, env, expr[1], depth + 1 )
         return [ LSymbol('BACKQUOTE'), inner ]

      if head == 'COMMA':
         if depth == 1:
            return Interpreter._lEval( ctx, env, expr[1] )
         else:
            inner = Interpreter._lbackquoteExpand( ctx, env, expr[1], depth - 1 )
            return [ LSymbol('COMMA'), inner ]

      if head == 'COMMA-AT':
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



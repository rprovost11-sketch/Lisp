import time
from fractions import Fraction
from pathlib import Path
from typing import Callable, Any, Sequence

from pythonslisp.LispParser import LispParser
from pythonslisp.Listener import Interpreter, retrieveFileList
from pythonslisp.Environment import Environment
from pythonslisp.LispAST import ( LSymbol, L_T, L_NIL,
                                  LCallable, LFunction, LPrimitive, LMacro, LContinuation,
                                  prettyPrintSExpr )
from pythonslisp.LispExceptions import ( LispRuntimeError, LispRuntimeFuncError,
                                         LispArgBindingError, ContinuationInvoked,
                                         ReturnFrom, Thrown )
from pythonslisp.LispArgBinder import bindArguments
from pythonslisp.LispExpander import LispExpander


class LispInterpreter( Interpreter ):
   DEFAULT_LIB_DIR = Path(__file__).parent / 'lib'

   outStrm = None
   _setf_registry: dict[str, str] = {}   # accessor-name → field-dict-key

   # --- Tracing state ---
   _traced:       set   = set()    # function names registered via (trace fn)
   _trace_global: bool  = False    # global toggle set by ]trace listener command
   _trace_depth:  int   = 0       # current call nesting depth (managed in _lApply)
   _apply_hook:   Any   = None    # set to _trace_fn when any tracing is active

   def __init__( self, runtimeLibraryDir: (Path|str) = DEFAULT_LIB_DIR ) -> None:
      self._libDir = runtimeLibraryDir
      self._parser: LispParser = LispParser( )

   def reboot( self, outStrm=None ) -> None:
      # Reset tracing state so stale traced-function names don't linger
      LispInterpreter._traced       = set()
      LispInterpreter._trace_global = False
      LispInterpreter._trace_depth  = 0
      LispInterpreter._apply_hook   = None

      # Load in the primitives
      primitiveDict: dict[str, Any] = LispInterpreter._lconstructPrimitives( self._parser.parse )
      self._env:Environment = Environment( parent=None, initialBindings=primitiveDict )  # Create the GLOBAL environment

      # Load in the runtime library
      if self._libDir:
         filenameList = retrieveFileList( self._libDir )
         for filename in filenameList:
            self.evalFile( filename, outStrm )

   def eval( self, source: str, outStrm=None ) -> str:
      returnVal = self.rawEval( source, outStrm=outStrm )
      return prettyPrintSExpr( returnVal ).strip()

   def eval_instrumented( self, source: str, outStrm=None ) -> str:
      returnVal,parseTime,execTime = self.rawEval_instrumented( source, outStrm=outStrm )
      return prettyPrintSExpr( returnVal ).strip(), parseTime, execTime

   def evalFile( self, filename: str, outStrm=None ) -> str:
      self.rawEvalFile( filename, outStrm=outStrm )

   def rawEval( self, source: str, outStrm=None ) -> Any:
      LispInterpreter.outStrm = outStrm
      try:
         ast = self._parser.parse( source )   # (progn form1 form2 ...)
         top_level_forms = ast[1:]            # strip progn wrapper
         returnVal = L_NIL
         for form in top_level_forms:
            form = LispExpander.expand( self._env, form )
            returnVal = LispInterpreter._lEval( self._env, form )
      except ContinuationInvoked:
         raise LispRuntimeError( 'Continuation invoked outside its dynamic extent.' )
      except Thrown as e:
         raise LispRuntimeError( f'throw: no catch for tag {prettyPrintSExpr(e.tag)}.' )
      except ReturnFrom as e:
         raise LispRuntimeError( f'return-from: no block named {e.name} is currently active.' )
      finally:
         LispInterpreter.outStrm = None
      return returnVal

   def rawEval_instrumented( self, source: str, outStrm=None ) -> Any:
      LispInterpreter.outStrm = outStrm
      try:
         parseStartTime = time.perf_counter()
         ast = self._parser.parse( source )
         parseTime = time.perf_counter() - parseStartTime

         startTime = time.perf_counter()
         top_level_forms = ast[1:]
         returnVal = L_NIL
         for form in top_level_forms:
            form = LispExpander.expand( self._env, form )
            returnVal = LispInterpreter._lEval( self._env, form )
         evalTime = time.perf_counter() - startTime
      except ContinuationInvoked:
         raise LispRuntimeError( 'Continuation invoked outside its dynamic extent.' )
      except Thrown as e:
         raise LispRuntimeError( f'throw: no catch for tag {prettyPrintSExpr(e.tag)}.' )
      except ReturnFrom as e:
         raise LispRuntimeError( f'return-from: no block named {e.name} is currently active.' )
      finally:
         LispInterpreter.outStrm = None
      return returnVal, parseTime, evalTime

   def rawEvalFile( self, filename: str, outStrm=None ) -> Any:
      LispInterpreter.outStrm = outStrm
      try:
         ast = self._parser.parseFile( filename )   # (progn form1 form2 ...)
         top_level_forms = ast[1:]                  # strip progn wrapper
         returnVal = L_NIL
         for form in top_level_forms:
            form = LispExpander.expand( self._env, form )
            returnVal = LispInterpreter._lEval( self._env, form )
      except ContinuationInvoked:
         raise LispRuntimeError( 'Continuation invoked outside its dynamic extent.' )
      except Thrown as e:
         raise LispRuntimeError( f'throw: no catch for tag {prettyPrintSExpr(e.tag)}.' )
      except ReturnFrom as e:
         raise LispRuntimeError( f'return-from: no block named {e.name} is currently active.' )
      finally:
         LispInterpreter.outStrm = None
      return returnVal

   @staticmethod
   def _lTrue( sExpr: Any ) -> bool:
      if isinstance(sExpr, list):
         return len(sExpr) != 0
      return True

   @staticmethod
   def _lEql( a: Any, b: Any ) -> bool:
      '''CL eql semantics: symbols compare by name; numbers compare by type and
      value (so 1 and 1.0 are not eql); everything else compares by identity.'''
      if isinstance(a, LSymbol) and isinstance(b, LSymbol):
         return a.strval == b.strval
      if type(a) is type(b) and isinstance(a, (int, float, Fraction)):
         return a == b
      return a is b

   @staticmethod
   def _lEval( env: Environment, sExprAST: Any ) -> Any:
      '''This is a recursive tree-walk evaluator.  It's the interpreter's main
      evaluation function.  Pass it any s-expression in the form of a LispAST;
      eval will return the result.'''
      while True:
         if isinstance(sExprAST, LSymbol):
            try:
               return env.lookup( sExprAST.strval )
            except KeyError:
               if sExprAST.isKeyArg():
                  return sExprAST
               raise LispRuntimeError( f'Unbound Variable: {sExprAST.strval}.' )
         elif not isinstance(sExprAST, list):  # atom or map
            return sExprAST
   
         # sExpr is a list expression - function call
   
         if len(sExprAST) == 0:
            return L_NIL  # An empty list always evaluates to an empty list
   
         # Primary ought to evaluate to a callable (LPrimitive, LFunction or LMacro)
         primary, *args = sExprAST
         if primary == 'QUOTE':
            if (len(args) != 1):
               raise LispRuntimeFuncError( env.lookup('QUOTE'), '1 argument expected.' )
            return args[0]
         
         elif primary == 'IF':
            numArgs = len(args)
            if not(2 <= numArgs <= 3):
               raise LispRuntimeFuncError( env.lookup('IF'), '2 or 3 arguments expected.' )
      
            condValue = LispInterpreter._lEval( env, args[0] )
            if LispInterpreter._lTrue(condValue):
               sExprAST = args[1]
            elif numArgs == 3:
               sExprAST = args[2]
            else:
               sExprAST = L_NIL
         
         elif primary == 'LAMBDA':
            try:
               funcParams, *funcBody = args
            except ValueError:
               raise LispRuntimeFuncError( env.lookup('LAMBDA'), '1 or more arguments expected.' )

            bodyLen = len(funcBody)
            if bodyLen == 0:
               docString = ''
            elif isinstance(funcBody[0], str):
               docString, *funcBody = funcBody
            else:
               docString = ''

            return LFunction( LSymbol(""), funcParams, docString, funcBody, capturedEnvironment=env )
         
         elif primary == 'LET':
            try:
               vardefs, *body = args
            except ValueError:
               raise LispRuntimeFuncError( env.lookup('LET'), '2 or more arguments expected.' )
      
            if not isinstance(vardefs,  list):
               raise LispRuntimeFuncError( env.lookup('LET'), 'The first argument to let expected to be a list of variable initializations.' )
      
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
                     raise LispRuntimeFuncError( env.lookup('LET'), 'Variable initializer spec expected to be 1 or 2 elements long.' )
      
                  if not isinstance(varName, LSymbol):
                     raise LispRuntimeFuncError( env.lookup('LET'), 'First element of a variable initializer pair expected to be a symbol.' )
               else:
                  raise LispRuntimeFuncError( env.lookup('LET'), 'Variable initializer spec expected to be a symbol or a list.' )
      
               initDict[varName.strval] = LispInterpreter._lEval(env, initForm)
      
            # Open the new scope
            env = Environment( env, initialBindings=initDict )
      
            # Evaluate each body sexpr in the new env/scope
            if len(body) == 0:
               sExprAST = L_NIL
            elif len(body) == 1:
               sExprAST = body[0]
            else:
               for sexpr in body[:-1]:
                  LispInterpreter._lEval( env, sexpr )
               sExprAST = body[-1]
         
         elif primary == 'LET*':
            try:
               vardefs, *body = args
            except ValueError:
               raise LispRuntimeFuncError( env.lookup('LET*'), '2 or more arguments expected.' )

            if not isinstance(vardefs,  list):
               raise LispRuntimeFuncError( env.lookup('LET*'), 'The first argument to let expected to be a list of variable initializations.' )

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
                     raise LispRuntimeFuncError( env.lookup('LET*'), 'Variable initializer spec expected to be 1 or 2 elements long.' )

                  if not isinstance(varName, LSymbol):
                     raise LispRuntimeFuncError( env.lookup('LET*'), 'First element of a variable initializer pair expected to be a symbol.' )
               else:
                  raise LispRuntimeFuncError( env.lookup('LET*'), 'Variable initializer spec expected to be a symbol or a list.' )
      
               env.bindLocal( varName.strval, LispInterpreter._lEval(env, initForm) )
      
            # Evaluate each body sexpr in the new env/scope.
            if len(body) == 0:
               sExprAST = L_NIL
            elif len(body) == 1:
               sExprAST = body[0]
            else:
               for sexpr in body[:-1]:
                  LispInterpreter._lEval( env, sexpr )
               sExprAST = body[-1]
         
         elif primary == 'PROGN':
            if len(args) == 0:
               sExprAST = L_NIL
            elif len(args) == 1:
               sExprAST = args[0]
            else:
               for expr in args[:-1]:
                  LispInterpreter._lEval( env, expr )
               sExprAST = args[-1]
         
         elif primary == 'SETQ':
            numArgs = len(args)
      
            if numArgs == 0:
               raise LispRuntimeFuncError( env.lookup('SETQ'), 'At least 2 arguments expected.' )

            if (numArgs % 2) != 0:
               raise LispRuntimeFuncError( env.lookup('SETQ'), f'An even number of arguments is expected.  Received {numArgs}.' )

            rval = L_NIL
            while len(args) > 0:
               lval,rval,*args = args

               rval = LispInterpreter._lEval(env, rval)

               if isinstance(rval, (LMacro, LFunction)) and (rval.name == ''):
                  rval.name = lval.strval
               
               # Case where the lvalue is a symbol form:  (setq variable newValue)
               if isinstance(lval, LSymbol ):
                  sym = lval.strval
                  theSymTab = env.findDef( sym )
                  if theSymTab:
                     theSymTab.bindLocal( sym, rval )
                  else:
                     env.bindGlobal( sym, rval )
               else:
                  raise LispRuntimeFuncError( env.lookup('SETQ'), 'First of setf pair must be a symbol.' )
      
            return rval

         elif primary == 'BLOCK':
            if len(args) < 1:
               raise LispRuntimeError( 'block: at least 1 argument (name) expected.' )
            blockName = args[0]
            if not isinstance( blockName, LSymbol ):
               raise LispRuntimeError( 'block: name must be a symbol.' )
            body = args[1:]
            if len(body) == 0:
               return L_NIL
            try:
               for sexpr in body[:-1]:
                  LispInterpreter._lEval( env, sexpr )
               return LispInterpreter._lEval( env, body[-1] )
            except ReturnFrom as e:
               if e.name == blockName.strval:
                  return e.value
               raise

         elif primary == 'RETURN-FROM':
            if len(args) < 1 or len(args) > 2:
               raise LispRuntimeError( 'return-from: 1 or 2 arguments expected.' )
            blockName = args[0]
            if not isinstance( blockName, LSymbol ):
               raise LispRuntimeError( 'return-from: name must be a symbol.' )
            value = LispInterpreter._lEval( env, args[1] ) if len(args) == 2 else L_NIL
            raise ReturnFrom( blockName.strval, value )

         elif primary == 'CATCH':
            if len(args) < 1:
               raise LispRuntimeError( 'catch: at least 1 argument (tag) expected.' )
            tag  = LispInterpreter._lEval( env, args[0] )
            body = args[1:]
            try:
               result = L_NIL
               for sexpr in body:
                  result = LispInterpreter._lEval( env, sexpr )
               return result
            except Thrown as e:
               if LispInterpreter._lEql( e.tag, tag ):
                  return e.value
               raise

         else:
            function = LispInterpreter._lEval( env, primary )
            if not isinstance( function, LCallable ):
               raise LispRuntimeError( f'Badly formed list expression \'{primary}\'.  The first element should evaluate to a callable.' )

            # Continuation invocation: bypass tracing, raise immediately
            if isinstance( function, LContinuation ):
               if len(args) != 1:
                  raise LispRuntimeError( f'Continuation expects exactly 1 argument, got {len(args)}.' )
               value = LispInterpreter._lEval( env, args[0] )
               raise ContinuationInvoked( function.token, value )

            # Tracing: determine whether this call should be traced.
            hook    = LispInterpreter._apply_hook
            printed = False
            depth   = LispInterpreter._trace_depth
            if hook:
               isNamed  = function.name in LispInterpreter._traced
               isUserFn = isinstance( function, LFunction )
               if isNamed or (LispInterpreter._trace_global and isUserFn):
                  printed = hook( 'enter', function, args, depth )
                  if printed:
                     LispInterpreter._trace_depth = depth + 1

            # Call the function with its arguments
            if not function.specialForm:
               args = [ LispInterpreter._lEval(env, arg) for arg in args ]

            try:
               if isinstance( function, LPrimitive ):
                  result = function.pythonFn( env, *args )
               else:
                  env = Environment( function.capturedEnvironment )
                  bindArguments( env, function.lambdaListAST, args, LispInterpreter._lEval )
                  # Untraced: TCO — update env/sExprAST and continue the loop.
                  bodyLen = len( function.bodyAST )
                  if bodyLen == 0:
                     sExprAST = L_NIL
                  elif bodyLen == 1:
                     sExprAST = function.bodyAST[0]
                  else:
                     for sexpr in function.bodyAST[:-1]:
                        LispInterpreter._lEval( env, sexpr )
                     sExprAST = function.bodyAST[-1]
                  continue   # TCO: next iteration of the while loop
            except LispArgBindingError as ex:
               if printed:
                  LispInterpreter._trace_depth = depth
               errorMsg = ex.args[-1]
               fnName = function.name
               if fnName == '':
                  raise LispRuntimeError( f'Error binding arguments in call to "(lambda ...)".\n{errorMsg}')
               else:
                  raise LispRuntimeError( f'Error binding arguments in call to function "{fnName}".\n{errorMsg}')

            # Reached for LPrimitive and traced LFunction (not the TCO path).
            if printed:
               LispInterpreter._trace_depth = depth
               hook( 'exit', function, result, depth )
            return result

   @staticmethod
   def _lApply( env: Environment, function: LCallable, args: Sequence ) -> Any:
      # Continuation invocation: raise immediately, no tracing
      if isinstance( function, LContinuation ):
         if len(args) != 1:
            raise LispRuntimeError( f'Continuation expects exactly 1 argument, got {len(args)}.' )
         raise ContinuationInvoked( function.token, args[0] )

      hook    = LispInterpreter._apply_hook
      depth   = LispInterpreter._trace_depth
      printed = False
      if hook:
         printed = hook( 'enter', function, args, depth )
         if printed:
            LispInterpreter._trace_depth = depth + 1   # only advance visible depth when tracing this call
      try:
         if isinstance( function, LPrimitive ):
            result = function.pythonFn( env, *args )
         elif isinstance( function, LFunction ):
            env = Environment( function.capturedEnvironment ) # Open a new scope on the function's captured env to support closures.

            # store the arguments as locals
            bindArguments( env, function.lambdaListAST, args, LispInterpreter._lEval )

            # evaluate the body expressions.
            result = L_NIL
            for sexpr in function.bodyAST:
               result = LispInterpreter._lEval( env, sexpr )
         else:   # LMacro — should have been expanded before reaching here
            raise LispRuntimeError( f'Macro "{function.name}" was not expanded before evaluation.' )
      except LispArgBindingError as ex:
         errorMsg = ex.args[-1]
         fnName = function.name
         if fnName == '':
            raise LispRuntimeError( f'Error binding arguments in call to "(lambda ...)".\n{errorMsg}')
         else:
            raise LispRuntimeError( f'Error binding arguments in call to function "{fnName}".\n{errorMsg}')
      finally:
         if printed:
            LispInterpreter._trace_depth = depth   # restore depth on both normal exit and exception
      if printed:
         hook( 'exit', function, result, depth )
      return result

   @staticmethod
   def _set_trace_hook() -> None:
      '''Activate or deactivate _apply_hook based on current trace state.'''
      if LispInterpreter._trace_global or LispInterpreter._traced:
         LispInterpreter._apply_hook = LispInterpreter._trace_fn
      else:
         LispInterpreter._apply_hook = None

   @staticmethod
   def _trace_fn( phase: str, function: LCallable, data: Any, depth: int ) -> bool:
      '''The apply hook used for tracing.  Filters by _traced set or _trace_global flag,
      then prints enter/exit lines to outStrm.  Returns True if a line was printed.'''
      traced    = LispInterpreter._traced
      global_   = LispInterpreter._trace_global
      isNamed  = function.name in traced
      isUserFn = isinstance( function, LFunction )

      # Global tracing shows all LFunctions; named tracing shows whatever was named.
      if not isNamed and not (global_ and isUserFn):
         return False

      name   = function.name if function.name else 'LAMBDA'
      indent = '  ' * depth
      outStrm = LispInterpreter.outStrm

      if phase == 'enter':
         argStr = ' '.join( prettyPrintSExpr(a) for a in data )
         line = f'{depth:2d}: {indent}({name}{" " + argStr if argStr else ""})'
      else:
         line = f'{depth:2d}: {indent}{name} returned {prettyPrintSExpr(data)}'

      print( line, file=outStrm )
      return True

   @staticmethod
   def _lbackquoteExpand( env: Environment, expr: Any, depth: int = 1 ) -> Any:
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
         inner = LispInterpreter._lbackquoteExpand( env, expr[1], depth + 1 )
         return [ LSymbol('BACKQUOTE'), inner ]

      if primary == 'COMMA':
         if depth == 1:
            return LispInterpreter._lEval( env, expr[1] )
         else:
            inner = LispInterpreter._lbackquoteExpand( env, expr[1], depth - 1 )
            return [ LSymbol('COMMA'), inner ]

      if primary == 'COMMA-AT':
         if depth == 1:
            result = LispInterpreter._lEval( env, expr[1] )
            if not isinstance( result, list ):
               raise LispRuntimeFuncError( env.lookup('COMMA-AT'), 'Argument 1 must evaluate to a List.' )
            return [ LSymbol('COMMA-AT'), result ]
         else:
            inner = LispInterpreter._lbackquoteExpand( env, expr[1], depth - 1 )
            return [ LSymbol('COMMA-AT'), inner ]

      # Regular list — process each element at the same depth.
      # Splice COMMA-AT sentinels only at depth 1.
      resultList: list[Any] = [ ]
      for listElt in expr:
         resultListElt = LispInterpreter._lbackquoteExpand( env, listElt, depth )
         if ( depth == 1 and
              isinstance(resultListElt, list) and
              len(resultListElt) > 0 and
              resultListElt[0] == LSymbol('COMMA-AT') ):
            for elt in resultListElt[1]:
               resultList.append( elt )
         else:
            resultList.append( resultListElt )
      return resultList

   @staticmethod
   def _lconstructPrimitives( parseLispString: Callable[[str], Any] ) -> dict[str, Any]:
      primitiveDict: dict[str, Any] = {}
      primitiveDict[ 'T'   ] = L_T
      primitiveDict[ 'NIL' ] = L_NIL

      class primitive:
         def __init__( self, primitiveSymbolString: str, paramsString: str = '', specialForm: bool = False ) -> None:
            self._name:         str  = primitiveSymbolString.upper()
            self._paramsString: str  = paramsString
            self._specialForm:  bool = specialForm
         def __call__( self, pythonFn ):
            docString    = pythonFn.__doc__ if pythonFn.__doc__ is not None else ''
            lPrimitivObj = LPrimitive( pythonFn, self._name, self._paramsString, docString,
                                       specialForm=self._specialForm )
            primitiveDict[ self._name ] = lPrimitivObj
            return lPrimitivObj

      from pythonslisp.primitives import p_meta, p_control, p_sequences, p_math, p_types, p_strings, p_io
      p_meta.register( primitive )
      p_control.register( primitive )
      p_sequences.register( primitive )
      p_math.register( primitive, primitiveDict )    # also adds PI and E
      p_types.register( primitive, parseLispString )
      p_strings.register( primitive )
      p_io.register( primitive, parseLispString )

      return primitiveDict

import time
from pathlib import Path
from typing import Callable, Any, Sequence

from pythonslisp.LispParser import LispParser
from pythonslisp.Listener import Interpreter, retrieveFileList
from pythonslisp.Environment import Environment
from pythonslisp.LispAST import ( LSymbol, L_T, L_NIL,
                                  LCallable, LFunction, LPrimitive, LMacro,
                                  prettyPrintSExpr )
from pythonslisp.LispExceptions import ( LispRuntimeError, LispRuntimeFuncError,
                                         LispArgBindingError )
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
      finally:
         LispInterpreter.outStrm = None
      return returnVal

   @staticmethod
   def _lTrue( sExpr: Any ) -> bool:
      if isinstance(sExpr, list):
         return len(sExpr) != 0
      return True

   @staticmethod
   def _lEval( env: Environment, sExprAST: Any ) -> Any:
      '''This is a recursive tree-walk evaluator.  It's the interpreter's main
      evaluation function.  Pass it any s-expression in the form of a LispAST;
      eval will return the result.'''
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
      primary = sExprAST[0]
      function = LispInterpreter._lEval( env, primary )
      if not isinstance( function, LCallable ):
         raise LispRuntimeError( f'Badly formed list expression \'{primary}\'.  The first element should evaluate to a callable.' )

      # Call the function with its arguments
      if function.specialForm:
         return LispInterpreter._lApply( env, function, sExprAST[1:] )
      else:
         nArgs = len(sExprAST)
         evalArgs = [ LispInterpreter._lEval(env, sExprAST[i]) for i in range(1, nArgs) ]
         return LispInterpreter._lApply( env, function, evalArgs )

   @staticmethod
   def _lApply( env: Environment, function: LCallable, args: Sequence ) -> Any:
      hook    = LispInterpreter._apply_hook
      depth   = LispInterpreter._trace_depth
      printed = False
      if hook:
         printed = hook( 'enter', function, args, depth )
         if printed:
            LispInterpreter._trace_depth = depth + 1   # only advance visible depth when tracing this call
      try:
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

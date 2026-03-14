from __future__ import annotations
import importlib.util
import sys
import time
from fractions import Fraction
from pathlib import Path
from typing import Any, Sequence

from pythonslisp.Parser import Parser
from pythonslisp.ltk.Listener import InterpreterBase
from pythonslisp.AST import ( LSymbol, L_T, L_NIL,
                              LCallable, LFunction, LPrimitive, LMacro, LContinuation,
                              LMultipleValues, prettyPrintSExpr )
from pythonslisp.Exceptions import ( LRuntimeError, LRuntimePrimError,
                                         LArgBindingError, ContinuationInvoked,
                                         ReturnFrom, Thrown, Signaled )
from pythonslisp.Environment import Environment
from pythonslisp.Expander import Expander
from pythonslisp.Analyzer import Analyzer
from pythonslisp.Tracer import Tracer
from pythonslisp.Context import Context
from pythonslisp.extensions import LambdaListMode, primitive as _ext_primitive
from pythonslisp.CEK import cek_eval as _cek_eval


def _primary( val: Any ) -> Any:
   """In scalar context, extract the primary (first) value from LMultipleValues.
All other objects pass through unchanged."""
   if type(val) is LMultipleValues:
      return val.values[0] if val.values else L_NIL
   return val


class Interpreter( InterpreterBase ):
   BUILTIN_EXT_DIR       = Path(__file__).parent / 'extensions'

   def __init__( self, ext_dir=None, outStrm=None ) -> None:
      sys.setrecursionlimit( 3000 )
      self._parser:       Parser           = Parser()
      self._tracer:       Tracer           = Tracer()
      self._setf_registry: dict[str, str]  = {}
      self._ctx:          Context          = None
      self._env:          Environment      = None
      self.reboot( ext_dir=ext_dir, outStrm=outStrm )

   def reboot( self, ext_dir=None, outStrm=None ) -> None:
      # Reset tracing state so stale traced-function names don't linger
      self._tracer.reset()
      self._setf_registry.clear()

      # Bootstrap: create the global environment env; initialize with T and NIL;
      #    bind extensions into it.
      primitiveDict: dict[str, Any] = {'T': L_T, 'NIL': L_NIL}
      self._ctx = self._makeContext( outStrm )
      self._env = Environment( parent=None, initialBindings=primitiveDict,
                               evalFn=self._ctx.lEval )

      # Load built-in extensions (pythonslisp/extensions/)
      self._loadExtDir( self.BUILTIN_EXT_DIR, outStrm )

      # Load caller-specified extension dir
      if ext_dir is not None:
         self._loadExtDir( Path(ext_dir), outStrm )

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
      if type(returnVal) is LMultipleValues:
         return '\n'.join( prettyPrintSExpr(v) for v in returnVal.values )
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
            returnVal = _cek_eval( ctx, self._env, form )
      except ContinuationInvoked:
         raise LRuntimeError( 'Continuation invoked outside its dynamic extent.' )
      except Thrown as e:
         raise LRuntimeError( f'throw: no catch for tag {prettyPrintSExpr(e.tag)}.' )
      except ReturnFrom as e:
         raise LRuntimeError( f'return-from: no block named {e.name} is currently active.' )
      except Signaled as e:
         raise LRuntimeError( f'Unhandled condition: {prettyPrintSExpr(e.condition)}' )
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
            returnVal = _cek_eval( ctx, self._env, form )
         evalTime = time.perf_counter() - startTime
      except ContinuationInvoked:
         raise LRuntimeError( 'Continuation invoked outside its dynamic extent.' )
      except Thrown as e:
         raise LRuntimeError( f'throw: no catch for tag {prettyPrintSExpr(e.tag)}.' )
      except ReturnFrom as e:
         raise LRuntimeError( f'return-from: no block named {e.name} is currently active.' )
      except Signaled as e:
         raise LRuntimeError( f'Unhandled condition: {prettyPrintSExpr(e.condition)}' )
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
            returnVal = _cek_eval( ctx, self._env, form )
      except ContinuationInvoked:
         raise LRuntimeError( 'Continuation invoked outside its dynamic extent.' )
      except Thrown as e:
         raise LRuntimeError( f'throw: no catch for tag {prettyPrintSExpr(e.tag)}.' )
      except ReturnFrom as e:
         raise LRuntimeError( f'return-from: no block named {e.name} is currently active.' )
      except Signaled as e:
         raise LRuntimeError( f'Unhandled condition: {prettyPrintSExpr(e.condition)}' )
      return returnVal

   def _makeContext( self, outStrm ) -> Context:
      ctx = Context( outStrm, self._tracer, self._setf_registry )
      ctx.lEval            = lambda env, sexpr: _cek_eval( ctx, env, sexpr )
      ctx.lApply           = Interpreter._lApply
      ctx.lQuasiquoteExpand = Interpreter._lquasiquoteExpand
      ctx.parse            = self._parser.parse
      ctx.parseFile        = self._parser.parseFile
      ctx.parseOne         = self._parser.parseOne
      ctx.expand           = lambda env, ast: Expander.expand( ctx, env, ast )
      ctx.analyze          = lambda env, ast: Analyzer.analyze( env, ast )
      ctx.loadExt          = lambda path, targetEnv=None: self._loadExtFile( Path(path), ctx.outStrm, targetEnv )
      ctx.loadExtDir       = lambda path: self._loadExtDir( Path(path), ctx.outStrm )
      ctx.reboot           = lambda: self.reboot( outStrm=ctx.outStrm )
      return ctx

   def _makeLispFunction( self, targetEnv=None ):
      """Returns the primitive decorator class used by extension .py files.
When targetEnv is a ModuleEnvironment, primitives are bound into that module
instead of the global environment."""
      _UNSET      = object()
      _KW_MARKERS = frozenset({'&OPTIONAL', '&REST', '&BODY', '&KEY',
                               '&AUX', '&ALLOW-OTHER-KEYS'})
      interpreter  = self
      _target_env  = targetEnv    # capture for closure

      def _derive_arity( ll_ast: list ) -> tuple[int, int|None]:
         min_args   = 0
         max_args   = 0
         in_section = 'required'
         unbounded  = False
         for item in ll_ast:
            if isinstance( item, LSymbol ) and item.name in _KW_MARKERS:
               marker = item.name
               if marker in ('&REST', '&BODY'):
                  in_section = 'rest'
                  unbounded  = True
               elif marker == '&OPTIONAL':
                  in_section = 'optional'
               elif marker in ('&KEY', '&ALLOW-OTHER-KEYS'):
                  in_section = 'key'
                  unbounded  = True
               elif marker == '&AUX':
                  in_section = 'aux'
            else:
               if in_section == 'required':
                  min_args += 1
                  max_args += 1
               elif in_section == 'optional':
                  max_args += 1
         if unbounded:
            return min_args, None
         return min_args, max_args

      def _make_arity_msg( min_args: int, max_args: int|None ) -> str:
         if max_args is None:
            if min_args == 0:
               return ''
            if min_args == 1:
               return 'At least 1 argument expected.'
            return f'At least {min_args} arguments expected.'
         if min_args == max_args:
            if min_args == 0:
               return '0 arguments expected.'
            if min_args == 1:
               return '1 argument expected.'
            return f'{min_args} arguments expected.'
         if max_args == min_args + 1:
            return f'{min_args} or {max_args} arguments expected.'
         return f'{min_args} to {max_args} arguments expected.'

      class primitive:
         def __init__( self, primitiveSymbolString: str, params: str = '',
                       preEvalArgs: bool = True,
                       mode: LambdaListMode = LambdaListMode.ARITY_ONLY,
                       min_args=_UNSET, max_args=_UNSET ) -> None:
            self._name        = primitiveSymbolString.upper()
            self._preEvalArgs = preEvalArgs
            if mode is LambdaListMode.FULL_BINDING:
               ll_ast = interpreter._parser.parse( params )[1]
               self._lambdaListAST = ll_ast
               stripped = params.strip()
               if stripped.startswith('(') and stripped.endswith(')'):
                  stripped = stripped[1:-1].strip()
               self._paramsString = stripped
               derived_min, derived_max = _derive_arity( ll_ast )
               self._min_args = derived_min if min_args is _UNSET else min_args
               self._max_args = derived_max if max_args is _UNSET else max_args
            elif mode is LambdaListMode.ARITY_ONLY:
               ll_ast = interpreter._parser.parse( params )[1]
               self._lambdaListAST = None
               stripped = params.strip()
               if stripped.startswith('(') and stripped.endswith(')'):
                  stripped = stripped[1:-1].strip()
               self._paramsString = stripped
               derived_min, derived_max = _derive_arity( ll_ast )
               self._min_args = derived_min if min_args is _UNSET else min_args
               self._max_args = derived_max if max_args is _UNSET else max_args
            else:  # DOC_ONLY
               self._lambdaListAST = None
               stripped = params.strip()
               if stripped.startswith('(') and stripped.endswith(')'):
                  stripped = stripped[1:-1].strip()
               self._paramsString  = stripped
               self._min_args = 0    if min_args is _UNSET else min_args
               self._max_args = None if max_args is _UNSET else max_args
            self._arity_msg = _make_arity_msg( self._min_args, self._max_args )

         def __call__( self, pythonFn ):
            docString    = pythonFn.__doc__ if pythonFn.__doc__ is not None else ''
            lPrimitivObj = LPrimitive( pythonFn, self._name, self._paramsString, docString,
                                       preEvalArgs=self._preEvalArgs,
                                       min_args=self._min_args, max_args=self._max_args,
                                       arity_msg=self._arity_msg,
                                       lambdaListAST=self._lambdaListAST )
            if _target_env is not None:
               _target_env.bindLocal( self._name, lPrimitivObj )
            else:
               interpreter._env.bindGlobal( self._name, lPrimitivObj )
            pythonFn.primitive = lPrimitivObj
            return pythonFn

      return primitive

   def _loadExtDir( self, ext_dir: Path, outStrm=None ) -> None:
      ext_dir = Path(ext_dir)
      if not ext_dir.is_dir():
         return
      for py_file in sorted( ext_dir.glob('*.py') ):
         self._loadExtFile( py_file, outStrm )
      for lisp_file in sorted( ext_dir.glob('*.lisp') ):
         self._loadExtFile( lisp_file, outStrm )

   def _loadExtFile( self, path: Path, outStrm=None, targetEnv=None ) -> None:
      path = Path(path)
      if path.suffix == '.py':
         spec   = importlib.util.spec_from_file_location( path.stem, path )
         module = importlib.util.module_from_spec( spec )
         spec.loader.exec_module( module )
         _ext_primitive._flush( self._makeLispFunction( targetEnv ) )
      elif path.suffix == '.lisp':
         if targetEnv is not None:
            ast = self._parser.parseFile( str(path) )
            _cek_eval( self._ctx, targetEnv, ast )
         else:
            self.evalFile( str(path), outStrm )

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
               result = function.pythonFn( ctx, kw_env, args )
            else:
               result = function.pythonFn( ctx, env, args )
         elif isinstance( function, LFunction ):
            env = Environment( function.capturedEnvironment, evalFn=ctx.lEval ) # Open a new scope on the function's captured env to support closures.

            # store the arguments as locals
            env.bindArguments( function.lambdaListAST, args )

            # evaluate the body expressions.
            result = L_NIL
            for sexpr in function.bodyAST:
               result = _cek_eval( ctx, env, sexpr )
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
      result = _primary( result )
      if printed:
         tracer.trace( 'exit', function, result, depth, ctx.outStrm )
      return result

   @staticmethod
   def _lquasiquoteExpand( ctx: Context, env: Environment, expr: Any, depth: int = 1 ) -> Any:
      '''Expand a quasiquote expression, tracking nesting depth.
      depth=1 is the innermost (active) quasiquote level.  Commas and
      splices are only evaluated at depth 1; at deeper levels they are
      preserved as template structure for the inner quasiquote.'''
      if not isinstance(expr, list):
         return expr       # atoms/symbols pass through unchanged at any depth

      if len(expr) == 0:
         return expr

      head = expr[0]

      if head == 'QUASIQUOTE':
         # Nested quasiquote — increase depth, process content, rewrap
         inner = Interpreter._lquasiquoteExpand( ctx, env, expr[1], depth + 1 )
         return [ LSymbol('QUASIQUOTE'), inner ]

      if head == 'UNQUOTE':
         if depth == 1:
            return ctx.lEval( env, expr[1] )
         else:
            inner = Interpreter._lquasiquoteExpand( ctx, env, expr[1], depth - 1 )
            return [ LSymbol('UNQUOTE'), inner ]

      if head == 'UNQUOTE-SPLICING':
         if depth == 1:
            result = ctx.lEval( env, expr[1] )
            if not isinstance( result, list ):
               raise LRuntimePrimError( env.lookup('UNQUOTE-SPLICING'), 'Argument 1 must evaluate to a List.' )
            return [ LSymbol('UNQUOTE-SPLICING'), result ]
         else:
            inner = Interpreter._lquasiquoteExpand( ctx, env, expr[1], depth - 1 )
            return [ LSymbol('UNQUOTE-SPLICING'), inner ]

      # Regular list — process each element at the same depth.
      # Splice UNQUOTE-SPLICING sentinels only at depth 1.
      resultList: list[Any] = [ ]
      for listElt in expr:
         resultListElt = Interpreter._lquasiquoteExpand( ctx, env, listElt, depth )
         if ( depth == 1 and
              isinstance(resultListElt, list) and
              len(resultListElt) > 0 and
              resultListElt[0] == 'UNQUOTE-SPLICING' ):
            for elt in resultListElt[1]:
               resultList.append( elt )
         else:
            resultList.append( resultListElt )
      return resultList



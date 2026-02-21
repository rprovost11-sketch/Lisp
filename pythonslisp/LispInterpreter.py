import functools
import math
import random
import time
from fractions import Fraction
from typing import Callable, Any, Sequence

from pythonslisp.helpTopics import topics
from pythonslisp.LispParser import LispParser, ParseError
from pythonslisp.Listener import Interpreter, retrieveFileList, columnize
from pythonslisp.Environment import Environment
from pythonslisp.LispAST import ( LSymbol, LNUMBER,
                                  LCallable, LFunction, LPrimitive, LMacro, LContinuation,
                                  prettyPrint, prettyPrintSExpr )

L_T = LSymbol( 'T' )
L_NIL = []

class LispRuntimeError( Exception ):
   pass


class LispRuntimeFuncError( LispRuntimeError ):
   def __init__( self, lispCallable: LPrimitive, errorMsg: str ) -> None:
      fnName = lispCallable.name
      usage = lispCallable.usageString()
      errStr = f"ERROR '{fnName}': {errorMsg}\nUSAGE: {usage}" if usage else f"ERROR '{fnName}': {errorMsg}"
      super().__init__( errStr )


class LispArgBindingError( LispRuntimeError ):
   pass


class ContinuationInvoked( Exception ):
   '''Raised when an escape continuation is invoked.

   Propagates up through re-entrant _lEval calls (from while, doTimes,
   foreach, etc.) until it reaches the _lEval whose local stack object
   matches targetStack.  That wrapper then restores the stack and restarts
   the CEK loop with the supplied value.
   '''
   __slots__ = ('value', 'restoreStack', 'targetStack', 'continuation')

   def __init__( self, value: Any, restoreStack: list, targetStack: list, continuation: Any ) -> None:
      self.value        = value
      self.restoreStack = restoreStack
      self.targetStack  = targetStack
      self.continuation = continuation


class LispInterpreter( Interpreter ):
   outStrm = None
   _setf_registry: dict[str, str] = {}   # accessor-name → field-dict-key

   def __init__( self, runtimeLibraryDir: (str|None)=None ) -> None:
      self._libDir = runtimeLibraryDir
      self._parser: LispParser = LispParser( )

   def reboot( self, outStrm=None ) -> None:
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
         from pythonslisp.LispExpander import LispExpander
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

         from pythonslisp.LispExpander import LispExpander
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
         from pythonslisp.LispExpander import LispExpander
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
   def _parse_let_bindings( fn: Any, vardefs: list ) -> list:
      '''Parse LET/LET* binding specs. Returns list of (varname_str, initexpr) tuples.'''
      pairs = []
      for varSpec in vardefs:
         if isinstance(varSpec, LSymbol):
            pairs.append( (varSpec.strval, L_NIL) )
         elif isinstance(varSpec, list):
            vs_len = len(varSpec)
            if vs_len == 1:
               if not isinstance(varSpec[0], LSymbol):
                  raise LispRuntimeFuncError( fn, 'First element of a variable initializer pair expected to be a symbol.' )
               pairs.append( (varSpec[0].strval, L_NIL) )
            elif vs_len == 2:
               if not isinstance(varSpec[0], LSymbol):
                  raise LispRuntimeFuncError( fn, 'First element of a variable initializer pair expected to be a symbol.' )
               pairs.append( (varSpec[0].strval, varSpec[1]) )
            else:
               raise LispRuntimeFuncError( fn, 'Variable initializer spec expected to be 1 or 2 elements long.' )
         else:
            raise LispRuntimeFuncError( fn, 'Variable initializer spec expected to be a symbol or a list.' )
      return pairs

   @staticmethod
   def _lEval( env: Environment, sExprAST: Any ) -> Any:
      '''Start the CEK machine for a single top-level expression.

      Wraps _lEvalLoop in a restart loop so that escape continuations invoked
      from inside re-entrant _lEval calls (while, doTimes, foreach …) can
      propagate a ContinuationInvoked exception back to the _lEval that owns
      the target stack and have it restore state and continue correctly.
      '''
      stack     = [('halt',)]
      expr      = sExprAST
      need_eval = True
      val       = None
      while True:
         try:
            return LispInterpreter._lEvalLoop( stack, expr, env, need_eval, val )
         except ContinuationInvoked as ci:
            if ci.targetStack is stack:
               ci.continuation.callccIsActive = False
               stack.clear()
               stack.extend( ci.restoreStack )
               val       = ci.value
               need_eval = False
               expr      = None    # irrelevant; apply phase uses envs from frames
            else:
               raise              # for an outer _lEval's stack

   @staticmethod
   def _lEvalLoop( stack: list, expr: Any, env: Environment, need_eval: bool, val: Any ) -> Any:
      '''Iterative CEK machine evaluator loop.

      Continuation stack frames (tuples):
        (\'ah\', env, raw_args)                                       ApplyHead
        (\'aa\', env, fn, evaled, remaining)                          ApplyArg  (mutable list)
        (\'if\', env, then_e, else_e)                                 If
        (\'body\', env, remaining)                                     Body/Progn
        (\'and\', env, remaining)                                      And
        (\'or\',  env, remaining)                                      Or
        (\'li\', orig_env, new_env, var, remaining_pairs, coll, body) LetInit
        (\'ls\', new_env, var, remaining_specs, body)                  LetStar
        (\'ct\', env, then_body, remaining_clauses, clause_num, fn)   CondTest
        (\'cs\', env, case_list, fn)                                   CaseStart
        (\'ck\', env, key_val, case_body, remaining, case_num, fn)    CaseKey
        (\'cc\', k)                                                    CallCC sentinel
      '''
      while True:
         if need_eval:
            # ---- EVAL PHASE ----
            if isinstance(expr, LSymbol):
               try:
                  val = env.lookup( expr.strval )
               except KeyError:
                  if expr.isKeyArg():
                     val = expr
                  else:
                     raise LispRuntimeError( f'Unbound Variable: {expr.strval}.' )
            elif not isinstance(expr, list):
               val = expr
            elif len(expr) == 0:
               val = L_NIL
            else:
               # List: push ApplyHead continuation, evaluate head next
               stack.append( ('ah', env, expr[1:]) )
               expr = expr[0]
               continue   # stay in eval phase
            need_eval = False

         # ---- APPLY PHASE ----
         while True:
            frame = stack[-1]
            tag   = frame[0]

            # Fast-path: ApplyArg accumulation (hottest path, zero allocation)
            if tag == 'aa':
               frame[3].append(val)          # mutate evaled list in-place
               if frame[4]:                  # remaining (reversed) non-empty
                  env = frame[1]
                  expr = frame[4].pop()      # O(1) pop from reversed list
                  need_eval = True; break
               else:
                  fenv = frame[1]; fn = frame[2]; evaled = frame[3]
                  stack.pop()
                  val, need_eval, expr, env = LispInterpreter._cek_apply(stack, fenv, fn, evaled)
                  if need_eval: break
               continue                      # need_eval=False: continue apply phase

            # Halt sentinel
            if tag == 'halt':
               return val

            # --- ApplyHead ---
            if tag == 'ah':
               stack.pop()
               _, fenv, raw_args = frame
               fn = val
               if not isinstance(fn, LCallable):
                  raise LispRuntimeError(
                     f"Badly formed list expression '{fn}'.  "
                     f"The first element should evaluate to a callable." )

               if fn.specialForm:
                  fname = fn.name

                  if fname == 'PROGN':
                     if not raw_args:
                        val = L_NIL
                     elif len(raw_args) == 1:
                        expr = raw_args[0]; env = fenv; need_eval = True; break
                     else:
                        stack.append( ('body', fenv, list(raw_args[1:])) )
                        expr = raw_args[0]; env = fenv; need_eval = True; break

                  elif fname == 'IF':
                     n = len(raw_args)
                     if not (2 <= n <= 3):
                        raise LispRuntimeFuncError( fn, '2 or 3 arguments expected.' )
                     then_e = raw_args[1]
                     else_e = raw_args[2] if n == 3 else L_NIL
                     stack.append( ('if', fenv, then_e, else_e) )
                     expr = raw_args[0]; env = fenv; need_eval = True; break

                  elif fname == 'AND':
                     if len(raw_args) < 2:
                        raise LispRuntimeFuncError( fn, '2 or more arguments expected.' )
                     stack.append( ('and', fenv, list(raw_args[1:])) )
                     expr = raw_args[0]; env = fenv; need_eval = True; break

                  elif fname == 'OR':
                     if len(raw_args) < 2:
                        raise LispRuntimeFuncError( fn, '2 or more arguments expected.' )
                     stack.append( ('or', fenv, list(raw_args[1:])) )
                     expr = raw_args[0]; env = fenv; need_eval = True; break

                  elif fname == 'LET':
                     if len(raw_args) < 1:
                        raise LispRuntimeFuncError( fn, '2 or more arguments expected.' )
                     vardefs = raw_args[0]
                     body    = list(raw_args[1:])
                     if not isinstance(vardefs, list):
                        raise LispRuntimeFuncError( fn, 'The first argument to let expected to be a list of variable initializations.' )
                     new_env = Environment(fenv)
                     pairs   = LispInterpreter._parse_let_bindings(fn, vardefs)
                     if not pairs:
                        if not body:
                           val = L_NIL
                        elif len(body) == 1:
                           expr = body[0]; env = new_env; need_eval = True; break
                        else:
                           stack.append( ('body', new_env, body[1:]) )
                           expr = body[0]; env = new_env; need_eval = True; break
                     else:
                        first_var, first_init = pairs[0]
                        stack.append( ('li', fenv, new_env, first_var, pairs[1:], [], body) )
                        expr = first_init; env = fenv; need_eval = True; break

                  elif fname == 'LET*':
                     if len(raw_args) < 1:
                        raise LispRuntimeFuncError( fn, '2 or more arguments expected.' )
                     vardefs = raw_args[0]
                     body    = list(raw_args[1:])
                     if not isinstance(vardefs, list):
                        raise LispRuntimeFuncError( fn, 'The first argument to let expected to be a list of variable initializations.' )
                     new_env = Environment(fenv)
                     specs   = LispInterpreter._parse_let_bindings(fn, vardefs)
                     if not specs:
                        if not body:
                           val = L_NIL
                        elif len(body) == 1:
                           expr = body[0]; env = new_env; need_eval = True; break
                        else:
                           stack.append( ('body', new_env, body[1:]) )
                           expr = body[0]; env = new_env; need_eval = True; break
                     else:
                        first_var, first_init = specs[0]
                        stack.append( ('ls', new_env, first_var, specs[1:], body) )
                        expr = first_init; env = new_env; need_eval = True; break

                  elif fname == 'COND':
                     if len(raw_args) < 1:
                        raise LispRuntimeFuncError( fn, '1 or more argument expected.' )
                     clauses = list(raw_args)
                     try:
                        test_expr, *then_body = clauses[0]
                     except (ValueError, TypeError):
                        raise LispRuntimeFuncError( fn, 'Entry 1 does not contain a (<cond:expr> <body:expr>) pair.' )
                     if len(then_body) < 1:
                        raise LispRuntimeFuncError( fn, 'Entry 1 expects at least one body expression.' )
                     stack.append( ('ct', fenv, then_body, clauses[1:], 2, fn) )
                     expr = test_expr; env = fenv; need_eval = True; break

                  elif fname == 'CASE':
                     if len(raw_args) < 1:
                        raise LispRuntimeFuncError( fn, '2 or more arguments expected.' )
                     key_expr  = raw_args[0]
                     case_list = list(raw_args[1:])
                     if not case_list:
                        raise LispRuntimeFuncError( fn, 'At least one case expected.' )
                     stack.append( ('cs', fenv, case_list, fn) )
                     expr = key_expr; env = fenv; need_eval = True; break

                  else:
                     # All other special forms (setf, lambda, quote, etc.): call directly
                     val = fn.pythonFn(fenv, *raw_args)
                     # stay in apply phase

               else:
                  # Normal (non-special) function: evaluate args
                  if not raw_args:
                     val, need_eval, expr, env = LispInterpreter._cek_apply(stack, fenv, fn, [])
                     if need_eval:
                        break
                  else:
                     stack.append( ['aa', fenv, fn, [], list(reversed(raw_args[1:]))] )
                     expr = raw_args[0]; env = fenv; need_eval = True; break

            # --- If ---
            elif tag == 'if':
               stack.pop()
               _, fenv, then_e, else_e = frame
               expr = then_e if LispInterpreter._lTrue(val) else else_e
               env = fenv; need_eval = True; break

            # --- Body / Progn ---
            elif tag == 'body':
               stack.pop()
               _, fenv, remaining = frame
               if not remaining:
                  pass   # val is the result; continue apply phase
               elif len(remaining) == 1:
                  expr = remaining[0]; env = fenv; need_eval = True; break   # TCO
               else:
                  stack.append( ('body', fenv, remaining[1:]) )
                  expr = remaining[0]; env = fenv; need_eval = True; break

            # --- And ---
            elif tag == 'and':
               stack.pop()
               _, fenv, remaining = frame
               if not LispInterpreter._lTrue(val):
                  val = L_NIL      # short-circuit to NIL
               elif not remaining:
                  val = L_T        # all truthy: return T
               else:
                  stack.append( ('and', fenv, remaining[1:]) )
                  expr = remaining[0]; env = fenv; need_eval = True; break

            # --- Or ---
            elif tag == 'or':
               stack.pop()
               _, fenv, remaining = frame
               if LispInterpreter._lTrue(val):
                  val = L_T        # short-circuit: return T
               elif not remaining:
                  val = L_NIL      # all falsy
               else:
                  stack.append( ('or', fenv, remaining[1:]) )
                  expr = remaining[0]; env = fenv; need_eval = True; break

            # --- LetInit ---
            elif tag == 'li':
               stack.pop()
               _, orig_env, new_env, var_to_bind, remaining_pairs, collected, body = frame
               collected = collected + [(var_to_bind, val)]
               if not remaining_pairs:
                  for vname, vval in collected:
                     new_env.bindLocal(vname, vval)
                  if not body:
                     val = L_NIL
                  elif len(body) == 1:
                     expr = body[0]; env = new_env; need_eval = True; break   # TCO
                  else:
                     stack.append( ('body', new_env, body[1:]) )
                     expr = body[0]; env = new_env; need_eval = True; break
               else:
                  next_var, next_init = remaining_pairs[0]
                  stack.append( ('li', orig_env, new_env, next_var, remaining_pairs[1:], collected, body) )
                  expr = next_init; env = orig_env; need_eval = True; break

            # --- LetStar ---
            elif tag == 'ls':
               stack.pop()
               _, new_env, var_to_bind, remaining_specs, body = frame
               new_env.bindLocal(var_to_bind, val)
               if not remaining_specs:
                  if not body:
                     val = L_NIL
                  elif len(body) == 1:
                     expr = body[0]; env = new_env; need_eval = True; break   # TCO
                  else:
                     stack.append( ('body', new_env, body[1:]) )
                     expr = body[0]; env = new_env; need_eval = True; break
               else:
                  next_var, next_init = remaining_specs[0]
                  stack.append( ('ls', new_env, next_var, remaining_specs[1:], body) )
                  expr = next_init; env = new_env; need_eval = True; break

            # --- CondTest ---
            elif tag == 'ct':
               stack.pop()
               _, fenv, then_body, remaining_clauses, clause_num, fn = frame
               if LispInterpreter._lTrue(val):
                  if len(then_body) == 1:
                     expr = then_body[0]; env = fenv; need_eval = True; break   # TCO
                  else:
                     stack.append( ('body', fenv, then_body[1:]) )
                     expr = then_body[0]; env = fenv; need_eval = True; break
               elif not remaining_clauses:
                  val = L_NIL
               else:
                  clause = remaining_clauses[0]
                  try:
                     test_expr, *then_body2 = clause
                  except (ValueError, TypeError):
                     raise LispRuntimeFuncError( fn, f'Entry {clause_num} does not contain a (<cond:expr> <body:expr>) pair.' )
                  if len(then_body2) < 1:
                     raise LispRuntimeFuncError( fn, f'Entry {clause_num} expects at least one body expression.' )
                  stack.append( ('ct', fenv, then_body2, remaining_clauses[1:], clause_num + 1, fn) )
                  expr = test_expr; env = fenv; need_eval = True; break

            # --- CaseStart ---
            elif tag == 'cs':
               stack.pop()
               _, fenv, case_list, fn = frame
               key_val = val
               if not case_list:
                  val = L_NIL
               else:
                  first_case = case_list[0]
                  try:
                     case_expr, *case_body = first_case
                  except (ValueError, TypeError):
                     raise LispRuntimeFuncError( fn, 'Entry 1 does not contain a (<val> <body>) pair.' )
                  if len(case_body) < 1:
                     raise LispRuntimeFuncError( fn, 'Case body expected.' )
                  stack.append( ('ck', fenv, key_val, case_body, case_list[1:], 2, fn) )
                  expr = case_expr; env = fenv; need_eval = True; break

            # --- CaseKey ---
            elif tag == 'ck':
               stack.pop()
               _, fenv, key_val, case_body, remaining_cases, case_num, fn = frame
               if val == key_val:
                  if len(case_body) == 1:
                     expr = case_body[0]; env = fenv; need_eval = True; break   # TCO
                  else:
                     stack.append( ('body', fenv, case_body[1:]) )
                     expr = case_body[0]; env = fenv; need_eval = True; break
               elif not remaining_cases:
                  val = L_NIL
               else:
                  next_case = remaining_cases[0]
                  try:
                     case_expr2, *case_body2 = next_case
                  except (ValueError, TypeError):
                     raise LispRuntimeFuncError( fn, f'Entry {case_num} does not contain a (<val> <body>) pair.' )
                  if len(case_body2) < 1:
                     raise LispRuntimeFuncError( fn, 'Case body expected.' )
                  stack.append( ('ck', fenv, key_val, case_body2, remaining_cases[1:], case_num + 1, fn) )
                  expr = case_expr2; env = fenv; need_eval = True; break

            # --- CallCC sentinel ---
            elif tag == 'cc':
               stack.pop()
               _, k = frame
               k.callccIsActive = False   # user fn returned without calling k
               # val passes through as the result of the call/cc expression

            else:
               raise LispRuntimeError( f'Unknown CEK frame type: {tag}' )


   @staticmethod
   def _copy_stack( stack: list ) -> list:
      '''Snapshot the CEK continuation stack for call/cc.

      Tuples (all frame types except 'aa') are immutable and safe to share.
      'aa' frames are mutable lists — the frame itself and its evaled/remaining
      inner lists are copied so the saved and live stacks do not alias each other.
      Environments are intentionally shared: a continuation should see the same
      variable bindings as any closure captured at the same point.
      '''
      result = []
      for frame in stack:
         if isinstance(frame, list):           # only 'aa' frames are lists
            result.append( [frame[0], frame[1], frame[2],
                             list(frame[3]),   # evaled    — copy
                             list(frame[4])] ) # remaining — copy
         else:
            result.append(frame)               # tuple: immutable, share safely
      return result

   @staticmethod
   def _cek_apply( stack: list, env: Environment, fn: LCallable, args: list ) -> tuple:
      '''Apply fn to args in CEK style.
      Returns (val, need_eval, new_expr, new_env).
      If need_eval is True:  caller should evaluate new_expr in new_env.
      If need_eval is False: val is the result (new_expr and new_env are None).
      Intercepts FUNCALL, APPLY, and EVAL for tail-call optimisation.
      '''
      try:
         if isinstance(fn, LPrimitive):
            fname = fn.name

            if fname == 'FUNCALL':
               if not args:
                  raise LispRuntimeFuncError( fn, "1 or more arguments expected" )
               real_fn = args[0]
               if isinstance(real_fn, LSymbol):
                  try:
                     real_fn = env.lookup( real_fn.strval )
                  except KeyError:
                     raise LispRuntimeFuncError( fn, f'First argument "{real_fn}" expected to be the name of a callable.' )
               if not isinstance(real_fn, LCallable):
                  raise LispRuntimeFuncError( fn, "First argument expected to be a callable." )
               return LispInterpreter._cek_apply(stack, env, real_fn, args[1:])

            elif fname == 'APPLY':
               if len(args) < 2:
                  raise LispRuntimeFuncError( fn, "At least 2 arguments expected to apply." )
               list_arg = args[-1]
               if not isinstance(list_arg, list):
                  raise LispRuntimeFuncError( fn, "Last argument expected to be a list." )
               primary = args[0]
               if isinstance(primary, LCallable):
                  fn_obj = primary
               elif isinstance(primary, LSymbol):
                  try:
                     fn_obj = env.lookup( primary.strval )
                  except KeyError:
                     raise LispRuntimeFuncError( fn, f'First argument "{primary}" expected to be the name of a callable.' )
               else:
                  raise LispRuntimeFuncError( fn, "First argument expected to be a symbol." )
               if fn_obj.specialForm:
                  raise LispRuntimeFuncError( fn, "First argument may not be a special form." )
               fn_args = list(args[1:-1]) + list(list_arg)
               return LispInterpreter._cek_apply(stack, env, fn_obj, fn_args)

            elif fname == 'EVAL':
               if len(args) != 1:
                  raise LispRuntimeFuncError( fn, '1 argument expected.' )
               return (None, True, args[0], env)

            elif fname == 'CALL/CC':
               if len(args) != 1:
                  raise LispRuntimeFuncError( fn, '1 argument expected.' )
               user_fn = args[0]
               if isinstance(user_fn, LSymbol):
                  try:
                     user_fn = env.lookup( user_fn.strval )
                  except KeyError:
                     raise LispRuntimeFuncError( fn, f'Argument "{user_fn}" is not bound.' )
               if not isinstance(user_fn, LCallable):
                  raise LispRuntimeFuncError( fn, 'Argument must be a callable.' )
               if user_fn.specialForm:
                  raise LispRuntimeFuncError( fn, 'Argument may not be a special form.' )
               # Capture the current continuation (everything waiting for our result)
               k = LContinuation( LispInterpreter._copy_stack(stack), stack )
               # Push sentinel so we know when the user fn returns without calling k
               stack.append( ('cc', k) )
               # Call the user function with k as its sole argument
               return LispInterpreter._cek_apply( stack, env, user_fn, [k] )

            else:
               return (fn.pythonFn(env, *args), False, None, None)

         elif isinstance(fn, LFunction):
            fn_env = Environment( fn.capturedEnvironment )
            LispInterpreter._lbindArguments( fn_env, fn.lambdaListAST, args )
            body = fn.bodyAST
            if not body:
               return (L_NIL, False, None, None)
            if len(body) > 1:
               stack.append( ('body', fn_env, list(body[1:])) )
            return (None, True, body[0], fn_env)   # TCO: tail-call into body

         elif isinstance(fn, LMacro):
            raise LispRuntimeError( f'Macro "{fn.name}" was not expanded before evaluation.' )

         elif isinstance(fn, LContinuation):
            if len(args) != 1:
               raise LispRuntimeError( "Continuation expects exactly 1 argument." )
            if not fn.callccIsActive:
               raise LispRuntimeError( "Escape continuation invoked outside its dynamic extent." )
            # Raise ContinuationInvoked so it propagates through any re-entrant
            # _lEval calls (while, doTimes, …) up to the _lEval that owns the
            # target stack, which will restore the stack and restart the CEK loop.
            raise ContinuationInvoked(
               args[0],
               LispInterpreter._copy_stack(fn.capturedStack),
               fn.targetStack,
               fn )

         else:
            raise LispRuntimeError( "Cannot apply non-callable value." )

      except LispArgBindingError as ex:
         errorMsg = ex.args[-1]
         fnName = fn.name if hasattr(fn, 'name') else ''
         if fnName == '':
            raise LispRuntimeError( f'Error binding arguments in call to "(lambda ...)".\n{errorMsg}' )
         else:
            raise LispRuntimeError( f'Error binding arguments in call to function "{fnName}".\n{errorMsg}' )

   @staticmethod
   def _lvalidateNoDuplicateParams( lambdaListAST: list[Any] ) -> None:
      '''Pre-pass: verify no variable name appears more than once in a lambda list.'''
      seen: set[str] = set()

      def _check( name: Any, context: str ) -> None:
         if not isinstance(name, LSymbol):
            return     # other validation will catch non-symbols
         if name.startswith('&'):
            return     # &-keywords are not parameter names
         if name.strval in seen:
            raise LispArgBindingError( f'Duplicate parameter name {name.strval} in {context}.' )
         seen.add( name.strval )

      index = 0
      lambdaListLen = len( lambdaListAST )
      while index < lambdaListLen:
         spec = lambdaListAST[index]

         if isinstance( spec, LSymbol ):
            if spec == '&REST':
               index += 1
               if index < lambdaListLen:
                  _check( lambdaListAST[index], '&REST' )
            elif not spec.startswith('&'):
               _check( spec, 'positional parameters' )

         elif isinstance( spec, list ) and len(spec) > 0:
            # First element is either a symbol (var) or a list ((keyword var))
            keyVarSpec = spec[0]
            if isinstance( keyVarSpec, LSymbol ):
               _check( keyVarSpec, 'lambda list' )
            elif isinstance( keyVarSpec, list ) and len(keyVarSpec) >= 2:
               _check( keyVarSpec[1], '&KEY (keyword var) pair' )
            # Check svar: 3rd element if present (for &optional and &key)
            if len(spec) >= 3:
               _check( spec[2], 'supplied-p variable' )

         index += 1

   @staticmethod
   def _lbindArguments( env: Environment, lambdaListAST: list[Any], argList: Sequence[Any] ) -> None:
      LispInterpreter._lvalidateNoDuplicateParams( lambdaListAST )
      paramListLength = len(lambdaListAST)
      argListLength = len(argList)

      paramNum, argNum = LispInterpreter._lbindPositionalArgs( env, lambdaListAST, 0, argList, 0 )
      
      # Retrieve the next param which should be a symbol
      try:
         nextParam = lambdaListAST[paramNum]
      except IndexError:
         # There are no more params to process. So argNum should be == or > argListLength.
         # So, if argNum < argListLength, then there are still unprocessed args.
         if argNum < argListLength:
            raise LispArgBindingError( f'Too many arguments.  Received {argListLength}.' )
         return          # All params used up.  Return gracefully
      if not isinstance(nextParam, LSymbol):
         raise LispArgBindingError( f"Param {paramNum} expected to be a symbol." )

      if nextParam == '&OPTIONAL':
         paramNum, argNum = LispInterpreter._lbindOptionalArgs( env, lambdaListAST, paramNum+1, argList, argNum )

         # Retrieve the next param which should be a symbol
         try:
            nextParam = lambdaListAST[paramNum]
         except IndexError:
            if argNum < argListLength:
               raise LispArgBindingError( f'Too many arguments.  Received {argListLength}.' )
            return          # All params used up.  Return gracefully
         if not isinstance(nextParam, LSymbol):
            raise LispArgBindingError( f"Param {paramNum} expected to be a symbol." )
      
      if nextParam == '&REST':
         paramNum, argNum = LispInterpreter._lbindRestArgs( env, lambdaListAST, paramNum+1, argList, argNum )
      
         # Retrieve the next param which should be a symbol
         try:
            nextParam = lambdaListAST[paramNum]
         except IndexError:
            return          # All params used up.  Return gracefully
         if not isinstance(nextParam, LSymbol):
            raise LispArgBindingError( f"Param {paramNum} expected to be a symbol." )
      
      if nextParam == '&KEY':
         paramNum, argNum = LispInterpreter._lbindKeyArgs( env, lambdaListAST, paramNum+1, argList, argNum )
      
         # Retrieve the next param which should be a symbol
         try:
            nextParam = lambdaListAST[paramNum]
         except IndexError:
            return          # All params used up.  Return gracefully
         if not isinstance(nextParam, LSymbol):
            raise LispArgBindingError( f"Param {paramNum} expected to be a symbol." )
      
      if nextParam == '&AUX':
         paramNum, argNum = LispInterpreter._lbindAuxArgs( env, lambdaListAST, paramNum+1, argList, argNum )
      elif nextParam.startswith('&'):
         _KNOWN_KEYWORDS = {'&OPTIONAL', '&REST', '&KEY', '&AUX', '&ALLOW-OTHER-KEYS'}
         if nextParam.strval in _KNOWN_KEYWORDS:
            raise LispArgBindingError( f'{nextParam} is misplaced in the lambda list.  Valid order: &optional, &rest, &key, &aux.' )
         else:
            raise LispArgBindingError( f'Unknown lambda list keyword: {nextParam}.' )

      if paramNum < paramListLength:
         raise LispArgBindingError( f'Unexpected content at position {paramNum} in lambda list.' )

   @staticmethod
   def _lbindPositionalArgs( env: Environment, lambdaListAST: list[Any], paramNum: int, argList: Sequence[Any], argNum: int ) -> (int, int):
      paramListLength = len(lambdaListAST)

      while paramNum < paramListLength:
         # Get the next parameter name.  Ensure it's a symbol but doesn't start with '&'.
         paramName = lambdaListAST[paramNum]
         if not isinstance(paramName, LSymbol):
            raise LispArgBindingError( f"Positional param {paramNum} expected to be a symbol." )
         if paramName.startswith('&'):
            break
         
         # Get the next argument value
         try:
            argVal = argList[argNum]
         except IndexError:
            raise LispArgBindingError( "Too few positional arguments." )
         
         # Bind paramName to argVal
         env.bindLocal( paramName.strval, argVal )
      
         # Prepare for the next iteration
         paramNum += 1
         argNum += 1
      
      return paramNum, argNum
   
   @staticmethod
   def _lbindOptionalArgs( env: Environment, lambdaListAST: list[Any], paramNum: int, argList: Sequence[Any], argNum: int ) -> (int, int):
      '''Syntax:  &optional {var | (var [initform [svar]])}*'''
      paramListLength = len(lambdaListAST)
      argListLength = len(argList)
      
      # Loop over the optional parameters and arguments
      while (paramNum < paramListLength):
         paramSpec = lambdaListAST[paramNum]
         
         # Extract the next parameter's values
         if isinstance( paramSpec, LSymbol ):
            if paramSpec.startswith('&'):
               break
            varName = paramSpec
            initForm = list( )
            svarName = None
         elif isinstance(paramSpec, list):
            paramSpecLen = len(paramSpec)
            if paramSpecLen == 1:
               varName = paramSpec[0]
               initForm = list()
               svarName = None
            elif paramSpecLen == 2:
               varName, initForm = paramSpec
               svarName = None
            elif paramSpecLen == 3:
               varName, initForm, svarName = paramSpec
            else:
               raise LispArgBindingError( 'Parameter spec following &OPTIONAL must be a list of (<variable> [<defaultvalue> [<svar>]] ).' )

            if not isinstance(varName, LSymbol):
               raise LispArgBindingError( 'Parameter variable in &OPTIONAL spec must be a symbol.' )
            if varName.startswith('&'):
               raise LispArgBindingError( f'Lambda list keyword {varName} cannot be used as a variable name in &OPTIONAL spec.' )
            if svarName and (not isinstance(svarName, LSymbol)):
               raise LispArgBindingError( f'Parameter svar following {varName} must be a symbol.' )
            if isinstance(svarName, LSymbol) and svarName.startswith('&'):
               raise LispArgBindingError( f'Lambda list keyword {svarName} cannot be used as a supplied-p variable in &OPTIONAL spec.' )
         else:
            raise LispArgBindingError( 'Parameter spec following &OPTIONAL must be a <variable> or a list of (<variable> <defaultvalue>). ' )
         paramNum += 1
        
         # Extract the next argument's value
         originalInitForm = initForm                  # save before potential overwrite
         if argNum < argListLength:
            initForm = argList[argNum]

         if (argNum >= argListLength) or (isinstance(initForm, LSymbol) and (initForm.startswith(':'))):
            initForm = LispInterpreter._lEval( env, originalInitForm )   # evaluate the spec default
            svarVal = list()   # Nil, False
         else:
            argNum += 1
            svarVal = env.lookupGlobal('T')   # T, True

         # Bind the parameters
         env.bindLocal( varName.strval, initForm )
         
         if svarName:
            env.bindLocal( svarName.strval, svarVal )
      
      return paramNum, argNum

   @staticmethod
   def _lbindRestArgs( env:  Environment, lambdaListAST: list[Any], paramNum: int, argList: Sequence[Any], argNum: int ) -> (int, int):
      '''Syntax:  &rest var'''
      try:
         paramName = lambdaListAST[paramNum]
      except IndexError:
         raise LispArgBindingError( f'Param name expected after &rest.' )

      if not isinstance(paramName, LSymbol ) or paramName.startswith('&'):
         raise LispArgBindingError( 'Symbol expected after &rest.' )
      
      theRestArgs = argList[argNum:]
      env.bindLocal( paramName.strval, list(theRestArgs) )
      
      return paramNum + 1, argNum

   @staticmethod
   def _lbindKeyArgs( env: Environment, lambdaListAST: list[Any], paramNum: int, argList: Sequence[Any], argNum: int ) -> (int, int):
      '''syntax:  &key {var | ( {var | ( keyword var )} [initForm [svar]])}* [&allow-other-keys]'''
      paramListLength = len(lambdaListAST)
      argListLength = len(argList)

      keysDict = {}         # keyStr -> (varName, svarName, initForm)
      keyParamOrder = []    # ordered list of keyStr for incremental evaluation

      # ---- Phase 1: Parse key parameter specs (store initForms, don't evaluate yet) ----
      while paramNum < paramListLength:
         paramSpec = lambdaListAST[paramNum]
         if isinstance(paramSpec, LSymbol):
            if paramSpec.startswith('&'):
               break
            keyName = paramSpec
            varName = paramSpec
            initForm = list()
            svarName = None
         elif isinstance(paramSpec, list):
            if len(paramSpec) == 0:
               raise LispArgBindingError( f'Empty parameter spec () in &KEY lambda list.' )
            keyVarSpec, *initFormSpec = paramSpec

            # Extract the keyName and varName from keyVarSpec
            if isinstance(keyVarSpec, LSymbol):
               if keyVarSpec.startswith('&'):
                  raise LispArgBindingError( f'Lambda list keyword {keyVarSpec} cannot be used as a variable name in &KEY spec.' )
               keyName = keyVarSpec
               varName = keyVarSpec
            elif isinstance(keyVarSpec, list):
               try:
                  keyName, varName = keyVarSpec
               except ValueError:
                  raise LispArgBindingError( f'Key Var pair following &KEY must contain exactly two elements.' )
               if not isinstance(keyName, LSymbol) or not keyName.startswith(':'):
                  raise LispArgBindingError( f'The key in a &KEY (keyword var) pair must be a keyword symbol (e.g. :mykey).' )
               if not isinstance(varName, LSymbol):
                  raise LispArgBindingError( f'Variable in &KEY (keyword var) pair must be a symbol.' )
            else:
               raise LispArgBindingError( f'&KEY key/var spec must be either a symbol or a list (:keySymbol varSymbol).' )

            # Extract initForm and svarName from initFormSpec
            initFormSpecLen = len(initFormSpec)
            if initFormSpecLen == 0:
               initForm = list()
               svarName = None
            elif initFormSpecLen == 1:
               initForm = initFormSpec[0]
               svarName = None
            elif initFormSpecLen == 2:
               initForm, svarName = initFormSpec
               if svarName and not isinstance(svarName, LSymbol):
                  raise LispArgBindingError( f'svar for &KEY parameter {varName} must be a symbol.' )
               if isinstance(svarName, LSymbol) and svarName.startswith('&'):
                  raise LispArgBindingError( f'Lambda list keyword {svarName} cannot be used as a supplied-p variable in &KEY spec.' )
            else:
               raise LispArgBindingError( f'Too many arguments specified in a parameter keyword initialization list.' )
         else:
            raise LispArgBindingError( f'Parameter spec following &KEY must be a symbol or a list.' )

         keyStr = keyName.strval[1:] if keyName.startswith(':') else keyName.strval
         keysDict[keyStr] = (varName, svarName, initForm)
         keyParamOrder.append( keyStr )
         paramNum += 1

      # ---- Phase 2: Handle &allow-other-keys in lambda list ----
      allowOtherKeys = False
      if (paramNum < paramListLength):
         nextParam = lambdaListAST[paramNum]
         if isinstance(nextParam, LSymbol) and (nextParam == '&ALLOW-OTHER-KEYS'):
            allowOtherKeys = True
            paramNum += 1

      # ---- Phase 3: Scan supplied keyword arguments ----
      # Pre-scan for :allow-other-keys in args (CL 3.4.1.4.1)
      scanIdx = argNum
      while scanIdx + 1 < argListLength:
         scanArg = argList[scanIdx]
         if isinstance(scanArg, LSymbol) and scanArg.strval == ':ALLOW-OTHER-KEYS':
            if argList[scanIdx + 1] != list():   # non-NIL value enables it
               allowOtherKeys = True
            break
         scanIdx += 2

      # Collect supplied keyword arguments (first occurrence wins per CL 3.4.1.4.1)
      suppliedArgs = {}    # keyStr -> argVal
      while argNum < argListLength:
         keyArg = argList[argNum]
         if (not isinstance(keyArg, LSymbol)) or (not keyArg.startswith(':')):
            raise LispArgBindingError( f'Keyword expected, found {keyArg}.' )
         keyArgStr = keyArg.strval[1:]  # Strip the leading colon
         argNum += 1
         
         try:
            argVal = argList[argNum]
         except IndexError:
            raise LispArgBindingError( f'Keyword {keyArgStr} expected to be followed by a value.' )
         argNum += 1

         # :allow-other-keys is always accepted; skip binding if not a declared key param
         if keyArgStr == 'ALLOW-OTHER-KEYS' and keyArgStr not in keysDict:
            continue

         if (not allowOtherKeys) and (keyArgStr not in keysDict):
            raise LispArgBindingError( f'Unexpected keyword found {keyArgStr}.' )

         # First occurrence wins; skip unknown keys when allowOtherKeys
         if keyArgStr in keysDict and keyArgStr not in suppliedArgs:
            suppliedArgs[keyArgStr] = argVal

      # ---- Phase 4: Bind key parameters incrementally (CL 3.4.1 eval order) ----
      nilVal = list()
      tVal = env.lookupGlobal('T')
      for keyStr in keyParamOrder:
         varName, svarName, initForm = keysDict[keyStr]
         if keyStr in suppliedArgs:
            env.bindLocal( varName.strval, suppliedArgs[keyStr] )
            if svarName:
               env.bindLocal( svarName.strval, tVal )
         else:
            env.bindLocal( varName.strval, LispInterpreter._lEval( env, initForm ) )
            if svarName:
               env.bindLocal( svarName.strval, nilVal )

      return paramNum, argNum

   @staticmethod
   def _lbindAuxArgs( env: Environment, lambdaListAST: list[Any], paramNum: int, argList: Sequence[Any], argNum: int ) -> (int, int):
      '''Syntax:  &aux {var | (var [initForm])}*
      These are not really arguments.  At least: these parameters get no corresponding arguments.
      These parameters are strictly local variables for the function.'''
      paramListLength = len(lambdaListAST)
      
      while (paramNum < paramListLength):
         paramSpec = lambdaListAST[paramNum]

         # Extract the next parameter's name and value
         if isinstance( paramSpec, LSymbol ):
            if paramSpec.startswith('&'):
               raise LispArgBindingError( f'{paramSpec} occurs after &AUX.' )
            varName = paramSpec
            initForm = list( )
         elif isinstance(paramSpec, list):
            paramSpecLen = len(paramSpec)
            if paramSpecLen == 1:
               varName = paramSpec[0]
               initForm = list()
            elif paramSpecLen == 2:
               varName, initForm = paramSpec
            else:
               raise LispArgBindingError( 'Parameter spec following &AUX must be a list of (<variable> [<defaultvalue>]).' )

            if not isinstance(varName, LSymbol) or varName.startswith('&'):
               raise LispArgBindingError( 'Parameter spec following &AUX must be a list of (<variable> [<defaultvalue>]).' )

            initForm = LispInterpreter._lEval( env, initForm )
         else:
            raise LispArgBindingError( 'Parameter spec following &AUX must be a <variable> or a list of (<variable> [<defaultvalue>]).' )
         
         # Bind the parameters
         env.bindLocal( varName.strval, initForm )
         
         # Prepare for the next iteration
         paramNum += 1
      
      return paramNum, argNum

   @staticmethod
   def _lbackquoteExpand( env: Environment, expr: Any ) -> Any:
      '''Expand a backquote List expression.'''
      if isinstance(expr, list):
         if len(expr) == 0:
            return expr

         primary = expr[0]
         if ( (primary == 'COMMA') or (primary == 'COMMA-AT') ):
            result = LispInterpreter._lEval(env, expr)
            return result

         resultList: list[Any] = [ ]
         for listElt in expr:
            resultListElt = LispInterpreter._lbackquoteExpand( env, listElt )
            if ( isinstance(resultListElt, list) and
                 (len(resultListElt) > 0) and
                 (resultListElt[0] == LSymbol('COMMA-AT')) ):
               for elt in resultListElt[1]:
                  resultList.append( elt )
            else:
               resultList.append( resultListElt )
         return resultList
      else:
         return expr

   @staticmethod
   def _lconstructPrimitives( parseLispString: Callable[[str], Any] ) -> dict[str, Any]:
      primitiveDict: dict[str, Any] = { }
      INSIDE_BACKQUOTE = False
      
      # ###################################
      # Lisp Object & Primitive Definitions
      # ###################################
      primitiveDict[ 'T'    ] = L_T
      primitiveDict[ 'NIL'  ] = L_NIL
      primitiveDict[ 'PI'   ] = math.pi
      primitiveDict[ 'E'    ] = math.e

      class primitive( object ):
         '''Decorator to simplify the definition of a lisp primitive as a python function.'''
         def __init__( self, primitiveSymbolString: str, paramsString: str='', specialForm: bool=False ) -> None:
            '''Arguments:
            primitiveSymbol, the string representation of the lisp symbol used
               to name the primitive in the interpreter.
            params, a documentation string listing the arguments taken by this
               primitive.
            specialForm, Defaults to False.  A true value indicates to the
               interpreter that that this primitive wants its arguments
               unevaluated prior to its call. (Otherwise all arguments are
               evaluated prior to calling this function.)
            '''
            self._name:str  = primitiveSymbolString.upper( )
            self._paramsString:str = paramsString
            self._specialForm:bool = specialForm

         def __call__( self, primitiveDef ):
            '''primitiveDef is a python function to implement the lisp primitive.'''
            nonlocal primitiveDict
            docString = primitiveDef.__doc__ if primitiveDef.__doc__ != None else ''
            lPrimitivObj = LPrimitive( primitiveDef, self._name,
                                       self._paramsString, docString, 
                                       specialForm=self._specialForm )
            primitiveDict[ self._name ] = lPrimitivObj
            return lPrimitivObj

      # =================
      # Symbol Definition
      # -----------------
      @primitive( 'defmacro', '<symbol> <lambda-list> &optional <body>', specialForm=True )
      def LP_defmacro( env: Environment, *args ) -> Any:
         """Defines and returns a new globally named macro.  The first expr of the body
can be an optional documentation string."""
         try:
            fnName, funcParams, *funcBody = args
         except ValueError:
            raise LispRuntimeFuncError( LP_defmacro, "3 or more arguments expected." )
   
         if not isinstance(fnName, LSymbol):
            raise LispRuntimeFuncError( LP_defmacro, "Argument 1 expected to be a symbol." )
   
         if not isinstance(funcParams, list):
            raise LispRuntimeFuncError( LP_defmacro, "Argument 2 expected to be a list of params." )
   
         if len(funcBody) < 1:
            raise LispRuntimeFuncError( LP_defmacro, "At least one body expression expected." )

         if isinstance(funcBody[0], str):
            docString = funcBody[0]
            funcBody = funcBody[1:]
         else:
            docString = ''

         if len(funcBody) < 1:
            raise LispRuntimeFuncError( LP_defmacro, "At least one body expression expected after docstring." )
      
         theFunc = LMacro( fnName, funcParams, docString, funcBody )
         return env.bindGlobal( fnName.strval, theFunc )
   
      @primitive( 'macroexpand', '\'(<macroName> <arg1> <arg2> ...)' )
      def LP_macroexpand( env: Environment, *args ) -> Any:
         """Performs the expansion of a macro and returns the results of those expansions in a list."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_macroexpand, 'Exactly 1 argument expected.' )
         theMacroCall = args[0]

         if not isinstance(theMacroCall, list):
            raise LispRuntimeFuncError( LP_macroexpand, 'Argument 1 expected to be a list.' )

         if len(theMacroCall) < 1:
            raise LispRuntimeFuncError( LP_macroexpand, 'Macro call must be at least one element in length.' )

         # Break the list contents into a function and a list of args
         primary, *exprArgs = theMacroCall

         # fn is an LPrimitive, LFunction or a macro name symbol
         # Use this information to get the function definition
         macroDef = LispInterpreter._lEval( env, primary )
         if not isinstance( macroDef, LMacro ):
            raise LispRuntimeFuncError( LP_macroexpand, 'Badly formed list expression.  The first element should evaluate to a macro.' )

         from pythonslisp.LispExpander import LispExpander
         return [ LispExpander._expandMacroCall( env, macroDef, exprArgs ) ]
   
      @primitive( 'defsetf-internal', '<accessor-symbol> <field-symbol>' )
      def LP_defsetf_internal( env: Environment, *args ) -> Any:
         """Register a struct field accessor as a valid setf target."""
         if len(args) != 2:
            raise LispRuntimeFuncError( LP_defsetf_internal, '2 arguments expected.' )
         accessor_sym, field_sym = args
         if not isinstance(accessor_sym, LSymbol) or not isinstance(field_sym, LSymbol):
            raise LispRuntimeFuncError( LP_defsetf_internal, 'Both arguments must be symbols.' )
         LispInterpreter._setf_registry[accessor_sym.strval] = field_sym.strval
         return accessor_sym

      @primitive( 'setf', '<symbol> <sexpr>', specialForm=True )
      def LP_setf( env: Environment, *args ) -> Any:
         """Updates a variable's value, returns value.  The search for the variable begins
locally and proceeds to search ever less local scopes until the global scope
is searched.  If the variable is located in this search its value is updated.
If it's not located a new global is defined and set the value.

Alternate usage: (setf (at <keyOrIndex> <mapOrList>) <newValue>)"""         
         numArgs = len(args)
         
         if numArgs == 0:
            raise LispRuntimeFuncError( LP_setf, 'At least 2 arguments expected.' )

         if (numArgs % 2) != 0:
            raise LispRuntimeFuncError( LP_setf, f'An even number of arguments is expected.  Received {numArgs}.' )
         
         rval = list()
         while len(args) > 0:
            lval,rval,*args = args
   
            rval = LispInterpreter._lEval(env, rval)
            
            # Case where the lvalue is a symbol form:  (setf variable newValue)
            if isinstance(lval, LSymbol ):
               sym = lval
               if isinstance(rval,(LFunction,LMacro)) and (rval.name == ''):
                  rval.name = sym.strval
               sym = sym.strval
      
               # If sym exists somewhere in the symbol table hierarchy, set its
               # value to rval.  If it doesn't exist, define it in the global
               # symbol table and set its value to rval.
               theSymTab = env.findDef( sym )
               if theSymTab:
                  theSymTab.bindLocal( sym, rval )
               else:
                  env.bindGlobal( sym, rval )
         
            # Case where the lvalue is an 'at' form:  (setf (at key collection) newValue)
            elif isinstance(lval, list):       # s-expression
               if len(lval) == 0:
                  raise LispRuntimeFuncError( LP_setf, 'lvalue cannot be NIL or ().' )
               
               primitive = lval[0]
               if primitive == 'AT':
                  try:
                     primitive, keyOrIndex, mapOrLst = lval
                  except ValueError:
                     raise LispRuntimeFuncError( LP_setf, 'lvalue \'at\' form expected 3 elements.' )
                  
                  theSelector = LispInterpreter._lEval(env,keyOrIndex)
                  theContainer = LispInterpreter._lEval(env,mapOrLst)
                  
                  if not isinstance(theContainer, (list, dict)):
                     raise LispRuntimeFuncError( LP_setf, 'Invalid container type following \'AT\' primitive.  Expected list or map.' )
   
                  try:
                     theContainer[theSelector] = rval
                  except (KeyError, IndexError, TypeError):
                     raise LispRuntimeFuncError( LP_setf, f'Invalid key or index supplied to \'AT\' form.  Received {theSelector}.' )
               elif isinstance(primitive, LSymbol) and primitive.strval in LispInterpreter._setf_registry:
                  field_key = LispInterpreter._setf_registry[primitive.strval]
                  if len(lval) != 2:
                     raise LispRuntimeFuncError( LP_setf,
                        f'Struct accessor setf expects exactly 1 instance argument, got {len(lval)-1}.' )
                  instance = LispInterpreter._lEval(env, lval[1])
                  if not isinstance(instance, dict):
                     raise LispRuntimeFuncError( LP_setf,
                        f'Expected a struct instance as argument to ({primitive.strval} ...).' )
                  instance[field_key] = rval
               else:
                  raise LispRuntimeFuncError( LP_setf, 'Unrecognized setf place.' )

         return rval

      @primitive( 'undef!', '<symbol>', specialForm=True )
      def LP_undef( env: Environment, *args ) -> Any:
         """Undefines the global definition for a symbol and returns nil."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_undef, '1 argument expected.' )
         key = args[0]
         if not isinstance(key, LSymbol):
            raise LispRuntimeFuncError( LP_undef, 'Argument expected to be a symbol.' )
         env.getGlobalEnv().unbind( key.strval )
         return L_NIL
   
      @primitive( 'symtab!', '' )
      def LP_symtab( env: Environment, *args ) -> Any:
         """Prints the entire environment stack and returns nil.  Each scope is printed
in a separate list and begins on a new line.  The local scope is first; global
is last."""
         if len(args) > 0:
            raise LispRuntimeFuncError( LP_symtab, '0 arguments expected.' )
   
         print( 'Symbol Table Dump:  Inner-Most Scope First')
         print( '------------------------------------------')
         scope: (Environment | None) = env
         while scope:
            symList = scope.localSymbols()
            print( symList )
            scope = scope.parentEnv( )
   
         return L_NIL
   
      # ==================
      # Control Structures
      # ------------------
      @primitive( 'lambda', '<lambda-list> &optional <body>', specialForm=True )
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
   
      @primitive( 'let', '( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) &optional <body>', specialForm=True )
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
               varName = varSpec
               initForm = list()
            elif isinstance(varSpec, list):
               varSpecLen = len(varSpec)
               if varSpecLen == 1:
                  varName = varSpec[0]
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
         env = Environment( env, initialBindings=initDict )     # Open a new scope. Auto closes when env goes out of scope.
   
         # Evaluate each body sexpr in the new env/scope
         lastResult = L_NIL
         for sexpr in body:
            lastResult = LispInterpreter._lEval( env, sexpr )
         return lastResult
   
      @primitive( 'let*', '( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) &optional <body>', specialForm=True )
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
         env = Environment( env )    #  Open a new scope. Auto closes when env goes out of scope.
   
         for varSpec in vardefs:
            if isinstance(varSpec, LSymbol):
               varName = varSpec
               initForm = list()
            elif isinstance(varSpec, list):
               varSpecLen = len(varSpec)
               if varSpecLen == 1:
                  varName = varSpec[0]
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
   
      @primitive( 'if', '<cond> <conseq> &optional (<alt> nil)', specialForm=True )
      def LP_if( env: Environment, *args ) -> Any:
         """Evaluates the condition.  If truthy (non-nil) then consequence is
evaluated and its result returned, otherwise alt is evaluated and its result
is returned."""         
         numArgs = len(args)
         if not(2 <= numArgs <= 3):
            raise LispRuntimeFuncError( LP_if, '2 or 3 arguments expected.' )

         condValue = LispInterpreter._lEval( env, args[0] )
         if LispInterpreter._lTrue(condValue):
            return LispInterpreter._lEval( env, args[1])    # The THEN part
         elif numArgs == 3:
            return LispInterpreter._lEval( env, args[2])    # The ELSE part
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
   
            # If the case condition is true evaluate the body of the case
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
         """Similar to quote but allows command comma-at expressions within expr."""
         nonlocal INSIDE_BACKQUOTE
         if INSIDE_BACKQUOTE:
            raise LispRuntimeFuncError( LP_backquote, 'Cannot nest backquotes.')
   
         if (len(args) != 1):
            raise LispRuntimeFuncError( LP_backquote, '1 argument expected.' )
         sExpr = args[0]
   
         try:
            INSIDE_BACKQUOTE = True
            expandedForm = LispInterpreter._lbackquoteExpand( env, sExpr )
         finally:
            INSIDE_BACKQUOTE = False
   
         return expandedForm
   
      @primitive( 'comma', '<sexpr>', specialForm=True )
      def LP_comma( env: Environment, *args ) -> Any:
         """Must occur within a backquote expr or it's an error.  Evaluates expr
(even if within a quoted expr) and returns it to the enclosing expr."""         
         nonlocal INSIDE_BACKQUOTE
         if not INSIDE_BACKQUOTE:
            raise LispRuntimeFuncError( LP_comma, 'COMMA can only occur inside a BACKQUOTE.')
   
         if (len(args) != 1):
            raise LispRuntimeFuncError( LP_comma, '1 argument expected.' )
         subordinateExpr = args[0]
         result = LispInterpreter._lEval( env, subordinateExpr )
         return result
   
      @primitive( 'comma-at', '<sexpr>', specialForm=True )
      def LP_comma_at( env: Environment, *args ) -> Any:
         """Must occur within a backquote expr or it's an error.  Evaluates expr.
Result must be a list.  Inserts the elements of the resulting list into the
enclosing list (eliminating a level of parentheses)."""         
         nonlocal INSIDE_BACKQUOTE
         if not INSIDE_BACKQUOTE:
            raise LispRuntimeFuncError( LP_comma_at, 'COMMA-AT can only occur inside a BACKQUOTE.')
   
         if (len(args) != 1):
            raise LispRuntimeFuncError( LP_comma_at, '1 argument expected.' )
         subordinateExpr = args[0]
         result = LispInterpreter._lEval( env, subordinateExpr )
         if not isinstance(result, list):
            raise LispRuntimeFuncError( LP_comma_at, 'Argument 1 must evaluate to a List.' )
         retValue = [ LSymbol('COMMA-AT'), result ]
         return retValue
   
      @primitive( 'funcall', '<fnNameSymbol> <arg1> <arg2> ...' )
      def LP_funcall( env: Environment, *args ) -> Any:
         """Calls a function with the args listed.
         Handled by the CEK machine (_cek_apply); this body is never reached."""
         raise LispRuntimeError( 'Internal error: FUNCALL must be handled by the CEK machine.' )
   
      @primitive( 'eval', '<sexpr>' )
      def LP_eval( env: Environment, *args ) -> Any:
         """Evaluates expr in the current scope."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_eval, '1 argument expected.' )
         expr = args[0]
         return LispInterpreter._lEval( env, expr )
   
      @primitive( 'apply', '<function> &rest <args> <argsList>' )
      def LP_apply( env: Environment, *args ) -> Any:
         """Applies a function to a list of args.
         Handled by the CEK machine (_cek_apply); this body is never reached."""
         raise LispRuntimeError( 'Internal error: APPLY must be handled by the CEK machine.' )

      @primitive( 'call/cc', '<callable>' )
      def LP_callcc( env: Environment, *args ) -> Any:
         """Calls callable with the current escape continuation as its sole argument.
         The continuation, when invoked, immediately returns its argument as the
         value of the enclosing call/cc expression, bypassing any intervening
         computation.  The continuation becomes inactive once the callable returns
         normally without having invoked it.
         Handled by the CEK machine (_cek_apply); this body is never reached."""
         raise LispRuntimeError( 'Internal error: CALL/CC must be handled by the CEK machine.' )
      
   
      @primitive( 'parse', '<string>' )
      def LP_parse( env: Environment, *args ) -> Any:
         """Parses the string as a Lisp sexpression and returns the resulting expression tree."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_parse, '1 string argument expected.' )
         theExprStr = args[0]
         if not isinstance(theExprStr, str):
            raise LispRuntimeFuncError( LP_parse, 'Argument expected to be a string.' )
         theExprAST = parseLispString( theExprStr )
         return theExprAST
   
      @primitive( 'python', '<string>' )
      def LP_python( env: Environment, *args ) -> Any:
         """Executes some python code from Lisp."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_python, '1 string argument expected by python.' )
         thePythonCode = args[0]
         if not isinstance(thePythonCode, str):
            raise LispRuntimeFuncError( LP_python, 'Argument expected to be a string.' )
         theReturnVal = eval( thePythonCode, globals(), locals() )
         return theReturnVal
   
      # =======================
      # List & Map Manipulation
      # -----------------------
      @primitive( 'map', '(<key1> <val1>) (<key2> <val2>) ...', specialForm=True )
      def LP_map( env: Environment, *args ) -> Any:
         """Constructs and returns a map of key-value pairs."""
         theMapping = dict()
         requiredKeyType = None
         for entryNum,key_expr_pair in enumerate(args):
            try:
               key,expr = key_expr_pair
            except (ValueError, TypeError):
               raise LispRuntimeFuncError( LP_map, f'Entry {entryNum + 1} does not contain a (key value) pair.' )

            if isinstance( key,  LSymbol ):
               key = key.strval

            if isinstance( key, (int,float,str) ):
               if requiredKeyType is None:
                  requiredKeyType = type(key)
               elif type(key) != requiredKeyType:
                  raise LispRuntimeFuncError( LP_map,
                     f'All keys in a map must be the same type. '
                     f'Entry {entryNum + 1} is {type(key).__name__}'
                     f', expected {requiredKeyType.__name__}.' )
               theMapping[ key ] = LispInterpreter._lEval( env, expr )
            else:
               raise LispRuntimeFuncError( LP_map, f'Entry {entryNum+1} has an invalid <key> type.' )
         return theMapping
   
      @primitive( 'car', '<list>' )
      def LP_car( env: Environment, *args ) -> Any:
         """Returns the first item in a list."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_car, '1 argument expected.' )
         theList = args[0]
   
         if not isinstance(theList, list):
            raise LispRuntimeFuncError( LP_car, '1st argument expected to be a list.' )
   
         try:
            return theList[0]
         except IndexError:
            return L_NIL

      @primitive( 'cdr', '<list>' )
      def LP_cdr( env: Environment, *args ) -> Any:
         """Returns a copy of the list minus the first element."""
         numArgs = len(args)
         if numArgs != 1:
            raise LispRuntimeFuncError( LP_cdr, '1 argument expected.' )
         theList = args[0]
   
         if not isinstance(theList, list):
            raise LispRuntimeFuncError( LP_cdr, '1st argument expected to be a list.' )

         return theList[1:]
   
      @primitive( 'cons', '<obj> <list>' )
      def LP_cons( env: Environment, *args ) -> Any:
         """Returns a copy of list with obj inserted into the front of the copy."""
         try:
            obj,consList = args
         except ValueError:
            raise LispRuntimeFuncError( LP_cons, '2 arguments expected.' )

         if not isinstance(consList, list):
            raise LispRuntimeFuncError( LP_cons, '2nd argument expected to be a list.' )

         consList = [ obj, *consList ]
         return consList
   
      @primitive( 'push!', '<list> <value>' )
      def LP_push( env: Environment, *args ) -> Any:
         """Pushes a value onto the back of a list."""
         try:
            alist, value = args
         except ValueError:
            raise LispRuntimeFuncError( LP_push, '2 arguments expected.' )
   
         if not isinstance(alist, list):
            raise LispRuntimeFuncError( LP_push, '1st argument expected to be a list.' )
         alist.append( value )
         return alist
   
      @primitive( 'pop!', '<list>' )
      def LP_pop( env: Environment, *args ) -> Any:
         """Pops and returns the last value of a list."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_pop, '1 argument expected.' )
         alist = args[0]

         if not isinstance(alist, list):
            raise LispRuntimeFuncError( LP_pop, '1st argument expected to be a list.' )

         try:
            value = alist.pop()
         except IndexError:
            raise LispRuntimeFuncError( LP_pop, 'Invalid argument.' )
         return value
   
      @primitive( 'at', '<keyOrIndex> <mapListOrStr>' )
      def LP_at( env: Environment, *args ) -> Any:
         """Returns the value at a specified index of a list or string,
         or specified key of a map."""
         try:
            key,keyed = args
         except ValueError:
            raise LispRuntimeFuncError( LP_at, '2 arguments expected.' )
   
         if not isinstance(keyed, (list, dict, str) ):
            raise LispRuntimeFuncError( LP_at, 'Invalid argument.  List, Map, or String expected.' )
   
         if isinstance( key, LSymbol ):
            key = key.strval
   
         try:
            return keyed[ key ]
         except ( KeyError, IndexError, TypeError ):
            raise LispRuntimeFuncError( LP_at, 'Invalid argument key/index.' )
   
      @primitive( 'at-delete', '<keyOrIndex> <mapOrList>' )
      def LP_atDelete( env: Environment, *args ) -> bool:
         """Deletes the key-value pair from a map or list specified by keyOrIndex."""
         try:
            key, keyed = args
         except ValueError:
            raise LispRuntimeFuncError( LP_atDelete, "Exactly 2 arguments expected." )
         
         if not isinstance( keyed, (list, dict) ):
            raise LispRuntimeFuncError( LP_atDelete, "Argument 2 expected to be a list or map." )
   
         try:
            del keyed[key]
         except ( IndexError, KeyError, TypeError ):
            raise LispRuntimeFuncError( LP_atDelete, "Bad index or key into collection." )
         
         return L_T
      
      @primitive( 'at-insert', '<index> <list> <newItem>' )
      def LP_atInsert( env: Environment, *args ) -> bool:
         """Inserts newItem into list at the position specified by index.  Returns newItem."""
         try:
            index, lst, newItem = args
         except ValueError:
            raise LispRuntimeFuncError( LP_atInsert, "Exactly 3 arguments expected." )
         
         if not isinstance(index, int):
            raise LispRuntimeFuncError( LP_atInsert, "Argument 1 expected to be an integer index." )
         
         if not isinstance( lst, list ):
            raise LispRuntimeFuncError( LP_atInsert, "Argument 2 expected to be a list." )
         
         lst.insert( index, newItem )
         return newItem
      
      @primitive( 'append', '<list1> <list2> ...' )
      def LP_append( env: Environment, *args ) -> Any:
         """Returns a new list with the contents of the argument lists merged.  Order is retained."""
         if len(args) < 2:
            raise LispRuntimeFuncError( LP_append, 'At least 2 arguments expected.' )
   
         resultList = list( )
         for lst in args:
            if not isinstance( lst,  list ):
               raise LispRuntimeFuncError( LP_append, 'Invalid argument.' )
            for item in lst:
               resultList.append( item )
         return resultList
   
      @primitive( 'hasValue?', '<listOrMap> <value>' )
      def LP_hasValue( env: Environment, *args ) -> Any:
         """Returns t if the list/map contains value otherwise nil."""
         try:
            keyed,aVal = args
         except ValueError:
            raise LispRuntimeFuncError( LP_hasValue, '2 arguments expected.' )
   
         if isinstance(keyed, list):
            pass
         elif isinstance(keyed, dict):
            keyed = keyed.values()
         else:
            raise LispRuntimeFuncError( LP_hasValue, 'Invalid argument.  Argument 1 expected to be a list or map.')
   
         return L_T if aVal in keyed else L_NIL    # T or NIL
   
      @primitive( 'update!', '<map1> <map2>' )
      def LP_update( env: Environment, *args ) -> Any:
         """Updates map1's data with map2's."""
         try:
            map1,map2 = args
         except ValueError:
            raise LispRuntimeFuncError( LP_update, '2 arguments expected.' )
   
         if not isinstance( map1, dict ):
            raise LispRuntimeFuncError( LP_update, 'Argument 1 expected to be a map.' )
   
         if not isinstance( map2, dict ):
            raise LispRuntimeFuncError( LP_update, 'Argument 2 expected to be a map.' )
   
         map1.update( map2 )
         return map1
   
      @primitive( 'hasKey?', '<map> <key>' )
      def LP_hasKey( env: Environment, *args ) -> Any:
         """Returns t if the key is in the map otherwise nil."""
         try:
            aMap,aKey = args
         except ValueError:
            raise LispRuntimeFuncError( LP_hasKey, '2 arguments expected.' )
   
         if not isinstance(aMap, dict):
            raise LispRuntimeFuncError( LP_hasKey, 'Invalid argument 1.  Map expected.')
   
         if isinstance( aKey, LSymbol ):
            aKey = aKey.strval
   
         return L_T if aKey in aMap else L_NIL   # T or NIL
   
      @primitive( 'sorted', '<list>' )
      def LP_sorted( env: Environment, *args ) -> Any:
         """Returns a copy of the list sorted."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_sorted, "Exactly 1 argument expected." )
         
         theList = args[0]
         if not isinstance(theList, list):
            raise LispRuntimeFuncError( LP_sorted, "Argument 1 expected to be a list." )
         
         try:
            return sorted( theList )
         except TypeError:
            raise LispRuntimeFuncError( LP_sorted, 'Cannot sort a list with incomparable types.' )
      
      # =====================
      # Arithmetic Operations
      # ---------------------
      @primitive( '+', '<number1> <number2> ...' )
      def LP_add( env: Environment, *args ) -> Any:
         """Returns the sum of numbers."""
         try:
            return sum(args)
         except TypeError:
            raise LispRuntimeFuncError( LP_add, 'Invalid argument.' )
   
      @primitive( '-', '<number1> <number2> ...' )
      def LP_sub( env: Environment, *args ) -> Any:
         """Returns the difference of numbers."""
         try:
            if len(args) == 1:
               arg = args[0]
               if not isinstance( arg, LNUMBER ):
                  raise TypeError( )
               return -1 * arg
            else:
               return functools.reduce( lambda x,y: x - y, args )
         except TypeError:
            raise LispRuntimeFuncError( LP_sub, 'Invalid argument.' )
   
      @primitive( '*', '<number1> <number2> ...' )
      def LP_mul( env: Environment, *args ) -> Any:
         """Returns the product of numbers."""
         try:
            return functools.reduce( lambda x,y: x * y, iter(args) )
         except TypeError:
            raise LispRuntimeFuncError( LP_mul, 'Invalid argument.' )
   
      @primitive( '/', '<number1> <number2> ...' )
      def LP_div( env: Environment, *args ) -> Any:
         """Returns the quotient of numbers."""
         try:
            return functools.reduce( lambda x,y: x / y, iter(args) )
         except TypeError:
            raise LispRuntimeFuncError( LP_div, 'Invalid argument.' )
         except ZeroDivisionError:
            raise LispRuntimeFuncError( LP_div, 'division by zero' )
   
      @primitive( '//', '<number1> <number2>' )
      def LP_intdiv( env: Environment, *args ) -> Any:
         """Return the integer division of two numbers."""
         if len(args) != 2:
            raise LispRuntimeFuncError( LP_intdiv, '2 arguments expected.' )
         try:
            return args[0] // args[1]
         except TypeError:
            raise LispRuntimeFuncError( LP_intdiv, 'Invalid argument.' )
         except ZeroDivisionError:
            raise LispRuntimeFuncError( LP_intdiv, 'division by zero' )

      @primitive( 'mod', '<number1> <number2>' )
      def LP_moddiv( env: Environment, *args ) -> Any:
         """Returns the integer remainder of division of two numbers."""
         if len(args) != 2:
            raise LispRuntimeFuncError( LP_moddiv, '2 arguments expected.' )
         try:
            return args[0] % args[1]
         except TypeError:
            raise LispRuntimeFuncError( LP_moddiv, 'Invalid argument.' )
         except ZeroDivisionError:
            raise LispRuntimeFuncError( LP_moddiv, 'division by zero' )
   
      @primitive( 'gcd', '<integer1> <integer2> ...' )
      def LP_gcd( env: Environment,  *args ) -> Any:
         """Returns the greatest common divisor of some integers."""
         try:
            return math.gcd( *args )
         except TypeError:
            raise LispRuntimeFuncError( LP_gcd, 'Invalid argument.' )
   
      @primitive( 'lcm', '<integer1> <integer2> ...' )
      def LP_lcm( env: Environment,  *args ) -> Any:
         """Returns the least common multiple of some integers."""
         try:
            return math.lcm( *args )
         except TypeError:
            raise LispRuntimeFuncError( LP_lcm, 'Invalid argument.' )
   
      @primitive( 'log', '<number> &optional ( <base> e )' )
      def LP_log( env: Environment, *args ) -> Any:
         """Returns the logarithm of a number.  With one argument, returns the natural
logarithm (base e).  An optional second argument specifies the base."""
         numArgs = len(args)
         if not( 1 <= numArgs <= 2 ):
            raise LispRuntimeFuncError( LP_log, '1 or 2 arguments expected.' )
   
         try:
            num,*rest = args
            base = math.e if len(rest) == 0 else rest[0]
            return math.log(num,base)
         except (ValueError, TypeError):
            raise LispRuntimeFuncError( LP_log, 'Invalid argument.' )
   
      @primitive( 'expt', '<base> <power>' )
      def LP_expt( env: Environment, *args ) -> Any:
         """Returns base raised to a power."""
         try:
            base,power = args
            result = base ** power
         except ValueError:
            raise LispRuntimeFuncError( LP_expt, 'Exactly two arguments expected.' )
         except TypeError:
            raise LispRuntimeFuncError( LP_expt, 'Invalid argument type.  Arguments expected to be numbers.' )
   
         return result.real if isinstance(result, complex) else result
   
      @primitive( 'sin', '<radians>' )
      def LP_sin( env: Environment, *args ) -> Any:
         """Returns the sine of radians."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_sin, '1 argument expected.' )
         try:
            return math.sin(args[0])
         except (TypeError, ValueError):
            raise LispRuntimeFuncError( LP_sin, 'Invalid argument.' )

      @primitive( 'cos', '<radians>' )
      def LP_cos( env: Environment, *args ) -> Any:
         """Returns the cosine of radians."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_cos, '1 argument expected.' )
         try:
            return math.cos(args[0])
         except (TypeError, ValueError):
            raise LispRuntimeFuncError( LP_cos, 'Invalid argument.' )
   
      @primitive( 'asin', '<number>' )
      def LP_asin( env: Environment, *args ) -> Any:
         """Returns the arcsine of a number in radians."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_asin, '1 argument expected.' )
         try:
            return math.asin(args[0])
         except (TypeError, ValueError):
            raise LispRuntimeFuncError( LP_asin, 'Invalid argument.' )

      @primitive( 'acos', '<number>' )
      def LP_acos( env: Environment, *args ) -> Any:
         """Returns the arccosine of a number in radians."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_acos, '1 argument expected.' )
         try:
            return math.acos(args[0])
         except (TypeError, ValueError):
            raise LispRuntimeFuncError( LP_acos, 'Invalid argument.' )
   
      @primitive( 'atan', '<number1> &optional <number2>' )
      def LP_atan( env: Environment, *args ) -> Any:
         """Returns the arctangent of one or two numbers in radians."""
         numArgs = len(args)
         if numArgs == 1:
            try:
               return math.atan( args[0] )
            except (TypeError, ValueError):
               raise LispRuntimeFuncError( LP_atan, 'Invalid argument.' )
         elif numArgs == 2:
            try:
               return math.atan2( args[0], args[1] )
            except (TypeError, ValueError):
               raise LispRuntimeFuncError( LP_atan, 'Invalid argument.' )
         else:
            raise LispRuntimeFuncError( LP_atan, '1 or 2 arguments expected.' )
   
      @primitive( 'min', '<number1> <number2> ...' )
      def LP_min( env: Environment, *args ) -> Any:
         """Returns the smallest of a set of numbers."""
         try:
            return min( *args )
         except (TypeError, ValueError):
            raise LispRuntimeFuncError( LP_min, 'Invalid argument.' )
   
      @primitive( 'max', '<number1> <number2> ...' )
      def LP_max( env: Environment, *args ) -> Any:
         """Returns the largest of a set of numbers."""
         try:
            return max( *args )
         except (TypeError, ValueError):
            raise LispRuntimeFuncError( LP_max, 'Invalid argument.' )
   
      @primitive( 'random', '<integerOrFloat>' )
      def LP_random( env: Environment, *args ) -> Any:
         """Returns a random int or float in the range 0 <= n <= arg.  If the argument
is an int the random number will be an int, if the argument is a float the
random number will be a float."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_random, 'Exactly 1 number argument expected.' )
         num = args[0]
   
         if isinstance( num,  int ):
            if num < 0:
               raise LispRuntimeFuncError( LP_random, 'Argument expected to be non-negative.' )
            return random.randint(0, num)
         elif isinstance( num,  float ):
            return random.uniform(0.0, num)
         else:
            raise LispRuntimeFuncError( LP_random, 'Invalid argument type.' )
   
      # ==========
      # Predicates
      # ----------
      @primitive( 'numberp', '<sexpr>' )
      def LP_numberp( env: Environment, *args ) -> Any:
         """Returns t if expr is a number otherwise nil."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_numberp, '1 argument expected.' )
         return L_T if isinstance( args[0], LNUMBER ) else L_NIL
   
      @primitive( 'integerp', '<sexpr>' )
      def LP_integerp( env: Environment,  *args ) -> Any:
         """Returns t if expr is an integer otherwise nil."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_integerp, '1 argument expected.' )
         return L_T if isinstance( args[0], int ) else L_NIL
   
      @primitive( 'rationalp', '<sexpr>' )
      def LP_rationalp( env: Environment,  *args ) -> Any:
         """Returns t if expr is an integer or fraction otherwise nil."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_rationalp, '1 argument expected.' )
         return L_T if isinstance( args[0], (int,Fraction) ) else L_NIL
   
      @primitive( 'floatp', '<sexpr>' )
      def LP_floatp( env: Environment,  *args ) -> Any:
         """Returns t if expr is a float otherwise nil."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_floatp, '1 argument expected.' )
         return L_T if isinstance( args[0], float ) else L_NIL
   
      @primitive( 'symbolp', '<sexpr>' )
      def LP_symbolp( env: Environment, *args ) -> Any:
         """Returns t if expr is a symbol otherwise nil."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_symbolp, '1 argument expected.' )
         return L_T if isinstance( args[0], LSymbol ) else L_NIL
   
      @primitive( 'atom', '<sexpr>' )
      def LP_atom( env: Environment, *args ) -> Any:
         """Returns t if expr is an atom (int,float,string,map or nil) otherwise nil."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_atom, '1 argument expected.' )
         arg = args[0]
         if isinstance(arg, list):
            return L_T if len(arg) == 0 else L_NIL         # NIL or () is an atom even through it's a list.
         return L_T
   
      @primitive( 'listp', '<sexpr>' )
      def LP_listp( env: Environment, *args ) -> Any:
         """Returns t if expr is a list otherwise nil."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_listp, '1 argument expected.' )
         return L_T if isinstance(args[0], list) else L_NIL
   
      @primitive( 'mapp', '<sexpr>' )
      def LP_mapp( env: Environment, *args ) -> Any:
         """Returns t if expr is a map otherwise nil."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_mapp, '1 argument expected.' )
         return L_T if isinstance(args[0], dict) else L_NIL
   
      @primitive( 'stringp', '<sexpr>' )
      def LP_stringp( env: Environment, *args ) -> Any:
         """Returns t if expr is a string otherwise nil."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_stringp, '1 argument expected.' )
         return L_T if isinstance( args[0], str ) else L_NIL
   
      @primitive( 'functionp', '<sexpr>' )
      def LP_functionp( env: Environment, *args ) -> Any:
         """Returns t if expr is a function otherwise nil."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_functionp, '1 argument expected.' )
         return L_T if isinstance( args[0], LFunction ) else L_NIL
   
      @primitive( 'macrop', '<sexpr>' )
      def LP_macrop( env: Environment, *args ) -> Any:
         """Returns t if expr is a macro otherwise nil."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_macrop, '1 argument expected.' )
         return L_T if isinstance( args[0], LMacro ) else L_NIL

      @primitive( 'continuationp', '<sexpr>' )
      def LP_continuationp( env: Environment, *args ) -> Any:
         """Returns t if expr is an escape continuation captured by call/cc, otherwise nil."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_continuationp, '1 argument expected.' )
         return L_T if isinstance( args[0], LContinuation ) else L_NIL

      @primitive( 'type-of', '<sexpr>' )
      def LP_typeof( env: Environment, *args ) -> Any:
         """Returns the type of its argument as a symbol (CL type-of conventions)."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_typeof, '1 argument expected.' )
         arg = args[0]
         if isinstance( arg, list ):
            return LSymbol('NULL') if len(arg) == 0 else LSymbol('CONS')
         elif isinstance( arg, int ):
            return LSymbol('INTEGER')
         elif isinstance( arg, float ):
            return LSymbol('FLOAT')
         elif isinstance( arg, Fraction ):
            return LSymbol('RATIO')
         elif isinstance( arg, str ):
            return LSymbol('STRING')
         elif isinstance( arg, LSymbol ):
            return LSymbol('SYMBOL')
         elif isinstance( arg, dict ):
            struct_type = arg.get('STRUCT-TYPE')   # map stores symbol keys as strings
            return struct_type if struct_type is not None else LSymbol('HASH-TABLE')
         elif isinstance( arg, LFunction ):
            return LSymbol('FUNCTION')
         elif isinstance( arg, LMacro ):
            return LSymbol('MACRO')
         elif isinstance( arg, LPrimitive ):
            return LSymbol('PRIMITIVE')
         elif isinstance( arg, LContinuation ):
            return LSymbol('CONTINUATION')
         else:
            return LSymbol('T')

      # ====================
      # Relational Operators
      # --------------------
      @primitive( 'is?', '<expr1> <expr2>' )
      def LP_is( env: Environment, *args ) -> Any:
         """Returns t if the two values are the same object otherwise nil."""
         try:
            arg1,arg2 = args
         except ValueError:
            raise LispRuntimeFuncError( LP_is, '2 arguments expected.' )
   
         if isinstance(arg1, (int,float,str)):
            return L_T if (arg1 == arg2) else L_NIL
         else:
            return L_T if (arg1 is arg2) else L_NIL
   
      @primitive( '=', '<expr1> <expr2> ...' )
      def LP_equal( env: Environment, *args ) -> Any:
         """Returns t if the two exprs are the same value otherwise nil."""
         numArgs = len(args)
         if numArgs < 2:
            raise LispRuntimeFuncError( LP_equal, '2 or more arguments expected.' )
   
         pairs = [ ]
         prior = None
         for mbr in args:
            if prior is not None:
               pairs.append( (prior,mbr) )
            prior = mbr
   
         for arg1,arg2 in pairs:
            if not( arg1 == arg2 ):
               return L_NIL
   
         return L_T
   
      @primitive( '/=', '<expr1> <expr2> ...' )
      def LP_notEqual( env: Environment, *args ) -> Any:
         """Returns t if the two exprs are different values otherwise nil."""
         numArgs = len(args)
         if numArgs < 2:
            raise LispRuntimeFuncError( LP_notEqual, '2 or more arguments expected.' )
   
         pairs = [ ]
         prior = None
         for mbr in args:
            if prior is not None:
               pairs.append( (prior,mbr) )
            prior = mbr
   
         for arg1,arg2 in pairs:
            if not( arg1 != arg2 ):
               return L_NIL
   
         return L_T
   
      @primitive( '<', '<expr1> <expr2> ...' )
      def LP_less( env: Environment, *args ) -> Any:
         """Returns t if the arguments are in ascending order."""
         numArgs = len(args)
         if numArgs < 2:
            raise LispRuntimeFuncError( LP_less, '2 or more arguments expected.' )
   
         pairs = [ ]
         prior = None
         for mbr in args:
            if prior is not None:
               pairs.append( (prior,mbr) )
            prior = mbr
   
         try:
            for arg1,arg2 in pairs:
               if not( arg1 < arg2 ):
                  return L_NIL
         except TypeError:
            raise LispRuntimeFuncError( LP_less, 'Invalid argument.  Arguments are not comparable.' )

         return L_T

      @primitive( '<=', '<expr1> <expr2> ...' )
      def LP_lessOrEqual( env: Environment, *args ) -> Any:
         """Returns t if the adjacent arguments are less-than-or-equal otherwise nil."""
         numArgs = len(args)
         if numArgs < 2:
            raise LispRuntimeFuncError( LP_lessOrEqual, '2 or more arguments expected.' )
   
         pairs = [ ]
         prior = None
         for mbr in args:
            if prior is not None:
               pairs.append( (prior,mbr) )
            prior = mbr
   
         try:
            for arg1,arg2 in pairs:
               if not( arg1 <= arg2 ):
                  return L_NIL
         except TypeError:
            raise LispRuntimeFuncError( LP_lessOrEqual, 'Invalid argument.  Arguments are not comparable.' )

         return L_T

      @primitive( '>', '<expr1> <expr2> ...' )
      def LP_greater( env: Environment, *args ) -> Any:
         """Returns t if the arguments are in descending order otherwise nil."""
         numArgs = len(args)
         if numArgs < 2:
            raise LispRuntimeFuncError( LP_greater, '2 or more arguments expected.' )
   
         pairs = [ ]
         prior = None
         for mbr in args:
            if prior is not None:
               pairs.append( (prior,mbr) )
            prior = mbr
   
         try:
            for arg1,arg2 in pairs:
               if not( arg1 > arg2 ):
                  return L_NIL
         except TypeError:
            raise LispRuntimeFuncError( LP_greater, 'Invalid argument.  Arguments are not comparable.' )

         return L_T

      @primitive( '>=', '<expr1> <expr2> ...' )
      def LP_greaterOrEqual( env: Environment, *args ) -> Any:
         """Returns t if the adjacent arguments are greater-than-or-equal otherwise nil."""
         numArgs = len(args)
         if numArgs < 2:
            raise LispRuntimeFuncError( LP_greaterOrEqual, '2 or more arguments expected.' )
   
         pairs = [ ]
         prior = None
         for mbr in args:
            if prior is not None:
               pairs.append( (prior,mbr) )
            prior = mbr
   
         try:
            for arg1,arg2 in pairs:
               if not( arg1 >= arg2 ):
                  return L_NIL
         except TypeError:
            raise LispRuntimeFuncError( LP_greaterOrEqual, 'Invalid argument.  Arguments are not comparable.' )

         return L_T

      # =================
      # Logical Operators
      # -----------------
      @primitive( 'not', '<boolean>' )
      def LP_not( env: Environment, *args ) -> Any:
         """Returns t if the argument is nil otherwise returns nil."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_not, '1 argument expected.' )
         arg1 = args[0]
         return L_T if (isinstance(arg1,list) and (len(arg1)==0)) else L_NIL
   
      @primitive( 'and', '<boolean1> <boolean2> ...', specialForm=True )
      def LP_and( env: Environment, *args ) -> Any:
         """Returns t if all arguments are truthy (non-nil).
Short-circuits: stops evaluating arguments upon encountering the first nil."""
         if len(args) < 2:
            raise LispRuntimeFuncError( LP_and, '2 or more arguments expected.' )
   
         # short-circuits: returns nil on the first nil argument otherwise true
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
   
         # short-circuits: returns t on first non-nil argument otherwise nil
         for arg in args:
            if LispInterpreter._lTrue(LispInterpreter._lEval(env, arg)):
               return L_T

         return L_NIL
   
      # ===============
      # Type Conversion
      # ---------------
      @primitive( 'float', '<number>' )
      def LP_float( env: Environment, *args ) -> Any:
         """Returns val as a float.  Val can be any number type or a string containing a valid lisp float."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_float, '1 argument expected.' )

         try:
            return float(args[0])
         except (ValueError, TypeError):
            raise LispRuntimeFuncError( LP_float, 'Invalid argument.' )
   
      @primitive( 'integer', '<number> &optional (<base> 10)' )
      def LP_integer( env: Environment, *args ) -> Any:
         """Returns val as an integer.  Val can be any number type or a string containing a valid lisp integer."""
         numArgs = len(args)
         if (numArgs < 1) or (numArgs > 2):
            raise LispRuntimeFuncError( LP_integer, '1 or 2 arguments expected.' )
   
         try:
            return int(*args)
         except (TypeError, ValueError):
            raise LispRuntimeFuncError( LP_integer, 'Invalid argument.' )
   
      @primitive( 'rational', '<number>' )
      def LP_rational( env: Environment, *args ) -> Any:
         """Returns its argument as a fraction.  Val can be any number or a string
containing a valid lisp number that can be expressed as a fraction."""
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_rational, 'Exactly 1 argument expected.' )
   
         try:
            return Fraction(args[0])
         except (IndexError, TypeError, ValueError):
            raise LispRuntimeFuncError( LP_rational, 'Invalid argument.' )
   
      @primitive( 'string', '<object1> <object2> ...' )
      def LP_string( env: Environment, *args ) -> Any:
         """PrettyPrints as programmer readable strings each argument object and
concatenates the results to form a new string."""
         if len(args) == 0:
            raise LispRuntimeFuncError( LP_string, '1 or more arguments expected.' )
   
         resultStrs = [ prettyPrint(sExpr) for sExpr in args ]
         return ''.join(resultStrs)
   
      @primitive( 'symbol', '<string1> <string2> ...' )
      def LP_symbol( env: Environment, *args ) -> Any:
         """PrettyPrints as user readable strings each argument object and
concatenates the results to form a new string which is used to define a new
symbol object."""
         if len(args) == 0:
            raise LispRuntimeFuncError( LP_symbol, '1 or more string argument expected.' )
   
         strList = [ prettyPrint(arg) for arg in args ]
         symstr = ''.join(strList)
         try:
            parsed = parseLispString(symstr)
         except ParseError:
            raise LispRuntimeFuncError( LP_symbol, f'The resulting string "{symstr}" is not a valid Lisp symbol.' )
         # parseLispString returns (PROGN <expr>); extract the inner expression
         sym = parsed[1] if isinstance(parsed, list) and len(parsed) == 2 else parsed
         if not isinstance(sym, LSymbol):
            raise LispRuntimeFuncError( LP_symbol, f'The resulting string "{symstr}" is not a valid Lisp symbol.' )
         return sym
   
      def _decode_escapes( s: str ) -> str:
         """Interpret standard escape sequences without corrupting non-ASCII text."""
         return ( s.replace('\\\\', '\x00')
                   .replace('\\n', '\n')
                   .replace('\\t', '\t')
                   .replace('\\"', '"')
                   .replace('\x00', '\\') )

      # ===============
      # I/O
      # ---------------
      @primitive( 'writef', '<formatString> <MapOrList>' )
      def LP_writef( env: Environment, *args ) -> str:
         """Writes formatted text.  Returns the string that is written.
Takes a python f-string and a map or list of values for the f-string.
Returns the formatted output string."""
         try:
            formatString, mapOrList = args
         except ValueError:
            raise LispRuntimeFuncError( LP_writef, "2 arguments expected." )
         
         if not isinstance( formatString, str ):
            raise LispRuntimeFuncError( LP_writef, "1st argument expected to be a format string." )
         
         try:
            if isinstance( mapOrList, list ):
               formattedStr = formatString.format( *mapOrList )
            elif isinstance( mapOrList, dict ):
               formattedStr = formatString.format( **mapOrList )
            else:
               raise LispRuntimeFuncError( LP_writef, "2nd argument expected to be a list or map." )
         except (IndexError, KeyError, ValueError) as e:
            raise LispRuntimeFuncError( LP_writef, f"Format error: {e}" )
         
         outputStr = _decode_escapes( formattedStr )
         print( outputStr, end='', file=LispInterpreter.outStrm )
         return outputStr
      
      @primitive( 'write!', '<obj1> <obj2> ...', )
      def LP_write( env: Environment, *args ) -> Any:
         """Sequentially prettyPrints in programmer readable text the objects listed.
Returns the last value printed."""
         if len(args) == 0:
            raise LispRuntimeFuncError( LP_write, '1 or more arguments expected.' )
         return lwrite( *args, end='' )
   
      @primitive( 'writeLn!', '<obj1> <obj2> ...' )
      def LP_writeln( env: Environment, *args ) -> Any:
         """Sequentially prettyPrints in programmer readable text the objects listed.
Terminates the output with a newline character.  Returns the last value printed."""
         if len(args) == 0:
            raise LispRuntimeFuncError( LP_writeln, '1 or more arguments expected.' )
         return lwrite( *args, end='\n' )
   
      def lwrite( *values, end='' ):
         if not values:
            return []
         for value in values:
            valueStr = prettyPrintSExpr( value )
            valueStr = _decode_escapes( valueStr )
            print( valueStr, end='', file=LispInterpreter.outStrm )
         if end:
            print( end=end, file=LispInterpreter.outStrm )
         return values[-1]
   
      @primitive( 'uwrite!', '<obj1> <obj2> ...' )
      def LP_uwrite( env: Environment, *args ) -> Any:
         """Sequentially prettyPrints in user readable text the objects listed.  Returns the last value printed."""
         if len(args) == 0:
            raise LispRuntimeFuncError( LP_uwrite, '1 or more arguments expected.' )
         return luwrite( *args, end='' )
   
      @primitive( 'uwriteLn!', '<obj1> <obj2> ...' )
      def LP_uwriteln( env: Environment, *args ) -> Any:
         """Sequentially prettyPrints in user readable text the objects listed.
Terminates the output with a newline character.  Returns the last value printed."""
         if len(args) == 0:
            raise LispRuntimeFuncError( LP_uwriteln, '1 or more arguments expected.' )
         return luwrite( *args, end='\n' )
   
      def luwrite( *values, end='' ):
         if not values:
            return []
         for value in values:
            valueStr = prettyPrint( value )
            valueStr = _decode_escapes( valueStr )
            print( valueStr, end='', file=LispInterpreter.outStrm )
         if end:
            print( end=end, file=LispInterpreter.outStrm )
         return values[-1]
   
      @primitive( 'readLn!', '' )
      def LP_readln( env: Environment, *args ) -> Any:
         """Reads and returns text input from standard input.  This function blocks
while it waits for the input return key to be pressed at the end of text entry."""
         if len(args) > 0:
            raise LispRuntimeFuncError( LP_readln, '0 arguments expected.' )
         return input()
   
      # ===============
      # System Level
      # ---------------
      @primitive( 'help', '&optional callableSymbol' )
      def LP_help( env: Environment, *args ) -> Any:
         """Prints a set of tables for all the globally defined symbols and
topics currently available in Python's Lisp. Or prints the usage and
documentation for a specific callable (primitive, function or macro) or topic.

Type '(help <callable>)' for available documentation on a callable.
Type '(help "topic")' for available documentation on the named topic."""
         numArgs = len(args)
         if numArgs > 1:
            raise LispRuntimeFuncError( LP_help, f'Too many arguments.  Received {numArgs}' )
         
         elif numArgs == 0:
            printHelpListings( env )
            return L_T
         
         # numArgs == 1
         arg = args[0]
         if isinstance(arg, str):
            # Show help on a topic
            topicName = arg.upper()
            try:
               print(topics[topicName], file=LispInterpreter.outStrm )
            except KeyError:
               print( f'Unknown topic: "{topicName}"', file=LispInterpreter.outStrm )
            return L_T
         
         # ensure arg is an LCallable
         if not isinstance(arg, LCallable):
            raise LispRuntimeFuncError( LP_help, 'First argument expected to be a callable.' )
         callableObj = arg
         
         # Show help on the callableObj
         print( "   USAGE: ", callableObj.usageString(), file=LispInterpreter.outStrm )
         print( file=LispInterpreter.outStrm )
         if callableObj.docString != '':
            valueStr = prettyPrint( callableObj.docString )
            valueStr = _decode_escapes( valueStr )
            print( valueStr, LispInterpreter.outStrm )
         
         return L_T
      
      def printHelpListings( env: Environment ) -> None:
         primitivesList = []
         functionsList = []
         macrosList = []
         topicsList = [ f'"{topic}"' for topic in sorted(topics.keys()) ]
         outStrm = LispInterpreter.outStrm
         
         # Bin the global symbols each into one of the lists defined above
         for symbolStr in env.getGlobalEnv().localSymbols():
            obj = env.lookupGlobal(symbolStr)
            if isinstance(obj, LPrimitive):
               primitivesList.append(symbolStr)
            elif isinstance(obj, LFunction):
               functionsList.append(symbolStr)
            elif isinstance(obj, LMacro):
               macrosList.append(symbolStr)
         
         # Print tables for each of the categories of symbols
         print( "Predefined Symbols", file=outStrm )
         print( "==================", file=outStrm )
         print( "E  NIL  PI  T", file=outStrm )
         print( file=outStrm )
         print( "Primitives", file=outStrm )
         print( "==========", file=outStrm )
         columnize( primitivesList, 78, file=outStrm )
         print( file=outStrm )
         print( "Functions", file=outStrm )
         print( "=========", file=outStrm )
         columnize( functionsList, 78, file=outStrm )
         print( file=outStrm )
         print( "Macros", file=outStrm )
         print( "======", file=outStrm )
         columnize( macrosList, 78, file=outStrm )
         print( file=outStrm )
         print( "TOPICS", file=outStrm )
         print( "======", file=outStrm )
         columnize( topicsList, 78, file=outStrm )
         print( file=outStrm )
         print( "Type '(help <callable>)' for available documentation on a callable.", file=outStrm )
         print( "Type '(help \"topic\")' for available documentation on the named topic.", file=outStrm )
      
      return primitiveDict

"""CEK Machine evaluator for Python's Lisp.

The machine state is a triple (C, E, K):
  C — Control     : current expression to reduce, OR a _Val-wrapped value to deliver
  E — Environment : current lexical environment
  K — Kontinuation: explicit continuation stack (Python list of frame objects)

Key invariant
-------------
In _lEval a recursive call always returns a value directly (return result).
In the CEK loop, a looked-up or computed value would be set as C and looped —
but then _is_value would misidentify a data list as code to evaluate.

Solution: wrap every computed value in _Val().  The loop's first check is
isinstance(C, _Val); only _Val-tagged objects (and self-evaluating atoms) are
delivered to continuation frames.  Non-empty lists without _Val are always
treated as expressions (source code) to reduce.

Frames that return expressions  → return (ast_node, env)
Frames that return values       → return (_Val(v),  env)
"""
from __future__ import annotations
from typing import Any

from pythonslisp.AST import ( LSymbol, L_NIL,
                               LCallable, LFunction, LPrimitive, LMacro, LContinuation,
                               LMultipleValues )
from pythonslisp.Exceptions import ( LRuntimeError, LArgBindingError,
                                      ContinuationInvoked )
from pythonslisp.Environment  import Environment
from pythonslisp.Expander     import Expander


# ---------------------------------------------------------------------------
# Value wrapper
# ---------------------------------------------------------------------------

class _Val:
   """Tag a computed value so the machine loop does not re-evaluate it."""
   __slots__ = ('v',)
   def __init__( self, v ):
      self.v = v


# ---------------------------------------------------------------------------
# Tiny helpers
# ---------------------------------------------------------------------------

def _primary( val: Any ) -> Any:
   """In scalar context extract the first value from LMultipleValues."""
   if type(val) is LMultipleValues:
      return val.values[0] if val.values else L_NIL
   return val


def _lTrue( x: Any ) -> bool:
   if isinstance(x, list):
      return len(x) != 0
   return True


def _is_value( x: Any ) -> bool:
   """True when x is self-evidently a value that needs no further reduction.
   Non-empty lists without _Val are always treated as expressions."""
   if isinstance(x, _Val):
      return True
   if type(x) is LSymbol:
      return False
   if isinstance(x, list) and len(x) > 0:
      return False
   return True


def _unwrap( x: Any ) -> Any:
   """Strip a _Val wrapper if present."""
   return x.v if isinstance(x, _Val) else x


def _extract_vardefs( vardefs ) -> list:
   """Convert a LET/LET* vardef list to [(name_str, init_form), ...] pairs."""
   pairs = []
   for varSpec in vardefs:
      if type(varSpec) is LSymbol:
         pairs.append( (varSpec.name, L_NIL) )
      elif len(varSpec) == 1:
         pairs.append( (varSpec[0].name, L_NIL) )
      else:
         pairs.append( (varSpec[0].name, varSpec[1]) )
   return pairs


def _eval_body( body, env, K ) -> tuple:
   """Return (C, E) to begin evaluating a body sequence.
   Pushes PrognFrame for all but the last form (which is in tail position).
   Always returns an expression (source AST node), never a _Val."""
   if not body:
      return L_NIL, env         # L_NIL is an empty list — _is_value → True, delivered as NIL
   if len(body) == 1:
      return body[0], env       # tail form — expression
   K.append( PrognFrame(list(body[1:]), env) )
   return body[0], env          # first non-tail form — expression


# ---------------------------------------------------------------------------
# Continuation frame classes
# ---------------------------------------------------------------------------

class IfFrame:
   """Waiting for the IF condition.  Picks the correct branch on arrival."""
   __slots__ = ('then_expr', 'else_expr', 'env')

   def __init__( self, then_expr, else_expr, env ):
      self.then_expr = then_expr
      self.else_expr = else_expr
      self.env       = env

   def step( self, value, E, K, ctx ):
      # Returns an expression (AST node) — not _Val-wrapped.
      branch = self.then_expr if _lTrue(_primary(value)) else self.else_expr
      return branch, self.env


class PrognFrame:
   """Sequence non-tail body forms; deliver the last form into tail position."""
   __slots__ = ('remaining', 'env')

   def __init__( self, remaining, env ):
      self.remaining = remaining   # list of forms; the last IS the tail form
      self.env       = env

   def step( self, value, E, K, ctx ):
      # The incoming value (previous form's result) is discarded.
      # Returns an expression — not _Val-wrapped.
      if len(self.remaining) == 1:
         return self.remaining[0], self.env        # tail — TCO
      nxt            = self.remaining[0]
      self.remaining = self.remaining[1:]
      K.append(self)
      return nxt, self.env


class LetFrame:
   """Collect LET init values, all evaluated in the outer environment."""
   __slots__ = ('current_name', 'pending', 'bound', 'body', 'outer_env')

   def __init__( self, current_name, pending, bound, body, outer_env ):
      self.current_name = current_name
      self.pending      = pending
      self.bound        = bound
      self.body         = body
      self.outer_env    = outer_env

   def step( self, value, E, K, ctx ):
      self.bound[self.current_name] = _primary(value)
      if self.pending:
         name, form    = self.pending[0]
         self.current_name = name
         self.pending  = self.pending[1:]
         K.append(self)
         return form, self.outer_env    # expression
      new_env = Environment( self.outer_env,
                              initialBindings=self.bound,
                              evalFn=ctx.lEval )
      return _eval_body( self.body, new_env, K )   # expression


class LetStarFrame:
   """Collect LET* init values sequentially, each extending the inner env."""
   __slots__ = ('current_name', 'pending', 'inner_env', 'body')

   def __init__( self, current_name, pending, inner_env, body ):
      self.current_name = current_name
      self.pending      = pending
      self.inner_env    = inner_env
      self.body         = body

   def step( self, value, E, K, ctx ):
      self.inner_env.bindLocal( self.current_name, _primary(value) )
      if self.pending:
         name, form    = self.pending[0]
         self.current_name = name
         self.pending  = self.pending[1:]
         K.append(self)
         return form, self.inner_env    # expression
      return _eval_body( self.body, self.inner_env, K )   # expression


class SetqFrame:
   """Collect SETQ rvalues and bind each name in turn."""
   __slots__ = ('lval_name', 'pending', 'env')

   def __init__( self, lval_name, pending, env ):
      self.lval_name = lval_name
      self.pending   = pending
      self.env       = env

   def step( self, value, E, K, ctx ):
      rval = _primary(value)
      if (type(rval) is LMacro or type(rval) is LFunction) and rval.name == '':
         rval.name = self.lval_name
      self.env.bind( self.lval_name, rval )
      if self.pending:
         name, form = self.pending[0]
         self.lval_name = name
         self.pending   = self.pending[1:]
         K.append(self)
         return form, self.env          # expression
      return _Val(rval), self.env       # value — wrap so it isn't re-evaluated


class CondFrame:
   """Evaluate COND tests; take the body of the first true clause."""
   __slots__ = ('clause_body', 'remaining_clauses', 'env')

   def __init__( self, clause_body, remaining_clauses, env ):
      self.clause_body       = clause_body
      self.remaining_clauses = remaining_clauses
      self.env               = env

   def step( self, value, E, K, ctx ):
      if _lTrue( _primary(value) ):
         return _eval_body( self.clause_body, self.env, K )   # expression
      if not self.remaining_clauses:
         return L_NIL, self.env   # empty list → _is_value True
      clause                 = self.remaining_clauses[0]
      self.clause_body       = list(clause[1:])
      self.remaining_clauses = self.remaining_clauses[1:]
      K.append(self)
      return clause[0], self.env     # test expression


class CaseFrame:
   """Evaluate the CASE key then dispatch to the matching clause."""
   __slots__ = ('clauses', 'env')

   def __init__( self, clauses, env ):
      self.clauses = clauses
      self.env     = env

   def step( self, value, E, K, ctx ):
      value = _primary(value)
      for clause in self.clauses:
         case_val = clause[0]
         body     = clause[1:]
         if type(case_val) is LSymbol and case_val.name in ('T', 'OTHERWISE'):
            matched = True
         elif isinstance(case_val, list):
            matched = value in case_val
         else:
            matched = (case_val == value)
         if matched:
            return _eval_body( body, self.env, K )   # expression
      return L_NIL, self.env


class ArgFrame:
   """Collect the function value then each argument value for a call.

   mode:
     'call'    — common call path; macros are inline-expanded
     'funcall' — FUNCALL; rejects macros and preEvalArgs=False forms
     'apply'   — APPLY;   like funcall but spreads the final list arg
   """
   __slots__ = ('fn', 'pending', 'done', 'env', 'mode')

   def __init__( self, fn, pending, done, env, mode='call' ):
      self.fn      = fn
      self.pending = pending
      self.done    = done
      self.env     = env
      self.mode    = mode

   def step( self, value, E, K, ctx ):
      # ------------------------------------------------------------------
      # Phase 1: value IS the function.
      # ------------------------------------------------------------------
      if self.fn is None:
         fn = _primary(value)

         if self.mode == 'funcall':
            if not isinstance(fn, LCallable):
               raise LRuntimeError( 'FUNCALL: first argument must evaluate to a callable.' )
            if type(fn) is LContinuation:
               if len(self.pending) != 1:
                  raise LRuntimeError( f'Continuation expects exactly 1 argument, got {len(self.pending)}.' )
               K.append( _ContinuationInvokeFrame(fn) )
               return self.pending[0], self.env   # expression
            if type(fn) is LMacro:
               raise LRuntimeError( f'FUNCALL: cannot call macro "{fn.name}".' )
            if not fn.preEvalArgs:
               raise LRuntimeError( 'FUNCALL: first argument may not be a special form.' )

         elif self.mode == 'apply':
            if isinstance(fn, LCallable):
               pass
            elif type(fn) is LSymbol:
               try:
                  fn = self.env.lookup(fn.name)
               except KeyError:
                  raise LRuntimeError( f'APPLY: "{fn}" is not bound to a callable.' )
               if not isinstance(fn, LCallable):
                  raise LRuntimeError( f'APPLY: "{fn}" is not bound to a callable.' )
            else:
               raise LRuntimeError( 'APPLY: first argument must evaluate to a callable or symbol.' )
            if type(fn) is LMacro:
               raise LRuntimeError( f'APPLY: cannot apply macro "{fn.name}".' )
            if not fn.preEvalArgs:
               raise LRuntimeError( 'APPLY: first argument may not be a special form.' )

         else:  # 'call'
            if not isinstance(fn, LCallable):
               raise LRuntimeError( f"Badly formed list expression '{fn}'.  The first element should evaluate to a callable." )
            if type(fn) is LContinuation:
               if len(self.pending) != 1:
                  raise LRuntimeError( f'Continuation expects exactly 1 argument, got {len(self.pending)}.' )
               K.append( _ContinuationInvokeFrame(fn) )
               return self.pending[0], self.env   # expression
            if type(fn) is LMacro:
               # Inline macro expansion — return the expansion as an expression.
               expansion = Expander._expandMacroCall( ctx, self.env, fn, self.pending )
               return expansion, self.env         # expression (code to evaluate)

         # preEvalArgs=False: phase-1 limitation — call directly with raw args.
         if not fn.preEvalArgs:
            result = fn.pythonFn( ctx, self.env, self.pending )
            return _Val(result), self.env   # VALUE — wrap; LMultipleValues preserved

         self.fn = fn
         if not self.pending:
            return _do_apply( fn, self.done, self.env, K, ctx )
         nxt          = self.pending[0]
         self.pending = self.pending[1:]
         K.append(self)
         return nxt, self.env   # expression

      # ------------------------------------------------------------------
      # Phase 2: value is an evaluated argument.
      # ------------------------------------------------------------------
      self.done.append( _primary(value) )
      if self.pending:
         nxt          = self.pending[0]
         self.pending = self.pending[1:]
         K.append(self)
         return nxt, self.env   # expression

      if self.mode == 'apply':
         list_arg = self.done.pop()
         if not isinstance(list_arg, list):
            raise LRuntimeError( 'APPLY: last argument must be a list.' )
         if type(self.fn) is LContinuation:
            spread = self.done + list_arg
            if len(spread) != 1:
               raise LRuntimeError( f'Continuation expects exactly 1 argument, got {len(spread)}.' )
            raise ContinuationInvoked( self.fn.token, spread[0] )
         self.done.extend(list_arg)

      return _do_apply( self.fn, self.done, self.env, K, ctx )


class _ContinuationInvokeFrame:
   """Receives the single argument and raises ContinuationInvoked."""
   __slots__ = ('continuation',)

   def __init__( self, continuation ):
      self.continuation = continuation

   def step( self, value, E, K, ctx ):
      raise ContinuationInvoked( self.continuation.token, _primary(value) )


class BodyFrame:
   """Evaluate a traced LFunction body; fire the exit trace after the last form."""
   __slots__ = ('remaining', 'fn', 'depth', 'env', 'tracer')

   def __init__( self, remaining, fn, depth, env, tracer ):
      self.remaining = remaining
      self.fn        = fn
      self.depth     = depth
      self.env       = env
      self.tracer    = tracer

   def step( self, value, E, K, ctx ):
      if self.remaining:
         nxt            = self.remaining[0]
         self.remaining = self.remaining[1:]
         K.append(self)
         return nxt, self.env         # expression
      self.tracer.setMaxTraceDepth( self.depth )
      self.tracer.trace( 'exit', self.fn, value, self.depth, ctx.outStrm )
      return _Val(value), E           # VALUE — wrap so it isn't re-evaluated


# ---------------------------------------------------------------------------
# Apply helper
# ---------------------------------------------------------------------------

def _do_apply( fn, args, env, K, ctx ) -> tuple:
   """Perform a function application.  Returns (C, E) for the machine loop.
   LPrimitive results are _Val-wrapped (values).
   LFunction results are expressions (body to evaluate) — no wrapping."""
   tracer  = ctx.tracer
   printed = False
   if tracer.isActive():
      depth   = tracer.getMaxTraceDepth()
      printed = tracer.trace( 'enter', fn, args, depth, ctx.outStrm )
      if printed:
         tracer.setMaxTraceDepth( depth + 1 )

   try:
      if type(fn) is LPrimitive:
         if fn.lambdaListAST:
            kw_env = Environment( env, evalFn=ctx.lEval )
            kw_env.bindArguments( fn.lambdaListAST, args )
            result = fn.pythonFn( ctx, kw_env, args )
         else:
            result = fn.pythonFn( ctx, env, args )
         if printed:
            tracer.setMaxTraceDepth( depth )
            tracer.trace( 'exit', fn, result, depth, ctx.outStrm )
         return _Val(result), env   # VALUE — wrap; LMultipleValues preserved for frames to strip

      else:  # LFunction
         new_env = Environment( fn.capturedEnvironment, evalFn=ctx.lEval )
         new_env.bindArguments( fn.lambdaListAST, args )
         body = fn.bodyAST
         if not body:
            if printed:
               tracer.setMaxTraceDepth( depth )
               tracer.trace( 'exit', fn, L_NIL, depth, ctx.outStrm )
            return L_NIL, env         # L_NIL → _is_value True, delivered as NIL
         if printed:
            # Traced: cannot TCO — BodyFrame observes the return value.
            K.append( BodyFrame(list(body[1:]), fn, depth, new_env, tracer) )
            return body[0], new_env   # expression
         return _eval_body( body, new_env, K )   # expression (TCO)

   except LArgBindingError as ex:
      if printed:
         tracer.setMaxTraceDepth( depth )
      errorMsg = ex.args[-1]
      fnName   = fn.name
      if fnName == '':
         raise LRuntimeError( f'Error binding arguments in call to "(lambda ...)".\n{errorMsg}' )
      raise LRuntimeError( f'Error binding arguments in call to function "{fnName}".\n{errorMsg}' )


# ---------------------------------------------------------------------------
# The CEK machine loop
# ---------------------------------------------------------------------------

_SPECIAL_OPERATOR_SET = frozenset({'IF', 'QUOTE', 'LET', 'LET*', 'PROGN',
                                    'SETQ', 'COND', 'CASE', 'FUNCALL', 'APPLY'})

def cek_eval( ctx, env, expr ) -> Any:
   """CEK machine evaluator.  Drop-in replacement for Interpreter._lEval."""
   C = expr
   E = env
   K = []   # continuation stack

   while True:

      # ----------------------------------------------------------------
      # Deliver a value to the top continuation frame.
      # _Val-wrapped values and self-evaluating atoms come here.
      # ----------------------------------------------------------------
      if _is_value(C):
         v = C.v if isinstance(C, _Val) else C
         if not K:
            return v
         frame = K.pop()
         C, E  = frame.step(v, E, K, ctx)
         continue

      # ----------------------------------------------------------------
      # Symbol — look it up; wrap result in _Val so it is not re-evaluated.
      # ----------------------------------------------------------------
      if type(C) is LSymbol:
         try:
            C = _Val( E.lookup(C.name) )
         except KeyError:
            if C.isKeyArg():
               C = _Val(C)    # keyword arg is self-evaluating
            else:
               raise LRuntimeError( f'Unbound Variable: {C.name}.' )
         continue

      # ----------------------------------------------------------------
      # Empty list — NIL.
      # ----------------------------------------------------------------
      if len(C) == 0:
         C = L_NIL   # empty list → _is_value True
         continue

      # ----------------------------------------------------------------
      # Non-empty list — an expression to reduce.
      # ----------------------------------------------------------------
      head    = C[0]
      headStr = head.name if type(head) is LSymbol else None

      if headStr not in _SPECIAL_OPERATOR_SET:
         # Common function-call path.
         K.append( ArgFrame(None, list(C[1:]), [], E, 'call') )
         C = C[0]
         continue

      # ----------------------------------------------------------------
      # Special operators
      # ----------------------------------------------------------------

      if headStr == 'QUOTE':
         C = _Val(C[1])   # quoted datum is a value — wrap
         continue

      if headStr == 'IF':
         then_ = C[2] if len(C) > 2 else L_NIL
         else_ = C[3] if len(C) > 3 else L_NIL
         K.append( IfFrame(then_, else_, E) )
         C = C[1]
         continue

      if headStr == 'LET':
         vardefs = C[1]
         body    = C[2:]
         pairs   = _extract_vardefs(vardefs)
         if not pairs:
            new_env = Environment(E, evalFn=ctx.lEval)
            C, E    = _eval_body(body, new_env, K)
            continue
         first_name, first_form = pairs[0]
         K.append( LetFrame(first_name, pairs[1:], {}, body, E) )
         C = first_form
         continue

      if headStr == 'LET*':
         vardefs = C[1]
         body    = C[2:]
         pairs   = _extract_vardefs(vardefs)
         inner   = Environment(E, evalFn=ctx.lEval)
         if not pairs:
            C, E = _eval_body(body, inner, K)
            continue
         first_name, first_form = pairs[0]
         K.append( LetStarFrame(first_name, pairs[1:], inner, body) )
         C = first_form
         E = inner
         continue

      if headStr == 'PROGN':
         if len(C) == 1:
            C = L_NIL
            continue
         if len(C) == 2:
            C = C[1]
            continue
         K.append( PrognFrame(list(C[2:]), E) )
         C = C[1]
         continue

      if headStr == 'SETQ':
         pairs = []
         for i in range(1, len(C), 2):
            pairs.append( (C[i].name, C[i + 1]) )
         first_name, first_form = pairs[0]
         K.append( SetqFrame(first_name, pairs[1:], E) )
         C = first_form
         continue

      if headStr == 'COND':
         clauses = C[1:]
         if not clauses:
            C = L_NIL
            continue
         first_clause = clauses[0]
         K.append( CondFrame(list(first_clause[1:]), list(clauses[1:]), E) )
         C = first_clause[0]
         continue

      if headStr == 'CASE':
         K.append( CaseFrame(list(C[2:]), E) )
         C = C[1]
         continue

      if headStr == 'FUNCALL':
         args = C[1:]
         if len(args) < 1:
            raise LRuntimeError( 'Error binding arguments in call to function "FUNCALL".\nToo few positional arguments.' )
         K.append( ArgFrame(None, list(args[1:]), [], E, 'funcall') )
         C = args[0]
         continue

      if headStr == 'APPLY':
         args = C[1:]
         if len(args) < 2:
            raise LRuntimeError( 'Error binding arguments in call to function "APPLY".\nToo few positional arguments.' )
         K.append( ArgFrame(None, list(args[1:]), [], E, 'apply') )
         C = args[0]
         continue

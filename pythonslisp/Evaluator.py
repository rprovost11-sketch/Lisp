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

from pythonslisp.AST import ( LSymbol, L_NIL, L_T,
                               LCallable, LFunction, LPrimitive, LMacro, LContinuation,
                               LMultipleValues, eql, prettyPrintSExpr )
from pythonslisp.Exceptions import ( LRuntimeError, LArgBindingError,
                                      ContinuationInvoked, ReturnFrom,
                                      Thrown, Signaled )
from pythonslisp.Environment  import Environment, ModuleEnvironment
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


def _block_name( obj ):
   if isinstance(obj, LSymbol):
      return obj.name
   if isinstance(obj, list) and not obj:   # L_NIL
      return 'NIL'
   raise LRuntimeError('block: name must be a symbol.')


def _lquasiquoteExpand( ctx, env, expr, depth=1 ):
   if not isinstance(expr, list):
      return expr
   if len(expr) == 0:
      return expr
   head = expr[0]
   headName = head.name if type(head) is LSymbol else None
   if headName == 'QUASIQUOTE':
      inner = _lquasiquoteExpand(ctx, env, expr[1], depth + 1)
      return [LSymbol('QUASIQUOTE'), inner]
   if headName == 'UNQUOTE':
      if depth == 1:
         return ctx.lEval(env, expr[1])
      else:
         inner = _lquasiquoteExpand(ctx, env, expr[1], depth - 1)
         return [LSymbol('UNQUOTE'), inner]
   if headName == 'UNQUOTE-SPLICING':
      if depth == 1:
         result = ctx.lEval(env, expr[1])
         if not isinstance(result, list):
            raise LRuntimeError(
               "ERROR 'UNQUOTE-SPLICING': Argument 1 must evaluate to a List.\n"
               "PRIMITIVE USAGE: (UNQUOTE-SPLICING sexpr)")
         return [LSymbol('UNQUOTE-SPLICING'), result]
      else:
         inner = _lquasiquoteExpand(ctx, env, expr[1], depth - 1)
         return [LSymbol('UNQUOTE-SPLICING'), inner]
   resultList = []
   for listElt in expr:
      resultListElt = _lquasiquoteExpand(ctx, env, listElt, depth)
      if (depth == 1 and
              isinstance(resultListElt, list) and
              len(resultListElt) > 0 and
              type(resultListElt[0]) is LSymbol and
              resultListElt[0].name == 'UNQUOTE-SPLICING'):
         for elt in resultListElt[1]:
            resultList.append(elt)
      else:
         resultList.append(resultListElt)
   return resultList


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
     'funcall' — FUNCALL; rejects macros
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
            if type(fn) is LPrimitive and fn.name in _SPECIAL_OPERATOR_SET:
               raise LRuntimeError( f'FUNCALL: first argument may not be a special form.' )


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
            if type(fn) is LPrimitive and fn.name in _SPECIAL_OPERATOR_SET:
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
               expansion = Expander.expandMacroCall( ctx, self.env, fn, self.pending )
               return expansion, self.env         # expression (code to evaluate)

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
# New frame classes for inlined special operators
# ---------------------------------------------------------------------------

class BlockFrame:
   """Marks an active block on K; receives the block body's final value."""
   __slots__ = ('name',)
   def __init__( self, name ):
      self.name = name
   def step( self, value, E, K, ctx ):
      return _Val(value), E


class ReturnFromFrame:
   """Receives return-from value; unwinds K to matching BlockFrame."""
   __slots__ = ('name',)
   def __init__( self, name ):
      self.name = name
   def step( self, value, E, K, ctx ):
      for i in reversed(range(len(K))):
         if isinstance(K[i], BlockFrame) and K[i].name == self.name:
            del K[i:]   # removes BlockFrame and everything above it
            return _Val(value), E
      raise ReturnFrom(self.name, _primary(value))


class CatchTagFrame:
   """Receives evaluated catch tag; pushes CatchBodyFrame and starts body."""
   __slots__ = ('body', 'env')
   def __init__( self, body, env ):
      self.body = body
      self.env  = env
   def step( self, tag, E, K, ctx ):
      K.append( CatchBodyFrame(_primary(tag)) )
      return _eval_body(self.body, self.env, K)


class CatchBodyFrame:
   """K-stack marker for throw unwinding; passes normal completion through."""
   __slots__ = ('tag',)
   def __init__( self, tag ):
      self.tag = tag
   def step( self, value, E, K, ctx ):
      return _Val(_primary(value)), E


class HandlerCaseBodyFrame:
   """K-stack marker for signal/error handling; passes normal completion through."""
   __slots__ = ('clauses', 'env')
   def __init__( self, clauses, env ):
      # clauses: list of (type_sym, var_sym_or_None, body_forms)
      self.clauses = clauses
      self.env     = env
   def find_handler( self, type_name ):
      for ctype, var, body in self.clauses:
         if isinstance(ctype, LSymbol):
            if ctype.name in ('T', 'ERROR'):
               return var, body
            if ctype.name == type_name:
               return var, body
      return None, None
   def step( self, value, E, K, ctx ):
      return _Val(_primary(value)), E


class MVBindFrame:
   """Receives values-form result (NOT primary-stripped); binds vars, evals body."""
   __slots__ = ('var_list', 'body', 'outer_env')
   def __init__( self, var_list, body, outer_env ):
      self.var_list  = var_list
      self.body      = body
      self.outer_env = outer_env
   def step( self, value, E, K, ctx ):
      vals = value.values if type(value) is LMultipleValues else [value]
      new_env = Environment(self.outer_env, evalFn=ctx.lEval)
      for i, var in enumerate(self.var_list):
         new_env.bindLocal(var.name, vals[i] if i < len(vals) else L_NIL)
      return _eval_body(self.body, new_env, K)


class MVListFrame:
   """Receives form result; returns all values as a list."""
   __slots__ = ()
   def step( self, value, E, K, ctx ):
      if type(value) is LMultipleValues:
         return _Val(list(value.values)), E
      return _Val([value]), E


class NthValueBodyFrame:
   """Receives n; validates it; queues form evaluation."""
   __slots__ = ('form', 'env')
   def __init__( self, form, env ):
      self.form = form
      self.env  = env
   def step( self, n_raw, E, K, ctx ):
      n = _primary(n_raw)
      if not isinstance(n, int) or isinstance(n, bool) or n < 0:
         raise LRuntimeError('nth-value: first argument must be a non-negative integer.')
      K.append( NthValueDeliverFrame(n) )
      return self.form, self.env


class NthValueDeliverFrame:
   """Receives form result; extracts nth value (NOT primary-stripped)."""
   __slots__ = ('n',)
   def __init__( self, n ):
      self.n = n
   def step( self, value, E, K, ctx ):
      if type(value) is LMultipleValues:
         return _Val(value.values[self.n] if self.n < len(value.values) else L_NIL), E
      return _Val(value if self.n == 0 else L_NIL), E


class DictBuildFrame:
   """Collects evaluated dict values; keys are the raw (unevaluated) key forms."""
   __slots__ = ('current_key', 'remaining', 'built', 'env')
   def __init__( self, current_key, remaining, built, env ):
      self.current_key = current_key
      self.remaining   = remaining
      self.built       = built
      self.env         = env
   def step( self, value, E, K, ctx ):
      self.built[self.current_key] = _primary(value)
      if self.remaining:
         key, form        = self.remaining[0]
         self.current_key = key
         self.remaining   = self.remaining[1:]
         K.append(self)
         return form, self.env
      return _Val(dict(self.built)), self.env


class ColonFrame:
   """Receives evaluated root module; navigates the raw-symbol path."""
   __slots__ = ('path', 'env')
   def __init__( self, path, env ):
      self.path = path
      self.env  = env
   def step( self, root, E, K, ctx ):
      from pythonslisp.AST import prettyPrint
      _USAGE = "PRIMITIVE USAGE: (: module-or-pkg &rest path)"
      current = _primary(root)
      for sym in self.path:
         if not isinstance(sym, LSymbol):
            raise LRuntimeError(f"ERROR ':': Path elements must be symbols.\n{_USAGE}")
         if not isinstance(current, ModuleEnvironment):
            raise LRuntimeError(f"ERROR ':': {prettyPrint(current)} is not a module or package.\n{_USAGE}")
         try:
            current = current._bindings[sym.name]
         except KeyError:
            raise LRuntimeError(f"ERROR ':': {sym.name} not found in module {current.name}.\n{_USAGE}")
      return _Val(current), self.env


class CallCCProcFrame:
   """Receives evaluated procedure for call/cc; validates it; sets up the continuation call."""
   __slots__ = ('token', 'cont')
   def __init__( self, token, cont ):
      self.token = token
      self.cont  = cont
   def step( self, value, E, K, ctx ):
      proc = _primary(value)
      if not isinstance( proc, LCallable ):
         raise LRuntimeError( "ERROR 'CALL/CC': Argument must be a callable.\nPRIMITIVE USAGE: (CALL/CC procedure)" )
      if type(proc) is LMacro:
         raise LRuntimeError( "ERROR 'CALL/CC': Argument may not be a macro.\nPRIMITIVE USAGE: (CALL/CC procedure)" )
      if type(proc) is LPrimitive and proc.name in _SPECIAL_OPERATOR_SET:
         raise LRuntimeError( "ERROR 'CALL/CC': Argument may not be a special form.\nPRIMITIVE USAGE: (CALL/CC procedure)" )
      K.append( CallCCGuardFrame(self.token) )
      K.append( ArgFrame(None, [_Val(self.cont)], [], E, 'funcall') )
      return _Val(proc), E


class CallCCGuardFrame:
   """Boundary marker for call/cc. Passes normal return through; matched by ContinuationInvoked handler."""
   __slots__ = ('token',)
   def __init__( self, token ):
      self.token = token
   def step( self, value, E, K, ctx ):
      return _Val(_primary(value)), E


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


def cek_apply( ctx, env, function, args ) -> Any:
   """Apply a callable to pre-evaluated args using the CEK machine.
   Used by sequences, types, and other primitives that need to call
   Lisp functions with already-evaluated argument lists.
   Returns the primary (first) value in scalar context."""
   if isinstance( function, LContinuation ):
      if len(args) != 1:
         raise LRuntimeError( f'Continuation expects exactly 1 argument, got {len(args)}.' )
      raise ContinuationInvoked( function.token, args[0] )

   # Use _do_apply directly; run the full loop with the returned state.
   K = []
   C, E = _do_apply( function, list(args), env, K, ctx )
   return _primary( _run_loop( C, E, K, ctx ) )


def _run_loop( C, E, K, ctx ) -> Any:
   """Run the CEK loop starting from (C, E, K).
   Returns the final value (not primary-stripped)."""
   while True:
      if _is_value(C):
         v = C.v if isinstance(C, _Val) else C
         if not K:
            return v
         frame = K.pop()
         C, E  = frame.step(v, E, K, ctx)
         continue

      if type(C) is LSymbol:
         try:
            C = _Val( E.lookup(C.name) )
         except KeyError:
            if C.isKeyArg():
               C = _Val(C)
            else:
               raise LRuntimeError( f'Unbound Variable: {C.name}.' )
         continue

      if isinstance(C, list) and len(C) == 0:
         C = L_NIL
         continue

      head    = C[0]
      headStr = head.name if type(head) is LSymbol else None

      if headStr not in _SPECIAL_OPERATOR_SET:
         K.append( ArgFrame(None, list(C[1:]), [], E, 'call') )
         C = C[0]
         continue

      # For special operators in this loop, delegate to the full cek_eval.
      # This is a sub-eval; any remaining K frames wait for its result.
      sub_result = cek_eval( ctx, E, C )
      C = _Val( sub_result )
      continue


# ---------------------------------------------------------------------------
# The CEK machine loop
# ---------------------------------------------------------------------------

_SPECIAL_OPERATOR_SET = frozenset({
   # existing
   'IF', 'QUOTE', 'LET', 'LET*', 'PROGN', 'SETQ', 'COND', 'CASE',
   'FUNCALL', 'APPLY',
   # new
   'LAMBDA', 'DEFMACRO',
   'QUASIQUOTE', 'UNQUOTE', 'UNQUOTE-SPLICING',
   'TRACE', 'UNTRACE',
   'BLOCK', 'RETURN-FROM', 'RETURN',
   'CATCH',
   'HANDLER-CASE',
   'MULTIPLE-VALUE-BIND', 'MULTIPLE-VALUE-LIST', 'NTH-VALUE',
   'MAKE-DICT',
   ':', 'CALL/CC',
})


def cek_eval( ctx, env, expr ) -> Any:
   """CEK machine evaluator."""
   C = expr
   E = env
   K = []

   while True:
      try:

         # ----------------------------------------------------------------
         # Deliver a value to the top continuation frame.
         # ----------------------------------------------------------------
         if _is_value(C):
            v = C.v if isinstance(C, _Val) else C
            if not K:
               return v
            frame = K.pop()
            C, E  = frame.step(v, E, K, ctx)
            continue

         # ----------------------------------------------------------------
         # Symbol lookup
         # ----------------------------------------------------------------
         if type(C) is LSymbol:
            try:
               C = _Val( E.lookup(C.name) )
            except KeyError:
               if C.isKeyArg():
                  C = _Val(C)
               else:
                  raise LRuntimeError( f'Unbound Variable: {C.name}.' )
            continue

         # ----------------------------------------------------------------
         # Empty list — NIL
         # ----------------------------------------------------------------
         if len(C) == 0:
            C = L_NIL
            continue

         # ----------------------------------------------------------------
         # Non-empty list — expression to reduce
         # ----------------------------------------------------------------
         head    = C[0]
         headStr = head.name if type(head) is LSymbol else None

         if headStr not in _SPECIAL_OPERATOR_SET:
            K.append( ArgFrame(None, list(C[1:]), [], E, 'call') )
            C = C[0]
            continue

         # ----------------------------------------------------------------
         # Special operators — existing
         # ----------------------------------------------------------------

         if headStr == 'QUOTE':
            C = _Val(C[1])
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

         # ----------------------------------------------------------------
         # Special operators — new
         # ----------------------------------------------------------------

         if headStr == 'LAMBDA':
            funcParams = C[1]
            funcBody   = list(C[2:])
            if funcBody and isinstance(funcBody[0], str):
               docString = funcBody[0]
               funcBody  = funcBody[1:]
            else:
               docString = ''
            Environment._validateNoDuplicateParams( funcParams )
            C = _Val( LFunction(LSymbol(''), funcParams, docString, funcBody,
                                capturedEnvironment=E) )
            continue

         if headStr == 'DEFMACRO':
            fnName     = C[1]
            funcParams = C[2]
            funcBody   = list(C[3:])
            if funcBody and isinstance(funcBody[0], str):
               docString = funcBody[0]
               funcBody  = funcBody[1:]
            else:
               docString = ''
            Environment._validateNoDuplicateParams( funcParams )
            macro = LMacro(fnName, funcParams, docString, funcBody)
            E.bindGlobal(fnName.name, macro)
            C = _Val(macro)
            continue

         if headStr == 'QUASIQUOTE':
            result = _lquasiquoteExpand(ctx, E, C[1])
            if ( isinstance(result, list) and
                 len(result) > 0 and
                 result[0] == LSymbol('UNQUOTE-SPLICING') ):
               raise LRuntimeError(
                  'Ill-placed ,@ (UNQUOTE-SPLICING): splice requires a list context.')
            C = _Val(result)
            continue

         if headStr == 'UNQUOTE':
            raise LRuntimeError(
               "ERROR 'UNQUOTE': UNQUOTE can only occur inside a QUASIQUOTE.\n"
               "PRIMITIVE USAGE: (UNQUOTE sexpr)")

         if headStr == 'UNQUOTE-SPLICING':
            raise LRuntimeError(
               "ERROR 'UNQUOTE-SPLICING': UNQUOTE-SPLICING can only occur inside a QUASIQUOTE.\n"
               "PRIMITIVE USAGE: (UNQUOTE-SPLICING sexpr)")

         if headStr == 'TRACE':
            tracer = ctx.tracer
            syms   = C[1:]
            if not syms:
               C = _Val( [LSymbol(name) for name in sorted(tracer.getFnsToTrace())] )
               continue
            for sym in syms:
               if not isinstance(sym, LSymbol):
                  raise LRuntimeError("trace: arguments must be symbols.")
               tracer.addFnTrace(sym.name)
            C = _Val( [LSymbol(name) for name in sorted(tracer.getFnsToTrace())] )
            continue

         if headStr == 'UNTRACE':
            tracer = ctx.tracer
            syms   = C[1:]
            if not syms:
               tracer.removeAll()
            else:
               for sym in syms:
                  if not isinstance(sym, LSymbol):
                     raise LRuntimeError("untrace: arguments must be symbols.")
                  tracer.removeFnTrace(sym.name)
            C = _Val( [LSymbol(name) for name in sorted(tracer.getFnsToTrace())] )
            continue

         if headStr == 'BLOCK':
            name_str = _block_name(C[1])
            body     = C[2:]
            if not body:
               C = L_NIL
               continue
            K.append( BlockFrame(name_str) )
            C, E = _eval_body(body, E, K)
            continue

         if headStr == 'RETURN-FROM':
            name_str = _block_name(C[1])
            K.append( ReturnFromFrame(name_str) )
            C = C[2] if len(C) > 2 else L_NIL
            continue

         if headStr == 'RETURN':
            K.append( ReturnFromFrame('NIL') )
            C = C[1] if len(C) > 1 else L_NIL
            continue

         if headStr == 'CATCH':
            body = list(C[2:])
            K.append( CatchTagFrame(body, E) )
            C = C[1]   # eval the tag
            continue

         if headStr == 'HANDLER-CASE':
            if len(C) < 2:
               raise LRuntimeError('handler-case: requires a protected form.')
            form    = C[1]
            clauses = []
            for clause in C[2:]:
               if not isinstance(clause, list) or len(clause) < 2:
                  raise LRuntimeError('handler-case: malformed clause.')
               ctype   = clause[0]
               var_lst = clause[1]
               body    = clause[2:]
               var     = var_lst[0] if isinstance(var_lst, list) and var_lst else None
               clauses.append( (ctype, var, body) )
            K.append( HandlerCaseBodyFrame(clauses, E) )
            C = form
            continue

         if headStr == 'MULTIPLE-VALUE-BIND':
            var_list    = C[1]
            values_form = C[2]
            body        = list(C[3:])
            K.append( MVBindFrame(var_list, body, E) )
            C = values_form
            continue

         if headStr == 'MULTIPLE-VALUE-LIST':
            K.append( MVListFrame() )
            C = C[1]
            continue

         if headStr == 'NTH-VALUE':
            K.append( NthValueBodyFrame(C[2], E) )
            C = C[1]   # eval n first
            continue

         if headStr == 'MAKE-DICT':
            pairs = C[1:]
            if not pairs:
               C = _Val({})
               continue
            all_pairs             = [(p[0], p[1]) for p in pairs]
            first_key, first_form = all_pairs[0]
            K.append( DictBuildFrame(first_key, all_pairs[1:], {}, E) )
            C = first_form
            continue

         if headStr == ':':
            if len(C) < 2:
               raise LRuntimeError("ERROR ':': requires at least a root argument.\nPRIMITIVE USAGE: (: module-or-pkg &rest path)")
            K.append( ColonFrame(list(C[2:]), E) )
            C = C[1]   # eval the root
            continue

         if headStr == 'CALL/CC':
            token = object()
            cont  = LContinuation(token)
            K.append( CallCCProcFrame(token, cont) )
            C = C[1]   # eval the procedure
            continue

      except ReturnFrom as e:
         for i in reversed(range(len(K))):
            if isinstance(K[i], BlockFrame) and K[i].name == e.name:
               del K[i:]
               C = _Val(e.value)
               break
         else:
            raise   # no block in this machine — propagate to outer cek_eval or rawEval
         continue

      except Thrown as e:
         for i in reversed(range(len(K))):
            if isinstance(K[i], CatchBodyFrame) and eql(K[i].tag, e.tag):
               del K[i:]
               C = _Val(e.value)
               break
         else:
            raise   # no catch in this machine — propagate to outer cek_eval or rawEval
         continue

      except Signaled as e:
         ctype_sym = e.condition.get('CONDITION-TYPE', LSymbol('ERROR'))
         type_name = ctype_sym.name if isinstance(ctype_sym, LSymbol) else 'ERROR'
         for i in reversed(range(len(K))):
            if isinstance(K[i], HandlerCaseBodyFrame):
               var, body = K[i].find_handler(type_name)
               if body is not None:
                  frame   = K[i]
                  del K[i:]
                  new_env = Environment(frame.env, evalFn=ctx.lEval)
                  if var is not None:
                     new_env.bindLocal(var.name, e.condition)
                  C, E = _eval_body(body, new_env, K)
                  break
         else:
            raise
         continue

      except LRuntimeError as e:
         for i in reversed(range(len(K))):
            if isinstance(K[i], HandlerCaseBodyFrame):
               var, body = K[i].find_handler('ERROR')
               if body is not None:
                  frame   = K[i]
                  del K[i:]
                  new_env = Environment(frame.env, evalFn=ctx.lEval)
                  cond    = {'CONDITION-TYPE': LSymbol('ERROR'), 'MESSAGE': str(e)}
                  if var is not None:
                     new_env.bindLocal(var.name, cond)
                  C, E = _eval_body(body, new_env, K)
                  break
         else:
            raise
         continue

      except ContinuationInvoked as e:
         for i in reversed(range(len(K))):
            if isinstance(K[i], CallCCGuardFrame) and K[i].token is e.token:
               del K[i:]
               C = _Val(e.value)
               break
         else:
            raise   # no matching guard — propagate to outer cek_eval or rawEval
         continue

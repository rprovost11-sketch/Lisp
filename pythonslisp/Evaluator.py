"""CEK Machine evaluator for Python's Lisp.

The machine state is a triple (C, E, K):
  C - Control     : current expression to reduce, OR a _Val-wrapped value to deliver
  E - Environment : current lexical environment
  K - Kontinuation: explicit continuation stack (Python list of frame objects)

Key invariant
-------------
In _lEval a recursive call always returns a value directly (return result).
In the CEK loop, a looked-up or computed value would be set as C and looped -
but that would misidentify a data list as code to evaluate.

Solution: wrap every computed value in _Val().  The loop's first check tests
whether C is a value: not a LSymbol AND not a non-empty list.  _Val-tagged
objects and self-evaluating atoms satisfy this; non-empty lists are always
treated as expressions (source code) to reduce.

Frames that return expressions  → return (ast_node, env)
Frames that return values       → return (_Val(v),  env)
"""
from __future__ import annotations
from typing import Any

from pythonslisp.AST import ( LSymbol, L_NIL, L_T,
                               LCallable, LFunction, LPrimitive, LSpecialOperator, LMacro, LContinuation,
                               LMultipleValues, LList, eql, prettyPrintSExpr )
from pythonslisp.Exceptions import ( LRuntimeError, LArgBindingError,
                                      ContinuationInvoked, ReturnFrom,
                                      Thrown, Signaled )
from pythonslisp.Environment  import Environment, ModuleEnvironment
from pythonslisp.Expander     import Expander
from pythonslisp.LambdaList   import compileLambdaList


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




def _extract_vardefs( vardefs ) -> list:
   """Convert a LET/LET* vardef list to [(LSymbol, init_form), ...] pairs."""
   pairs = []
   for varSpec in vardefs:
      if type(varSpec) is LSymbol:
         pairs.append( (varSpec, L_NIL) )
      elif len(varSpec) == 1:
         pairs.append( (varSpec[0], L_NIL) )
      else:
         pairs.append( (varSpec[0], varSpec[1]) )
   return pairs


def _eval_body( body, env, K ) -> tuple:
   """Return (C, E) to begin evaluating a body sequence.
   Pushes PrognFrame for all but the last form (which is in tail position).
   Always returns an expression (source AST node), never a _Val."""
   if not body:
      return L_NIL, env         # L_NIL is an empty list - _is_value → True, delivered as NIL
   if len(body) == 1:
      return body[0], env       # tail form - expression
   K.append( PrognFrame(list(body[1:]), env) )
   return body[0], env          # first non-tail form - expression


def _block_name( obj ):
   if type(obj) is LSymbol:
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

   def copy( self ):
      return self

   def step( self, value, E, K, ctx ):
      # Returns an expression (AST node) - not _Val-wrapped.
      branch = self.then_expr if _lTrue(_primary(value)) else self.else_expr
      return branch, self.env


class PrognFrame:
   """Sequence non-tail body forms; deliver the last form into tail position."""
   __slots__ = ('remaining', 'env')

   def __init__( self, remaining, env ):
      self.remaining = remaining   # list of forms; the last IS the tail form
      self.env       = env

   def copy( self ):
      return PrognFrame( list(self.remaining), self.env )

   def step( self, value, E, K, ctx ):
      # The incoming value (previous form's result) is discarded.
      # Returns an expression - not _Val-wrapped.
      if len(self.remaining) == 1:
         return self.remaining[0], self.env        # tail - TCO
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

   def copy( self ):
      return LetFrame( self.current_name, list(self.pending), dict(self.bound), self.body, self.outer_env )

   def step( self, value, E, K, ctx ):
      self.bound[self.current_name.name] = _primary(value)
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

   def copy( self ):
      return LetStarFrame( self.current_name, list(self.pending), self.inner_env, self.body )

   def step( self, value, E, K, ctx ):
      self.inner_env.bindLocalSym( self.current_name, _primary(value) )
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

   def copy( self ):
      return SetqFrame( self.lval_name, list(self.pending), self.env )

   def step( self, value, E, K, ctx ):
      rval = _primary(value)
      if (type(rval) is LMacro or type(rval) is LFunction) and rval.name == '':
         rval.name = self.lval_name.name
      self.env.bindSym( self.lval_name, rval )
      if self.pending:
         name, form = self.pending[0]
         self.lval_name = name
         self.pending   = self.pending[1:]
         K.append(self)
         return form, self.env          # expression
      return _Val(rval), self.env       # value - wrap so it isn't re-evaluated


class CondFrame:
   """Evaluate COND tests; take the body of the first true clause."""
   __slots__ = ('clause_body', 'remaining_clauses', 'env')

   def __init__( self, clause_body, remaining_clauses, env ):
      self.clause_body       = clause_body
      self.remaining_clauses = remaining_clauses
      self.env               = env

   def copy( self ):
      return CondFrame( list(self.clause_body), list(self.remaining_clauses), self.env )

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

   def copy( self ):
      return self

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
     'call'    - common call path; macros are inline-expanded
     'funcall' - FUNCALL; rejects macros
     'apply'   - APPLY;   like funcall but spreads the final list arg
   """
   __slots__ = ('fn', 'pending', 'done', 'env', 'mode', 'call_form')

   def __init__( self, fn, pending, done, env, mode='call' ):
      self.fn      = fn
      self.pending = pending
      self.done    = done
      self.env     = env
      self.mode    = mode
      # call_form is NOT set here; _cek_eval_traced sets it after construction

   def copy( self ):
      f = ArgFrame( self.fn, list(self.pending), list(self.done), self.env, self.mode )
      cf = getattr( self, 'call_form', None )
      if cf is not None:
         f.call_form = cf
      return f

   def step( self, value, E, K, ctx ):
      # ------------------------------------------------------------------
      # Phase 1: value IS the function.
      # ------------------------------------------------------------------
      if self.fn is None:
         fn = _primary(value)

         if self.mode == 'funcall':
            if not isinstance(fn, LCallable):
               raise LRuntimeError( 'Invalid argument 1. CALLABLE expected.' )
            if type(fn) is LContinuation:
               if len(self.pending) != 1:
                  raise LRuntimeError( f'Continuation expects exactly 1 argument, got {len(self.pending)}.' )
               K.append( _ContinuationInvokeFrame(fn) )
               return self.pending[0], self.env   # expression
            if type(fn) is LMacro:
               raise LRuntimeError( f'Invalid argument 1. CALLABLE expected; macros are not callable via FUNCALL.' )
            if isinstance(fn, LSpecialOperator):
               raise LRuntimeError( 'Invalid argument 1. CALLABLE expected; special forms are not callable.' )


         elif self.mode == 'apply':
            if isinstance(fn, LCallable):
               pass
            elif type(fn) is LSymbol:
               try:
                  fn = self.env.lookupSym(fn)
               except KeyError:
                  raise LRuntimeError( f'APPLY: "{fn}" is not bound to a callable.' )
               if not isinstance(fn, LCallable):
                  raise LRuntimeError( f'APPLY: "{fn}" is not bound to a callable.' )
            else:
               raise LRuntimeError( 'Invalid argument 1. CALLABLE or SYMBOL expected.' )
            if type(fn) is LMacro:
               raise LRuntimeError( f'Invalid argument 1. CALLABLE expected; macros cannot be applied.' )
            if type(fn) is LPrimitive and fn.name in _SPECIAL_OPERATOR_SET:
               raise LRuntimeError( 'Invalid argument 1. CALLABLE expected; special forms cannot be applied.' )

         else:  # 'call'
            if not isinstance(fn, LCallable):
               raise LRuntimeError( f"Expression head is not callable: '{fn}'." )
            if type(fn) is LContinuation:
               if len(self.pending) != 1:
                  raise LRuntimeError( f'Continuation expects exactly 1 argument, got {len(self.pending)}.' )
               K.append( _ContinuationInvokeFrame(fn) )
               return self.pending[0], self.env   # expression
            if type(fn) is LMacro:
               # Inline macro expansion - return the expansion as an expression.
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
            raise LRuntimeError( 'Invalid last argument. LIST expected.' )
         if type(self.fn) is LContinuation:
            spread = self.done + list_arg
            if len(spread) != 1:
               raise LRuntimeError( f'Continuation expects exactly 1 argument, got {len(spread)}.' )
            _do_wind_transition( ctx, self.fn.wind_stack, E )
            K[:] = _copy_k( self.fn.saved_k )
            return _Val( spread[0] ), E
         self.done.extend(list_arg)

      return _do_apply( self.fn, self.done, self.env, K, ctx )


class _ContinuationInvokeFrame:
   """Receives the single argument and performs the continuation jump inline."""
   __slots__ = ('continuation',)

   def __init__( self, continuation ):
      self.continuation = continuation

   def copy( self ):
      return self

   def step( self, value, E, K, ctx ):
      _do_wind_transition( ctx, self.continuation.wind_stack, E )
      K[:] = _copy_k( self.continuation.saved_k )
      return _Val( _primary(value) ), E


class BodyFrame:
   """Evaluate a traced LFunction body; fire the exit trace after the last form."""
   __slots__ = ('remaining', 'fn', 'depth', 'env', 'tracer')

   def __init__( self, remaining, fn, depth, env, tracer ):
      self.remaining = remaining
      self.fn        = fn
      self.depth     = depth
      self.env       = env
      self.tracer    = tracer

   def copy( self ):
      return BodyFrame( list(self.remaining), self.fn, self.depth, self.env, self.tracer )

   def step( self, value, E, K, ctx ):
      if self.remaining:
         nxt            = self.remaining[0]
         self.remaining = self.remaining[1:]
         K.append(self)
         return nxt, self.env         # expression
      self.tracer.setMaxTraceDepth( self.depth )
      self.tracer.trace( 'exit', self.fn, value, self.depth, ctx.outStrm )
      return _Val(value), E           # VALUE - wrap so it isn't re-evaluated


# ---------------------------------------------------------------------------
# New frame classes for inlined special operators
# ---------------------------------------------------------------------------

class BlockFrame:
   """Marks an active block on K; receives the block body's final value."""
   __slots__ = ('name',)
   def __init__( self, name ):
      self.name = name
   def copy( self ):
      return self
   
   def step( self, value, E, K, ctx ):
      return _Val(value), E


class ReturnFromFrame:
   """Receives return-from value; unwinds K to matching BlockFrame."""
   __slots__ = ('name',)
   def __init__( self, name ):
      self.name = name
   def copy( self ):
      return self
   
   def step( self, value, E, K, ctx ):
      for i in reversed(range(len(K))):
         if isinstance(K[i], BlockFrame) and K[i].name == self.name:
            _unwind_dynwind_above(K, i + 1, ctx, E)
            del K[i:]   # removes BlockFrame and everything above it
            return _Val(value), E
      raise ReturnFrom(self.name, _primary(value))


class CatchTagFrame:
   """Receives evaluated catch tag; pushes CatchBodyFrame and starts body."""
   __slots__ = ('body', 'env')
   def __init__( self, body, env ):
      self.body = body
      self.env  = env
      
   def copy( self ):
      return self
   
   def step( self, tag, E, K, ctx ):
      K.append( CatchBodyFrame(_primary(tag)) )
      return _eval_body(self.body, self.env, K)


class CatchBodyFrame:
   """K-stack marker for throw unwinding; passes normal completion through."""
   __slots__ = ('tag',)
   def __init__( self, tag ):
      self.tag = tag
   
   def copy( self ):
      return self
   
   def step( self, value, E, K, ctx ):
      return _Val(_primary(value)), E


class HandlerCaseBodyFrame:
   """K-stack marker for signal/error handling; passes normal completion through."""
   __slots__ = ('clauses', 'env')
   def __init__( self, clauses, env ):
      # clauses: list of (type_sym, var_sym_or_None, body_forms)
      self.clauses = clauses
      self.env     = env
   
   def copy( self ):
      return self
   
   def find_handler( self, type_name ):
      for ctype, var, body in self.clauses:
         if type(ctype) is LSymbol:
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
   
   def copy( self ):
      return self
   
   def step( self, value, E, K, ctx ):
      vals = value.values if type(value) is LMultipleValues else [value]
      new_env = Environment(self.outer_env, evalFn=ctx.lEval)
      for i, var in enumerate(self.var_list):
         new_env.bindLocalSym(var, vals[i] if i < len(vals) else L_NIL)
      return _eval_body(self.body, new_env, K)


class MVListFrame:
   """Receives form result; returns all values as a list."""
   __slots__ = ()
   def copy( self ):
      return self
   
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
   
   def copy( self ):
      return self
   
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
   
   def copy( self ):
      return self
   
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
   
   def copy( self ):
      return DictBuildFrame( self.current_key, list(self.remaining), dict(self.built), self.env )
   
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
   
   def copy( self ):
      return self
   
   def step( self, root, E, K, ctx ):
      from pythonslisp.AST import prettyPrint
      _USAGE = "PRIMITIVE USAGE: (: module-or-pkg &rest path)"
      current = _primary(root)
      for sym in self.path:
         if type(sym) is not LSymbol:
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
   __slots__ = ('cont',)
   def __init__( self, cont ):
      self.cont = cont
   
   def copy( self ):
      return self
   
   def step( self, value, E, K, ctx ):
      proc = _primary(value)
      if not isinstance( proc, LCallable ):
         raise LRuntimeError( "ERROR 'CALL/CC': Invalid argument 1. CALLABLE expected.\nSPECIAL OPERATOR USAGE: (CALL/CC procedure)" )
      if type(proc) is LMacro:
         raise LRuntimeError( "ERROR 'CALL/CC': Invalid argument 1. CALLABLE expected; macros are not callable.\nSPECIAL OPERATOR USAGE: (CALL/CC procedure)" )
      if isinstance(proc, LSpecialOperator):
         raise LRuntimeError( "ERROR 'CALL/CC': Invalid argument 1. CALLABLE expected; special forms are not callable.\nSPECIAL OPERATOR USAGE: (CALL/CC procedure)" )
      K.append( ArgFrame(None, [_Val(self.cont)], [], E, 'funcall') )
      return _Val(proc), E


# ---------------------------------------------------------------------------
# Continuation helpers
# ---------------------------------------------------------------------------

def _copy_k( K: list ) -> list:
   """Return a snapshot of K suitable for storing in an LContinuation.
   Mutable frames are copied; immutable frames share the original instance."""
   return [ f.copy() for f in K ]


# ---------------------------------------------------------------------------
# dynamic-wind helpers
# ---------------------------------------------------------------------------

def _common_prefix_len( s1: list, s2: list ) -> int:
   """Return the length of the longest common prefix of two wind stacks,
   comparing entries by identity."""
   n = min( len(s1), len(s2) )
   for i in range(n):
      if s1[i] is not s2[i]:
         return i
   return n


def _unwind_dynwind_above( K: list, from_idx: int, ctx, env ) -> None:
   """Call after thunks for all DynWindFrames at indices >= from_idx in K.
   Removes those frames from K.  Calls innermost (rightmost) first."""
   above      = K[from_idx:]
   dw_frames  = [ f for f in reversed(above) if isinstance(f, DynWindFrame) ]
   K[from_idx:] = [ f for f in above if not isinstance(f, DynWindFrame) ]
   for frame in dw_frames:
      if frame.wind_entry in ctx.wind_stack:
         ctx.wind_stack.remove( frame.wind_entry )
      cek_apply( ctx, env, frame.after_fn, [] )


def _do_wind_transition( ctx, target_wind_stack: list, env ) -> None:
   """Transition ctx.wind_stack from its current state to target_wind_stack.
   Calls after thunks for entries being exited (innermost first) and
   before thunks for entries being entered (outermost first)."""
   common = _common_prefix_len( ctx.wind_stack, target_wind_stack )
   while len(ctx.wind_stack) > common:
      entry = ctx.wind_stack.pop()
      cek_apply( ctx, env, entry[1], [] )   # after
   for entry in target_wind_stack[common:]:
      ctx.wind_stack.append( entry )
      cek_apply( ctx, env, entry[0], [] )   # before


# ---------------------------------------------------------------------------
# dynamic-wind frame classes
# ---------------------------------------------------------------------------

class DynWindCollectFrame:
   """Collect before/thunk/after callables; call before; chain to DynWindExecuteFrame."""
   __slots__ = ('forms', 'done', 'env')

   def __init__( self, forms, done, env ):
      self.forms = forms   # remaining unevaluated forms (thunk, after)
      self.done  = done    # evaluated callables so far
      self.env   = env

   def copy( self ):
      return DynWindCollectFrame( list(self.forms), list(self.done), self.env )

   def step( self, value, E, K, ctx ):
      self.done.append( _primary(value) )
      if self.forms:
         nxt        = self.forms[0]
         self.forms = self.forms[1:]
         K.append( self )
         return nxt, self.env
      # All three args collected
      before_fn, thunk_fn, after_fn = self.done
      for fn, label in ( (before_fn, 'before'), (thunk_fn, 'thunk'), (after_fn, 'after') ):
         if not isinstance(fn, LCallable) or type(fn) is LMacro:
            raise LRuntimeError( f'Invalid {label} argument. CALLABLE expected.' )
      wind_entry = [before_fn, after_fn]
      K.append( DynWindExecuteFrame(thunk_fn, after_fn, wind_entry, self.env) )
      return _do_apply( before_fn, [], self.env, K, ctx )


class DynWindExecuteFrame:
   """Before thunk done; push wind entry and call the thunk."""
   __slots__ = ('thunk_fn', 'after_fn', 'wind_entry', 'env')

   def __init__( self, thunk_fn, after_fn, wind_entry, env ):
      self.thunk_fn   = thunk_fn
      self.after_fn   = after_fn
      self.wind_entry = wind_entry
      self.env        = env

   def copy( self ):
      return DynWindExecuteFrame( self.thunk_fn, self.after_fn, list(self.wind_entry), self.env )

   def step( self, before_result, E, K, ctx ):
      ctx.wind_stack.append( self.wind_entry )
      K.append( DynWindFrame(self.after_fn, self.wind_entry, self.env) )
      return _do_apply( self.thunk_fn, [], self.env, K, ctx )


class DynWindFrame:
   """Marks the active dynamic-wind scope while the thunk runs.
   On normal exit: pops the wind entry and calls after."""
   __slots__ = ('after_fn', 'wind_entry', 'env')

   def __init__( self, after_fn, wind_entry, env ):
      self.after_fn   = after_fn
      self.wind_entry = wind_entry
      self.env        = env

   def copy( self ):
      return DynWindFrame( self.after_fn, list(self.wind_entry), self.env )

   def step( self, thunk_result, E, K, ctx ):
      thunk_val = _primary(thunk_result)
      if self.wind_entry in ctx.wind_stack:
         ctx.wind_stack.remove( self.wind_entry )
      K.append( DynWindReturnFrame(thunk_val, self.env) )
      return _do_apply( self.after_fn, [], self.env, K, ctx )


class DynWindReturnFrame:
   """After thunk completed normally; deliver the saved thunk value."""
   __slots__ = ('thunk_val', 'env')

   def __init__( self, thunk_val, env ):
      self.thunk_val = thunk_val
      self.env       = env

   def copy( self ):
      return self

   def step( self, after_result, E, K, ctx ):
      return _Val(self.thunk_val), self.env


# ---------------------------------------------------------------------------
# Apply helper
# ---------------------------------------------------------------------------

def _do_apply( fn, args, env, K, ctx ) -> tuple:
   """Perform a function application.  Returns (C, E) for the machine loop.
   LPrimitive results are _Val-wrapped (values).
   LFunction results are expressions (body to evaluate) - no wrapping."""
   tracer  = ctx.tracer
   printed = False
   if tracer._active:   # inlined for performance; see Tracer._active
      depth   = tracer.getMaxTraceDepth()
      printed = tracer.trace( 'enter', fn, args, depth, ctx.outStrm )
      if printed:
         tracer.setMaxTraceDepth( depth + 1 )

   try:
      if isinstance(fn, LPrimitive):
         if fn.compiledLambdaList:
            kw_env = Environment( env, evalFn=ctx.lEval )
            kw_env.bindArguments( fn.compiledLambdaList, args )
            result = fn.pythonFn( ctx, kw_env, args )
         else:
            result = fn.pythonFn( ctx, env, args )
         if printed:
            tracer.setMaxTraceDepth( depth )
            tracer.trace( 'exit', fn, result, depth, ctx.outStrm )
         return _Val(result), env   # VALUE - wrap; LMultipleValues preserved for frames to strip

      else:  # LFunction
         new_env = Environment( fn.capturedEnvironment, evalFn=ctx.lEval )
         new_env.bindArguments( fn.compiledLambdaList, args )
         body = fn.bodyAST
         if not body:
            if printed:
               tracer.setMaxTraceDepth( depth )
               tracer.trace( 'exit', fn, L_NIL, depth, ctx.outStrm )
            return L_NIL, env         # L_NIL → _is_value True, delivered as NIL
         if printed:
            # Traced: cannot TCO - BodyFrame observes the return value.
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
   if type( function ) is LContinuation:
      if len(args) != 1:
         raise LRuntimeError( f'Continuation expects exactly 1 argument, got {len(args)}.' )
      raise ContinuationInvoked( function.saved_k, function.wind_stack, args[0] )

   # Use _do_apply directly; run the full loop with the returned state.
   K = []
   C, E = _do_apply( function, list(args), env, K, ctx )
   return _primary( _run_loop( C, E, K, ctx ) )


def _run_loop( C, E, K, ctx ) -> Any:
   """Run the CEK loop starting from (C, E, K).
   Returns the final value (not primary-stripped)."""
   while True:
      if type(C) is not LSymbol and not (isinstance(C, list) and C):
         v = C.v if type(C) is _Val else C
         if not K:
            return v
         frame = K.pop()
         C, E  = frame.step(v, E, K, ctx)
         continue

      if type(C) is LSymbol:
         try:
            C = _Val( E.lookupSym(C) )
         except KeyError:
            if C.isKeyword():
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
      sub_result = ctx.lEval( E, C )
      C = _Val( sub_result )
      continue


# ---------------------------------------------------------------------------
# The CEK machine loop
# ---------------------------------------------------------------------------

_stack_traces_enabled: bool = False   # toggled by Interpreter.set_tracing()

def set_stack_traces( enabled: bool ) -> None:
   """Enable or disable call-stack tracking in cek_eval."""
   global _stack_traces_enabled
   _stack_traces_enabled = enabled


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
   ':', 'CALL/CC', 'DYNAMIC-WIND',
})


def cek_eval( ctx, env, expr ) -> Any:
   """CEK machine evaluator.
   When _stack_traces_enabled is True, ArgFrame.call_form is recorded on every
   call and a call stack is built on unhandled LRuntimeError."""
   C = expr
   E = env
   K = []
   _last_call_form = None   # most recent non-special function call form; used for error annotation

   while True:
      try:

         # ----------------------------------------------------------------
         # Deliver a value to the top continuation frame.
         # ----------------------------------------------------------------
         if type(C) is not LSymbol and not (isinstance(C, list) and C):
            v = C.v if type(C) is _Val else C
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
               C = _Val( E.lookupSym(C) )
            except KeyError:
               if C.isKeyword():
                  C = _Val(C)
               else:
                  raise LRuntimeError( f'Unbound Variable: {C.name}.' )
            continue

         # ----------------------------------------------------------------
         # Empty list - NIL
         # ----------------------------------------------------------------
         if len(C) == 0:
            C = L_NIL
            continue

         # ----------------------------------------------------------------
         # Non-empty list - expression to reduce
         # ----------------------------------------------------------------
         head    = C[0]
         headStr = head.name if type(head) is LSymbol else None

         if headStr not in _SPECIAL_OPERATOR_SET:
            _last_call_form = C   # capture before C is overwritten; used by error handler
            K.append( ArgFrame(None, list(C[1:]), [], E) )
            if _stack_traces_enabled:
               K[-1].call_form = C
            C = C[0]
            continue

         # ----------------------------------------------------------------
         # Special operators - existing
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
               pairs.append( (C[i], C[i + 1]) )
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
         # Special operators - new
         # ----------------------------------------------------------------

         if headStr == 'LAMBDA':
            funcParams = C[1]
            funcBody   = list(C[2:])
            if funcBody and isinstance(funcBody[0], str):
               docString = funcBody[0]
               funcBody  = funcBody[1:]
            else:
               docString = ''
            compiledLL = compileLambdaList( funcParams )
            C = _Val( LFunction(LSymbol(''), funcParams, docString, funcBody,
                                capturedEnvironment=E, compiledLambdaList=compiledLL) )
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
            compiledLL = compileLambdaList( funcParams, destructuring=True )
            macro = LMacro(fnName, funcParams, docString, funcBody, compiledLambdaList=compiledLL)
            E.bindGlobalSym(fnName, macro)
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
               if type(sym) is not LSymbol:
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
                  if type(sym) is not LSymbol:
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
            cont = LContinuation( _copy_k(K), list(ctx.wind_stack) )
            K.append( CallCCProcFrame(cont) )
            C = C[1]   # eval the procedure
            continue

         if headStr == 'DYNAMIC-WIND':
            if len(C) != 4:
               raise LRuntimeError( 'dynamic-wind: requires exactly 3 arguments (before thunk after).' )
            K.append( DynWindCollectFrame( [C[2], C[3]], [], E ) )
            C = C[1]   # eval before first
            continue

      except ReturnFrom as e:
         handler_idx = None
         for i in reversed(range(len(K))):
            if isinstance(K[i], BlockFrame) and K[i].name == e.name:
               handler_idx = i
               break
         if handler_idx is None:
            _unwind_dynwind_above(K, 0, ctx, E)
            raise   # no block in this machine - propagate to outer cek_eval or rawEval
         _unwind_dynwind_above(K, handler_idx + 1, ctx, E)
         del K[handler_idx:]
         C = _Val(e.value)
         continue

      except Thrown as e:
         handler_idx = None
         for i in reversed(range(len(K))):
            if isinstance(K[i], CatchBodyFrame) and eql(K[i].tag, e.tag):
               handler_idx = i
               break
         if handler_idx is None:
            _unwind_dynwind_above(K, 0, ctx, E)
            raise   # no catch in this machine - propagate to outer cek_eval or rawEval
         _unwind_dynwind_above(K, handler_idx + 1, ctx, E)
         del K[handler_idx:]
         C = _Val(e.value)
         continue

      except Signaled as e:
         ctype_sym   = e.condition.get('CONDITION-TYPE', LSymbol('ERROR'))
         type_name   = ctype_sym.name if type(ctype_sym) is LSymbol else 'ERROR'
         handler_idx = None
         handler_frame = None
         for i in reversed(range(len(K))):
            if isinstance(K[i], HandlerCaseBodyFrame):
               var, body = K[i].find_handler(type_name)
               if body is not None:
                  handler_idx   = i
                  handler_frame = K[i]
                  break
         if handler_idx is None:
            _unwind_dynwind_above(K, 0, ctx, E)
            raise
         _unwind_dynwind_above(K, handler_idx + 1, ctx, E)
         del K[handler_idx:]
         new_env = Environment(handler_frame.env, evalFn=ctx.lEval)
         if var is not None:
            new_env.bindLocalSym(var, e.condition)
         C, E = _eval_body(body, new_env, K)
         continue

      except LRuntimeError as e:
         if e.source_info is None and isinstance( _last_call_form, LList ) and _last_call_form.has_source_info():
            e.source_info = ( _last_call_form.filename, _last_call_form.line_num,
                               _last_call_form.col_num, _last_call_form.source_line )
         handler_idx   = None
         handler_frame = None
         for i in reversed(range(len(K))):
            if isinstance(K[i], HandlerCaseBodyFrame):
               var, body = K[i].find_handler('ERROR')
               if body is not None:
                  handler_idx   = i
                  handler_frame = K[i]
                  break
         if handler_idx is None:
            if _stack_traces_enabled and not e.call_stack:
               frames = []
               for frame in K:
                  cf = getattr( frame, 'call_form', None )
                  if ( isinstance(frame, ArgFrame)
                       and frame.fn is not None
                       and isinstance(cf, LList)
                       and cf.has_source_info()
                       and cf.filename ):
                     fn_name = frame.fn.name or '(anonymous)'
                     indent  = ' ' * (cf.col_num - 1)
                     frames.append( f'  {fn_name}  "{cf.filename}" ({cf.line_num},{cf.col_num})\n  {cf.source_line}\n  {indent}^' )
               e.call_stack = frames
            _unwind_dynwind_above(K, 0, ctx, E)
            raise
         _unwind_dynwind_above(K, handler_idx + 1, ctx, E)
         del K[handler_idx:]
         new_env = Environment(handler_frame.env, evalFn=ctx.lEval)
         cond    = {'CONDITION-TYPE': LSymbol('ERROR'), 'MESSAGE': e.args[0] if e.args else ''}
         if var is not None:
            new_env.bindLocalSym(var, cond)
         C, E = _eval_body(body, new_env, K)
         continue

      except ContinuationInvoked as e:
         _do_wind_transition(ctx, e.saved_wind_stack, E)
         K[:] = _copy_k( e.saved_k )
         C    = _Val( e.value )
         continue

"""
Expander - Macro expansion and structural normalization as a single pass.

expand() walks the AST once, interleaving top-down macro expansion with
bottom-up structural normalization.  The result is ready for Analyzer
and then cek_eval.
"""
from __future__ import annotations

from typing import Any
from pythonslisp.Environment import Environment
from pythonslisp.AST import LSymbol, LMacro, LList, L_NIL, prettyPrintSExpr
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LArgBindingError, LRuntimeError


class Expander:
   """Handles macro expansion and structural normalization as a single pass."""

   @staticmethod
   def expand(ctx: Context, env: Environment, sexpr: Any, maxIterations: int = 1000) -> Any:
      """
      Walk sexpr once, expanding macros top-down and normalizing bottom-up.

      Normalizations applied (all zero-behavior-change):
        (if cond then)       → (if cond then nil)
        (progn)              → nil
        (progn e)            → e
        (let  () body ...)   → (progn body ...)
        (let* () body ...)   → (progn body ...)
      """
      return Expander._expand(ctx, env, sexpr, maxIterations)

   @staticmethod
   def _expand(ctx: Context, env: Environment, sexpr: Any, maxIterations: int = 1000) -> Any:
      # Atoms and symbols pass through unchanged
      if isinstance(sexpr, LSymbol):
         return sexpr
      if not isinstance(sexpr, list) or len(sexpr) == 0:
         return sexpr

      # Don't expand or normalize inside QUOTE or QUASIQUOTE - they are data / templates
      if isinstance(sexpr[0], LSymbol) and sexpr[0].name in ('QUOTE', 'QUASIQUOTE'):
         return sexpr

      # --- Step 1: macro expand at this level (top-down, fixed-point) ---
      expandedOnce = Expander._expandOnce(ctx, env, sexpr)
      if expandedOnce is not sexpr:
         # A macro fired - recurse on the result (handles nested macros)
         if maxIterations <= 1:
            raise RuntimeError("Macro expansion limit exceeded - possible infinite macro loop.")
         return Expander._expand(ctx, env, expandedOnce, maxIterations - 1)

      # --- Step 2: recurse into sub-elements (bottom-up) ---
      # Preserve LList type so source position info survives expansion
      if isinstance(sexpr, LList):
         expanded = LList( [Expander._expand(ctx, env, elt, maxIterations) for elt in sexpr],
                           filename=sexpr.filename, line_num=sexpr.line_num,
                           col_num=sexpr.col_num, source_line=sexpr.source_line )
      else:
         expanded = [Expander._expand(ctx, env, elt, maxIterations) for elt in sexpr]

      # --- Step 3: apply structural normalization rules ---
      if not isinstance(expanded[0], LSymbol):
         return expanded

      head = expanded[0].name

      # (if cond then) → (if cond then nil)
      if head == 'IF' and len(expanded) == 3:
         return expanded + [L_NIL]

      # (progn) → nil
      if head == 'PROGN' and len(expanded) == 1:
         return L_NIL

      # (progn e) → e
      if head == 'PROGN' and len(expanded) == 2:
         return expanded[1]

      # (let () body ...) / (let* () body ...) → (progn body ...)
      if head in ('LET', 'LET*') and len(expanded) >= 2:
         bindings = expanded[1]
         if isinstance(bindings, list) and len(bindings) == 0:
            body = expanded[2:]
            return [LSymbol('PROGN')] + body

      return expanded

   @staticmethod
   def _expandOnce(ctx: Context, env: Environment, sexpr: list) -> Any:
      """
      Attempt to expand sexpr if it's a macro call.
      Returns expanded form if it's a macro, otherwise returns sexpr unchanged.
      """
      if not isinstance(sexpr, list) or len(sexpr) == 0:
         return sexpr

      head = sexpr[0]

      if isinstance(head, LSymbol):
         try:
            callableObj = env.lookup(head.name)
            if isinstance(callableObj, LMacro):
               args = sexpr[1:]
               try:
                  return Expander.expandMacroCall(ctx, env, callableObj, args)
               except LArgBindingError as ex:
                  msg = (f'Error binding arguments in call to macro {head.name}.\n'
                         f'{ex.args[-1]}\n'
                         f'{callableObj.usageString()}')
                  err = LRuntimeError( msg )
                  if isinstance(sexpr, LList) and sexpr.has_source_info():
                     err.source_info = ( sexpr.filename, sexpr.line_num,
                                         sexpr.col_num, sexpr.source_line )
                  raise err
         except KeyError:
            pass

      return sexpr

   @staticmethod
   def expandMacroCall(ctx: Context, env: Environment, macro: LMacro, argsList: list) -> Any:
      """Expand a single macro call and return the unevaluated expansion."""
      expansionEnv = Environment(env, evalFn=ctx.lEval)
      expansionEnv.bindArguments(macro.lambdaListAST, argsList, destructuring=True)

      result = L_NIL
      for bodySExpr in macro.bodyAST:
         result = ctx.lEval(expansionEnv, bodySExpr)

      return result

   @staticmethod
   def expandWithDebug(ctx: Context, env: Environment, sexpr: Any, maxIterations: int = 1000) -> tuple[Any, list[str]]:
      """
      Expand macros and return both result and expansion trace.
      Useful for debugging and understanding macro expansion.

      Returns:
         (expanded_sexpr, trace_list)
      """
      trace = []
      iterationsRemaining = [maxIterations]   # mutable container for nested access

      def expandTraced(sexpr, depth=0):
         indent = "  " * depth

         if isinstance(sexpr, list) and len(sexpr) > 0:
            # Don't expand inside quote or quasiquote - content is literal data/template
            if isinstance(sexpr[0], LSymbol) and sexpr[0].name in ('QUOTE', 'QUASIQUOTE'):
               return sexpr
            head = sexpr[0]
            if isinstance(head, LSymbol):
               try:
                  fn = env.lookup(head.name)
                  if isinstance(fn, LMacro):
                     if iterationsRemaining[0] <= 0:
                        raise RuntimeError("Macro expansion limit exceeded - possible infinite macro loop.")
                     iterationsRemaining[0] -= 1
                     trace.append(f"{indent}Expanding: {Expander._formatSExpr(sexpr)}")
                     expanded_once = Expander._expandOnce(ctx, env, sexpr)
                     trace.append(f"{indent}       => {Expander._formatSExpr(expanded_once)}")
                     return expandTraced(expanded_once, depth + 1)
               except KeyError:
                  pass

         # Recursively expand elements
         if isinstance(sexpr, list):
            return [expandTraced(elt, depth) for elt in sexpr]
         else:
            return sexpr

      expanded = expandTraced(sexpr)
      return expanded, trace

   @staticmethod
   def _formatSExpr(sexpr: Any) -> str:
      """Format s-expression for debug output (keep it short)."""
      s = prettyPrintSExpr(sexpr)
      if len(s) > 60:
         return s[:60] + "..."
      return s

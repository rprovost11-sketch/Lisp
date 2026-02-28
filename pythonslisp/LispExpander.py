"""
LispExpander - Macro expansion and structural normalization as a single pass.

expand() walks the AST once, interleaving top-down macro expansion with
bottom-up structural normalization.  The result is ready for LispAnalyzer
and then _lEval.
"""

from typing import Any
from pythonslisp.Environment import Environment
from pythonslisp.LispAST import LSymbol, LMacro, L_NIL, prettyPrintSExpr
from pythonslisp.LispEnvironment import LispEnvironment
from pythonslisp.LispContext import LispContext


class LispExpander:
    """Handles macro expansion and structural normalization as a single pass."""

    @staticmethod
    def expand(ctx: LispContext, env: Environment, sexpr: Any, maxIterations: int = 1000) -> Any:
        """
        Walk sexpr once, expanding macros top-down and normalizing bottom-up.

        Normalizations applied (all zero-behavior-change):
          (if cond then)       → (if cond then nil)
          (progn)              → nil
          (progn e)            → e
          (let  () body ...)   → (progn body ...)
          (let* () body ...)   → (progn body ...)
        """
        return LispExpander._expand(ctx, env, sexpr, maxIterations)

    @staticmethod
    def _expand(ctx: LispContext, env: Environment, sexpr: Any, maxIterations: int = 1000) -> Any:
        # Atoms and symbols pass through unchanged
        if isinstance(sexpr, LSymbol):
            return sexpr
        if not isinstance(sexpr, list) or len(sexpr) == 0:
            return sexpr

        # Don't expand or normalize inside QUOTE or BACKQUOTE — they are data / templates
        if isinstance(sexpr[0], LSymbol) and sexpr[0].strval in ('QUOTE', 'BACKQUOTE'):
            return sexpr

        # --- Step 1: macro expand at this level (top-down, fixed-point) ---
        expandedOnce = LispExpander._expandOnce(ctx, env, sexpr)
        if expandedOnce is not sexpr:
            # A macro fired — recurse on the result (handles nested macros)
            if maxIterations <= 1:
                raise RuntimeError("Macro expansion limit exceeded — possible infinite macro loop.")
            return LispExpander._expand(ctx, env, expandedOnce, maxIterations - 1)

        # --- Step 2: recurse into sub-elements (bottom-up) ---
        expanded = [LispExpander._expand(ctx, env, elt, maxIterations) for elt in sexpr]

        # --- Step 3: apply structural normalization rules ---
        if not isinstance(expanded[0], LSymbol):
            return expanded

        head = expanded[0].strval

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
    def _expandOnce(ctx: LispContext, env: Environment, sexpr: list) -> Any:
        """
        Attempt to expand sexpr if it's a macro call.
        Returns expanded form if it's a macro, otherwise returns sexpr unchanged.
        """
        if not isinstance(sexpr, list) or len(sexpr) == 0:
            return sexpr

        primary = sexpr[0]

        if isinstance(primary, LSymbol):
            try:
                callableObj = env.lookup(primary.strval)
                if isinstance(callableObj, LMacro):
                    args = sexpr[1:]
                    return LispExpander._expandMacroCall(ctx, env, callableObj, args)
            except KeyError:
                pass

        return sexpr

    @staticmethod
    def _expandMacroCall(ctx: LispContext, env: Environment, macro: LMacro, argsList: list) -> Any:
        """Expand a single macro call and return the unevaluated expansion."""
        expansionEnv = LispEnvironment(env, evalFn=ctx.lEval)
        expansionEnv.bindArguments(macro.lambdaListAST, argsList)

        result = L_NIL
        for bodySExpr in macro.bodyAST:
            result = ctx.lEval(expansionEnv, bodySExpr)

        return result

    @staticmethod
    def expandWithDebug(ctx: LispContext, env: Environment, sexpr: Any, maxIterations: int = 1000) -> tuple[Any, list[str]]:
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
                # Don't expand inside quote or backquote — content is literal data/template
                if isinstance(sexpr[0], LSymbol) and sexpr[0].strval in ('QUOTE', 'BACKQUOTE'):
                    return sexpr
                primary = sexpr[0]
                if isinstance(primary, LSymbol):
                    try:
                        fn = env.lookup(primary.strval)
                        if isinstance(fn, LMacro):
                            if iterationsRemaining[0] <= 0:
                                raise RuntimeError("Macro expansion limit exceeded — possible infinite macro loop.")
                            iterationsRemaining[0] -= 1
                            trace.append(f"{indent}Expanding: {LispExpander._formatSExpr(sexpr)}")
                            expanded_once = LispExpander._expandOnce(ctx, env, sexpr)
                            trace.append(f"{indent}       => {LispExpander._formatSExpr(expanded_once)}")
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

"""
LispExpander - Macro expansion as a separate phase

This module handles macro expansion as a preprocessing step before evaluation.
Instead of expanding macros during evaluation (which happens on every call),
macros are expanded once after parsing and before evaluation begins.

Benefits:
- Performance: Each macro expanded once, not on every evaluation
- Separation of concerns: Expansion is a distinct phase
- Debugging: Can inspect expanded code before evaluation
- Optimization: Opens door for further AST optimizations
"""

from typing import Any
from pythonslisp.Environment import Environment
from pythonslisp.LispAST import LSymbol, LMacro


class LispExpander:
    """Handles macro expansion as a separate compilation phase."""
    @staticmethod
    def expand(env: Environment, sexpr: Any, max_iterations: int = 1000) -> Any:
        """
        Recursively expand all macros in sexpr.

        Args:
            env: Environment containing macro definitions
            sexpr: S-expression (AST) to expand
            max_iterations: Safety limit to prevent infinite expansion loops

        Returns:
            New AST with all macro calls replaced by their expansions

        Algorithm:
        1. If sexpr is a symbol or atom, return as-is (no expansion needed)
        2. If sexpr is empty list, return as-is
        3. If sexpr is a non-empty list:
           a. Check if primary (first element) is a macro
           b. If yes: expand the macro call and recursively expand the result
           c. If no: recursively expand each element of the list
        4. Repeat until no more macros found (fixed-point expansion)
        """
        if isinstance(sexpr, LSymbol):
            return sexpr  # Symbols don't expand

        elif not isinstance(sexpr, list):
            return sexpr  # Atoms (numbers, strings) don't expand

        elif len(sexpr) == 0:
            return list()  # Empty list doesn't expand

        # Don't expand inside quote — the content is literal data, not code
        if isinstance(sexpr[0], LSymbol) and sexpr[0].strval == 'QUOTE':
            return sexpr

        # Non-empty list - could be a macro call
        # Strategy: Try to expand, then recursively expand the result
        # This handles nested macros: (unless ...) → (when ...) → (if ...)

        expanded_once = LispExpander._expandOnce(env, sexpr)

        # Did anything change?
        if expanded_once is sexpr:
            # No macro expansion happened at top level
            # Recursively expand elements
            return [LispExpander.expand(env, elt, max_iterations) for elt in sexpr]
        else:
            # Macro was expanded - recursively expand the result
            # (handles nested macro calls)
            if max_iterations <= 1:
                raise RuntimeError("Macro expansion limit exceeded — possible infinite macro loop.")
            return LispExpander.expand(env, expanded_once, max_iterations - 1)

    @staticmethod
    def _expandOnce(env: Environment, sexpr: list) -> Any:
        """
        Attempt to expand sexpr if it's a macro call.
        Returns expanded form if it's a macro, otherwise returns sexpr unchanged.

        Args:
            env: Environment containing macro definitions
            sexpr: List s-expression that might be a macro call

        Returns:
            Expanded form if macro call, otherwise original sexpr
        """
        if not isinstance(sexpr, list) or len(sexpr) == 0:
            return sexpr

        primary = sexpr[0]

        # Check if primary is a symbol that's bound to a macro
        if isinstance(primary, LSymbol):
            try:
                fn = env.lookup(primary.strval)
                if isinstance(fn, LMacro):
                    # It's a macro call - expand it!
                    args = sexpr[1:]  # Arguments to the macro
                    expanded = LispExpander._expandMacroCall(env, fn, args)
                    return expanded
            except KeyError:
                # Symbol not bound - not a macro
                pass

        # Not a macro call, return unchanged
        return sexpr

    @staticmethod
    def _expandMacroCall(env: Environment, macro: LMacro, argsList: list) -> Any:
        """
        Expand a single macro call.

        This is similar to LispInterpreter._macroexpand but with a key difference:
        - Old: Expands AND evaluates the result
        - New: Just expands, evaluation happens later

        Args:
            env: Environment for macro expansion
            macro: LMacro object to expand
            args: Unevaluated arguments to the macro

        Returns:
            Expanded s-expression (still an AST, not evaluated)
        """
        # Import here to avoid circular dependency
        from pythonslisp.LispInterpreter import LispInterpreter

        # Create new environment for macro expansion
        # This is where macro parameters get bound
        expansion_env = Environment(env)

        # Bind macro parameters to (unevaluated) arguments
        # Example: (when cond body...) binds cond=(> x 0), body=[(print x)]
        LispInterpreter._lbindArguments(expansion_env, macro.lambdaListAST, argsList)

        # Evaluate macro body to generate the expansion
        # This typically evaluates a backquote expression
        # Example: `(if ,cond (progn ,@body))
        #       → (if (> x 0) (progn (print x)))
        result = list()
        for bodySExpr in macro.bodyAST:
            result = LispInterpreter._lEval(expansion_env, bodySExpr)

        return result

    @staticmethod
    def expandWithDebug(env: Environment, sexpr: Any, max_iterations: int = 1000) -> tuple[Any, list[str]]:
        """
        Expand macros and return both result and expansion trace.
        Useful for debugging and understanding macro expansion.

        Returns:
            (expanded_sexpr, trace_list)
        """
        trace = []
        iterations_remaining = [max_iterations]   # mutable container for nested access

        def expand_traced(sexpr, depth=0):
            indent = "  " * depth

            if isinstance(sexpr, list) and len(sexpr) > 0:
                # Don't expand inside quote — the content is literal data
                if isinstance(sexpr[0], LSymbol) and sexpr[0].strval == 'QUOTE':
                    return sexpr
                primary = sexpr[0]
                if isinstance(primary, LSymbol):
                    try:
                        fn = env.lookup(primary.strval)
                        if isinstance(fn, LMacro):
                            if iterations_remaining[0] <= 0:
                                raise RuntimeError("Macro expansion limit exceeded — possible infinite macro loop.")
                            iterations_remaining[0] -= 1
                            trace.append(f"{indent}Expanding: {LispExpander._format_sexpr(sexpr)}")
                            expanded_once = LispExpander._expandOnce(env, sexpr)
                            trace.append(f"{indent}       => {LispExpander._format_sexpr(expanded_once)}")
                            return expand_traced(expanded_once, depth + 1)
                    except KeyError:
                        pass

            # Recursively expand elements
            if isinstance(sexpr, list):
                return [expand_traced(elt, depth) for elt in sexpr]
            else:
                return sexpr

        expanded = expand_traced(sexpr)
        return expanded, trace

    @staticmethod
    def _format_sexpr(sexpr: Any) -> str:
        """Format s-expression for debug output (keep it short)."""
        from pythonslisp.LispAST import prettyPrintSExpr
        s = prettyPrintSExpr(sexpr)
        if len(s) > 60:
            return s[:60] + "..."
        return s

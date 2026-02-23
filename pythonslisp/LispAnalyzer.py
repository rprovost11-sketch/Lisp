"""
LispAnalyzer - Semantic analysis as a separate phase

This module performs semantic analysis on a fully-expanded, normalized AST.
It walks the AST and raises LispAnalysisError for structural / semantic
problems, providing earlier and clearer error messages than the evaluator.

Phase 1: skeleton — analyze() is a no-op stub.
Phase 2: migrate arity / type checks out of _lEval inline cases.
Phase 3: migrate arity / type checks out of primitives.
"""

from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.LispExceptions import LispAnalysisError     # noqa: F401  (re-exported for callers)


class LispAnalyzer:
   """Performs semantic analysis on a fully-expanded, normalized AST."""

   @staticmethod
   def analyze( env: Environment, sexpr: Any ) -> None:
      """
      Walk sexpr and raise LispAnalysisError on semantic problems.

      Phase 1: no-op stub — all checks are added in later phases.

      Args:
          env:    Current global environment (used to resolve symbols).
          sexpr:  Fully-expanded, normalized AST node to analyse.
      """
      pass

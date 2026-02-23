"""
LispAnalyzer - Semantic analysis as a separate phase

This module performs semantic analysis on a fully-expanded, normalized AST.
It walks the AST and raises errors for structural / semantic problems,
providing earlier and clearer diagnostics than the evaluator.

Phase 2: structural checks for all inline special forms migrated out of _lEval.
Phase 3: arity / type checks migrated out of primitives.
"""

from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.LispAST import LSymbol, LPrimitive
from pythonslisp.LispExceptions import ( LispAnalysisError,      # noqa: F401 (re-exported)
                                         LispRuntimeError,
                                         LispRuntimeFuncError )


class LispAnalyzer:
   """Performs semantic analysis on a fully-expanded, normalized AST."""

   @staticmethod
   def analyze( env: Environment, sexpr: Any ) -> None:
      """
      Recursively walk sexpr, raising on structural / semantic problems.

      Checks are pure AST-structure checks (no evaluation).  Runtime checks
      (e.g., whether a variable is bound) remain in _lEval.
      """
      if not isinstance(sexpr, list) or len(sexpr) == 0:
         return

      head = sexpr[0]
      args = sexpr[1:]

      if not isinstance(head, LSymbol):
         # Compound head — recurse into all elements
         for elt in sexpr:
            LispAnalyzer.analyze(env, elt)
         return

      name = head.strval

      # ---------- QUOTE --------------------------------------------------
      if name == 'QUOTE':
         if len(args) != 1:
            raise LispRuntimeFuncError(env.lookup('QUOTE'), '1 argument expected.')
         return  # Don't recurse into quoted data

      # ---------- BACKQUOTE -----------------------------------------------
      if name == 'BACKQUOTE':
         if len(args) != 1:
            raise LispRuntimeFuncError(env.lookup('BACKQUOTE'), '1 argument expected.')
         return  # Don't recurse into backquote templates

      # ---------- IF -------------------------------------------------------
      if name == 'IF':
         numArgs = len(args)
         if not (2 <= numArgs <= 3):
            raise LispRuntimeFuncError(env.lookup('IF'), '2 or 3 arguments expected.')
         for elt in args:
            LispAnalyzer.analyze(env, elt)
         return

      # ---------- LET / LET* ----------------------------------------------
      if name in ('LET', 'LET*'):
         LispAnalyzer._analyzeLet(env, name, args)
         return

      # ---------- PROGN ---------------------------------------------------
      if name == 'PROGN':
         for elt in args:
            LispAnalyzer.analyze(env, elt)
         return

      # ---------- SETQ ----------------------------------------------------
      if name == 'SETQ':
         LispAnalyzer._analyzeSetq(env, args)
         return

      # ---------- BLOCK ----------------------------------------------------
      if name == 'BLOCK':
         LispAnalyzer._analyzeBlock(env, args)
         return

      # ---------- RETURN-FROM ----------------------------------------------
      if name == 'RETURN-FROM':
         LispAnalyzer._analyzeReturnFrom(env, args)
         return

      # ---------- CATCH ----------------------------------------------------
      if name == 'CATCH':
         if len(args) < 1:
            raise LispRuntimeError('catch: at least 1 argument (tag) expected.')
         for elt in args:
            LispAnalyzer.analyze(env, elt)
         return

      # ---------- LAMBDA --------------------------------------------------
      if name == 'LAMBDA':
         if len(args) < 1:
            raise LispRuntimeFuncError(env.lookup('LAMBDA'), '1 or more arguments expected.')
         if not isinstance(args[0], list):
            raise LispRuntimeFuncError(env.lookup('LAMBDA'), 'First argument expected to be a list of params.')
         for bodyForm in args[1:]:
            LispAnalyzer.analyze(env, bodyForm)
         return

      # ---------- DEFMACRO ------------------------------------------------
      if name == 'DEFMACRO':
         if len(args) < 2:
            raise LispRuntimeFuncError(env.lookup('DEFMACRO'), '3 or more arguments expected.')
         if not isinstance(args[0], LSymbol):
            raise LispRuntimeFuncError(env.lookup('DEFMACRO'), 'Argument 1 expected to be a symbol.')
         if not isinstance(args[1], list):
            raise LispRuntimeFuncError(env.lookup('DEFMACRO'), 'Argument 2 expected to be a list of params.')
         funcBody = list(args[2:])
         if len(funcBody) < 1:
            raise LispRuntimeFuncError(env.lookup('DEFMACRO'), 'At least one body expression expected.')
         if isinstance(funcBody[0], str):
            funcBody = funcBody[1:]
         if len(funcBody) < 1:
            raise LispRuntimeFuncError(env.lookup('DEFMACRO'), 'At least one body expression expected after docstring.')
         return

      # ---------- Everything else (regular calls, unknown special forms) ---
      # Generic arity check for primitives that carry arity metadata.
      if isinstance(head, LSymbol):
         try:
            callableObj = env.lookup(head.strval)
            if isinstance(callableObj, LPrimitive) and callableObj.arity_msg:
               numArgs = len(args)
               tooFew  = numArgs < callableObj.min_args
               tooMany = callableObj.max_args is not None and numArgs > callableObj.max_args
               if tooFew or tooMany:
                  raise LispRuntimeFuncError(callableObj, callableObj.arity_msg)
         except KeyError:
            pass
      for elt in sexpr:
         LispAnalyzer.analyze(env, elt)

   # -----------------------------------------------------------------------

   @staticmethod
   def _analyzeLet( env: Environment, name: str, args: list ) -> None:
      """Structural checks for (let ...) and (let* ...) forms."""
      if len(args) < 1:
         raise LispRuntimeFuncError(env.lookup(name), '2 or more arguments expected.')

      vardefs = args[0]
      body    = args[1:]

      if not isinstance(vardefs, list):
         raise LispRuntimeFuncError(env.lookup(name),
               'The first argument to let expected to be a list of variable initializations.')

      for varSpec in vardefs:
         if isinstance(varSpec, LSymbol):
            pass  # bare symbol — valid, no init form
         elif isinstance(varSpec, list):
            varSpecLen = len(varSpec)
            if varSpecLen == 1:
               if not isinstance(varSpec[0], LSymbol):
                  raise LispRuntimeFuncError(env.lookup(name),
                        'First element of a variable initializer pair expected to be a symbol.')
            elif varSpecLen == 2:
               varName, initForm = varSpec
               if not isinstance(varName, LSymbol):
                  raise LispRuntimeFuncError(env.lookup(name),
                        'First element of a variable initializer pair expected to be a symbol.')
               LispAnalyzer.analyze(env, initForm)
            else:
               raise LispRuntimeFuncError(env.lookup(name),
                     'Variable initializer spec expected to be 1 or 2 elements long.')
         else:
            raise LispRuntimeFuncError(env.lookup(name),
                  'Variable initializer spec expected to be a symbol or a list.')

      for bodyForm in body:
         LispAnalyzer.analyze(env, bodyForm)

   @staticmethod
   def _analyzeSetq( env: Environment, args: list ) -> None:
      """Structural checks for (setq ...) forms."""
      numArgs = len(args)
      if numArgs == 0:
         raise LispRuntimeFuncError(env.lookup('SETQ'), 'At least 2 arguments expected.')
      if (numArgs % 2) != 0:
         raise LispRuntimeFuncError(env.lookup('SETQ'),
               f'An even number of arguments is expected.  Received {numArgs}.')
      for i in range(0, numArgs, 2):
         lval = args[i]
         rval = args[i + 1]
         if not isinstance(lval, LSymbol):
            raise LispRuntimeFuncError(env.lookup('SETQ'), 'First of setf pair must be a symbol.')
         LispAnalyzer.analyze(env, rval)

   @staticmethod
   def _analyzeBlock( env: Environment, args: list ) -> None:
      """Structural checks for (block ...) forms."""
      if len(args) < 1:
         raise LispRuntimeError('block: at least 1 argument (name) expected.')
      if not isinstance(args[0], LSymbol):
         raise LispRuntimeError('block: name must be a symbol.')
      for bodyForm in args[1:]:
         LispAnalyzer.analyze(env, bodyForm)

   @staticmethod
   def _analyzeReturnFrom( env: Environment, args: list ) -> None:
      """Structural checks for (return-from ...) forms."""
      if len(args) < 1 or len(args) > 2:
         raise LispRuntimeError('return-from: 1 or 2 arguments expected.')
      if not isinstance(args[0], LSymbol):
         raise LispRuntimeError('return-from: name must be a symbol.')
      if len(args) == 2:
         LispAnalyzer.analyze(env, args[1])

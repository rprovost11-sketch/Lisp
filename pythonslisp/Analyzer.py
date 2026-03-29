"""
Analyzer - Semantic analysis as a separate phase

This module performs semantic analysis on a fully-expanded, normalized AST.
It walks the AST and raises errors for structural / semantic problems,
providing earlier and clearer diagnostics than the evaluator.

Phase 2: structural checks for all inline special forms migrated out of the evaluator.
Phase 3: arity / type checks migrated out of primitives.
"""
from __future__ import annotations

from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.AST import LSymbol, LPrimitive, LFunction, LList, arity_mismatch_msg, derive_arity
from pythonslisp.Exceptions import ( LAnalysisError,      # noqa: F401 (re-exported)
                                         LRuntimeError,
                                         LRuntimePrimError,
                                         LArgBindingError )


class Analyzer:
   """Performs semantic analysis on a fully-expanded, normalized AST."""

   @staticmethod
   def analyze( env: Environment, sexpr: Any ) -> None:
      """Wrapper: runs _do_analyze and annotates any raised error with
      the source position of sexpr if it is an LList and the error has
      not already been annotated by a deeper recursive call."""
      try:
         Analyzer._do_analyze( env, sexpr )
      except LRuntimeError as e:
         if isinstance( sexpr, LList ) and sexpr.has_source_info() and e.source_info is None:
            e.source_info = ( sexpr.filename, sexpr.line_num, sexpr.col_num, sexpr.source_line )
         raise

   @staticmethod
   def _do_analyze( env: Environment, sexpr: Any ) -> None:
      """
      Recursively walk sexpr, raising on structural / semantic problems.

      Checks are pure AST-structure checks (no evaluation).  Runtime checks
      (e.g., whether a variable is bound) remain in the evaluator.
      """
      if not isinstance(sexpr, list) or len(sexpr) == 0:
         return

      head = sexpr[0]
      args = sexpr[1:]

      if not isinstance(head, LSymbol):
         # Compound head - recurse into all elements
         for elt in sexpr:
            Analyzer.analyze(env, elt)
         return

      name = head.name

      # ---------- QUOTE --------------------------------------------------
      if name == 'QUOTE':
         if len(args) != 1:
            raise LRuntimePrimError(env.lookup('QUOTE'), '1 argument expected.')
         return  # Don't recurse into quoted data

      # ---------- QUASIQUOTE -----------------------------------------------
      if name == 'QUASIQUOTE':
         if len(args) != 1:
            raise LRuntimePrimError(env.lookup('QUASIQUOTE'), '1 argument expected.')
         return  # Don't recurse into quasiquote templates

      # ---------- IF -------------------------------------------------------
      if name == 'IF':
         numArgs = len(args)
         if not (2 <= numArgs <= 3):
            raise LRuntimePrimError(env.lookup('IF'), '2 or 3 arguments expected.')
         for elt in args:
            Analyzer.analyze(env, elt)
         return

      # ---------- LET / LET* ----------------------------------------------
      if name in ('LET', 'LET*'):
         Analyzer._analyzeLet(env, name, args)
         return

      # ---------- PROGN ---------------------------------------------------
      if name == 'PROGN':
         for elt in args:
            Analyzer.analyze(env, elt)
         return

      # ---------- SETQ ----------------------------------------------------
      if name == 'SETQ':
         Analyzer._analyzeSetq(env, args)
         return

      # ---------- BLOCK ----------------------------------------------------
      if name == 'BLOCK':
         Analyzer._analyzeBlock(env, args)
         return

      # ---------- RETURN-FROM ----------------------------------------------
      if name == 'RETURN-FROM':
         Analyzer._analyzeReturnFrom(env, args)
         return

      # ---------- RETURN ---------------------------------------------------
      if name == 'RETURN':
         if len(args) > 1:
            raise LRuntimeError('return: 0 or 1 arguments expected.')
         if len(args) == 1:
            Analyzer.analyze(env, args[0])
         return

      # ---------- CATCH ----------------------------------------------------
      if name == 'CATCH':
         if len(args) < 1:
            raise LRuntimeError('catch: at least 1 argument (tag) expected.')
         for elt in args:
            Analyzer.analyze(env, elt)
         return

      # ---------- COND -----------------------------------------------------
      if name == 'COND':
         Analyzer._analyzeCond(env, args)
         return

      # ---------- CASE -----------------------------------------------------
      if name == 'CASE':
         Analyzer._analyzeCase(env, args)
         return

      # ---------- LAMBDA --------------------------------------------------
      if name == 'LAMBDA':
         if len(args) < 1:
            raise LRuntimePrimError(env.lookup('LAMBDA'), '1 or more arguments expected.')
         if not isinstance(args[0], list):
            raise LRuntimePrimError(env.lookup('LAMBDA'), 'Invalid argument 1. PARAMETER LIST expected.')
         Analyzer.analyzeLambdaList(args[0])
         for bodyForm in args[1:]:
            Analyzer.analyze(env, bodyForm)
         return

      # ---------- DEFMACRO ------------------------------------------------
      if name == 'DEFMACRO':
         if len(args) < 2:
            raise LRuntimePrimError(env.lookup('DEFMACRO'), '3 or more arguments expected.')
         if not isinstance(args[0], LSymbol):
            raise LRuntimePrimError(env.lookup('DEFMACRO'), 'Invalid argument 1. SYMBOL expected.')
         if not isinstance(args[1], list):
            raise LRuntimePrimError(env.lookup('DEFMACRO'), 'Invalid argument 2. PARAMETER LIST expected.')
         Analyzer.analyzeLambdaList(args[1], destructuring=True)
         funcBody = list(args[2:])
         if len(funcBody) < 1:
            raise LRuntimePrimError(env.lookup('DEFMACRO'), 'At least one body expression expected.')
         if isinstance(funcBody[0], str):
            funcBody = funcBody[1:]
         if len(funcBody) < 1:
            raise LRuntimePrimError(env.lookup('DEFMACRO'), 'At least one body expression expected after docstring.')
         return

      # ---------- MAKE-DICT ------------------------------------------------
      if name == 'MAKE-DICT':
         Analyzer._analyzeMakeDict(env, args)
         return

      # ---------- Everything else (regular calls, unknown special forms) ---
      # Generic arity check for primitives and functions.
      if isinstance(head, LSymbol):
         try:
            callableObj = env.lookup(head.name)
            if isinstance(callableObj, LPrimitive):
               if callableObj.arity_msg:
                  numArgs = len(args)
                  tooFew  = numArgs < callableObj.min_args
                  tooMany = callableObj.max_args is not None and numArgs > callableObj.max_args
                  if tooFew or tooMany:
                     raise LRuntimePrimError(callableObj,
                        arity_mismatch_msg(callableObj.min_args, callableObj.max_args, numArgs))
            elif isinstance(callableObj, LFunction):
               min_a, max_a = derive_arity(callableObj.lambdaListAST)
               numArgs = len(args)
               tooFew  = numArgs < min_a
               tooMany = max_a is not None and numArgs > max_a
               if tooFew or tooMany:
                  fnName = callableObj.name if callableObj.name else '(lambda ...)'
                  msg    = arity_mismatch_msg(min_a, max_a, numArgs)
                  usage  = callableObj.usageString()
                  raise LRuntimeError(
                     f'Error binding arguments in call to function "{fnName}".\n{msg}\n{usage}')
         except KeyError:
            pass
      for elt in sexpr:
         Analyzer.analyze(env, elt)

   # -----------------------------------------------------------------------

   @staticmethod
   def _analyzeLet( env: Environment, name: str, args: list ) -> None:
      """Structural checks for (let ...) and (let* ...) forms."""
      if len(args) < 1:
         raise LRuntimePrimError(env.lookup(name), '2 or more arguments expected.')

      vardefs = args[0]
      body    = args[1:]

      if not isinstance(vardefs, list):
         raise LRuntimePrimError(env.lookup(name),
               'Invalid argument 1. LIST OF VARIABLE BINDINGS expected.')

      for varSpec in vardefs:
         if isinstance(varSpec, LSymbol):
            pass  # bare symbol - valid, no init form
         elif isinstance(varSpec, list):
            varSpecLen = len(varSpec)
            if varSpecLen == 1:
               if not isinstance(varSpec[0], LSymbol):
                  raise LRuntimePrimError(env.lookup(name),
                        'Variable binding name expected to be a SYMBOL.')
            elif varSpecLen == 2:
               varName, initForm = varSpec
               if not isinstance(varName, LSymbol):
                  raise LRuntimePrimError(env.lookup(name),
                        'Variable binding name expected to be a SYMBOL.')
               Analyzer.analyze(env, initForm)
            else:
               raise LRuntimePrimError(env.lookup(name),
                     'Variable initializer expected to be 1 or 2 elements.')
         else:
            raise LRuntimePrimError(env.lookup(name),
                  'Variable initializer expected to be a SYMBOL or LIST.')

      for bodyForm in body:
         Analyzer.analyze(env, bodyForm)

   @staticmethod
   def _analyzeSetq( env: Environment, args: list ) -> None:
      """Structural checks for (setq ...) forms."""
      numArgs = len(args)
      if numArgs == 0:
         raise LRuntimePrimError(env.lookup('SETQ'), 'At least 2 arguments expected.')
      if (numArgs % 2) != 0:
         raise LRuntimePrimError(env.lookup('SETQ'),
               f'An even number of arguments expected; {numArgs} provided.')
      for i in range(0, numArgs, 2):
         lval = args[i]
         rval = args[i + 1]
         if not isinstance(lval, LSymbol):
            raise LRuntimePrimError(env.lookup('SETQ'), 'Assignment target must be a SYMBOL.')
         Analyzer.analyze(env, rval)

   @staticmethod
   def _is_nil_val( val ) -> bool:
      """True iff val is the Lisp NIL (empty list)."""
      return isinstance(val, list) and not val

   @staticmethod
   def _analyzeBlock( env: Environment, args: list ) -> None:
      """Structural checks for (block ...) forms."""
      if len(args) < 1:
         raise LRuntimeError('block: at least 1 argument expected.')
      if not (isinstance(args[0], LSymbol) or Analyzer._is_nil_val(args[0])):
         raise LRuntimeError('block: name must be a SYMBOL or NIL.')
      for bodyForm in args[1:]:
         Analyzer.analyze(env, bodyForm)

   @staticmethod
   def _analyzeReturnFrom( env: Environment, args: list ) -> None:
      """Structural checks for (return-from ...) forms."""
      if len(args) < 1 or len(args) > 2:
         raise LRuntimeError('return-from: 1 or 2 arguments expected.')
      if not (isinstance(args[0], LSymbol) or Analyzer._is_nil_val(args[0])):
         raise LRuntimeError('return-from: name must be a SYMBOL or NIL.')
      if len(args) == 2:
         Analyzer.analyze(env, args[1])

   @staticmethod
   def _analyzeCond( env: Environment, args: list ) -> None:
      """Structural checks for (cond ...) forms."""
      if len(args) < 1:
         raise LRuntimePrimError(env.lookup('COND'), '1 or more arguments expected.')
      for i, clause in enumerate(args):
         if not isinstance(clause, list) or len(clause) == 0:
            raise LRuntimePrimError(env.lookup('COND'),
                  f'Entry {i+1} must be a non-empty list.')
         Analyzer.analyze(env, clause[0])
         for bodyForm in clause[1:]:
            Analyzer.analyze(env, bodyForm)

   @staticmethod
   def _analyzeCase( env: Environment, args: list ) -> None:
      """Structural checks for (case ...) forms."""
      if len(args) < 2:
         raise LRuntimePrimError(env.lookup('CASE'), '2 or more arguments expected.')
      Analyzer.analyze(env, args[0])
      for i, clause in enumerate(args[1:]):
         if not isinstance(clause, list) or len(clause) == 0:
            raise LRuntimePrimError(env.lookup('CASE'),
                  f'Entry {i+1} must be a non-empty list.')
         Analyzer.analyze(env, clause[0])
         for bodyForm in clause[1:]:
            Analyzer.analyze(env, bodyForm)

   @staticmethod
   def analyzeLambdaList( lambdaListAST: list, destructuring: bool = False ) -> None:
      """Validate lambda list structure at definition time.

      Performs full structural validation and duplicate-name detection.
      Raises LArgBindingError for any structural problem.  After this
      passes, bindArguments can assume the lambda list is well-formed.
      """
      _KNOWN_KEYWORDS = {'&OPTIONAL', '&REST', '&BODY', '&KEY', '&AUX', '&ALLOW-OTHER-KEYS'}
      seen: set = set()

      def _no_dup( sym: LSymbol, context: str ) -> None:
         if sym.name in seen:
            raise LArgBindingError( f'Duplicate parameter name {sym.name} in {context}.' )
         seen.add( sym.name )

      def _validate( ll: list, destr: bool ) -> None:
         length = len( ll )
         i      = 0

         # ---- Positional params ----
         while i < length:
            param = ll[i]
            if isinstance( param, LSymbol ):
               if param.startswith( '&' ):
                  break
               _no_dup( param, 'positional parameters' )
            elif isinstance( param, list ) and destr:
               _validate( param, destr=True )
            else:
               raise LArgBindingError( f'Lambda list item {i + 1} must be a symbol.' )
            i += 1

         if i >= length:
            return

         keyword = ll[i]
         if not isinstance( keyword, LSymbol ):
            raise LArgBindingError( f'Lambda list item {i + 1} must be a symbol.' )

         # ---- &OPTIONAL ----
         if keyword.name == '&OPTIONAL':
            i += 1
            while i < length:
               spec = ll[i]
               if isinstance( spec, LSymbol ):
                  if spec.startswith( '&' ):
                     break
                  _no_dup( spec, 'positional parameters' )
               elif isinstance( spec, list ):
                  slen = len( spec )
                  if slen < 1 or slen > 3:
                     raise LArgBindingError(
                        'Parameter spec following &OPTIONAL must be a list of '
                        '(<variable> [<defaultvalue> [<pvar>]] ).' )
                  var = spec[0]
                  if not isinstance( var, LSymbol ):
                     raise LArgBindingError(
                        'Parameter variable in &OPTIONAL spec must be a symbol.' )
                  if var.startswith( '&' ):
                     raise LArgBindingError(
                        f'Lambda list keyword {var} cannot be used as a variable name in &OPTIONAL spec.' )
                  _no_dup( var, 'lambda list' )
                  if slen == 3:
                     pvar = spec[2]
                     if isinstance( pvar, LSymbol ):
                        if pvar.startswith( '&' ):
                           raise LArgBindingError(
                              f'Lambda list keyword {pvar} cannot be used as a supplied-p variable in &OPTIONAL spec.' )
                        _no_dup( pvar, 'supplied-p variable' )
                     elif not isinstance( pvar, list ):
                        raise LArgBindingError(
                           f'Parameter pvar following {var} must be a symbol.' )
               else:
                  raise LArgBindingError(
                     'Parameter spec following &OPTIONAL must be a <variable> or a list of '
                     '(<variable> <defaultvalue>).' )
               i += 1

            if i >= length:
               return
            keyword = ll[i]
            if not isinstance( keyword, LSymbol ):
               raise LArgBindingError( f'Lambda list item {i + 1} must be a symbol.' )

         # ---- &REST / &BODY ----
         if keyword.name == '&REST' or ( keyword.name == '&BODY' and destr ):
            i += 1
            if i >= length:
               raise LArgBindingError( f'Lambda list item {i + 1}: symbol expected after &rest.' )
            rest_var = ll[i]
            if not isinstance( rest_var, LSymbol ) or rest_var.startswith( '&' ):
               raise LArgBindingError( f'Lambda list item {i + 1}: symbol expected after &rest.' )
            _no_dup( rest_var, '&REST' )
            i += 1

            if i >= length:
               return
            keyword = ll[i]
            if not isinstance( keyword, LSymbol ):
               raise LArgBindingError( f'Lambda list item {i + 1} must be a symbol.' )

         # ---- &KEY ----
         if keyword.name == '&KEY':
            i += 1
            while i < length:
               spec = ll[i]
               if isinstance( spec, LSymbol ):
                  if spec.startswith( '&' ):
                     break
                  _no_dup( spec, 'positional parameters' )
               elif isinstance( spec, list ):
                  slen = len( spec )
                  if slen == 0:
                     raise LArgBindingError( 'Empty parameter spec () in &KEY lambda list.' )
                  key_var_spec = spec[0]
                  if isinstance( key_var_spec, LSymbol ):
                     if key_var_spec.startswith( '&' ):
                        raise LArgBindingError(
                           f'Lambda list keyword {key_var_spec} cannot be used as a variable name in &KEY spec.' )
                     _no_dup( key_var_spec, 'lambda list' )
                  elif isinstance( key_var_spec, list ):
                     if len( key_var_spec ) != 2:
                        raise LArgBindingError(
                           'Key Var pair following &KEY must contain exactly two elements.' )
                     key_sym, var_sym = key_var_spec
                     if not isinstance( key_sym, LSymbol ) or not key_sym.startswith( ':' ):
                        raise LArgBindingError(
                           'The key in a &KEY (keyword var) pair must be a keyword symbol (e.g. :mykey).' )
                     if not isinstance( var_sym, LSymbol ):
                        raise LArgBindingError(
                           'Variable in &KEY (keyword var) pair must be a symbol.' )
                     _no_dup( var_sym, '&KEY (keyword var) pair' )
                  else:
                     raise LArgBindingError(
                        '&KEY key/var spec must be either a symbol or a list (:keySymbol varSymbol).' )
                  if slen > 3:
                     raise LArgBindingError(
                        'Too many arguments specified in a parameter keyword initialization list.' )
                  if slen == 3:
                     pvar = spec[2]
                     if isinstance( pvar, LSymbol ):
                        if pvar.startswith( '&' ):
                           raise LArgBindingError(
                              f'Lambda list keyword {pvar} cannot be used as a supplied-p variable in &KEY spec.' )
                        _no_dup( pvar, 'supplied-p variable' )
                     elif not isinstance( pvar, list ):
                        raise LArgBindingError(
                           f'pvar for &KEY parameter {key_var_spec} must be a symbol.' )
               else:
                  raise LArgBindingError(
                     'Parameter spec following &KEY must be a symbol or a list.' )
               i += 1

            if i >= length:
               return
            keyword = ll[i]
            if not isinstance( keyword, LSymbol ):
               raise LArgBindingError( f'Lambda list item {i + 1} must be a symbol.' )

            if keyword.name == '&ALLOW-OTHER-KEYS':
               i += 1
               if i >= length:
                  return
               keyword = ll[i]
               if not isinstance( keyword, LSymbol ):
                  raise LArgBindingError( f'Lambda list item {i + 1} must be a symbol.' )

         # ---- &AUX ----
         if keyword.name == '&AUX':
            i += 1
            while i < length:
               spec = ll[i]
               if isinstance( spec, LSymbol ):
                  if spec.startswith( '&' ):
                     raise LArgBindingError( f'{spec} occurs after &AUX.' )
                  _no_dup( spec, 'positional parameters' )
               elif isinstance( spec, list ):
                  slen = len( spec )
                  if slen < 1 or slen > 2:
                     raise LArgBindingError(
                        'Parameter spec following &AUX must be a list of (<variable> [<defaultvalue>]).' )
                  var = spec[0]
                  if not isinstance( var, LSymbol ) or var.startswith( '&' ):
                     raise LArgBindingError(
                        'Parameter spec following &AUX must be a list of (<variable> [<defaultvalue>]).' )
                  _no_dup( var, 'positional parameters' )
               else:
                  raise LArgBindingError(
                     'Parameter spec following &AUX must be a <variable> or a list of (<variable> [<defaultvalue>]).' )
               i += 1
            return

         # ---- Unknown or misplaced &-keyword ----
         if keyword.startswith( '&' ):
            if keyword.name in _KNOWN_KEYWORDS:
               raise LArgBindingError(
                  f'{keyword} is misplaced in the lambda list.  '
                  f'Valid order: &optional, &rest, &key, &aux.' )
            raise LArgBindingError( f'Unknown lambda list keyword: {keyword}.' )

         raise LArgBindingError( f'Unexpected content at position {i + 1} in lambda list.' )

      _validate( lambdaListAST, destructuring )

   @staticmethod
   def _analyzeMakeDict( env: Environment, args: list ) -> None:
      """Structural checks for (make-dict ...) forms."""
      requiredKeyType = None
      for entryNum, pair in enumerate(args):
         if not isinstance(pair, list) or len(pair) != 2:
            raise LRuntimePrimError(env.lookup('MAKE-DICT'),
                  f'Entry {entryNum + 1} does not contain a (key value) pair.')
         key, expr = pair
         if isinstance(key, LSymbol):
            effectiveType = str
         elif isinstance(key, (int, float, str)):
            effectiveType = type(key)
         else:
            raise LRuntimePrimError(env.lookup('MAKE-DICT'),
                  f'Entry {entryNum + 1} has an invalid key type.')
         if requiredKeyType is None:
            requiredKeyType = effectiveType
         elif effectiveType != requiredKeyType:
            raise LRuntimePrimError(env.lookup('MAKE-DICT'),
                  f'All keys in a map must be the same type. '
                  f'Entry {entryNum + 1} is {effectiveType.__name__}, '
                  f'expected {requiredKeyType.__name__}.')
         Analyzer.analyze(env, expr)

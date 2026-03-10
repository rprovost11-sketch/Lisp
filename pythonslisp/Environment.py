"""Environment — EnvironmentBase subclass with Lisp argument binding.

Extends EnvironmentBase with the argument-binding methods needed by the evaluator
and expander.  Keeping them here makes the binding logic part of the environment
object that it mutates.

evalFn is stored as an instance attribute so argument-binding methods do not
need to carry it as a parameter.  Child environments inherit evalFn from their
parent; the evaluator (Interpreter._lApply) passes ctx.lEval when creating
a new scope so default-value evaluation always uses the current call context.
"""
from __future__ import annotations

from typing import Any, Callable, Sequence

from pythonslisp.ltk.EnvironmentBase import EnvironmentBase
from pythonslisp.AST import LSymbol
from pythonslisp.Exceptions import LArgBindingError


class Environment(EnvironmentBase):
   __slots__ = ('_evalFn',)

   def __init__( self, parent: (EnvironmentBase|None) = None,
                 initialBindings: (dict[str, Any]|None) = None,
                 evalFn: (Callable|None) = None ) -> None:
      super().__init__( parent, initialBindings )
      if evalFn is not None:
         self._evalFn = evalFn
      elif parent is not None:
         self._evalFn = parent._evalFn
      else:
         self._evalFn = None

   def bind( self, key: str, value: Any ) -> Any:
      '''Bind using lisp semantics.'''
      scope = self
      while scope:
         if key in scope._bindings:
            scope._bindings[key] = value
            return value
         scope = scope._parent
      self._GLOBAL_ENV._bindings[key] = value
      return value

   def lookup( self,  key: str) -> Any:
      '''Lookup using lisp semantics.'''
      scope: (EnvironmentBase | None) = self
      while scope:
         if key in scope._bindings:
            return scope._bindings[key]
         scope = scope._parent
      raise KeyError

   def lookup2( self, key: str) -> Any:
      '''This is slower than lookup() even though lookup2() seems more
      elegant.  While the lookup is fast, the exception handling (which is
      needed to traverse the parent list) is exceedingly slow.  Here's the
      code I tested these functions on:
         (defun test ()
            (let ()
               (let ()
                  (dotimes (i 1000000)
                     pi))))
      Looking up pi and e 1,000,000 times.  pi is defined globally, but the
      search for pi is started three scope levels in.
         lookup()  eval time between 0.44 and 0.74 seconds.
         lookup2() eval time between 4.52 and 6.28 seconds.
      by calling the versions of lookup() from Interpreter._lEval()
      '''
      scope: (EnvironmentBase | None) = self
      while scope:
         try:
            return scope._bindings[key]
         except KeyError:
            scope = scope._parent
      raise KeyError

   def bindArguments( self, lambdaListAST: list[Any], argList: Sequence[Any],
                      destructuring: bool = False, _validate: bool = True ) -> None:
      if _validate:
         self._validateNoDuplicateParams( lambdaListAST, destructuring )
      paramListLength = len(lambdaListAST)
      argListLength   = len(argList)

      paramNum, argNum = self._bindPositionalArgs( lambdaListAST, 0, argList, 0, destructuring )

      # Retrieve the next param which should be a symbol
      try:
         nextParam = lambdaListAST[paramNum]
      except IndexError:
         if argNum < argListLength:
            raise LArgBindingError( f'Too many arguments.  Received {argListLength}.' )
         return          # All params used up.  Return gracefully
      if not isinstance(nextParam, LSymbol):
         raise LArgBindingError( f"Param {paramNum} expected to be a symbol." )

      if nextParam == '&OPTIONAL':
         paramNum, argNum = self._bindOptionalArgs( lambdaListAST, paramNum+1, argList, argNum )

         try:
            nextParam = lambdaListAST[paramNum]
         except IndexError:
            if argNum < argListLength:
               raise LArgBindingError( f'Too many arguments.  Received {argListLength}.' )
            return          # All params used up.  Return gracefully
         if not isinstance(nextParam, LSymbol):
            raise LArgBindingError( f"Param {paramNum} expected to be a symbol." )

      if nextParam == '&REST' or (nextParam == '&BODY' and destructuring):
         paramNum, argNum = self._bindRestArgs( lambdaListAST, paramNum+1, argList, argNum )

         try:
            nextParam = lambdaListAST[paramNum]
         except IndexError:
            return          # All params used up.  Return gracefully
         if not isinstance(nextParam, LSymbol):
            raise LArgBindingError( f"Param {paramNum} expected to be a symbol." )

      if nextParam == '&KEY':
         paramNum, argNum = self._bindKeyArgs( lambdaListAST, paramNum+1, argList, argNum )

         try:
            nextParam = lambdaListAST[paramNum]
         except IndexError:
            return          # All params used up.  Return gracefully
         if not isinstance(nextParam, LSymbol):
            raise LArgBindingError( f"Param {paramNum} expected to be a symbol." )

      if nextParam == '&AUX':
         paramNum, argNum = self._bindAuxArgs( lambdaListAST, paramNum+1, argList, argNum )
      elif nextParam.startswith('&'):
         _KNOWN_KEYWORDS = {'&OPTIONAL', '&REST', '&BODY', '&KEY', '&AUX', '&ALLOW-OTHER-KEYS'}
         if nextParam.name in _KNOWN_KEYWORDS:
            raise LArgBindingError( f'{nextParam} is misplaced in the lambda list.  Valid order: &optional, &rest, &key, &aux.' )
         else:
            raise LArgBindingError( f'Unknown lambda list keyword: {nextParam}.' )

      if paramNum < paramListLength:
         raise LArgBindingError( f'Unexpected content at position {paramNum} in lambda list.' )

   # -----------------------------------------------------------------------

   def _validateNoDuplicateParams( self, lambdaListAST: list[Any],
                                    destructuring: bool = False ) -> None:
      '''Pre-pass: verify no variable name appears more than once in a lambda list.'''
      seen: set[str] = set()

      def _check( name: Any, context: str ) -> None:
         if not isinstance(name, LSymbol):
            return     # other validation will catch non-symbols
         if name.startswith('&'):
            return     # &-keywords are not parameter names
         if name.name in seen:
            raise LArgBindingError( f'Duplicate parameter name {name.name} in {context}.' )
         seen.add( name.name )

      def _check_pattern( pattern: Any, context: str ) -> None:
         '''Recursively check all variable names within a destructuring pattern.'''
         if isinstance( pattern, LSymbol ):
            _check( pattern, context )
         elif isinstance( pattern, list ):
            in_req = True
            i = 0
            while i < len(pattern):
               item = pattern[i]
               if isinstance( item, LSymbol ) and item.startswith('&'):
                  in_req = False
                  if item.name in ('&REST', '&BODY'):
                     i += 1
                     if i < len(pattern):
                        _check( pattern[i], '&REST in ' + context )
               elif in_req:
                  _check_pattern( item, context )
               else:
                  if isinstance( item, list ) and item:
                     _check( item[0], context )
                     if len(item) >= 3:
                        _check( item[2], context )   # supplied-p var
               i += 1

      in_required = True
      index       = 0
      lambdaListLen = len( lambdaListAST )
      while index < lambdaListLen:
         spec = lambdaListAST[index]

         if isinstance( spec, LSymbol ):
            if spec.name in ('&REST', '&BODY'):
               in_required = False
               index += 1
               if index < lambdaListLen:
                  _check( lambdaListAST[index], '&REST' )
            elif spec.startswith('&'):
               in_required = False
            else:
               _check( spec, 'positional parameters' )

         elif isinstance( spec, list ) and len(spec) > 0:
            if in_required and destructuring:
               _check_pattern( spec, 'destructuring pattern' )
            else:
               keyVarSpec = spec[0]
               if isinstance( keyVarSpec, LSymbol ):
                  _check( keyVarSpec, 'lambda list' )
               elif isinstance( keyVarSpec, list ) and len(keyVarSpec) >= 2:
                  _check( keyVarSpec[1], '&KEY (keyword var) pair' )
               if len(spec) >= 3:
                  _check( spec[2], 'supplied-p variable' )

         index += 1

   def _bindPositionalArgs( self, lambdaListAST: list[Any], paramNum: int,
                            argList: Sequence[Any], argNum: int,
                            destructuring: bool = False ) -> tuple[int, int]:
      paramListLength = len(lambdaListAST)

      while paramNum < paramListLength:
         paramName = lambdaListAST[paramNum]
         if isinstance( paramName, LSymbol ):
            if paramName.startswith('&'):
               break
         elif isinstance( paramName, list ) and destructuring:
            pass   # destructuring pattern — handled below
         else:
            raise LArgBindingError( f"Positional param {paramNum} expected to be a symbol." )

         try:
            argVal = argList[argNum]
         except IndexError:
            raise LArgBindingError( "Too few positional arguments." )

         if isinstance( paramName, LSymbol ):
            self._bindings[paramName.name] = argVal
         else:
            self._destructuringBind( paramName, argVal )

         paramNum += 1
         argNum   += 1

      return paramNum, argNum

   def _destructuringBind( self, pattern: list[Any], value: Any ) -> None:
      '''Bind value against a nested destructuring lambda list pattern.'''
      if not isinstance( value, list ):
         raise LArgBindingError(
            f'Destructuring pattern expects a list argument, got {type(value).__name__}.' )
      self.bindArguments( pattern, value, destructuring=True, _validate=False )

   def _bindOptionalArgs( self, lambdaListAST: list[Any], paramNum: int,
                          argList: Sequence[Any], argNum: int ) -> tuple[int, int]:
      '''Syntax:  &optional {var | (var [initform [pvar]])}*'''
      paramListLength = len(lambdaListAST)
      argListLength   = len(argList)

      while paramNum < paramListLength:
         paramSpec = lambdaListAST[paramNum]

         if isinstance( paramSpec, LSymbol ):
            if paramSpec.startswith('&'):
               break
            varName  = paramSpec
            initForm = list( )
            pvarName = None
         elif isinstance(paramSpec, list):
            paramSpecLen = len(paramSpec)
            if paramSpecLen == 1:
               varName  = paramSpec[0]
               initForm = list()
               pvarName = None
            elif paramSpecLen == 2:
               varName, initForm = paramSpec
               pvarName = None
            elif paramSpecLen == 3:
               varName, initForm, pvarName = paramSpec
            else:
               raise LArgBindingError( 'Parameter spec following &OPTIONAL must be a list of (<variable> [<defaultvalue> [<pvar>]] ).' )

            if not isinstance(varName, LSymbol):
               raise LArgBindingError( 'Parameter variable in &OPTIONAL spec must be a symbol.' )
            if varName.startswith('&'):
               raise LArgBindingError( f'Lambda list keyword {varName} cannot be used as a variable name in &OPTIONAL spec.' )
            if pvarName and (not isinstance(pvarName, LSymbol)):
               raise LArgBindingError( f'Parameter pvar following {varName} must be a symbol.' )
            if isinstance(pvarName, LSymbol) and pvarName.startswith('&'):
               raise LArgBindingError( f'Lambda list keyword {pvarName} cannot be used as a supplied-p variable in &OPTIONAL spec.' )
         else:
            raise LArgBindingError( 'Parameter spec following &OPTIONAL must be a <variable> or a list of (<variable> <defaultvalue>). ' )
         paramNum += 1

         originalInitForm = initForm                  # save before potential overwrite
         if argNum < argListLength:
            initForm = argList[argNum]

         if (argNum >= argListLength) or (isinstance(initForm, LSymbol) and (initForm.startswith(':'))):
            initForm = self._evalFn( self, originalInitForm )   # evaluate the spec default
            pvarVal  = list()   # Nil, False
         else:
            argNum  += 1
            pvarVal  = self.lookupGlobal('T')   # T, True

         self._bindings[varName.name] = initForm

         if pvarName:
            self._bindings[pvarName.name] = pvarVal

      return paramNum, argNum

   def _bindRestArgs( self, lambdaListAST: list[Any], paramNum: int,
                      argList: Sequence[Any], argNum: int ) -> tuple[int, int]:
      '''Syntax:  &rest var'''
      try:
         paramName = lambdaListAST[paramNum]
      except IndexError:
         raise LArgBindingError( f'Param name expected after &rest.' )

      if not isinstance(paramName, LSymbol ) or paramName.startswith('&'):
         raise LArgBindingError( 'Symbol expected after &rest.' )

      theRestArgs = argList[argNum:]
      self._bindings[paramName.name] = list(theRestArgs)

      return paramNum + 1, argNum

   def _bindKeyArgs( self, lambdaListAST: list[Any], paramNum: int,
                     argList: Sequence[Any], argNum: int ) -> tuple[int, int]:
      '''syntax:  &key {var | ( {var | ( keyword var )} [initForm [pvar]])}* [&allow-other-keys]'''
      paramListLength = len(lambdaListAST)
      argListLength   = len(argList)

      keysDict      = {}    # keyStr -> (varName, pvarName, initForm)
      keyParamOrder = []    # ordered list of keyStr for incremental evaluation

      # ---- Phase 1: Parse key parameter specs (store initForms, don't evaluate yet) ----
      while paramNum < paramListLength:
         paramSpec = lambdaListAST[paramNum]
         if isinstance(paramSpec, LSymbol):
            if paramSpec.startswith('&'):
               break
            keyName  = paramSpec
            varName  = paramSpec
            initForm = list()
            pvarName = None
         elif isinstance(paramSpec, list):
            if len(paramSpec) == 0:
               raise LArgBindingError( f'Empty parameter spec () in &KEY lambda list.' )
            keyVarSpec, *initFormSpec = paramSpec

            if isinstance(keyVarSpec, LSymbol):
               if keyVarSpec.startswith('&'):
                  raise LArgBindingError( f'Lambda list keyword {keyVarSpec} cannot be used as a variable name in &KEY spec.' )
               keyName = keyVarSpec
               varName = keyVarSpec
            elif isinstance(keyVarSpec, list):
               try:
                  keyName, varName = keyVarSpec
               except ValueError:
                  raise LArgBindingError( f'Key Var pair following &KEY must contain exactly two elements.' )
               if not isinstance(keyName, LSymbol) or not keyName.startswith(':'):
                  raise LArgBindingError( f'The key in a &KEY (keyword var) pair must be a keyword symbol (e.g. :mykey).' )
               if not isinstance(varName, LSymbol):
                  raise LArgBindingError( f'Variable in &KEY (keyword var) pair must be a symbol.' )
            else:
               raise LArgBindingError( f'&KEY key/var spec must be either a symbol or a list (:keySymbol varSymbol).' )

            initFormSpecLen = len(initFormSpec)
            if initFormSpecLen == 0:
               initForm = list()
               pvarName = None
            elif initFormSpecLen == 1:
               initForm = initFormSpec[0]
               pvarName = None
            elif initFormSpecLen == 2:
               initForm, pvarName = initFormSpec
               if pvarName and not isinstance(pvarName, LSymbol):
                  raise LArgBindingError( f'pvar for &KEY parameter {varName} must be a symbol.' )
               if isinstance(pvarName, LSymbol) and pvarName.startswith('&'):
                  raise LArgBindingError( f'Lambda list keyword {pvarName} cannot be used as a supplied-p variable in &KEY spec.' )
            else:
               raise LArgBindingError( f'Too many arguments specified in a parameter keyword initialization list.' )
         else:
            raise LArgBindingError( f'Parameter spec following &KEY must be a symbol or a list.' )

         keyStr = keyName.name[1:] if keyName.startswith(':') else keyName.name
         keysDict[keyStr]   = (varName, pvarName, initForm)
         keyParamOrder.append( keyStr )
         paramNum += 1

      # ---- Phase 2: Handle &allow-other-keys in lambda list ----
      allowOtherKeys = False
      if paramNum < paramListLength:
         nextParam = lambdaListAST[paramNum]
         if isinstance(nextParam, LSymbol) and (nextParam == '&ALLOW-OTHER-KEYS'):
            allowOtherKeys = True
            paramNum += 1

      # ---- Phase 3: Scan supplied keyword arguments ----
      # Pre-scan for :allow-other-keys in args (CL 3.4.1.4.1)
      scanIdx = argNum
      while scanIdx + 1 < argListLength:
         scanArg = argList[scanIdx]
         if isinstance(scanArg, LSymbol) and scanArg.name == ':ALLOW-OTHER-KEYS':
            if argList[scanIdx + 1] != list():   # non-NIL value enables it
               allowOtherKeys = True
            break
         scanIdx += 2

      # Collect supplied keyword arguments (first occurrence wins per CL 3.4.1.4.1)
      suppliedArgs = {}    # keyStr -> argVal
      while argNum < argListLength:
         keyArg = argList[argNum]
         if (not isinstance(keyArg, LSymbol)) or (not keyArg.startswith(':')):
            raise LArgBindingError( f'Keyword expected, found {keyArg}.' )
         keyArgStr = keyArg.name[1:]  # Strip the leading colon
         argNum += 1

         try:
            argVal = argList[argNum]
         except IndexError:
            raise LArgBindingError( f'Keyword {keyArgStr} expected to be followed by a value.' )
         argNum += 1

         # :allow-other-keys is always accepted; skip binding if not a declared key param
         if keyArgStr == 'ALLOW-OTHER-KEYS' and keyArgStr not in keysDict:
            continue

         if (not allowOtherKeys) and (keyArgStr not in keysDict):
            raise LArgBindingError( f'Unexpected keyword found {keyArgStr}.' )

         # First occurrence wins; skip unknown keys when allowOtherKeys
         if keyArgStr in keysDict and keyArgStr not in suppliedArgs:
            suppliedArgs[keyArgStr] = argVal

      # ---- Phase 4: Bind key parameters incrementally (CL 3.4.1 eval order) ----
      nilVal = list()
      tVal   = self.lookupGlobal('T')
      for keyStr in keyParamOrder:
         varName, pvarName, initForm = keysDict[keyStr]
         if keyStr in suppliedArgs:
            self._bindings[varName.name] = suppliedArgs[keyStr]
            if pvarName:
               self._bindings[pvarName.name] = tVal
         else:
            self._bindings[varName.name] = self._evalFn(self, initForm)
            if pvarName:
               self._bindings[pvarName.name] = nilVal

      return paramNum, argNum

   def _bindAuxArgs( self, lambdaListAST: list[Any], paramNum: int,
                     argList: Sequence[Any], argNum: int ) -> tuple[int, int]:
      '''Syntax:  &aux {var | (var [initForm])}*
      These are not really arguments.  At least: these parameters get no corresponding arguments.
      These parameters are strictly local variables for the function.'''
      paramListLength = len(lambdaListAST)

      while paramNum < paramListLength:
         paramSpec = lambdaListAST[paramNum]

         if isinstance( paramSpec, LSymbol ):
            if paramSpec.startswith('&'):
               raise LArgBindingError( f'{paramSpec} occurs after &AUX.' )
            varName  = paramSpec
            initForm = list( )
         elif isinstance(paramSpec, list):
            paramSpecLen = len(paramSpec)
            if paramSpecLen == 1:
               varName  = paramSpec[0]
               initForm = list()
            elif paramSpecLen == 2:
               varName, initForm = paramSpec
            else:
               raise LArgBindingError( 'Parameter spec following &AUX must be a list of (<variable> [<defaultvalue>]).' )

            if not isinstance(varName, LSymbol) or varName.startswith('&'):
               raise LArgBindingError( 'Parameter spec following &AUX must be a list of (<variable> [<defaultvalue>]).' )

            initForm = self._evalFn( self, initForm )
         else:
            raise LArgBindingError( 'Parameter spec following &AUX must be a <variable> or a list of (<variable> [<defaultvalue>]).' )

         self._bindings[varName.name] = initForm

         paramNum += 1

      return paramNum, argNum

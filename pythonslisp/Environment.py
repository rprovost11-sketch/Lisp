"""Environment - Lisp lexical environment with argument binding.

Manages lexical scoping, variable binding, and the argument-binding
logic needed by the evaluator.  Keeping binding logic here makes it
part of the environment object that it mutates.

evalFn is stored as an instance attribute so argument-binding methods
do not need to carry it as a parameter.  Child environments inherit
evalFn from their parent; the evaluator passes ctx.lEval when creating
a new scope so default-value evaluation always uses the current call context.
"""
from __future__ import annotations

from typing import Any, Callable, Sequence

from pythonslisp.AST import LSymbol, derive_arity, arity_mismatch_msg
from pythonslisp.Exceptions import LArgBindingError


class Environment:
   __slots__ = ('_bindings', '_parent', '_GLOBAL_ENV', '_evalFn')

   def __init__( self, parent: (Environment|None) = None,
                 initialBindings: (dict[str, Any]|None) = None,
                 evalFn: (Callable|None) = None ) -> None:
      self._bindings: dict[str, Any]    = initialBindings if initialBindings is not None else {}
      self._parent:   (Environment|None) = parent
      self._GLOBAL_ENV: Environment      = parent._GLOBAL_ENV if parent else self
      if evalFn is not None:
         self._evalFn = evalFn
      elif parent is not None:
         self._evalFn = parent._evalFn
      else:
         self._evalFn = None

   # -----------------------------------------------------------------------
   # Basic binding and lookup
   # -----------------------------------------------------------------------

   def updateLocals( self, newValues: dict[str, Any] ):
      self._bindings.update( newValues )

   def bindLocal( self, key: str, value: Any ) -> Any:
      self._bindings[ key ] = value
      return value

   def bindGlobal( self, key: str, value: Any ) -> Any:
      self._GLOBAL_ENV._bindings[ key ] = value
      return value

   def lookupLocal( self, key: str ) -> Any:
      return self._bindings[ key ]

   def lookupGlobal( self, key: str ) -> Any:
      return self._GLOBAL_ENV._bindings[ key ]

   def lookupLocalWithDefault( self, key: str, dfltVal: Any = None ) -> Any:
      return self._bindings.get( key, dfltVal )

   def lookupGlobalWithDefault( self, key: str, dfltVal: Any = None ) -> Any:
      return self._GLOBAL_ENV._bindings.get( key, dfltVal )

   def unbind( self, key: str ) -> None:
      self._bindings.pop( key, None )

   def getGlobalEnv( self ) -> Environment:
      return self._GLOBAL_ENV

   def localSymbols( self ) -> list[str]:
      return sorted( self._bindings.keys() )

   def parentEnv( self ) -> (Environment|None):
      return self._parent

   def findDef( self, key: str ) -> (Environment|None):
      '''Starting from the local-most scope, searches for the scope in which
      key is defined and returns that environment.  Returns None if not found.'''
      scope: (Environment|None) = self
      while scope:
         if key in scope._bindings:
            break
         scope = scope._parent
      return scope

   def bind( self, key: str, value: Any ) -> Any:
      '''Bind using Lisp semantics: update existing binding in nearest enclosing
      scope; if not found, bind at global scope.'''
      scope = self
      while scope:
         if key in scope._bindings:
            scope._bindings[ key ] = value
            return value
         scope = scope._parent
      self._GLOBAL_ENV._bindings[ key ] = value
      return value

   def lookup( self, key: str ) -> Any:
      '''Lookup using Lisp semantics: walk the scope chain.'''
      scope: (Environment|None) = self
      while scope:
         if key in scope._bindings:
            return scope._bindings[ key ]
         scope = scope._parent
      raise KeyError

   # -----------------------------------------------------------------------
   # Argument binding
   # -----------------------------------------------------------------------

   @staticmethod
   def _arityMsg( lambdaListAST: list, nProvided: int ) -> str:
      """Build the 'N provided; M expected' portion of an arity error."""
      min_a, max_a = derive_arity( lambdaListAST )
      return arity_mismatch_msg( min_a, max_a, nProvided )

   def bindArguments( self, lambdaListAST: list[Any], argList: Sequence[Any],
                      destructuring: bool = False ) -> None:
      argListLength = len(argList)

      paramNum, argNum = self._bindPositionalArgs( lambdaListAST, 0, argList, 0, destructuring )

      try:
         nextParam = lambdaListAST[paramNum]
      except IndexError:
         if argNum < argListLength:
            raise LArgBindingError( f'Too many arguments. {self._arityMsg(lambdaListAST, argListLength)}' )
         return

      if nextParam.name == '&OPTIONAL':
         paramNum, argNum = self._bindOptionalArgs( lambdaListAST, paramNum+1, argList, argNum )

         try:
            nextParam = lambdaListAST[paramNum]
         except IndexError:
            if argNum < argListLength:
               raise LArgBindingError( f'Too many arguments. {self._arityMsg(lambdaListAST, argListLength)}' )
            return

      rest_was_bound = False
      if nextParam.name == '&REST' or (nextParam.name == '&BODY' and destructuring):
         paramNum, argNum = self._bindRestArgs( lambdaListAST, paramNum+1, argList, argNum )
         rest_was_bound = True

         try:
            nextParam = lambdaListAST[paramNum]
         except IndexError:
            return

      if nextParam.name == '&KEY':
         paramNum, argNum = self._bindKeyArgs( lambdaListAST, paramNum+1, argList, argNum,
                                               skip_non_keywords=rest_was_bound )

         try:
            nextParam = lambdaListAST[paramNum]
         except IndexError:
            return

      if nextParam.name == '&AUX':
         self._bindAuxArgs( lambdaListAST, paramNum+1, argList, argNum )

   def _bindPositionalArgs( self, lambdaListAST: list[Any], paramNum: int,
                            argList: Sequence[Any], argNum: int,
                            destructuring: bool = False ) -> tuple[int, int]:
      paramListLength = len(lambdaListAST)

      while paramNum < paramListLength:
         paramName = lambdaListAST[paramNum]
         if isinstance( paramName, LSymbol ):
            if paramName.startswith('&'):
               break
         # else: list destructuring pattern (validated at definition time)

         try:
            argVal = argList[argNum]
         except IndexError:
            raise LArgBindingError( f'Too few arguments. {self._arityMsg(lambdaListAST, argNum)}' )

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
      self.bindArguments( pattern, value, destructuring=True )

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
            initForm = list()
            pvarName = None
         else:   # list spec (validated at definition time)
            paramSpecLen = len(paramSpec)
            if paramSpecLen == 1:
               varName  = paramSpec[0]
               initForm = list()
               pvarName = None
            elif paramSpecLen == 2:
               varName, initForm = paramSpec
               pvarName = None
            else:   # == 3
               varName, initForm, pvarName = paramSpec
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
      paramName   = lambdaListAST[paramNum]   # guaranteed valid by analyzeLambdaList
      theRestArgs = argList[argNum:]
      self._bindings[paramName.name] = list(theRestArgs)
      return paramNum + 1, argNum

   def _bindKeyArgs( self, lambdaListAST: list[Any], paramNum: int,
                     argList: Sequence[Any], argNum: int,
                     skip_non_keywords: bool = False ) -> tuple[int, int]:
      '''syntax:  &key {var | ( {var | ( keyword var )} [initForm [pvar]])}* [&allow-other-keys]
      skip_non_keywords: when True (after &rest), non-keyword args in the arg list are
      silently skipped rather than raising an error - they are already captured by &rest.'''
      paramListLength = len(lambdaListAST)
      argListLength   = len(argList)

      keysDict      = {}    # keyStr -> (varName, pvarName, initForm)
      keyParamOrder = []    # ordered list of keyStr for incremental evaluation

      # ---- Phase 1: Build keysDict from lambda list key specs ----
      while paramNum < paramListLength:
         paramSpec = lambdaListAST[paramNum]
         if isinstance(paramSpec, LSymbol):
            if paramSpec.startswith('&'):
               break
            keyName  = paramSpec
            varName  = paramSpec
            initForm = list()
            pvarName = None
         else:   # list spec (validated at definition time)
            keyVarSpec, *initFormSpec = paramSpec

            if isinstance(keyVarSpec, LSymbol):
               keyName = keyVarSpec
               varName = keyVarSpec
            else:   # (:key var) pair
               keyName, varName = keyVarSpec

            initFormSpecLen = len(initFormSpec)
            if initFormSpecLen == 0:
               initForm = list()
               pvarName = None
            elif initFormSpecLen == 1:
               initForm = initFormSpec[0]
               pvarName = None
            else:   # == 2
               initForm, pvarName = initFormSpec

         keyStr = keyName.name[1:] if keyName.startswith(':') else keyName.name
         keysDict[keyStr]   = (varName, pvarName, initForm)
         keyParamOrder.append( keyStr )
         paramNum += 1

      # ---- Phase 2: Handle &allow-other-keys in lambda list ----
      allowOtherKeys = False
      if paramNum < paramListLength:
         nextParam = lambdaListAST[paramNum]
         if isinstance(nextParam, LSymbol) and nextParam.name == '&ALLOW-OTHER-KEYS':
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
         keyArg  = argList[argNum]
         key_pos = argNum + 1  # 1-indexed position of this keyword in the arg list
         if (not isinstance(keyArg, LSymbol)) or (not keyArg.startswith(':')):
            if skip_non_keywords:
               argNum += 1
               continue
            raise LArgBindingError( f'Argument {key_pos}: keyword symbol expected, found {keyArg}.' )
         keyArgStr = keyArg.name[1:]  # Strip the leading colon
         argNum += 1

         try:
            argVal = argList[argNum]
         except IndexError:
            # No value follows this keyword symbol.  When skip_non_keywords is
            # True (i.e. we're in a &rest ... &key lambda list) an undeclared
            # keyword with no value is a keyword symbol that was already captured
            # by &rest - treat it as data and skip rather than raising an error.
            if skip_non_keywords and keyArgStr not in keysDict:
               continue
            raise LArgBindingError( f'Argument {key_pos}: keyword :{keyArgStr} must be followed by a value.' )
         argNum += 1

         # :allow-other-keys is always accepted; skip binding if not a declared key param
         if keyArgStr == 'ALLOW-OTHER-KEYS' and keyArgStr not in keysDict:
            continue

         if (not allowOtherKeys) and (keyArgStr not in keysDict):
            raise LArgBindingError( f'Argument {key_pos}: unexpected keyword :{keyArgStr}.' )

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
            varName  = paramSpec
            initForm = list()
         else:   # list spec (validated at definition time)
            paramSpecLen = len(paramSpec)
            if paramSpecLen == 1:
               varName  = paramSpec[0]
               initForm = list()
            else:   # == 2
               varName, initForm = paramSpec
            initForm = self._evalFn( self, initForm )

         self._bindings[varName.name] = initForm
         paramNum += 1

      return paramNum, argNum


class ModuleEnvironment(Environment):
   """A module-root environment.  All top-level definitions made during
   module loading bind here rather than in the global environment.
   ModuleEnvironment instances are first-class Lisp values."""

   __slots__ = ('name',)

   def __init__( self, name: str,
                 parent: (Environment|None) = None,
                 evalFn: (Callable|None) = None ) -> None:
      super().__init__( parent=parent, evalFn=evalFn )
      self.name = name

   def bind( self, key: str, value: Any ) -> Any:
      '''At module top level, always bind locally.  Child environments
      created inside the module (let scopes, function bodies) are plain
      Environment instances and use the standard walk-up bind(), which
      correctly finds and updates module-level bindings via the parent chain.'''
      self._bindings[key] = value
      return value

   def __repr__( self ) -> str:
      return f'#<MODULE {self.name}>'

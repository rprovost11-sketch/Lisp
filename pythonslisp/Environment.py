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

from pythonslisp.AST import LSymbol, arity_mismatch_msg
from pythonslisp.Exceptions import LArgBindingError
from pythonslisp.LambdaList import CompiledLambdaList


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

   def bindArguments( self, compiledLL: CompiledLambdaList, argList: Sequence[Any] ) -> None:
      argNum = self._bindPositionalArgs( compiledLL, argList, 0 )

      if compiledLL.optional:
         argNum = self._bindOptionalArgs( compiledLL, argList, argNum )

      if compiledLL.rest is not None:
         argNum, rest_was_bound = self._bindRestArgs( compiledLL, argList, argNum )
      else:
         rest_was_bound = False

      if compiledLL.keys is not None:
         self._bindKeyArgs( compiledLL, argList, argNum, skip_non_keywords=rest_was_bound )
      elif not rest_was_bound and argNum < len(argList):
         raise LArgBindingError(
            f'Too many arguments. {arity_mismatch_msg(compiledLL.min_args, compiledLL.max_args, len(argList))}' )

      if compiledLL.aux:
         self._bindAuxArgs( compiledLL )

   def _bindPositionalArgs( self, compiledLL: CompiledLambdaList,
                            argList: Sequence[Any], argNum: int ) -> int:
      for paramSpec in compiledLL.positional:
         try:
            argVal = argList[argNum]
         except IndexError:
            raise LArgBindingError(
               f'Too few arguments. {arity_mismatch_msg(compiledLL.min_args, compiledLL.max_args, argNum)}' )
         if type( paramSpec ) is str:
            self._bindings[paramSpec] = argVal
         else:   # nested CompiledLambdaList — destructuring sub-pattern
            self._destructuringBind( paramSpec, argVal )
         argNum += 1
      return argNum

   def _destructuringBind( self, compiledLL: CompiledLambdaList, value: Any ) -> None:
      '''Bind value against a nested destructuring lambda list pattern.'''
      if not isinstance( value, list ):
         raise LArgBindingError(
            f'Destructuring pattern expects a list argument, got {type(value).__name__}.' )
      self.bindArguments( compiledLL, value )

   def _bindOptionalArgs( self, compiledLL: CompiledLambdaList,
                          argList: Sequence[Any], argNum: int ) -> int:
      '''Syntax:  &optional {var | (var [initform [pvar]])}*'''
      argListLength = len(argList)
      for varName, initFormSpec, pvarName in compiledLL.optional:
         if argNum < argListLength:
            nextArg = argList[argNum]
         else:
            nextArg = None

         if argNum >= argListLength or ( type(nextArg) is LSymbol and nextArg.startswith(':') ):
            boundVal = self._evalFn( self, initFormSpec )   # evaluate the spec default
            pvarVal  = list()                                # Nil / False
         else:
            boundVal = nextArg
            argNum  += 1
            pvarVal  = self.lookupGlobal('T')               # T / True

         self._bindings[varName] = boundVal
         if pvarName:
            self._bindings[pvarName] = pvarVal

      return argNum

   def _bindRestArgs( self, compiledLL: CompiledLambdaList,
                      argList: Sequence[Any], argNum: int ) -> tuple[int, bool]:
      '''Syntax:  &rest var'''
      if compiledLL.rest is None:
         return argNum, False
      self._bindings[compiledLL.rest] = list( argList[argNum:] )
      return argNum, True

   def _bindKeyArgs( self, compiledLL: CompiledLambdaList,
                     argList: Sequence[Any], argNum: int,
                     skip_non_keywords: bool = False ) -> None:
      '''Syntax:  &key {var | ({var | (:keyword var)} [initForm [pvar]])}* [&allow-other-keys]
      skip_non_keywords: when True (after &rest), non-keyword args in the arg list are
      silently skipped rather than raising an error — they are already captured by &rest.'''
      keys      = compiledLL.keys        # dict[keyStr -> (varName, pvarName, initForm)]
      key_order = compiledLL.key_order
      argListLength = len(argList)

      allowOtherKeys = compiledLL.allow_other_keys

      # Pre-scan for :allow-other-keys in args (CL 3.4.1.4.1)
      scanIdx = argNum
      while scanIdx + 1 < argListLength:
         scanArg = argList[scanIdx]
         if type(scanArg) is LSymbol and scanArg.name == ':ALLOW-OTHER-KEYS':
            if argList[scanIdx + 1] != list():   # non-NIL value enables it
               allowOtherKeys = True
            break
         scanIdx += 2

      # Collect supplied keyword arguments (first occurrence wins per CL 3.4.1.4.1)
      suppliedArgs = {}
      while argNum < argListLength:
         keyArg  = argList[argNum]
         key_pos = argNum + 1
         if (type(keyArg) is not LSymbol) or (not keyArg.startswith(':')):
            if skip_non_keywords:
               argNum += 1
               continue
            raise LArgBindingError( f'Argument {key_pos}: keyword symbol expected, found {keyArg}.' )
         keyArgStr = keyArg.name[1:]   # strip leading colon
         argNum += 1

         try:
            argVal = argList[argNum]
         except IndexError:
            # No value follows this keyword symbol.  When skip_non_keywords is True
            # an undeclared keyword with no value was already captured by &rest —
            # treat it as data and skip rather than raising an error.
            if skip_non_keywords and keyArgStr not in keys:
               continue
            raise LArgBindingError(
               f'Argument {key_pos}: keyword :{keyArgStr} must be followed by a value.' )
         argNum += 1

         # :allow-other-keys is always accepted; skip binding if not a declared key param
         if keyArgStr == 'ALLOW-OTHER-KEYS' and keyArgStr not in keys:
            continue

         if (not allowOtherKeys) and (keyArgStr not in keys):
            raise LArgBindingError( f'Argument {key_pos}: unexpected keyword :{keyArgStr}.' )

         # First occurrence wins; skip unknown keys when allowOtherKeys
         if keyArgStr in keys and keyArgStr not in suppliedArgs:
            suppliedArgs[keyArgStr] = argVal

      # Bind key parameters in declaration order (CL 3.4.1 incremental eval)
      nilVal = list()
      tVal   = self.lookupGlobal('T')
      for keyStr in key_order:
         varName, pvarName, initForm = keys[keyStr]
         if keyStr in suppliedArgs:
            self._bindings[varName] = suppliedArgs[keyStr]
            if pvarName:
               self._bindings[pvarName] = tVal
         else:
            self._bindings[varName] = self._evalFn( self, initForm )
            if pvarName:
               self._bindings[pvarName] = nilVal

   def _bindAuxArgs( self, compiledLL: CompiledLambdaList ) -> None:
      '''Syntax:  &aux {var | (var [initForm])}*
      These are local variables, not true arguments.'''
      for varName, initForm in compiledLL.aux:
         self._bindings[varName] = self._evalFn( self, initForm )


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

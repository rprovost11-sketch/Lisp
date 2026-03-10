from __future__ import annotations
from typing import Any

class EnvironmentBase:
   __slots__ = ('_bindings', '_parent', '_GLOBAL_ENV')

   def __init__( self, parent: (EnvironmentBase|None)=None, initialBindings: (dict[str, Any]|None)=None ):
      self._bindings: dict[str, Any] = initialBindings if initialBindings is not None else {}
      self._parent: (EnvironmentBase | None) = parent
      self._GLOBAL_ENV: EnvironmentBase = parent._GLOBAL_ENV if parent else self

   def updateLocals( self, newValues: dict[str, Any] ):
      self._bindings.update(newValues)

   def bindLocal( self, key: str, value: Any ) -> Any:
      self._bindings[ key ] = value
      return value

   def bindGlobal( self, key: str, value: Any ) -> Any:
      self._GLOBAL_ENV._bindings[ key ] = value
      return value

   def lookupLocal( self, key: str ) -> Any:
      return self._bindings[key]
   
   def lookupGlobal(self, key: str ) -> Any:
      return self._GLOBAL_ENV._bindings[ key ]

   def lookupLocalWithDefault( self, key: str, dfltVal: Any = None ) -> Any:
      return self._bindings.get( key, dfltVal )

   def lookupGlobalWithDefault( self, key: str, dfltVal: Any = None ) -> Any:
      return self._GLOBAL_ENV._bindings.get(key, dfltVal)

   def unbind( self, key: str ) -> None:
      scope: (EnvironmentBase | None) = self
      while scope:
         if key in scope._bindings:
            del scope._bindings[ key ]
            return
         scope = scope._parent

   def getGlobalEnv( self ) -> EnvironmentBase:
      return self._GLOBAL_ENV

   def localSymbols( self ) -> list[str]:
      return sorted( self._bindings.keys() )

   def parentEnv( self ) -> (EnvironmentBase | None):
      return self._parent

   def findDef( self, key: str ) -> (EnvironmentBase | None):
      '''Starting from the local-most scope, this function searches for the
      scope in which key is defined and returns that SymbolTable.
      If the key is not defined, None is returned.'''
      scope: (EnvironmentBase | None) = self
      while scope:
         if key in scope._bindings:
            break
         scope = scope._parent
      return scope          # Returns None if the key isn't located.

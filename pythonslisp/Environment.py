from typing import Any

class Environment( object ):
   def __init__( self, parent: (Environment|None)=None, **initialNameValDict: dict[str, Any]):
      self._bindings: dict[str, Any] = initialNameValDict.copy()
      self._parent: (Environment | None) = parent
      self._GLOBAL_SCOPE: Environment = parent._GLOBAL_SCOPE if parent else self

   def updateLocals( self, newValues: dict[str, Any] ):
      self._bindings.update(newValues)
   
   def bindLocal( self, key: str, value: Any ) -> Any:
      self._bindings[ key ] = value
      return value

   def bindGlobal( self, key: str, value: Any ) -> Any:
      self._GLOBAL_SCOPE._bindings[ key ] = value
      return value

   def getValue( self,  key: str) -> Any:
      scope: (Environment | None) = self
      while scope:
         if key in scope._bindings:
            return scope._bindings[key]
         scope = scope._parent
      raise KeyError

   def getValue2( self, key: str) -> Any:
      '''This is slover than getValue().  While the lookup is fast, the
      exception handling (which is needed to traverse the parent list) is
      exceedingly slow.  Here's the code I tested these functions on:
         (defun test ()
            (let ()
               (let ()
                  (dotimes (i 1000000)
                     pi e))))
      Looking up pi 1,000,000 times.  pi is defined globally, but the search
      for pi is started three scope levels in.
         getValue()  eval time between 0.44 and 0.74 seconds.
         getValue2() eval time between 4.52 and 6.28 seconds.
         getValue3() is closer in performance to getValue() but still slower.
      by calling the versions of getValue() from LispInterpreter._lEval()
      '''
      scope: (Environment | None) = self
      while scope:
         try:
            return scope._bindings[key]
         except KeyError:
            scope = scope._parent
      raise KeyError

   def getValue3( self,  key: str) -> Any:
      scope: (Environment | None) = self
      while scope:
         val = scope._bindings.get(key)
         if val is None:
            scope = scope._parent
         else:
            return val
      raise KeyError

   def getGlobalValue(self, key: str ) -> Any:
      return self._GLOBAL_SCOPE._bindings[ key ]

   def getGlobalEnv( self ) -> Environment:
      return self._GLOBAL_SCOPE

   def undef( self, key: str ) -> None:
      scope: (Environment | None) = self
      while scope:
         if key in scope._bindings:
            del scope._bindings[ key ]
            return
         scope = scope._parent

   def localSymbols( self ) -> list[str]:
      return sorted( self._bindings.keys() )
   
   def parentEnv( self ) -> (Environment | None):
      return self._parent

   def findDef( self, key: str ) -> (Environment | None):
      '''Starting from the local-most scope, this function searches for the
      scope in which key is defined and returns that SymbolTable.
      If the key is not defined, None is returned.'''
      scope: (Environment | None) = self
      while scope:
         if key in scope._bindings:
            break
         scope = scope._parent
      return scope          # Returns None if the key isn't located.


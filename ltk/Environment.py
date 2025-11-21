from typing import Any, List, Dict

class Environment( object ):
   GLOBAL_SCOPE: (Environment | None) = None

   def __init__( self, parent: (Environment|None)=None, **initialNameValDict):
      self._parent: (Environment | None) = parent
      self._locals: Dict[str, Any] = initialNameValDict.copy()
      if Environment.GLOBAL_SCOPE is None:
         Environment.GLOBAL_SCOPE = self

   def reInitialize( self, **initialNameValDict ) -> Environment:
      root = Environment.GLOBAL_SCOPE
      assert isinstance(root, Environment)
      root._locals = initialNameValDict.copy()
      return root

   def defLocal( self, key: str, value: Any ) -> Any:
      self._locals[ key ] = value
      return value

   def defGlobal( self, key: str, value: Any ) -> Any:
      assert isinstance(Environment.GLOBAL_SCOPE, Environment)
      Environment.GLOBAL_SCOPE._locals[ key ] = value
      return value

   def getValue( self, key: str ) -> Any:
      scope: (Environment | None) = self
      while scope:
         try:
            return scope._locals[ key ]
         except KeyError:
            scope = scope._parent

      return None

   def getGlobalValue(self, key: str ) -> Any:
      assert isinstance(self.GLOBAL_SCOPE, Environment)
      return self.GLOBAL_SCOPE._locals[ key ]

   def undef( self, key: str ) -> None:
      scope: (Environment | None) = self
      while scope:
         try:
            del scope._locals[ key ]
            return
         except KeyError:
            scope = scope._parent

   def localSymbols( self ) -> List[str]:
      return sorted( self._locals.keys() )

   def parentEnv( self ) -> (Environment | None):
      return self._parent

   def openScope( self ) -> Environment:
      return Environment( self )

   def closeScope( self ) -> (Environment | None):
      return self._parent

   def isDefined( self, key: str ) -> bool:
      scope: (Environment | None) = self
      while scope:
         if key in scope._locals:
            return True

         scope = scope._parent

      return False

   def findDef( self, key: str ) -> (Environment | None):
      '''Starting from the local-most scope, this function searches for the
      scope in which a symbol (key) is defined and returns that SymbolTable.
      If the key is not defined, None is returned.'''
      scope: (Environment | None) = self
      while scope:
         if key in scope._locals:
            return scope

         scope = scope._parent

      return None

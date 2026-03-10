from __future__ import annotations

class LRuntimeError( Exception ):
   pass


class LRuntimePrimError( LRuntimeError ):
   def __init__( self, lispCallable, errorMsg: str ) -> None:
      prim   = getattr( lispCallable, 'primitive', lispCallable )
      fnName = prim.name
      usage  = prim.usageString()
      errStr = f"ERROR '{fnName}': {errorMsg}\n{usage}" if usage else f"ERROR '{fnName}': {errorMsg}"
      super().__init__( errStr )


class LArgBindingError( LRuntimeError ):
   pass


class LAnalysisError( LRuntimeError ):
   pass


# Non-Error exceptions.  These are used not for error handling but to
# implement Common Lisp features.

class ReturnFrom( Exception ):
   """Raised by return-from.  Propagates up to the matching block handler."""
   __slots__ = ('name', 'value')

   def __init__( self, name: str, value ) -> None:
      self.name = name    # uppercased block name
      self.value = value
      super().__init__()


class ContinuationInvoked( Exception ):
   """Raised when an escape continuation is invoked.  Propagates up to the matching call/cc handler."""
   __slots__ = ('token', 'value')

   def __init__( self, token: object, value ) -> None:
      self.token = token
      self.value = value
      super().__init__()


class Thrown( Exception ):
   """Raised by throw.  Propagates up to the nearest enclosing catch with a matching tag."""
   __slots__ = ('tag', 'value')

   def __init__( self, tag, value ) -> None:
      self.tag   = tag
      self.value = value
      super().__init__()

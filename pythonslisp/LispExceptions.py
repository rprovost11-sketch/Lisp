from pythonslisp.LispAST import LPrimitive


class LispRuntimeError( Exception ):
   pass


class LispRuntimeFuncError( LispRuntimeError ):
   def __init__( self, lispCallable: LPrimitive, errorMsg: str ) -> None:
      fnName = lispCallable.name
      usage = lispCallable.usageString()
      errStr = f"ERROR '{fnName}': {errorMsg}\nUSAGE: {usage}" if usage else f"ERROR '{fnName}': {errorMsg}"
      super().__init__( errStr )


class LispArgBindingError( LispRuntimeError ):
   pass


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

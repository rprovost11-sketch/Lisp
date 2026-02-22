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

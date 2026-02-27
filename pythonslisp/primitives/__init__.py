from typing import Callable, Any

from pythonslisp.LispAST import LPrimitive, L_T, L_NIL


def constructPrimitives( parseLispString: Callable[[str], Any] ) -> dict[str, Any]:
   primitiveDict: dict[str, Any] = {}
   primitiveDict[ 'T'   ] = L_T
   primitiveDict[ 'NIL' ] = L_NIL

   class primitive:
      def __init__( self, primitiveSymbolString: str, paramsString: str = '', specialForm: bool = False,
                    min_args: int = 0, max_args: (int|None) = None, arity_msg: str = '' ) -> None:
         self._name:         str       = primitiveSymbolString.upper()
         self._paramsString: str       = paramsString
         self._specialForm:  bool      = specialForm
         self._min_args:     int       = min_args
         self._max_args:     (int|None)= max_args
         self._arity_msg:    str       = arity_msg
      def __call__( self, pythonFn ):
         docString    = pythonFn.__doc__ if pythonFn.__doc__ is not None else ''
         lPrimitivObj = LPrimitive( pythonFn, self._name, self._paramsString, docString,
                                    specialForm=self._specialForm,
                                    min_args=self._min_args, max_args=self._max_args,
                                    arity_msg=self._arity_msg )
         primitiveDict[ self._name ] = lPrimitivObj
         return lPrimitivObj

   from pythonslisp.primitives import p_meta, p_control, p_sequences, p_math, p_types, p_strings, p_io
   p_meta.register( primitive )
   p_control.register( primitive )
   p_sequences.register( primitive, parseLispString )
   p_math.register( primitive, primitiveDict )    # also adds PI and E
   p_types.register( primitive, parseLispString )
   p_strings.register( primitive, parseLispString )
   p_io.register( primitive, parseLispString )

   return primitiveDict

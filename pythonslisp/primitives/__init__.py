from typing import Callable, Any

from pythonslisp.LispAST import LPrimitive, LSymbol, L_T, L_NIL


def constructPrimitives( parseLispString: Callable[[str], Any] ) -> dict[str, Any]:
   primitiveDict: dict[str, Any] = {}
   primitiveDict[ 'T'   ] = L_T
   primitiveDict[ 'NIL' ] = L_NIL

   _UNSET = object()   # sentinel: caller did not supply this argument

   _KW_MARKERS = frozenset({'&OPTIONAL', '&REST', '&BODY', '&KEY', '&AUX', '&ALLOW-OTHER-KEYS'})

   def _derive_arity( ll_ast: list ) -> tuple[int, int|None]:
      """Return (min_args, max_args) derived from a parsed lambda list AST."""
      min_args   = 0
      max_args   = 0
      in_section = 'required'
      unbounded  = False
      for item in ll_ast:
         if isinstance( item, LSymbol ) and item.strval in _KW_MARKERS:
            marker = item.strval
            if marker in ('&REST', '&BODY'):
               in_section = 'rest'
               unbounded  = True
            elif marker == '&OPTIONAL':
               in_section = 'optional'
            elif marker in ('&KEY', '&ALLOW-OTHER-KEYS'):
               in_section = 'key'
               unbounded  = True
            elif marker == '&AUX':
               in_section = 'aux'
         else:
            if in_section == 'required':
               min_args += 1
               max_args += 1
            elif in_section == 'optional':
               max_args += 1
            # &rest / &key / &aux: unbounded handles the rest
      if unbounded:
         return min_args, None
      return min_args, max_args

   class primitive:
      def __init__( self, primitiveSymbolString: str, paramsString: str = '', specialForm: bool = False,
                    min_args = _UNSET, max_args = _UNSET, arity_msg: str = '',
                    lambdaList: str = '' ) -> None:
         self._name        = primitiveSymbolString.upper()
         self._specialForm = specialForm
         self._arity_msg   = arity_msg
         if lambdaList:
            ll_ast = parseLispString( lambdaList )[1]
            self._lambdaListAST = ll_ast
            stripped = lambdaList.strip()
            if stripped.startswith('(') and stripped.endswith(')'):
               stripped = stripped[1:-1].strip()
            self._paramsString = stripped
            derived_min, derived_max = _derive_arity( ll_ast )
            self._min_args = derived_min if min_args is _UNSET else min_args
            self._max_args = derived_max if max_args is _UNSET else max_args
         else:
            self._lambdaListAST = None
            self._paramsString  = paramsString
            self._min_args = 0    if min_args is _UNSET else min_args
            self._max_args = None if max_args is _UNSET else max_args

      def __call__( self, pythonFn ):
         docString    = pythonFn.__doc__ if pythonFn.__doc__ is not None else ''
         lPrimitivObj = LPrimitive( pythonFn, self._name, self._paramsString, docString,
                                    specialForm=self._specialForm,
                                    min_args=self._min_args, max_args=self._max_args,
                                    arity_msg=self._arity_msg,
                                    lambdaListAST=self._lambdaListAST )
         primitiveDict[ self._name ] = lPrimitivObj
         return lPrimitivObj

   from pythonslisp.primitives import p_meta, p_control, p_sequences, p_math, p_types, p_strings, p_io
   p_meta.register( primitive )
   p_control.register( primitive )
   p_sequences.register( primitive )
   p_math.register( primitive, primitiveDict )    # also adds PI and E
   p_types.register( primitive, parseLispString )
   p_strings.register( primitive )
   p_io.register( primitive, parseLispString )

   return primitiveDict

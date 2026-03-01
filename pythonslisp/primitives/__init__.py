from enum import Enum, auto
from typing import Callable, Any

from pythonslisp.LispAST import LPrimitive, LSymbol, L_T, L_NIL


class LambdaListMode(Enum):
   DOC_ONLY     = auto()   # informal display string; min/max specified manually; arity_msg auto
   ARITY_ONLY   = auto()   # valid lambda list with outer (); arity auto-derived; no bindArguments at call time
   FULL_BINDING = auto()   # valid lambda list with outer (); arity auto-derived; bindArguments each call


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

   def _make_arity_msg( min_args: int, max_args: int|None ) -> str:
      """Auto-generate a standard arity error message from min/max arg counts."""
      if max_args is None:
         if min_args == 0:
            return ''
         if min_args == 1:
            return 'At least 1 argument expected.'
         return f'At least {min_args} arguments expected.'
      if min_args == max_args:
         if min_args == 0:
            return '0 arguments expected.'
         if min_args == 1:
            return '1 argument expected.'
         return f'{min_args} arguments expected.'
      if max_args == min_args + 1:
         return f'{min_args} or {max_args} arguments expected.'
      return f'{min_args} to {max_args} arguments expected.'

   class primitive:
      def __init__( self, primitiveSymbolString: str, params: str = '', specialForm: bool = False,
                    mode: LambdaListMode = LambdaListMode.ARITY_ONLY,
                    min_args = _UNSET, max_args = _UNSET ) -> None:
         self._name        = primitiveSymbolString.upper()
         self._specialForm = specialForm
         if mode is LambdaListMode.FULL_BINDING:
            # params must be a full lambda list with outer (...)
            ll_ast = parseLispString( params )[1]
            self._lambdaListAST = ll_ast
            stripped = params.strip()
            if stripped.startswith('(') and stripped.endswith(')'):
               stripped = stripped[1:-1].strip()
            self._paramsString = stripped
            derived_min, derived_max = _derive_arity( ll_ast )
            self._min_args = derived_min if min_args is _UNSET else min_args
            self._max_args = derived_max if max_args is _UNSET else max_args
         elif mode is LambdaListMode.ARITY_ONLY:
            # params is a valid lambda list WITH outer parens (same convention as FULL_BINDING)
            ll_ast = parseLispString( params )[1]
            self._lambdaListAST = None    # not used at call time
            stripped = params.strip()
            if stripped.startswith('(') and stripped.endswith(')'):
               stripped = stripped[1:-1].strip()
            self._paramsString = stripped
            derived_min, derived_max = _derive_arity( ll_ast )
            self._min_args = derived_min if min_args is _UNSET else min_args
            self._max_args = derived_max if max_args is _UNSET else max_args
         else:  # DOC_ONLY
            self._lambdaListAST = None
            stripped = params.strip()
            if stripped.startswith('(') and stripped.endswith(')'):
               stripped = stripped[1:-1].strip()
            self._paramsString  = stripped
            self._min_args = 0    if min_args is _UNSET else min_args
            self._max_args = None if max_args is _UNSET else max_args
         self._arity_msg = _make_arity_msg( self._min_args, self._max_args )

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

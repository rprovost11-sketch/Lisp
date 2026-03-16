from __future__ import annotations
from enum import Enum, auto


class LambdaListMode(Enum):
   DOC_ONLY     = auto()   # informal display string; min/max specified manually; arity_msg auto
   ARITY_ONLY   = auto()   # valid lambda list with outer (); arity auto-derived; no bindArguments at call time
   FULL_BINDING = auto()   # valid lambda list with outer (); arity auto-derived; bindArguments each call


class primitive:
   """Decorator for defining Lisp primitives in extension files.

   Usage::

      from pythonslisp.extensions import primitive

      @primitive('my-func', '(x y)')
      def LP_my_func(ctx, env, args):
          ...
   """
   _pending: list = []

   def __init__( self, name: str, params: str = '', **kwargs ) -> None:
      self._name   = name
      self._params = params
      self._kwargs = kwargs

   def __call__( self, fn ):
      primitive._pending.append( (self._name, self._params, self._kwargs, fn) )
      return fn

   @classmethod
   def _flush( cls, make_prim, bind_macro=None ) -> None:
      """Bind all pending primitives using the interpreter's binding machinery."""
      pending = cls._pending[:]
      cls._pending.clear()
      for name, params, kwargs, fn in pending:
         make_prim( name, params, **kwargs )( fn )
      if bind_macro is not None:
         macro._flush( bind_macro )


class macro:
   """Registration helper for Lisp macros defined from Python extension files.

   Usage::

      from pythonslisp.extensions import macro

      @macro('my-macro', '(&rest forms)', '(quote nil)')
      def _macro_comment(): pass  # docstring only; body is Lisp source

   The second argument is the Lisp lambda-list string.
   The third argument is the Lisp body (a single expression string).
   """
   _pending: list = []

   def __init__( self, name: str, params_str: str, body_str: str ) -> None:
      self._name      = name
      self._params_str = params_str
      self._body_str  = body_str

   def __call__( self, fn ):
      macro._pending.append( (self._name, self._params_str, self._body_str,
                               fn.__doc__ or '') )
      return fn

   @classmethod
   def _flush( cls, bind_macro ) -> None:
      """Bind all pending macros using the provided bind_macro callable."""
      pending = cls._pending[:]
      cls._pending.clear()
      for name, params_str, body_str, docstring in pending:
         bind_macro( name, params_str, body_str, docstring )

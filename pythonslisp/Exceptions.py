from __future__ import annotations

class LRuntimeError( Exception ):
   _label = 'Runtime Error'

   def __init__( self, message: str = '' ) -> None:
      super().__init__( message )
      self.source_info = None   # set to (filename, line_num, col_num, source_line) by evaluator/analyzer
      self.call_stack  = []    # list of formatted frame strings; prepended before error output

   def __str__( self ) -> str:
      base = self.args[0] if self.args else ''
      if self.source_info is None:
         error_str = base
      else:
         filename, line_num, col_num, source_line = self.source_info
         indent    = ' ' * ( col_num - 1 )
         error_str = f'{self._label}: "{filename}" ({line_num},{col_num})\n{source_line}\n{indent}^\n{base}'
      if not self.call_stack:
         return error_str
      return 'Call stack:\n' + '\n'.join( self.call_stack ) + '\n' + error_str


class LRuntimePrimError( LRuntimeError ):
   """Runtime error attributed to a specific primitive (no usage hint).
   Use for runtime conditions: division by zero, file not found, stream state, etc."""
   def __init__( self, lispCallable, errorMsg: str ) -> None:
      prim   = getattr( lispCallable, 'primitive', lispCallable )
      fnName = prim.name
      super().__init__( f"ERROR '{fnName}': {errorMsg}" )


class LRuntimeUsageError( LRuntimePrimError ):
   """Runtime error attributed to a specific primitive, with PRIMITIVE USAGE hint.
   Use for type/arity misuse: wrong argument type, wrong count, bad keyword, etc."""
   def __init__( self, lispCallable, errorMsg: str ) -> None:
      prim  = getattr( lispCallable, 'primitive', lispCallable )
      usage = prim.usageString()
      super().__init__( lispCallable, f"{errorMsg}\n{usage}" if usage else errorMsg )


class LArgBindingError( LRuntimeError ):
   _label = 'Binding Error'


class LAnalysisError( LRuntimeError ):
   _label = 'Analysis Error'


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
   """Raised when a continuation is invoked.  Propagates up to cek_eval's handler, which restores K."""
   __slots__ = ('saved_k', 'saved_wind_stack', 'value')

   def __init__( self, saved_k: list, saved_wind_stack: list, value ) -> None:
      self.saved_k          = saved_k
      self.saved_wind_stack = saved_wind_stack
      self.value            = value
      super().__init__()


class Thrown( Exception ):
   """Raised by throw.  Propagates up to the nearest enclosing catch with a matching tag."""
   __slots__ = ('tag', 'value')

   def __init__( self, tag, value ) -> None:
      self.tag   = tag
      self.value = value
      super().__init__()


class Signaled( Exception ):
   """Raised by signal.  Propagates up to the nearest enclosing handler-case with a matching type."""
   __slots__ = ('condition',)

   def __init__( self, condition ) -> None:
      self.condition = condition
      super().__init__()

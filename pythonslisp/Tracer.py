from typing import Any

from pythonslisp.AST import LCallable, LFunction, prettyPrintSExpr


class Tracer:
   """Manages function call tracing for the Lisp interpreter."""

   def __init__( self ) -> None:
      self._fnsToTrace:    set[str] = set()
      self._global:        bool     = False
      self._maxTraceDepth: int      = 0
      self._active:        bool     = False

   def reset( self ) -> None:
      """Clear all tracing state.  Called on interpreter reboot."""
      self._fnsToTrace    = set()
      self._global        = False
      self._maxTraceDepth = 0
      self._active        = False

   # --- Named function tracing ---

   def addFnTrace( self, name: str ) -> None:
      """Register a function name for tracing.  Called by (trace fn)."""
      self._fnsToTrace.add( name )
      self._active = True

   def removeFnTrace( self, name: str ) -> None:
      """Remove a function name from tracing.  Called by (untrace fn)."""
      self._fnsToTrace.discard( name )
      self._active = self._global or bool( self._fnsToTrace )

   def removeAll( self ) -> None:
      """Remove all named functions.  Called by (untrace) with no args."""
      self._fnsToTrace.clear()
      self._active = self._global

   def getFnsToTrace( self ) -> frozenset:
      """Read-only view of currently traced function names."""
      return frozenset( self._fnsToTrace )

   # --- Global toggle ---

   def getGlobalEnabled( self ) -> bool:
      return self._global

   def setGlobalEnabled( self, value: bool ) -> None:
      self._global = value
      self._active = value or bool( self._fnsToTrace )

   def toggle_global( self ) -> bool:
      """Toggle global tracing on/off.  Returns new state.  Called by ]trace."""
      self._global = not self._global
      self._active = self._global or bool( self._fnsToTrace )
      return self._global

   # --- Depth ---

   def getMaxTraceDepth( self ) -> int:
      return self._maxTraceDepth

   def setMaxTraceDepth( self, value: int ) -> None:
      self._maxTraceDepth = value

   # --- Active check ---

   def isActive( self ) -> bool:
      """True when any tracing is enabled.  Cheap check in the eval hot path."""
      return self._active

   # --- Hook ---

   def trace( self, phase: str, function: LCallable, data: Any,
              depth: int, outStrm: Any ) -> bool:
      """
      Called at function entry and exit when isActive() is True.
        phase='enter' : data is the argument list.
        phase='exit'  : data is the return value.
      Returns True if a line was printed (caller uses this to manage depth).
      """
      isNamed  = function.name in self._fnsToTrace
      isUserFn = isinstance( function, LFunction )
      if not isNamed and not ( self._global and isUserFn ):
         return False
      name   = function.name or 'LAMBDA'
      indent = '  ' * depth
      if phase == 'enter':
         argStr = ' '.join( prettyPrintSExpr(a) for a in data )
         line   = f'{depth:2d}: {indent}({name}{" " + argStr if argStr else ""})'
      else:
         line   = f'{depth:2d}: {indent}{name} returned {prettyPrintSExpr(data)}'
      print( line, file=outStrm )
      return True

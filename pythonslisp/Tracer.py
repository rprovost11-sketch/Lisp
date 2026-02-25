from typing import Any

from pythonslisp.LispAST import LCallable, LFunction, L_NIL, prettyPrintSExpr


class Tracer:
   """Manages function call tracing for the Lisp interpreter."""

   def __init__( self ) -> None:
      self._fnsToTrace:    set[str] = set()
      self._global:        bool     = False
      self._maxTraceDepth: int      = 0

   def reset( self ) -> None:
      """Clear all tracing state.  Called on interpreter reboot."""
      self._fnsToTrace    = set()
      self._global        = False
      self._maxTraceDepth = 0

   # --- Named function tracing ---

   def addFnTrace( self, name: str ) -> None:
      """Register a function name for tracing.  Called by (trace fn)."""
      self._fnsToTrace.add( name )

   def removeFnTrace( self, name: str ) -> None:
      """Remove a function name from tracing.  Called by (untrace fn)."""
      self._fnsToTrace.discard( name )

   def removeAll( self ) -> None:
      """Remove all named functions.  Called by (untrace) with no args."""
      self._fnsToTrace.clear()

   def getFnsToTrace( self ) -> frozenset:
      """Read-only view of currently traced function names."""
      return frozenset( self._fnsToTrace )

   # --- Global toggle ---

   def getGlobalEnabled( self ) -> bool:
      return self._global

   def setGlobalEnabled( self, value: bool ) -> None:
      self._global = value

   def toggle_global( self ) -> bool:
      """Toggle global tracing on/off.  Returns new state.  Called by ]trace."""
      self._global = not self._global
      return self._global

   # --- Depth ---

   def getMaxTraceDepth( self ) -> int:
      return self._maxTraceDepth

   def setMaxTraceDepth( self, value: int ) -> None:
      self._maxTraceDepth = value

   # --- Active check ---

   def isActive( self ) -> bool:
      """True when any tracing is enabled.  Cheap check in the eval hot path."""
      return bool( self._global or self._fnsToTrace )

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

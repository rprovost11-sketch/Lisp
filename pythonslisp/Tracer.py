from __future__ import annotations
from typing import Any

from pythonslisp.AST import LCallable, prettyPrintSExpr


class Tracer:
   """Manages function call tracing for the Lisp interpreter."""

   def __init__( self ) -> None:
      self._fnsToTrace:    set[str] = set()
      self._maxTraceDepth: int      = 0
      self._active:        bool     = False   # accessed directly by Evaluator for performance

   def reset( self ) -> None:
      """Clear all tracing state.  Called on interpreter reboot."""
      self._fnsToTrace    = set()
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
      self._active = bool( self._fnsToTrace )

   def removeAll( self ) -> None:
      """Remove all named functions.  Called by (untrace) with no args."""
      self._fnsToTrace.clear()
      self._active = False

   def getFnsToTrace( self ) -> frozenset:
      """Read-only view of currently traced function names."""
      return frozenset( self._fnsToTrace )

   # --- Depth ---

   def getMaxTraceDepth( self ) -> int:
      return self._maxTraceDepth

   def setMaxTraceDepth( self, value: int ) -> None:
      self._maxTraceDepth = value

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
      if not isNamed:
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

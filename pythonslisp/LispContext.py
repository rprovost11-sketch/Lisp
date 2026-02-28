from typing import Any, Callable


class LispContext:
   """Per-evaluation context object.  Carries the output stream, tracer, struct
   registry, and bound references to the core evaluator functions.  Passed as
   the first argument to every primitive function so they have no need to
   import LispInterpreter directly."""

   def __init__( self,
                 outStrm:      Any,
                 tracer:       Any,
                 setfRegistry: dict ) -> None:
      self.outStrm:          Any      = outStrm
      self.tracer:           Any      = tracer
      self.setfRegistry:     dict     = setfRegistry
      self.lEval:            Callable = None   # set by LispInterpreter after construction
      self.lApply:           Callable = None   # set by LispInterpreter after construction
      self.lBackquoteExpand: Callable = None   # set by LispInterpreter after construction

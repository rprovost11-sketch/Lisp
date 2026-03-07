from typing import Any, Callable


class Context:
   """Per-evaluation context object.  Carries the output stream, tracer, struct
   registry, and bound references to the core evaluator functions.  Passed as
   the first argument to every primitive function so they have no need to
   import Interpreter directly."""

   def __init__( self,
                 outStrm:      Any,
                 tracer:       Any,
                 setfRegistry: dict ) -> None:
      self.outStrm:          Any      = outStrm
      self.tracer:           Any      = tracer
      self.setfRegistry:     dict     = setfRegistry
      self.lEval:            Callable = None   # set by Interpreter after construction
      self.lApply:           Callable = None   # set by Interpreter after construction
      self.lBackquoteExpand: Callable = None   # set by Interpreter after construction
      self.parse:            Callable = None   # set by Interpreter after construction
      self.expand:           Callable = None   # set by Interpreter after construction
      self.analyze:          Callable = None   # set by Interpreter after construction
      self.loadExt:          Callable = None   # set by Interpreter after construction
      self.loadExtDir:       Callable = None   # set by Interpreter after construction
      self.setExtDirs:       Callable = None   # set by Interpreter after construction

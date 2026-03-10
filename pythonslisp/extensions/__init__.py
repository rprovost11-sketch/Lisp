from __future__ import annotations
from enum import Enum, auto


class LambdaListMode(Enum):
   DOC_ONLY     = auto()   # informal display string; min/max specified manually; arity_msg auto
   ARITY_ONLY   = auto()   # valid lambda list with outer (); arity auto-derived; no bindArguments at call time
   FULL_BINDING = auto()   # valid lambda list with outer (); arity auto-derived; bindArguments each call

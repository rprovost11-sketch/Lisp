"""LambdaList - Pre-compiled lambda list representation.

CompiledLambdaList stores a validated lambda list in a pre-parsed form
that avoids re-traversing the raw AST on every function call.

compileLambdaList() converts a validated lambda list AST (as produced by
Analyzer.analyzeLambdaList) into a CompiledLambdaList.  It assumes the
input is structurally valid — call Analyzer.analyzeLambdaList first.
"""
from __future__ import annotations

from typing import Any

from pythonslisp.AST import LSymbol


class CompiledLambdaList:
   """Pre-parsed lambda list.  All sections are resolved to plain Python
   types (strings, lists, dicts) so argument binding needs no AST traversal.

   positional    : list[str | CompiledLambdaList]
                   Each entry is either a parameter name (str) or a nested
                   CompiledLambdaList for a destructuring sub-pattern.
   optional      : list[(varName: str, initForm: Any, pvarName: str|None)]
   rest          : str | None       — name of the &rest / &body variable
   keys          : dict[str, (varName: str, pvarName: str|None, initForm: Any)] | None
                   None means no &KEY section was present.
   key_order     : list[str] | None — declaration order for incremental eval
   allow_other_keys : bool
   aux           : list[(varName: str, initForm: Any)]
   min_args      : int
   max_args      : int | None       — None when the list is unbounded
   template      : dict             — pre-sized {name: None} for all bound names;
                   copied at call time to avoid per-call dict growth and resize
   """
   __slots__ = ('positional', 'optional', 'rest', 'keys', 'key_order',
                'allow_other_keys', 'aux', 'min_args', 'max_args', 'template')

   def __init__( self, positional: list, optional: list, rest: (str|None),
                 keys: (dict|None), key_order: (list|None),
                 allow_other_keys: bool, aux: list,
                 min_args: int, max_args: (int|None) ) -> None:
      self.positional:       list         = positional
      self.optional:         list         = optional
      self.rest:             (str|None)   = rest
      self.keys:             (dict|None)  = keys
      self.key_order:        (list|None)  = key_order
      self.allow_other_keys: bool         = allow_other_keys
      self.aux:              list         = aux
      self.min_args:         int          = min_args
      self.max_args:         (int|None)   = max_args
      self.template:         dict         = dict.fromkeys( _collect_names(self) )


def _collect_names( cll: CompiledLambdaList ) -> list:
   """Return all variable names bound by cll, recursing into destructuring sub-patterns."""
   names = []
   for p in cll.positional:
      if isinstance( p, str ):
         names.append( p )
      else:   # nested CompiledLambdaList
         names.extend( _collect_names(p) )
   for varName, _, pvarName in cll.optional:
      names.append( varName )
      if pvarName:
         names.append( pvarName )
   if cll.rest is not None:
      names.append( cll.rest )
   if cll.keys is not None:
      for varName, pvarName, _ in cll.keys.values():
         names.append( varName )
         if pvarName:
            names.append( pvarName )
   for varName, _ in cll.aux:
      names.append( varName )
   return names


def compileLambdaList( ll: list, destructuring: bool = False ) -> CompiledLambdaList:
   """Build a CompiledLambdaList from a validated lambda list AST.

   Assumes the input has already been validated by Analyzer.analyzeLambdaList.
   When destructuring is True, &BODY is treated as &REST.
   """
   positional       = []
   optional         = []
   rest             = None
   keys             = None
   key_order        = None
   allow_other_keys = False
   aux              = []

   length = len( ll )
   i      = 0

   # ---- Positional params ----
   while i < length:
      param = ll[i]
      if isinstance( param, LSymbol ):
         if param.startswith( '&' ):
            break
         positional.append( param.name )
      else:   # nested list = destructuring sub-pattern
         positional.append( compileLambdaList( param, destructuring=True ) )
      i += 1

   if i >= length:
      n = len( positional )
      return CompiledLambdaList( positional, optional, rest, keys, key_order,
                                 allow_other_keys, aux, min_args=n, max_args=n )
   keyword = ll[i]

   # ---- &OPTIONAL ----
   if keyword.name == '&OPTIONAL':
      i += 1
      while i < length:
         spec = ll[i]
         if isinstance( spec, LSymbol ):
            if spec.startswith( '&' ):
               break
            optional.append( (spec.name, list(), None) )
         else:
            slen    = len( spec )
            varName = spec[0].name
            if slen == 1:
               optional.append( (varName, list(), None) )
            elif slen == 2:
               optional.append( (varName, spec[1], None) )
            else:   # == 3
               pvar     = spec[2]
               pvarName = pvar.name if isinstance( pvar, LSymbol ) else None
               optional.append( (varName, spec[1], pvarName) )
         i += 1

      if i >= length:
         npos = len( positional )
         return CompiledLambdaList( positional, optional, rest, keys, key_order,
                                    allow_other_keys, aux,
                                    min_args=npos, max_args=npos + len(optional) )
      keyword = ll[i]

   # ---- &REST / &BODY ----
   if keyword.name == '&REST' or ( keyword.name == '&BODY' and destructuring ):
      i   += 1
      rest = ll[i].name
      i   += 1

      if i >= length:
         return CompiledLambdaList( positional, optional, rest, keys, key_order,
                                    allow_other_keys, aux,
                                    min_args=len(positional), max_args=None )
      keyword = ll[i]

   # ---- &KEY ----
   if keyword.name == '&KEY':
      keys      = {}
      key_order = []
      i += 1
      while i < length:
         spec = ll[i]
         if isinstance( spec, LSymbol ):
            if spec.startswith( '&' ):
               break
            keyStr = spec.name[1:] if spec.startswith( ':' ) else spec.name
            keys[keyStr] = (spec.name, None, list())
            key_order.append( keyStr )
         else:
            keyVarSpec, *initFormSpec = spec
            if isinstance( keyVarSpec, LSymbol ):
               keyStr  = keyVarSpec.name[1:] if keyVarSpec.startswith( ':' ) else keyVarSpec.name
               varName = keyVarSpec.name
            else:   # (:keyword var) pair
               keyName, varSym = keyVarSpec
               keyStr  = keyName.name[1:] if keyName.startswith( ':' ) else keyName.name
               varName = varSym.name

            initFormSpecLen = len( initFormSpec )
            if initFormSpecLen == 0:
               initForm = list()
               pvarName = None
            elif initFormSpecLen == 1:
               initForm = initFormSpec[0]
               pvarName = None
            else:   # == 2
               initForm = initFormSpec[0]
               pvar     = initFormSpec[1]
               pvarName = pvar.name if isinstance( pvar, LSymbol ) else None

            keys[keyStr] = (varName, pvarName, initForm)
            key_order.append( keyStr )
         i += 1

      if i >= length:
         return CompiledLambdaList( positional, optional, rest, keys, key_order,
                                    allow_other_keys, aux,
                                    min_args=len(positional), max_args=None )
      keyword = ll[i]

      if keyword.name == '&ALLOW-OTHER-KEYS':
         allow_other_keys = True
         i += 1
         if i >= length:
            return CompiledLambdaList( positional, optional, rest, keys, key_order,
                                       allow_other_keys, aux,
                                       min_args=len(positional), max_args=None )
         keyword = ll[i]

   # ---- &AUX ----
   if keyword.name == '&AUX':
      i += 1
      while i < length:
         spec = ll[i]
         if isinstance( spec, LSymbol ):
            aux.append( (spec.name, list()) )
         else:
            varName = spec[0].name
            if len( spec ) == 1:
               aux.append( (varName, list()) )
            else:   # == 2
               aux.append( (varName, spec[1]) )
         i += 1

   unbounded = ( rest is not None ) or ( keys is not None )
   max_args  = None if unbounded else len(positional) + len(optional)
   return CompiledLambdaList( positional, optional, rest, keys, key_order,
                              allow_other_keys, aux,
                              min_args=len(positional), max_args=max_args )

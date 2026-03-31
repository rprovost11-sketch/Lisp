from __future__ import annotations
from fractions import Fraction
from io import IOBase, StringIO
from typing import Any, Callable, TYPE_CHECKING

if TYPE_CHECKING:
   from pythonslisp.Environment import Environment
   from pythonslisp.LambdaList import CompiledLambdaList

# #################
# Lisp Function API
# ###############################
# Lisp Runtime Object Definitions
LNUMBER = (int,float,Fraction)
LATOM   = (int,float,Fraction,str)

class LSymbol:
   __slots__ = ('name', )
   _intern_table: dict[str, 'LSymbol'] = {}

   def __init__( self, name: str ) -> None:
      raise TypeError( 'Use LSymbol.makeSymbol() to create symbols.' )

   @classmethod
   def makeSymbol( cls, name: str ) -> 'LSymbol':
      """Return the canonical interned LSymbol for name, creating it if needed."""
      key = name.upper()
      sym = cls._intern_table.get(key)
      if sym is None:
         sym = cls.__new__(cls)
         sym.name = key
         cls._intern_table[key] = sym
      return sym

   def __str__( self ) -> str:
      return self.name

   def __repr__( self ) -> str:
      return self.name

   def __eq__( self, other: Any ) -> bool:
      return self is other

   def __hash__( self ) -> int:
      return id(self)

   def __ne__( self, other: Any ) -> bool:
      return self is not other
   
   def startswith( self, asubstr:str ) -> bool:
      return self.name.startswith(asubstr)
   
   def isKeyword( self ):
      return self.name.startswith(':')


# Lisp lists will be represented by python list.

class LCallable:
   __slots__ = ('name', 'docString')

   def __init__( self, name: str, docString: str = '' ) -> None:
      self.name:str = name
      self.docString:str = docString

class LPrimitive( LCallable ):
   __slots__ = ('pythonFn', 'paramsString', 'min_args', 'max_args', 'arity_msg', 'compiledLambdaList')

   def __init__( self, fn: Callable[[Environment], Any], name: str, paramsString: str, docString: str = '',
                 min_args: int = 0, max_args: (int|None) = None, arity_msg: str = '',
                 compiledLambdaList: (CompiledLambdaList|None) = None ) -> None:
      self.pythonFn:Callable[[Environment], Any] = fn
      self.paramsString:str = paramsString
      self.min_args:int       = min_args
      self.max_args:(int|None) = max_args
      self.arity_msg:str      = arity_msg
      self.compiledLambdaList:(CompiledLambdaList|None) = compiledLambdaList
      super().__init__( name, docString )

   def typeLabel( self ):
      return 'Built-in Function'

   def callForm( self ):
      if self.paramsString:
         return f'({self.name} {self.paramsString})'
      return f'({self.name})'

   def usageString( self ):
      if self.paramsString:
         return f'PRIMITIVE USAGE: ({self.name} {self.paramsString})'
      return f'PRIMITIVE USAGE: ({self.name})'

   def idString( self ):
      params = f'({self.paramsString})' if self.paramsString else '()'
      return f'(PRIMITIVE {self.name} {params} ...)'

class LFunction( LCallable ):
   __slots__ = ('lambdaListAST', 'compiledLambdaList', 'bodyAST', 'capturedEnvironment')

   def __init__( self, name: (LSymbol|str), lambdaListAST: list, docString: str, bodyAST: list,
                 capturedEnvironment: Environment, compiledLambdaList: CompiledLambdaList ) -> None:
      self.lambdaListAST: list                         = lambdaListAST
      self.compiledLambdaList: CompiledLambdaList      = compiledLambdaList
      self.bodyAST: list                               = bodyAST
      self.capturedEnvironment: Environment            = capturedEnvironment
      super().__init__( name.name if isinstance(name, LSymbol) else name, docString )

   def typeLabel( self ):
      return 'Function'

   def callForm( self ):
      if len(self.lambdaListAST) == 0:
         return f'({self.name})'
      inner = prettyPrintSExpr(self.lambdaListAST)[1:-1]
      return f'({self.name} {inner})'

   def usageString( self ):
      if len(self.lambdaListAST) == 0:
         return f'FUNCTION USAGE: ({self.name})'
      inner = prettyPrintSExpr(self.lambdaListAST)[1:-1]
      return f'FUNCTION USAGE: ({self.name} {inner})'

   def idString( self ):
      if len(self.lambdaListAST) == 0:
         return f'(FUNCTION {self.name} () ...)'
      return f'(FUNCTION {self.name} {prettyPrintSExpr(self.lambdaListAST)} ...)'


class LMacro( LCallable ):
   __slots__ = ('lambdaListAST', 'compiledLambdaList', 'bodyAST')

   def __init__( self, name: LSymbol, lambdaListAST: list, docString: str, bodyAST: list,
                 compiledLambdaList: CompiledLambdaList ) -> None:
      self.lambdaListAST: list                    = lambdaListAST
      self.compiledLambdaList: CompiledLambdaList = compiledLambdaList
      self.bodyAST: list                          = bodyAST
      super().__init__( name.name, docString )

   def typeLabel( self ):
      return 'Macro'

   def callForm( self ):
      if len(self.lambdaListAST) == 0:
         return f'({self.name})'
      inner = prettyPrintSExpr(self.lambdaListAST)[1:-1]
      return f'({self.name} {inner})'

   def usageString( self ):
      if len(self.lambdaListAST) == 0:
         return f'MACRO USAGE: ({self.name})'
      inner = prettyPrintSExpr(self.lambdaListAST)[1:-1]
      return f'MACRO USAGE: ({self.name} {inner})'

   def idString( self ):
      if len(self.lambdaListAST) == 0:
         return f'(MACRO {self.name} () ...)'
      return f'(MACRO {self.name} {prettyPrintSExpr(self.lambdaListAST)} ...)'


class LMultipleValues:
   """Wrapper for zero or more values returned by (values ...).
In scalar context the primary (first) value is used; extra values are discarded.
Use multiple-value-bind or nth-value to capture all values."""
   __slots__ = ('values',)

   def __init__( self, values: list ) -> None:
      self.values: list = values


class LContinuation( LCallable ):
   """A first-class continuation captured by call/cc.  Invoking it raises ContinuationInvoked."""
   __slots__ = ('saved_k', 'wind_stack')

   def __init__( self, saved_k: list, wind_stack: list ) -> None:
      self.saved_k    = saved_k
      self.wind_stack = wind_stack
      super().__init__( 'continuation', '' )

   def typeLabel( self ) -> str:
      return 'Continuation'

   def callForm( self ) -> str:
      return '#<CONTINUATION>'

   def usageString( self ) -> str:
      return '#<CONTINUATION>'

   def idString( self ) -> str:
      return '#<CONTINUATION>'


def prettyPrintSExpr( sExpr: Any ) -> str:
   '''Return a printable, formatted string representation
   of a lisp object.'''
   if isinstance(sExpr, str):
      escaped = sExpr.encode( 'unicode_escape' ).decode( 'ascii' ).replace( '"', '\\"' )
      return f'"{escaped}"'
   elif isinstance(sExpr, Fraction):
      return f'{sExpr.numerator}/{sExpr.denominator}'
   elif isinstance(sExpr, list):
      if len(sExpr) == 0:
         return 'NIL'

      mbrList = [ prettyPrintSExpr(mbr) for mbr in sExpr ]
      mbrListStr = ' '.join(mbrList)
      resultStr = f'({mbrListStr})'
      return resultStr
   elif isinstance(sExpr, dict):
      resultStrLines = [ '(DICT' ]
      for key in sorted(sExpr.keys(), key=lambda k: k.name if isinstance(k, LSymbol) else k):
         value = sExpr[ key ]
         key = prettyPrintSExpr(key)
         value = prettyPrintSExpr( value )
         resultStrLines.append( f'   ({key} {value})')
      resultStrLines.append(')')
      return '\n'.join(resultStrLines)
   elif isinstance(sExpr, LMultipleValues):
      if not sExpr.values:
         return '#<VALUES>'
      parts = ' '.join( prettyPrintSExpr(v) for v in sExpr.values )
      return f'#<VALUES {parts}>'
   elif isinstance(sExpr, LCallable):
      return sExpr.idString()
   elif isinstance(sExpr, IOBase):
      return '#<STREAM>'
   else:
      return repr(sExpr)

def lisp_type_name( val: Any ) -> str:
   """Return the Lisp type name of val as a string.
Used by type-of and got_str for error messages."""
   if isinstance(val, list):
      return 'NULL' if not val else 'CONS'
   if isinstance(val, bool):   # bool is a subclass of int; check before int
      return 'SYMBOL'
   if isinstance(val, int):
      return 'INTEGER'
   if isinstance(val, float):
      return 'FLOAT'
   if isinstance(val, Fraction):
      return 'RATIO'
   if isinstance(val, str):
      return 'STRING'
   if isinstance(val, LSymbol):
      return 'SYMBOL'
   if isinstance(val, dict):
      struct_type = val.get( STRUCT_TYPE_SYM )
      if isinstance(struct_type, LSymbol):
         return struct_type.name
      return 'DICT'
   if isinstance(val, LContinuation):
      return 'CONTINUATION'
   if isinstance(val, LFunction):
      return 'FUNCTION'
   if isinstance(val, LMacro):
      return 'MACRO'
   if isinstance(val, LPrimitive):
      return 'PRIMITIVE'
   if isinstance(val, StringIO):
      return 'STRING-STREAM'
   if isinstance(val, IOBase):
      return 'FILE-STREAM'
   return 'T'


_MAX_GOT_LEN = 40

def got_str( val: Any ) -> str:
   """Return a ', got TYPE VALUE' suffix for error messages.
The value is truncated to _MAX_GOT_LEN characters."""
   type_name  = lisp_type_name(val)
   repr_str   = prettyPrintSExpr(val)
   first_line = repr_str.split('\n')[0]
   if len(first_line) > _MAX_GOT_LEN or '\n' in repr_str:
      display = first_line[:_MAX_GOT_LEN].rstrip() + '...'
   else:
      display = first_line
   return f', got {type_name} {display}'


def prettyPrint( sExpr: Any ) -> str:
   '''Return a printable, formatted string representation
   of a lisp object.'''
   if isinstance(sExpr, Fraction):
      return f'{sExpr.numerator}/{sExpr.denominator}'
   elif isinstance(sExpr, list):
      if len(sExpr) == 0:
         return 'NIL'

      mbrList = [ prettyPrint(mbr) for mbr in sExpr ]
      mbrListStr = ' '.join(mbrList)
      resultStr = f'({mbrListStr})'
      return resultStr
   elif isinstance(sExpr, dict):
      resultStrLines = [ '(DICT' ]
      for key in sorted(sExpr.keys(), key=lambda k: k.name if isinstance(k, LSymbol) else k):
         value = sExpr[ key ]
         key = prettyPrint(key)
         value = prettyPrint( value )
         resultStrLines.append( f'   ({key} {value})')
      resultStrLines.append(')')
      return '\n'.join(resultStrLines)
   elif isinstance(sExpr, LMultipleValues):
      if not sExpr.values:
         return '#<VALUES>'
      parts = ' '.join( prettyPrint(v) for v in sExpr.values )
      return f'#<VALUES {parts}>'
   elif isinstance(sExpr, LCallable):
      return sExpr.idString()
   elif isinstance(sExpr, IOBase):
      return '#<STREAM>'
   else:
      return str(sExpr)


def eql( a: Any, b: Any ) -> bool:
   """CL eql: symbols by identity (interned), numbers by type+value, everything else by identity."""
   if isinstance(a, LSymbol) and isinstance(b, LSymbol):
      return a is b
   if type(a) is type(b) and isinstance(a, (int, float, Fraction)):
      return a == b
   return a is b

def equal( a: Any, b: Any ) -> bool:
   """CL equal: recursive structural equality; falls back to eql at leaves."""
   if isinstance(a, list) and isinstance(b, list):
      return len(a) == len(b) and all(equal(x, y) for x, y in zip(a, b))
   if isinstance(a, str) and isinstance(b, str):
      return a == b
   if isinstance(a, dict) and isinstance(b, dict):
      return ( set(a.keys()) == set(b.keys()) and
               all(equal(a[k], b[k]) for k in a) )
   return eql(a, b)

def equalp( a: Any, b: Any ) -> bool:
   """CL equalp: equal + case-insensitive strings + cross-type numbers."""
   if isinstance(a, list) and isinstance(b, list):
      return len(a) == len(b) and all(equalp(x, y) for x, y in zip(a, b))
   if isinstance(a, str) and isinstance(b, str):
      return a.lower() == b.lower()
   if isinstance(a, (int, float, Fraction)) and isinstance(b, (int, float, Fraction)):
      return a == b
   if isinstance(a, dict) and isinstance(b, dict):
      return ( set(a.keys()) == set(b.keys()) and
               all(equalp(a[k], b[k]) for k in a) )
   return eql(a, b)


class LNil(list):
   """The canonical NIL singleton.  Immutable - mutation methods raise LRuntimeError."""
   def _immutable(self, *a, **kw):
      from pythonslisp.Exceptions import LRuntimeError
      raise LRuntimeError("NIL is immutable.")
   
   append      = _immutable
   insert      = _immutable
   pop         = _immutable
   remove      = _immutable
   clear       = _immutable
   sort        = _immutable
   reverse     = _immutable
   extend      = _immutable
   __setitem__ = _immutable
   __delitem__ = _immutable
   __iadd__    = _immutable
   __imul__    = _immutable

class LList(list):
   """A list AST node that carries source position information for error reporting."""
   __slots__ = ('filename', 'line_num', 'col_num', 'source_line')

   def __init__( self, iterable = (), *, filename: str = '', line_num: int = 0,
                 col_num: int = 0, source_line: str = '' ) -> None:
      super().__init__( iterable )
      self.filename    = filename
      self.line_num    = line_num
      self.col_num     = col_num
      self.source_line = source_line

   def has_source_info( self ) -> bool:
      return self.line_num != 0


_KW_MARKERS = frozenset({'&OPTIONAL', '&REST', '&BODY', '&KEY',
                         '&AUX', '&ALLOW-OTHER-KEYS'})

def derive_arity( ll_ast: list ) -> tuple[int, int | None]:
   """Return (min_args, max_args) for a lambda list AST.
   max_args is None when &rest, &body, or &key makes the upper bound unlimited.
   Top-level destructuring patterns (nested lists) each count as one argument."""
   min_args   = 0
   max_args   = 0
   in_section = 'required'
   unbounded  = False
   for item in ll_ast:
      if isinstance( item, LSymbol ) and item.name in _KW_MARKERS:
         marker = item.name
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
   if unbounded:
      return min_args, None
   return min_args, max_args


def arity_mismatch_msg( min_a: int, max_a: int | None, n_provided: int ) -> str:
   """Build 'N provided; M expected' arity mismatch string."""
   provided = f'{n_provided} argument{"s" if n_provided != 1 else ""} provided'
   if max_a is None:
      expected = f'at least {min_a} expected'
   elif max_a == min_a:
      expected = f'{min_a} expected'
   elif max_a == min_a + 1:
      expected = f'{min_a} or {max_a} expected'
   else:
      expected = f'{min_a} to {max_a} expected'
   return f'{provided}; {expected}.'


# Canonical Lisp constants - defined here so AST has no upstream deps
L_NIL: LNil  = LNil()

# Interned symbol constants — use makeSymbol so each name has exactly one object.
# Naming convention: <LISP-NAME>_SYM with hyphens replaced by underscores.

T_SYM                = LSymbol.makeSymbol('T')

# Special operators
IF_SYM               = LSymbol.makeSymbol('IF')
QUOTE_SYM            = LSymbol.makeSymbol('QUOTE')
LET_SYM              = LSymbol.makeSymbol('LET')
LETSTAR_SYM          = LSymbol.makeSymbol('LET*')
PROGN_SYM            = LSymbol.makeSymbol('PROGN')
SETQ_SYM             = LSymbol.makeSymbol('SETQ')
COND_SYM             = LSymbol.makeSymbol('COND')
CASE_SYM             = LSymbol.makeSymbol('CASE')
FUNCALL_SYM          = LSymbol.makeSymbol('FUNCALL')
APPLY_SYM            = LSymbol.makeSymbol('APPLY')
LAMBDA_SYM           = LSymbol.makeSymbol('LAMBDA')
DEFMACRO_SYM         = LSymbol.makeSymbol('DEFMACRO')
QUASIQUOTE_SYM       = LSymbol.makeSymbol('QUASIQUOTE')
UNQUOTE_SYM          = LSymbol.makeSymbol('UNQUOTE')
UNQUOTE_SPLICING_SYM = LSymbol.makeSymbol('UNQUOTE-SPLICING')
TRACE_SYM            = LSymbol.makeSymbol('TRACE')
UNTRACE_SYM          = LSymbol.makeSymbol('UNTRACE')
BLOCK_SYM            = LSymbol.makeSymbol('BLOCK')
RETURN_FROM_SYM      = LSymbol.makeSymbol('RETURN-FROM')
RETURN_SYM           = LSymbol.makeSymbol('RETURN')
CATCH_SYM            = LSymbol.makeSymbol('CATCH')
HANDLER_CASE_SYM     = LSymbol.makeSymbol('HANDLER-CASE')
MV_BIND_SYM          = LSymbol.makeSymbol('MULTIPLE-VALUE-BIND')
MV_LIST_SYM          = LSymbol.makeSymbol('MULTIPLE-VALUE-LIST')
NTH_VALUE_SYM        = LSymbol.makeSymbol('NTH-VALUE')
MAKE_DICT_SYM        = LSymbol.makeSymbol('MAKE-DICT')
COLON_SYM            = LSymbol.makeSymbol(':')
CALL_CC_SYM          = LSymbol.makeSymbol('CALL/CC')
DYNAMIC_WIND_SYM     = LSymbol.makeSymbol('DYNAMIC-WIND')

# Condition handling
ERROR_SYM            = LSymbol.makeSymbol('ERROR')
UNKNOWN_SYM          = LSymbol.makeSymbol('UNKNOWN')

# Struct dict keys
STRUCT_TYPE_SYM       = LSymbol.makeSymbol('STRUCT-TYPE')
STRUCT_INCLUDES_SYM   = LSymbol.makeSymbol('STRUCT-INCLUDES')
STRUCT_DESCRIPTOR_SYM = LSymbol.makeSymbol('%STRUCT-DESCRIPTOR%')
STRUCT_NAME_SYM       = LSymbol.makeSymbol('NAME')
STRUCT_DOCSTRING_SYM  = LSymbol.makeSymbol('DOCSTRING')
STRUCT_FIELDS_SYM     = LSymbol.makeSymbol('FIELDS')

# IO open keywords
KW_INPUT_SYM     = LSymbol.makeSymbol(':INPUT')
KW_OUTPUT_SYM    = LSymbol.makeSymbol(':OUTPUT')
KW_SUPERSEDE_SYM = LSymbol.makeSymbol(':SUPERSEDE')
KW_APPEND_SYM    = LSymbol.makeSymbol(':APPEND')
KW_ERROR_SYM     = LSymbol.makeSymbol(':ERROR')

# Miscellaneous
STAR_SYM         = LSymbol.makeSymbol('*')
MODULE_SYM       = LSymbol.makeSymbol('MODULE')
ANONYMOUS_SYM    = LSymbol.makeSymbol('')

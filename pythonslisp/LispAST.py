from fractions import Fraction
from io import IOBase
from typing import Any, Callable

from pythonslisp.Environment import Environment

# #################
# Lisp Function API
# ###############################
# Lisp Runtime Object Definitions
LNUMBER = (int,float,Fraction)
LATOM   = (int,float,Fraction,str)

class LSymbol:
   __slots__ = ('strval', )
   
   def __init__( self, val: str ) -> None:
      self.strval = val.upper()

   def __str__( self ) -> str:
      return self.strval

   def __repr__( self ) -> str:
      return self.strval

   def __eq__( self, other: Any ) -> bool:
      if isinstance(other, LSymbol):
         return self.strval == other.strval
      elif isinstance(other, str):
         return self.strval == other
      else:
         return False

   def __hash__( self ) -> int:
      return hash(self.strval)

   def __ne__( self, other: Any ) -> bool:
      if isinstance(other, LSymbol):
         return self.strval != other.strval
      elif isinstance(other, str):
         return self.strval != other
      else:
         return True
   
   def startswith( self, asubstr:str ) -> bool:
      return self.strval.startswith(asubstr)
   
   def isKeyArg( self ):
      return self.strval.startswith(':')


# Lisp lists will be represented by python list.

# A map type will be introduced to Lisp represented by python dict.

class LCallable:
   __slots__ = ('name', 'docString', 'specialForm')
   
   def __init__( self, name: str, docString: str = '', specialForm: bool = False ) -> None:
      self.name:str = name
      self.docString:str = docString
      self.specialForm:bool = specialForm

class LPrimitive( LCallable ):
   __slots__ = ('pythonFn', 'paramsString', 'min_args', 'max_args', 'arity_msg', 'lambdaListAST')

   def __init__( self, fn: Callable[[Environment], Any], name: str, paramsString: str, docString: str = '', specialForm: bool=False,
                 min_args: int = 0, max_args: (int|None) = None, arity_msg: str = '',
                 lambdaListAST: (list|None) = None ) -> None:
      self.pythonFn:Callable[[Environment], Any] = fn
      self.paramsString:str = paramsString
      self.min_args:int       = min_args
      self.max_args:(int|None) = max_args
      self.arity_msg:str      = arity_msg
      self.lambdaListAST:(list|None) = lambdaListAST
      super().__init__( name, docString, specialForm )
   
   def usageString( self ):
      if self.paramsString:
         return f'PRIMITIVE USAGE: ({self.name} {self.paramsString})'
      return f'PRIMITIVE USAGE: ({self.name})'

   def idString( self ):
      params = f'({self.paramsString})' if self.paramsString else '()'
      return f'(PRIMITIVE {self.name} {params} ...)'

class LFunction( LCallable ):
   __slots__ = ('lambdaListAST', 'bodyAST', 'capturedEnvironment')

   def __init__( self, name: LSymbol, lambdaListAST: list, docString: str, bodyAST: list, capturedEnvironment: Environment ) -> None:
      self.lambdaListAST: list = lambdaListAST
      self.bodyAST: list   = bodyAST
      self.capturedEnvironment: Environment = capturedEnvironment
      super().__init__( name, docString, specialForm=False )

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
   __slots__ = ('lambdaListAST', 'bodyAST')

   def __init__( self, name: LSymbol, lambdaListAST: list, docString: str, bodyAST: list ) -> None:
      self.lambdaListAST: list  = lambdaListAST
      self.bodyAST: list    = bodyAST
      super().__init__( name.strval, docString, specialForm=True )

   def usageString( self ):
      if len(self.lambdaListAST) == 0:
         return f'MACRO USAGE: ({self.name})'
      inner = prettyPrintSExpr(self.lambdaListAST)[1:-1]
      return f'MACRO USAGE: ({self.name} {inner})'

   def idString( self ):
      if len(self.lambdaListAST) == 0:
         return f'(MACRO {self.name} () ...)'
      return f'(MACRO {self.name} {prettyPrintSExpr(self.lambdaListAST)} ...)'


class LContinuation( LCallable ):
   """An escape continuation captured by call/cc.  Invoking it raises ContinuationInvoked."""
   __slots__ = ('token',)

   def __init__( self, token: object ) -> None:
      self.token = token
      super().__init__( 'continuation', '', specialForm=False )

   def usageString( self ) -> str:
      return '#<CONTINUATION>'

   def idString( self ) -> str:
      return '#<CONTINUATION>'


def prettyPrintSExpr( sExpr: Any ) -> str:
   '''Return a printable, formatted string representation
   of a lisp object.'''
   if isinstance(sExpr, str):
      escaped = ( sExpr
                  .replace('\\', '\\\\')
                  .replace('"',  '\\"')
                  .replace('\n', '\\n')
                  .replace('\t', '\\t')
                  .replace('\r', '\\r') )
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
      for key in sorted(sExpr.keys()):
         value = sExpr[ key ]
         key = prettyPrintSExpr(key)
         value = prettyPrintSExpr( value )
         resultStrLines.append( f'   ({key} {value})')
      resultStrLines.append(')')
      return '\n'.join(resultStrLines)
   elif isinstance(sExpr, LCallable):
      return sExpr.idString()
   elif isinstance(sExpr, IOBase):
      return '#<STREAM>'
   else:
      return repr(sExpr)

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
      for key in sorted(sExpr.keys()):
         value = sExpr[ key ]
         key = prettyPrint(key)
         value = prettyPrint( value )
         resultStrLines.append( f'   ({key} {value})')
      resultStrLines.append(')')
      return '\n'.join(resultStrLines)
   elif isinstance(sExpr, LCallable):
      return sExpr.idString()
   elif isinstance(sExpr, IOBase):
      return '#<STREAM>'
   else:
      return str(sExpr)


def eql( a: Any, b: Any ) -> bool:
   """CL eql: symbols by name, numbers by type+value, everything else by identity."""
   if isinstance(a, LSymbol) and isinstance(b, LSymbol):
      return a.strval == b.strval
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
   """The canonical NIL singleton.  Immutable — mutation methods raise LispRuntimeError."""
   def _immutable(self, *a, **kw):
      from pythonslisp.LispExceptions import LispRuntimeError
      raise LispRuntimeError("NIL is immutable.")
   
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

# Canonical Lisp constants — defined here so LispAST has no upstream deps
L_T: LSymbol = LSymbol('T')
L_NIL: LNil = LNil()

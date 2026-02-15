from fractions import Fraction
from typing import Any, Callable

from pythonslisp.Environment import Environment

# #################
# Lisp Function API
# ###############################
# Lisp Runtime Object Definitions
LNUMBER = (int,float,Fraction)
LATOM   = (int,float,Fraction,str)

class LSymbol( object ):
   __slots__ = ('strval')
   
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

   def __ne__( self, other: Any ) -> bool:
      if isinstance(other, LSymbol):
         return self.strval != other.strval
      elif isinstance(other, str):
         return self.strval != other
      else:
         return True
   
   def startswith( self, asubstr:str ) -> bool:
      return self.strval.startswith(asubstr)
   
   def isArgKey( self ):
      return self.strval.startswith(':')


# Lisp lists will be represented by python list.

# A map type will be introduced to Lisp represented by python dict.

class LCallable( object ):
   __slots__ = ('name', 'docString', 'specialForm')
   
   def __init__( self, name: str, docString: str = '', specialForm: bool = False ) -> None:
      self.name:str = name
      self.docString:str = docString
      self.specialForm:bool = specialForm

class LPrimitive( LCallable ):
   __slots__ = ('pythonFn', 'paramsString')
   
   def __init__( self, fn: Callable[[Environment], Any], name: str, params: str, docString: str = '', specialForm: bool=False ) -> None:
      self.pythonFn:Callable[[Environment], Any] = fn
      self.paramsString:str = params
      super().__init__( name, docString, specialForm )
   
   def usageString( self ):
      return f'({self.name} {self.paramsString})'

class LFunction( LCallable ):
   __slots__ = ('params', 'body', 'closure')
   
   def __init__( self, name: LSymbol, params: list, docString: str, bodyExprLst: list, closure: Environment ) -> None:
      self.params: list = params
      self.body: list   = bodyExprLst
      self.closure: Environment = closure
      super().__init__( name, docString, specialForm=False )
   
   def usageString( self ):
      return f'(Function {self.name} {prettyPrintSExpr(self.params)} ...)'


class LMacro( LCallable ):
   __slots__ = ('params', 'body')
   
   def __init__( self, name: LSymbol, params: list, docString: str, bodyExprList: list ) -> None:
      self.params: list  = params
      self.body: list    = bodyExprList
      super().__init__( name, docString, specialForm=True )
   
   def usageString( self ):
      return f'(Macro {self.name} {prettyPrintSExpr(self.params)} ...)'


def prettyPrintSExpr( sExpr: Any ) -> str:
   '''Return a printable, formatted string representation
   of a lisp object.'''
   if isinstance(sExpr, str):
      return f'\"{sExpr}\"'
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
      resultStrLines = [ '(MAP' ]
      for key in sorted(sExpr.keys()):
         value = sExpr[ str(key) ]
         key = prettyPrintSExpr(key)
         value = prettyPrintSExpr( value )
         resultStrLines.append( f'   ({key} {value})')
      resultStrLines.append(')\n')
      return '\n'.join(resultStrLines)
   elif isinstance(sExpr, LPrimitive):
      return sExpr.usageString()
   elif isinstance(sExpr, LFunction):
      return sExpr.usageString()
   elif isinstance(sExpr, LMacro):
      return sExpr.usageString()
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
      resultStrLines = [ '(MAP' ]
      for key in sorted(sExpr.keys()):
         value = sExpr[ str(key) ]
         key = prettyPrint(key)
         value = prettyPrint( value )
         resultStrLines.append( f'   ({key} {value})')
      resultStrLines.append(')\n')
      return '\n'.join(resultStrLines)
   elif isinstance(sExpr, LPrimitive):
      return sExpr.usageString()
   elif isinstance(sExpr, LFunction):
      return sExpr.usageString()
   elif isinstance(sExpr, LMacro):
      return sExpr.usageString()
   else:
      return str(sExpr)


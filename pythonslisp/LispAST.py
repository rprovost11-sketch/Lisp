from fractions import Fraction
from typing import Any, Callable

from pythonslisp.Environment import Environment

# #################
# Lisp Function API
def prettyPrintSExpr( sExpr: Any ) -> str:
   '''Return a printable, formatted string representation
   of a lisp object.'''
   if isinstance(sExpr, str):
      return f'\"{sExpr}\"'
   elif isinstance(sExpr, Fraction):
      return f'{sExpr.numerator}/{sExpr.denominator}'
   else:
      return repr(sExpr)

def prettyPrint( sExpr: Any ) -> str:
   '''Return a printable, formatted string representation
   of a lisp object.'''
   if isinstance(sExpr, Fraction):
      return f'{sExpr.numerator}/{sExpr.denominator}'
   else:
      return str(sExpr)

# ###############################
# Lisp Runtime Object Definitions
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


class LList( list ):
   def __init__( self, *elements ) -> None:
      super().__init__( elements )

   def __str__( self ) -> str:
      if len(self) == 0:
         return 'NIL'

      mbrList = [ prettyPrintSExpr(mbr) for mbr in self ]
      mbrListStr = ' '.join(mbrList)
      resultStr = f'({mbrListStr})'
      return resultStr

   def __repr__( self ) -> str:
      if len(self) == 0:
         return 'NIL'

      mbrList = [ prettyPrintSExpr(mbr) for mbr in self ]
      mbrListStr = ' '.join(mbrList)
      resultStr = f'({mbrListStr})'
      return resultStr


class LMap( dict ):
   def __init__( self, **mapping ):
      super().__init__(**mapping)

   def __str__( self ) -> str:
      return self.__repr__( )

   def __repr__( self ) -> str:
      resultStrLines = [ '(MAP' ]
      for key in sorted(self.keys()):
         value = super().__getitem__( str(key) )
         key = prettyPrintSExpr(key)
         value = prettyPrintSExpr( value )
         resultStrLines.append( f'   ({key} {value})')
      resultStrLines.append(')\n')
      return '\n'.join(resultStrLines)


class LCallable( object ):
   __slots__ = ('name', 'specialForm')
   
   def __init__( self, name: str, specialForm: bool = False ) -> None:
      self.name:str = name
      self.specialForm:bool = specialForm

class LPrimitive( LCallable ):
   __slots__ = ('fn', 'usageStr', 'paramsStr')
   
   def __init__( self, fn: Callable[[Environment], Any], name: str, usage: str, params: str, specialForm: bool=False ) -> None:
      self.fn:Callable[[Environment], Any] = fn
      self.usageStr:str = usage
      self.paramsStr:str = params
      super().__init__( name, specialForm )

   def __str__( self ) -> str:
      return self.__repr__()

   def __repr__( self ) -> str:
      if len(self.usageStr) > 0:
         return f'(Primitive {self.name} ({self.paramsStr}) ...)'
      else:
         return f'(Primitive {self.name} (...) ...)'

class LFunction( LCallable ):
   __slots__ = ('params', 'body')
   
   def __init__( self, name: LSymbol, params: LList, bodyExprLst: LList ) -> None:
      self.params: LList = params
      self.body: LList   = bodyExprLst
      super().__init__( name, specialForm=False )

   def __str__( self ) -> str:
      return self.__repr__( )

   def __repr__( self ) -> str:
      return f"(Function {self.name} {self.params} ... )"


class LMacro( LCallable ):
   __slots__ = ('params', 'body')
   
   def __init__( self, name: LSymbol, params: LList, bodyExprList: LList ) -> None:
      self.params: LList  = params
      self.body: LList    = bodyExprList
      super().__init__( name, specialForm=True )

   def __str__( self ) -> str:
      return self.__repr__()

   def __repr__( self ) -> str:
      return f"(Macro {self.name} {self.params} ... )"



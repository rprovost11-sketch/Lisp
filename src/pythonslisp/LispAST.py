from ltk.Environment import Environment

import fractions
from typing import Any, Callable

# #################
# Lisp Function API
def prettyPrintSExpr( sExpr: Any ) -> str:
   '''Return a printable, formatted string representation
   of a lisp object.'''
   if isinstance(sExpr, str):
      return f'\"{sExpr}\"'
   elif isinstance(sExpr, fractions.Fraction):
      return f'{sExpr.numerator}/{sExpr.denominator}'
   else:
      return repr(sExpr)

# ###############################
# Lisp Runtime Object Definitions
class LSymbol( object ):
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


class LPrimitive( object ):
   def __init__( self, fn: Callable[[Environment], Any], name: str, usage: str, specialOp: bool=False ) -> None:
      self.fn:Callable[[Environment], Any] = fn
      self._name:str = name
      self._usage:str = usage
      self.specialOp:bool = specialOp

   def __str__( self ) -> str:
      return self.__repr__()

   def __repr__( self ) -> str:
      return f'(Primitive {self._name})'

class LFunction( object ):
   def __init__( self, name: LSymbol, params: LList, bodyExprLst: LList ) -> None:
      self.name: LSymbol = name
      self.params: LList = params
      self.body: LList   = bodyExprLst
      self.specialOp:bool = False

   def __str__( self ) -> str:
      return self.__repr__( )

   def __repr__( self ) -> str:
      return f"(Function {self.name} {self.params} ... )"


class LMacro( object ):
   def __init__( self, name: LSymbol, params: LList, bodyExprList: LList ) -> None:
      self.name: LSymbol  = name
      self.params: LList  = params
      self.body: LList    = bodyExprList
      self.specialOp: bool = True

   def __str__( self ) -> str:
      return self.__repr__()

   def __repr__( self ) -> str:
      return f"(Macro {self.name} {self.params} ... )"



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

   def copy( self ) -> LList:
      return LList( *self[:] )

   def first( self ) -> Any:
      return self[ 0 ]

   def rest( self ) -> LList:
      if len(self) < 2:
         return LList( )
      else:
         return LList( *self[ 1 : ] )


class LMap( object ):
   def __init__( self, aMap: (dict[Any, Any]|None) = None ):
      self.dict: dict[Any, Any] = aMap if aMap else { }

   def __str__( self ) -> str:
      resultStrLines = [ '(MAP\n' ]
      for key in sorted(self.dict.keys()):
         value = self.dict[key]
         key = str(key)
         value = str(value)
         resultStrLines.append( f'   ({key} {value})\n')
      resultStrLines.append(')\n')
      return ''.join(resultStrLines)

   def __repr__( self ) -> str:
      resultStrLines = [ '(MAP\n' ]
      for key in sorted(self.dict.keys()):
         value = self.dict[key]
         key = prettyPrintSExpr(key)
         value = prettyPrintSExpr(value)
         resultStrLines.append( f'   ({key} {value})\n')
      resultStrLines.append(')\n')
      return ''.join(resultStrLines)

   def __setitem__( self, key: Any, val: Any ) -> None:
      if isinstance( key, LSymbol ):
         self.dict[ key.strval ] = val
      else:
         self.dict[ key ] = val

   def __getitem__( self, key: Any ) -> Any:
      if isinstance( key, LSymbol ):
         return self.dict[ key.strval ]
      else:
         return self.dict[ key ]


class LPrimitive( object ):
   def __init__( self, fn: Callable[[Environment], Any], name: str, usage: str, specialOp: bool=False ) -> None:
      self._fn:Callable[[Environment], Any] = fn
      self._name:str = name
      self._usage:str = usage
      self.specialOp:bool = specialOp

   def __call__( self, sExprEvaluator: Callable[[Environment, Any], Any], env: Environment, *args, **kwargs ) -> Any:
      return self._fn( env, *args, **kwargs )


class LFunction( object ):
   def __init__( self, name: LSymbol, params: LList, bodyExprLst: LList ) -> None:
      self._name: LSymbol = name
      self._reprStr: str  = ''
      self._params: LList = params
      self._body: LList   = bodyExprLst
      self.specialOp:bool = False
      self.setName( name )

   def __str__( self ) -> str:
      return self._reprStr

   def __repr__( self ) -> str:
      return self._reprStr

   def __call__( self, sExprEvaluator: Callable[[Environment, Any], Any], env: Environment, *args, **kwargs ) -> Any:
      return sExprEvaluator( env, self, *args, **kwargs )

   def setName( self, name: LSymbol ) -> None:
      self._name = name
      self._reprStr = f"(Function {self._name} {self._params} ... )"


class LMacro( object ):
   def __init__( self, name: LSymbol, params: LList, bodyExprList: LList ) -> None:
      self._name: LSymbol  = name
      self._reprStr:str    = ''
      self._params: LList  = params
      self._body: LList    = bodyExprList
      self.specialOp: bool = True
      self.setName( name )

   def __str__( self ) -> str:
      return self._reprStr

   def __repr__( self ) -> str:
      return self._reprStr

   def __call__( self, sExprEvaluator: Callable[[Environment, Any], Any], env: Environment, *args, **kwargs ) -> Any:
      # Expand macro body
      listOfExpandedExprs = sExprEvaluator( env, self, *args, **kwargs )

      # Eval each of the expanded exprs of the macro's body
      latestResult = None
      for expr in listOfExpandedExprs:
         latestResult = sExprEvaluator( env, expr )
      return latestResult

   def setName( self, name: LSymbol ) -> None:
      self._name = name
      self._reprStr = f"(Macro {self._name} {self._params} ... )"

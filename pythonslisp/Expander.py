from pythonslisp.Environment import Environment
from pythonslisp.LispAST import ( LSymbol, LList, LMap, LMacro, LCallable, LFunction, LPrimitive )
from pythonslisp.LispInterpreter import LispInterpreter, L_ATOM

class LispExpander( object ):
   def __init__( self ):
      super().__init__()
   
   @staticmethod
   def _expand( env: Environment, sExpr: Any ) -> Any:
      if isinstance( sExpr, L_ATOM ):
         return sExpr
      elif isinstance( sExpr, LSymbol ):
         return sExpr
      elif isinstance( sExpr, LMap ):
         newMap = LMap( )
         for key, val in sExpr.items():
            newKey = LispExpander._expand( env, key )
            newVal = LispExpander._expand( env, val )
            newMap[ newKey ] = newVal
         return newMap
      elif isinstance( sExpr, LList ):
         if len(sExpr) == 0:
            return LList( )
         
         primary, *args = sExpr
         if isinstance( primary, LSymbol ):
            fnDef = env.getValue( primary.strval )
            if isinstance(fnDef, LMacro):
               return LispExpander._expandMacro( env, fnDef, *args )
         newList = LList( primary )
         for arg in args:
            newList.append( arg )
         return newList
   
   @staticmethod
   def _expandMacro( env: Environment, macroDef: LMacro, *args ):
      env = Environment( env )
      LispInterpreter._lbindArguments( env, macroDef.params, list(args) )
      newList = LList( )
      for bodyExpr in macroDef.body:
         newBodyExpr = LispExpander._expandMacroBody( env, bodyExpr )
         newList.append( newBodyExpr )
      return newList
   
   @staticmethod
   def _expandMacroBody( env, bodyExpr ):
      if isinstance( bodyExpr, L_ATOM ):
         return bodyExpr
      elif isinstance( bodyExpr, LSymbol ):
         return bodyExpr
      elif isinstance( bodyExpr, LMap ):
         newMap = LMap( )
         for key, val in bodyExpr.items():
            newKey = LispExpander._expandMacroBody( env, key )
            newVal = LispExpander._expandMacroBody( env, val )
            newMap[ newKey ] = newVal
         return newMap
      elif isinstance( bodyExpr, LList ):
         return LList()


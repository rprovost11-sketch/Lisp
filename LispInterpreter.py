from LispAST import ( LSymbol, LList, LMap, LFunction, LPrimitive, LMacro,
                       prettyPrintSExpr )
from LispParser import LispParser
import ltk.Listener as Listener
from ltk.Environment import Environment

import os
import sys
import functools
import math
import random
import string
import fractions
from typing import Callable, Any, Dict, List


class LispRuntimeError( Exception ):
   def __init__( self, *args ) -> None:
      super().__init__( self, *args )


class LispRuntimeFuncError( LispRuntimeError ):
   def __init__( self, lispCallable: Callable[[Environment], Any], errorMsg: str ) -> None:
      assert isinstance(lispCallable, LPrimitive)
      fnName = lispCallable._name
      usage = lispCallable._usage
      errStr = f"ERROR '{fnName}': {errorMsg}\nUSAGE: {usage}" if usage else f"ERROR '{fnName}': {errorMsg}"
      super().__init__( errStr )


L_NUMBER = (int,float,fractions.Fraction)
L_ATOM   = (int,float,fractions.Fraction,str)

class LispInterpreter( Listener.Interpreter ):
   outStrm = None

   def __init__( self ) -> None:
      self._parser: LispParser = LispParser( )
      primitiveDict: Dict[str, Any] = LispInterpreter.constructPrimitives( self._parser.parse )
      self._env:Environment = Environment( parent=None, **primitiveDict )

   def reboot( self ) -> None:
      primitiveDict = LispInterpreter.constructPrimitives( self._parser.parse )
      self._env = self._env.reInitialize( **primitiveDict )

   def eval( self, inputExprStr: str, outStrm=None ) -> str:
      LispInterpreter.outStrm = outStrm
      ast = self._parser.parse( inputExprStr )
      resultExpr = LispInterpreter._lEval( self._env, ast )
      LispInterpreter.outStrm = None

      return prettyPrintSExpr( resultExpr ).strip()

   @staticmethod
   def _lTrue( sExpr: Any ) -> bool:
      if isinstance(sExpr, (LList, list)):
         return len(sExpr) != 0
      else:
         return True

   @staticmethod
   def _lEval( env: Environment, sExpr: Any, *args, **kwargs ) -> Any:
      '''Evaluate expr as a lisp expression.
      Note:  Symbols (including function names) need to be in capitals before
      invoking this function.
      '''
      if sExpr is None:
         return LList( )
      elif isinstance( sExpr, L_ATOM ):
         return sExpr
      elif isinstance( sExpr, LSymbol ):
         try:
            result = env.getValue( sExpr._val )
            return sExpr if result is None else result
         except:
            return sExpr
      elif  isinstance( sExpr, LList ):
         # This code is called when sExpr is an LList and it's contents (a
         # function call usually) need to be evaluated.
         if len(sExpr) == 0:
            return LList( )

         # Break the list contents into a function and a list of args
         try:
            primary, *exprArgs = sExpr
         except:
            raise LispRuntimeError( 'Badly formed list expression.' )

         if not isinstance( primary, (LList,LSymbol) ):
            raise LispRuntimeError( f'Badly formed list expression.  The first element should be a symbol or function.' )

         # fn is an LPrimitive, LFunction or a function name symbol
         # Use this information to get the function definition
         fnDef = LispInterpreter._lEval( env, primary )
         if not isinstance( fnDef, (LPrimitive, LFunction, LMacro) ):
            raise LispRuntimeError( 'Badly formed list expression.  The first element should evaluate to a primitive or function.' )

         # Determine if the function uses the standard evaluation order for arguments
         evaluatedKeys: Dict[str, Any] = { }
         if fnDef._specialOp:
            return fnDef( LispInterpreter._lEval, env, *exprArgs, **evaluatedKeys )

         # Evaluate each arg
         evaluatedArgs = [ ]
         for argNum,argExpr in enumerate(exprArgs):
            evaluatedArg = LispInterpreter._lEval( env, argExpr )
            evaluatedArgs.append( evaluatedArg )

         return fnDef( LispInterpreter._lEval, env, *evaluatedArgs, **evaluatedKeys )
      elif  isinstance( sExpr, LMap ):
         return sExpr
      elif  isinstance( sExpr, LFunction ):
         env = env.openScope( )

         # store the arguments as locals
         LispInterpreter.bindArguments( env, sExpr._params, list(args) ) #convert args from a tuple to a list

         # evaluate the body expressions.  Return the result of the last
         # body expression evaluated.
         latestResult = None
         for expr in sExpr._body:
            latestResult = LispInterpreter._lEval( env, expr )

         #env = env.closeScope( ) # occurs automatically when env goes out of scope
         return latestResult
      elif isinstance( sExpr, LMacro ):
         return LispInterpreter.macroexpand( env, sExpr, list(args) ) #convert args from a tuple to a list
      else:
         raise LispRuntimeError( 'Unknown lisp expression type.' )

   @staticmethod
   def bindArguments( env: Environment, paramList: List[Any], argsList: List[Any] ):
      paramNum = 0
      while paramNum < len(paramList):
         param = paramList[paramNum]
         if len(argsList) == 0:
            raise LispRuntimeError( 'Too few arguments.' )

         paramName = param
         if not isinstance(paramName, LSymbol):
            raise LispRuntimeError( 'Param {paramNum} expected to be a symbol.' )

         if paramName == LSymbol( '&REST' ):
            paramNum += 1
            try:
               paramName = paramList[paramNum]
            except IndexError:
               raise LispRuntimeError( 'Symbol expected after &REST' )

            env.setLocal( str(paramName), LList(*argsList))
            argsList = [ ]
            paramNum += 1

            if paramNum < len(paramList):
               raise LispRuntimeError( 'Symbol after &REST must be last parameter.' )

         elif paramName == LSymbol( '&OPTIONAL' ):
            paramNum += 1
            try:
               paramSpec = paramList[paramNum]
            except IndexError:
               raise LispRuntimeError( 'Parameter spec expected after &OPTIONAL.' )

            if isinstance(paramSpec, LSymbol):
               paramName = paramSpec
               paramDefVal = LList( )
            elif isinstance(paramSpec, LList):
               try:
                  paramName, defaultValExpr = paramSpec
               except:
                  raise LispRuntimeError( 'Parameter spec following &OPTIONAL must be a list of (<variable> <defaultvalue>).' )

               if not isinstance(paramName, LSymbol):
                  raise LispRuntimeError( 'Parameter spec following &OPTIONAL must be a list of (<variable> <defaultvalue>).' )

               # evaluate the defaultValExpr
               paramDefVal = LispInterpreter._lEval( env, defaultValExpr )

            if len(argsList) != 0:
               argVal = argsList[0]
               argsList = argsList[1:]
            else:
               argVal = paramDefVal

            env.setLocal( str(paramName), argVal )
            paramNum += 1
         elif paramName == LSymbol( '&KEY' ):
            raise LispRuntimeError( '&KEY parameters not implemented at this time.' )
         else:
            env.setLocal( str(paramName), argsList[0] )
            argsList = argsList[1:]
            paramNum += 1

      if len(argsList) != 0:
         raise LispRuntimeError( 'Too many arguments passed in.' )

   @staticmethod
   def macroexpand( env: Environment, expr: Any, args: List[Any] ) -> List[Any]:
      env = env.openScope( )

      # store the arguments as locals
      LispInterpreter.bindArguments( env, expr._params, args )

      # Evaluate each body expression; expanding each expr in turn to expand
      # any macros.  Place those expanded expressions in resultList.
      resultList = [ ]
      latestResult = None
      for expr in expr._body:
         latestResult = LispInterpreter._lEval( env, expr )
         resultList.append( latestResult )

      return resultList

   @staticmethod
   def backquote_expand( env: Environment, expr: Any ):
      '''Expand a backquote List expression.

      Note: This function is oddly dependent upon COMMA and COMMA-AT.
      '''
      if isinstance( expr, LList ):
         if len(expr) == 0:
            return LList( )

         primary = expr[0]
         if ( (primary == LSymbol('COMMA')) or (primary == LSymbol('COMMA-AT')) ):
            result = LispInterpreter._lEval(env, expr)
            return result

         resultList: List[Any] = [ ]
         for listElt in expr:
            resultListElt = LispInterpreter.backquote_expand( env, listElt )
            if ( isinstance( resultListElt, LList ) and
                 (resultListElt[0] == LSymbol('COMMA-AT')) ):
               for elt in resultListElt.rest().first():
                  resultList.append( elt )
            else:
               resultList.append( resultListElt )
         return LList( *resultList )
      else:
         return expr

   @staticmethod
   def constructPrimitives( parseLispString: Callable[[str], Any] ) -> Dict[str, Any]:
      primitiveDict: Dict[str, Any] = { }
      INSIDE_BACKQUOTE = False

      # ###################################
      # Lisp Object & Primitive Definitions
      # ###################################
      L_NIL = LList( )
      L_T  = LSymbol( 'T' )
      primitiveDict[ 'T'    ] = L_T
      primitiveDict[ 'NIL'  ] = L_NIL
      primitiveDict[ 'PI'   ] = math.pi
      primitiveDict[ 'E'    ] = math.e
      primitiveDict[ 'INF'  ] = math.inf
      primitiveDict[ '-INF' ] = -math.inf
      primitiveDict[ 'NAN'  ] = math.nan

      class LDefPrimitive( object ):
         def __init__( self, primitiveSymbol: str, args: str, specialOperation: bool=False ) -> None:
            '''standardEvalOrder indicates that this function evaluates its
            arguments in the usual way.  That is arguments each get evaluated
            in order of occurrence and the results of those valuations are
            passed to the function as the arguments.  False indicates that
            the evaluation order of the arguments is handled by the primitive.
            '''
            self._name:str  = primitiveSymbol.upper( )
            self._usage:str = f'({primitiveSymbol} {args})' if args else ''
            self._specialOp:bool = specialOperation

         def __call__( self, primitiveDef ):
            nonlocal primitiveDict
            lPrimitivObj = LPrimitive( primitiveDef, self._name,
                                       self._usage, self._specialOp )
            primitiveDict[ self._name ] = lPrimitivObj
            return lPrimitivObj

      # =================
      # Symbol Definition
      # -----------------
      @LDefPrimitive( 'def!', '\'<symbol> <object>' )                          # (def! '<symbol> <expr> )  ;; Define a var in the local scope.
      def LP_defLocal( env, *args, **kwargs ):
         try:
            key,val = args
         except:
            raise LispRuntimeFuncError( LP_defLocal, '2 arguments expected.', )

         try:
            if isinstance( val, LFunction ):
               val.setName( str(key) )

            return env.setLocal( str(key), val )
         except:
            raise LispRuntimeFuncError( LP_defLocal, 'Unknown error.' )

      @LDefPrimitive( 'def!!', '\'<symbol> <object>' )                         # (def!! '<symbol> <expr> ) ;; Define a var in the global scope.
      def LP_defGlobal( env, *args, **kwargs ):
         try:
            key,val = args
         except:
            raise LispRuntimeFuncError( LP_defGlobal, '2 arguments expected.' )

         try:
            if isinstance( val, LFunction ):
               val.setName( str(key) )

            return env.setGlobal( str(key), val)
         except:
            raise LispRuntimeFuncError( LP_defGlobal, 'Unknown error.' )

      @LDefPrimitive( 'defmacro!!', '<symbol> (<param1> <param2> ...) <expr1> <expr2> ...', specialOperation=True )
      def LP_defmacro( env, *args, **kwargs ):
         try:
            fnName, funcParams, *funcBody = args
         except:
            raise LispRuntimeFuncError( LP_defmacro, "3 or more arguments expected." )

         if not isinstance( fnName, LSymbol ):
            raise LispRuntimeFuncError( LP_defmacro, "Argument 1 expected to be a symbol." )

         if not isinstance( funcParams, LList ):
            raise LispRuntimeFuncError( LP_defmacro, "Argument 2 expected to be a list of symbols." )

         theFunc = LMacro( fnName, funcParams, funcBody )
         assert isinstance( env, Environment )
         env.setGlobal( str(fnName), theFunc )
         return theFunc

      @LDefPrimitive( 'macroexpand', '\'(macroName <arg1> <arg2> ...)' )
      def LP_macroexpand( env, *args, **kwargs ):
         assert isinstance( env, Environment )

         if len(args) != 1:
            raise LispRuntimeFuncError( LP_macroexpand, 'Exactly 1 argument expected.' )

         theMacroCall = args[0]

         if not isinstance( theMacroCall, LList ):
            raise LispRuntimeFuncError( LP_macroexpand, 'Argument 1 expected to be a list.' )

         # Break the list contents into a function and a list of args
         try:
            primary, *exprArgs = theMacroCall
         except:
            raise LispRuntimeError( 'Macro call must be at least two elements in length.' )

         if not isinstance( primary, LSymbol ):
            raise LispRuntimeError( f'Badly formed list expression.  The first element should be a symbol or function.' )

         # fn is an LPrimitive, LFunction or a function name symbol
         # Use this information to get the function definition
         macroDef = LispInterpreter._lEval( env, primary )
         if not isinstance( macroDef, LMacro ):
            raise LispRuntimeError( 'Badly formed list expression.  The first element should evaluate to a macro.' )

         listOfExpandedMacroBodyExprs = LispInterpreter._lEval( env, macroDef, *exprArgs )

         return LList( *listOfExpandedMacroBodyExprs)

      @LDefPrimitive( 'setf', '<symbol> <sexpr>', specialOperation=True )      # (setf '<symbol> <expr> )  ;; Update a variable.  If doesn't already exist make a global.
      def LP_setf( env, *args, **kwargs ):
         assert isinstance( env, Environment )

         try:
            key,val = args
         except:
            raise LispRuntimeFuncError( LP_setf, '2 arguments expected.' )

         val = LispInterpreter._lEval(env, val)
         key = str(key)
         if isinstance( val, LFunction ):
            val.setName( key )

         try:
            # If key exists somewhere in the symbol table hierarchy, set its
            # value to val.  If it doesn't exist, define it in the local-most
            # symbol table and set its value to val.
            theSymTab = env.findDef( str(key) )
            if theSymTab:
               theSymTab.setLocal( key, val )
            else:
               env.setGlobal( key, val )
            return val
         except:
            raise LispRuntimeFuncError( LP_setf, 'Unknown error.' )

      @LDefPrimitive( 'undef!', '\'<symbol>' )                                 # (undef! '<symbol>)   ;; undefine the most local definition for a symbol.
      def LP_undef( env, *args, **kwargs ):
         if len(args) == 1:
            key = args[0]
         else:
            raise LispRuntimeFuncError( LP_undef, '1 argument exptected.' )

         try:
            env.undef( str(key) )
            return L_NIL
         except Exception as ex:
            print(ex)
            raise LispRuntimeFuncError( LP_undef, 'Unknown error.' )

      @LDefPrimitive( 'symtab!', '')                                           # (symtab!) ;; print the symbol table.
      def LP_symtab( env, *args, **kwargs ):
         if len(args) > 0:
            raise LispRuntimeFuncError( LP_symtab, '0 arguments expected.' )

         print( 'Symbol Table Dump:  Inner-Most Scope First')
         print( '------------------------------------------')
         try:
            while env:
               symList = env.localSymbols()
               print( symList )
               env = env.parentEnv()
         except:
            pass

         return L_NIL

      # ==================
      # Control Structures
      # ------------------
      @LDefPrimitive( 'lambda', '(<param1> <param2> ... ) <expr1> <expr2> ...', specialOperation=True )
      def LP_lambda( env, *args, **kwargs ):
         try:
            funcParams, *funcBody = args
         except ValueError:
            raise LispRuntimeFuncError( LP_lambda, '2 arguments expected.' )

         return LFunction( LSymbol(""), funcParams, funcBody )

      @LDefPrimitive( 'block', '<expr1> <expr2> ...)', specialOperation=True )                   # (block <expr1> <expr2> ...)     ;; execute the sequence of expr's in a nested scope
      def LP_block( env, *args, **kwargs ):
         if len(args) < 1:
            raise LispRuntimeFuncError( LP_block, '1 or more arguments expected.' )

         env = env.openScope( )

         lastResult = L_NIL
         for expr in args:
            lastResult = LispInterpreter._lEval( env, expr )

         #env = env.closeScope( )  # Will occur when env goes out of scope

         return lastResult

      @LDefPrimitive( 'progn', '<sexpr1> <sexpr2> ...', specialOperation=True )
      def LP_progn( env, *args, **kwargs ):
         if len(args) < 1:
            raise LispRuntimeFuncError( LP_progn, '1 or more arguments expected.' )

         lastResult = L_NIL
         for expr in args:
            lastResult = LispInterpreter._lEval( env, expr )

         return lastResult

      @LDefPrimitive( 'if', '<cond> <conseq> [<alt>]', specialOperation=True )                   # (if <cond> <conseq> [<alt>])     ;; If statement
      def LP_if( env, *args, **kwargs ):
         numArgs = len(args)
         if not(2 <= numArgs <= 3):
            raise LispRuntimeFuncError( LP_if, '2 or 3 arguments expected.' )

         condExpr,*rest = args

         condResult = LispInterpreter._lEval( env, condExpr )
         if LispInterpreter._lTrue(condResult):
            return LispInterpreter._lEval( env, rest[0])    # The THEN part
         elif numArgs == 3:
            return LispInterpreter._lEval( env, rest[1])    # The ELSE part
         else:
            return L_NIL

      @LDefPrimitive( 'cond', '(<cond1> <body1>) (<cond2> <body2>)', specialOperation=True )     # (cond (<cond1> <expr1>) (<cond2> <expr2>) ...)
      def LP_cond( env, *args, **kwargs ):
         if len(args) < 1:
            raise LispRuntimeFuncError( LP_cond, '1 or more argument exptected.' )

         caseList = args
         for caseNum,case in enumerate(caseList):
            assert isinstance(case, LList)
            try:
               testExpr, *body = case
            except ValueError:
               raise LispRuntimeFuncError( LP_cond, f"Entry {caseNum+1} does not contain a (<cond:expr> <body:expr>) pair." )

            if LispInterpreter._lTrue(LispInterpreter._lEval(env,testExpr)):
               latestResult = None
               for sexpr in body:
                  latestResult = LispInterpreter._lEval( env, sexpr )
               return latestResult

         return LList( )

      @LDefPrimitive( 'case', '<expr> (<val1> <body1>) (<val2> <body2>) ...)', specialOperation=True )  # (case <expr> (<val1> <expr1>) (<val2> <expr2>) ...)
      def LP_case( env, *args, **kwargs ):
         try:
            expr, *caseList = args
         except ValueError:
            raise LispRuntimeFuncError( LP_case, '2 or more arguments exptected.' )

         exprVal = LispInterpreter._lEval( env, expr )

         for caseNum,case in enumerate(caseList):
            try:
               caseVal, *body = case
            except ValueError:
               raise LispRuntimeFuncError( LP_case, "Entry {0} does not contain a (<val> <body>) pair.".format(caseNum+1) )

            if len(body) < 1:
               raise LispRuntimeFuncError( LP_case, "Case body expected." )

            # If the case condition is true evaluate the body of the case
            if LispInterpreter._lEval(env,caseVal) == exprVal:
               latestResult = None
               for sexpr in body:
                  latestResult = LispInterpreter._lEval( env, sexpr )
               return latestResult

         return L_NIL

      @LDefPrimitive( 'quote', '<expr>', specialOperation=True )                                        # (quote <expr>)                     ;; return <expr> without evaluating it
      def LP_quote( env, *args, **kwargs ):
         if (len(args) != 1):
            raise LispRuntimeFuncError( LP_quote, '1 argument exptected.' )

         return args[0]

      @LDefPrimitive( 'backquote', '<expr>', specialOperation=True )
      def LP_backquote( env, *args, **kwargs ):
         nonlocal INSIDE_BACKQUOTE
         if (len(args) != 1):
            raise LispRuntimeFuncError( LP_backquote, '1 argument exptected.' )

         if INSIDE_BACKQUOTE:
            raise LispRuntimeFuncError( LP_backquote, 'Cannot nest backquotes.')

         sExpr = args[0]

         try:
            INSIDE_BACKQUOTE = True
            expandedForm = LispInterpreter.backquote_expand( env, sExpr )
         finally:
            INSIDE_BACKQUOTE = False

         return expandedForm

      @LDefPrimitive( 'comma', '<expr>', specialOperation=True )
      def LP_comma( env, *args, **kwargs ):
         nonlocal INSIDE_BACKQUOTE
         if (len(args) != 1):
            raise LispRuntimeFuncError( LP_comma, '1 argument exptected.' )

         if not INSIDE_BACKQUOTE:
            raise LispRuntimeFuncError( LP_comma, 'COMMA can only occur inside a BACKQUOTE.')

         subordinateExpr = args[0]
         result = LispInterpreter._lEval( env, subordinateExpr )
         return result

      @LDefPrimitive( 'comma-at', '<expr>', specialOperation=True )
      def LP_comma_at( env, *args, **kwargs ):
         nonlocal INSIDE_BACKQUOTE
         if (len(args) != 1):
            raise LispRuntimeFuncError( LP_comma_at, '1 argument exptected.' )

         if not INSIDE_BACKQUOTE:
            raise LispRuntimeFuncError( LP_comma_at, 'COMMA-AT can only occur inside a BACKQUOTE.')

         subordinateExpr = args[0]
         result = LispInterpreter._lEval( env, subordinateExpr )
         if not isinstance(result, LList):
            raise LispRuntimeFuncError( LP_comma_at, 'Argument 1 must evaluate to a List.' )
         retValue = LList( LSymbol('COMMA-AT'), result )
         return retValue

      @LDefPrimitive( 'gensym', '' )                                           # (gensym)  # Generate and return a random unique symbol.
      def LP_gensym( env, *args, **kwargs ):
         if len(args) > 0:
            raise LispRuntimeFuncError( LP_gensym, '0 arguments expected.' )

         symstr = random.choices( string.ascii_uppercase, k=16 )
         sym = LSymbol( symstr )
         return sym

      @LDefPrimitive( 'while', '<conditionExpr> <body>', specialOperation=True )        # (while <conditionExpr> <expr1> <expr2> ...)  ;; repeatedly evaluate body while condition is true.
      def LP_while( env, *args, **kwargs ):
         try:
            conditionExpr, *body = args
         except ValueError:
            raise LispRuntimeFuncError( LP_while, '2 arguments expected.' )

         latestResult = LList( )

         try:
            condResult = LispInterpreter._lEval(env, conditionExpr)
            while LispInterpreter._lTrue( condResult ):
               for expr in body:
                  latestResult = LispInterpreter._lEval( env, expr )
               condResult = LispInterpreter._lEval(env, conditionExpr )

         except Exception:
            raise LispRuntimeFuncError( LP_while, "Error evaluating condition for while loop." )

         return latestResult

      @LDefPrimitive( 'doTimes', '(<variable> <integer>) <sexpr1> <sexpr2> ...', specialOperation=True )
      def LP_dotimes( env, *args, **kwargs ):
         try:
            loopControl, *body = args
         except ValueError:
            raise LispRuntimeFuncError( LP_dotimes, '2 or more arguments expected.' )

         if not isinstance( loopControl, LList ):
            raise LispRuntimeFuncError( LP_dotimes, 'Argument 1 expected to be a list.' )

         try:
            variable, countExpr = loopControl
         except ValueError:
            raise LispRuntimeFuncError( LP_dotimes, 'Argument 1 expected to contain two elements.' )

         if not isinstance( variable, LSymbol ):
            raise LispRuntimeFuncError( LP_dotimes, 'Argument 1 of the control list expected to be a symbol.' )

         count = LispInterpreter._lEval( env, countExpr )

         if not isinstance( count, int ):
            raise LispRuntimeFuncError( LP_dotimes, 'Argument 2 of the control list expected to be an integer' )

         assert isinstance( env, Environment )

         latestResult = None
         for iterCount in range(count):
            env.setLocal( str(variable), iterCount )
            for sexpr in body:
               latestResult = LispInterpreter._lEval( env, sexpr )

         return latestResult

      @LDefPrimitive( 'dolist', '(<variable> <list>) <sexpr1> <sexpr2> ...' )
      def LP_dolist( env, *args, **kwargs ):
         try:
            controlInfo, *body = args
         except:
            raise LispRuntimeFuncError( LP_dolist, "2 or more arguments exptected." )

         try:
            controlVar, controlLst = controlInfo
         except:
            raise LispRuntimeFuncError( LP_dolist, "Control list exptects two arguments." )

         if not isinstance( controlVar, LSymbol ):
            raise LispRuntimeFuncError( LP_dolist, "Control variable expcted to be a symbol." )

         controlLst = LispInterpreter._lEval( env, controlLst )
         if not isinstance( controlLst, LList ):
            raise LispRuntimeFuncError( LP_dolist, "Control expected to be a list." )

         for element in controlLst:
            env.setLocal( str(controlVar), element )
            latestResult = None
            for sexpr in body:
               latestResult = LispInterpreter._lEval( env, sexpr )
            return latestResult

         return L_NIL

      @LDefPrimitive( 'funcall', '<fnNameSymbol> <arg1> <arg2> ...' )
      def LP_funcall( env, *args, **kwargs ):
         try:
            fnNameSymbol, *fnArgs = args
         except:
            raise LispRuntimeFuncError( LP_funcall, "1 or more arguments expected" )

         newExpr = LList( fnNameSymbol, *fnArgs )

         result = LispInterpreter._lEval( env, newExpr )
         return result

      @LDefPrimitive( 'eval', '<expr>' )                                       # (eval '<expr>)                     ;; evaluate <expr>
      def LP_eval( env, *args, **kwargs ):
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_eval, '1 argument exptected.' )

         return LispInterpreter._lEval( env, args[0] )

      @LDefPrimitive( 'parse', '<sExpressionString>')
      def LP_parse( env, *args, **kwargs ):
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_parse, '1 string argument expected.' )

         theExprStr = args[0]
         theExprAST = parseLispString( theExprStr )
         return theExprAST

      @LDefPrimitive( 'pprint', '<sExpr>' )
      def LP_pprint( env, *args, **kwargs) :
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_pprint, '1 lisp object argument expected.' )

         theExpr = args[0]
         theExprStr = prettyPrintSExpr( theExpr )
         return theExprStr

      # =======================
      # List & Map Manipulation
      # -----------------------
      @LDefPrimitive( 'list', '<expr1> <expr2> ...')                           # (list <expr1> <expr2> ...)         ;; return a list of evaluated expressions
      def LP_list( env, *args, **kwargs ):
         if len(args) == 0:
            raise LispRuntimeFuncError( LP_list, '1 or more arguments expected.' )

         theLst = [ ]

         for exprNum,expr in enumerate(args):
            theLst.append( expr )

         return LList( *theLst )

      @LDefPrimitive( 'map', '(<key1> <val1>) (<key2> <val>2) ...', specialOperation=True )       # (map (<key1> <val1>) (<key2> <val2>) ...)  ;; construct a map of key-value pairs
      def LP_map( env, *args, **kwargs ):
         if len(args) < 1:
            raise LispRuntimeFuncError( LP_map, '1 or more arguments exptected.' )

         theMapping = { }

         for entryNum,key_expr_pair in enumerate(args):
            try:
               key,expr =  key_expr_pair
            except:
               raise LispRuntimeFuncError( LP_map, f'Entry {entryNum + 1} does not contain a (key value) pair.' )

            if isinstance( key, (int,float,str,LSymbol) ):
               theMapping[ str(key) ] = LispInterpreter._lEval( env, expr)
            else:
               raise LispRuntimeFuncError( LP_map, f'Entry {entryNum+1} has an invalid <key> type.' )


         theLMapInst = LMap( aMap=theMapping )
         return theLMapInst

      @LDefPrimitive( 'first', '<list>' )                                      # (first <list>)                     ;; return the first item in the list
      def LP_first( env, *args, **kwargs ):
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_first, '1 argument expected.' )

         theList = args[0]
         if not isinstance(theList, LList):
            raise LispRuntimeFuncError( LP_first, '1st argument expected to be a list.' )

         try:
            return theList.first()
         except IndexError:
            return LList( )

      @LDefPrimitive( 'rest', '<list>' )                                       # (rest <list>)                      ;; return the list without the first item
      def LP_rest( env, *args, **kwargs ):
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_rest, '1 argument expected.', usage='(rest <list>)' )

         theList = args[0]
         if not isinstance(theList, LList):
            raise LispRuntimeFuncError( LP_first, '1st argument expected to be a list.' )

         try:
            return theList.rest()
         except IndexError:
            return LList( )

      @LDefPrimitive( 'cons', '\'<obj> \'<list>' )                             # (cons '<obj> '<list>)              ;; return the list with <obj> inserted into the front
      def LP_cons( env, *args, **kwargs ):
         try:
            arg1,arg2 = args
         except:
            raise LispRuntimeFuncError( LP_cons, '2 arguments exptected.' )

         try:
            copiedList = arg2.copy( )
            copiedList.insert( 0, arg1 )
         except:
            raise LispRuntimeFuncError( LP_cons, 'Invalid argument.' )

         return copiedList

      @LDefPrimitive( 'push!', '\'<list> \'<value>' )                          # (push! '<list> <value>)
      def LP_push( env, *args, **kwargs ):
         try:
            alist, value = args
         except Exception:
            raise LispRuntimeFuncError( LP_push, '2 arguments exptected.' )

         try:
            if isinstance(alist, LList):
               alist.append( value )
            else:
               alist = L_NIL
         except:
            raise LispRuntimeFuncError( LP_push, 'Invalid argument.' )

         return alist

      @LDefPrimitive( 'pop!', '\'<list>' )                                     # (pop! '<list>)
      def LP_pop( env, *args, **kwargs ):
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_pop, '1 argument expected.' )

         alist = args[0]

         try:
            value = alist.pop()
         except:
            raise LispRuntimeFuncError( LP_pop, 'Invalid argument.' )

         return value

      @LDefPrimitive( 'at', '\'<listORMap> \'<keyOrIndex>' )                   # (at '<listOrMap> '<keyOrIndex>)
      def LP_at( env, *args, **kwargs ):
         try:
            keyed,key = args
         except:
            raise LispRuntimeFuncError( LP_at, '2 arguments expected.' )

         if isinstance(keyed, LList):
            keyed = keyed
         elif isinstance(keyed, LMap):
            keyed = keyed._dict
         else:
            raise LispRuntimeFuncError( LP_at, 'Invalid argument.  List or Map expected.' )

         if isinstance(key, LSymbol):
            key = key._val

         try:
            value = keyed[ key ]
         except:
            raise LispRuntimeFuncError( LP_at, 'Invalid argument key/index.' )

         return value

      @LDefPrimitive( 'atSet!', '<listOrMap> <keyOrIndex> <value>' )           # (atSet! <listOrMap> <keyOrIndex> <value>)
      def LP_atSet( env, *args, **kwargs ):
         try:
            keyed,key,value = args
         except:
            raise LispRuntimeFuncError( LP_atSet, '3 arguments expected.' )

         if isinstance(keyed, LList):
            keyed = keyed
         elif isinstance(keyed, LMap):
            keyed = keyed._dict
         else:
            raise LispRuntimeFuncError( LP_atSet, 'Invalid argument.  List or map expeced as first argument.' )

         if isinstance(key, LSymbol):
            key = key._val

         try:
            keyed[ key ] = value
         except:
            raise LispRuntimeFuncError( LP_atSet, 'Invalid argument key/index.' )

         return value


      @LDefPrimitive( 'append', '\'<list1> \'<list2>' )                        # (append '<list-1> '<list-2>)
      def LP_append( env, *args, **kwargs ):
         try:
            arg1,arg2 = args
         except:
            raise LispRuntimeFuncError( LP_append, '2 arguments expected' )

         if isinstance( arg1, LList ) and isinstance( arg2, LList ):
            newList = arg1 + arg2
            return LList( *newList )
         else:
            raise LispRuntimeFuncError( LP_append, 'Invalid argument.' )

      @LDefPrimitive( 'hasValue?', '\'<listOrMap> \'<value>' )                 # (hasValue? '<listOrMap> '<value>)
      def LP_hasValue( env, *args, **kwargs ):
         try:
            keyed,aVal = args
         except:
            raise LispRuntimeFuncError( LP_hasValue, '2 arguments expected.' )

         if isinstance(keyed, LList):
            keyed = keyed
         elif isinstance(keyed, LMap):
            keyed = keyed._dict.values()
         else:
            raise LispRuntimeFuncError( LP_hasValue, 'Invalid argument.  Argument 1 expected to be a list or map.')

         try:
            return L_T if aVal in keyed else L_NIL    # T or NIL
         except:
            raise LispRuntimeFuncError( LP_hasValue, 'Invalid argument.')

      @LDefPrimitive( 'update!', '<map1> <map2>' )                             # (update! <map1> <map2>)                    ;; merge map2's data into map1
      def LP_update( env, *args, **kwargs ):
         try:
            map1,map2 = args
         except:
            raise LispRuntimeFuncError( LP_update, '2 arguments exptected.' )

         try:
            map1._dict.update( map2._dict )
            return map1
         except:
            raise LispRuntimeFuncError( LP_update, 'Invalid argument.' )

      @LDefPrimitive( 'hasKey?', '<map> <key>' )                               # (hasKey? <map> <key>)
      def LP_hasKey( env, *args, **kwargs ):
         try:
            aMap,aKey = args
         except:
            raise LispRuntimeFuncError( LP_hasKey, '2 arguments expected.' )

         if isinstance(aMap, LMap):
            aMap = aMap._dict
         else:
            raise LispRuntimeFuncError( LP_hasKey, 'Invalid argument 1.  Map expected.')

         if isinstance(aKey, LSymbol):
            aKey = aKey._val

         try:
            return L_T if aKey in aMap else L_NIL   # T or NIL
         except:
            raise LispRuntimeFuncError( LP_hasKey, 'Invalid argument.' )

      # =====================
      # Arithmetic Operations
      # ---------------------
      @LDefPrimitive( '+', '<expr1> <expr2> ...')                              # (+ <val1> <val2>)
      def LP_add( env, *args, **kwargs ):
         if len(args) < 1:
            raise LispRuntimeFuncError( LP_add, '1 or more arguments expected.' )

         try:
            return sum(args)
         except:
            raise LispRuntimeFuncError( LP_add, 'Invalid argument.' )

      @LDefPrimitive( '-', '<expr1> <expr2> ...')                              # (- <val1> <val2>)
      def LP_sub( env, *args, **kwargs ):
         argct = len(args)
         if len(args) < 1:
            raise LispRuntimeFuncError( LP_sub, '1 or more arguments expected.' )

         try:
            if argct == 1:
               return -1 * args[0]
            else:
               return functools.reduce( lambda x,y: x - y, args )
         except:
            raise LispRuntimeFuncError( LP_sub, 'Invalid argument.' )

      @LDefPrimitive( '*', '<expr1> <expr2> ...' )                             # (* <val1> <val2>)
      def LP_mul( env, *args, **kwargs ):
         if len(args) < 2:
            raise LispRuntimeFuncError( LP_mul, '2 or more arguments exptected.' )

         try:
            return functools.reduce( lambda x,y: x * y, iter(args) )
         except:
            raise LispRuntimeFuncError( LP_mul, 'Invalid argument.' )

      @LDefPrimitive( '/', '<expr1> <expr2> ...' )                             # (/ <val1> <val2>)
      def LP_div( env, *args, **kwargs ):
         if len(args) < 2:
            raise LispRuntimeFuncError( LP_div, '2 or more arguments exptected.' )

         try:
            return functools.reduce( lambda x,y: x / y, iter(args) )
         except:
            raise LispRuntimeFuncError( LP_div, 'Invalid argument.' )

      @LDefPrimitive( '//', '<expr1> <expr>')                                  # (// <val1> <val2>)
      def LP_intdiv( env, *args, **kwargs ):
         if len(args) != 2:
            raise LispRuntimeFuncError( LP_intdiv, '2 arguments expected.' )

         try:
            return args[0] // args[1]
         except:
            raise LispRuntimeFuncError( LP_intdiv, 'Invalid argument.' )

      @LDefPrimitive( 'mod', '<expr1> <expr>')                                 # (mod <val1> <val2>)
      def LP_moddiv( env, *args, **kwargs ):
         if len(args) != 2:
            raise LispRuntimeFuncError( LP_moddiv, '2 arguments expected.' )

         try:
            return args[0] % args[1]
         except:
            raise LispRuntimeFuncError( LP_moddiv, 'Invalid argument.' )

      @LDefPrimitive( 'log', '<expr> [ <base> ]')                              # (log <x> [<base>])                         ;; if base is not provided, 10 is used.
      def LP_log( env, *args, **kwargs ):
         numArgs = len(args)
         if not( 1 <= numArgs <= 2 ):
            raise LispRuntimeFuncError( LP_log, '1 or 2 arguments exptected.' )

         try:
            num,*rest = args
            base = 10 if len(rest) == 0 else rest[0]
            return math.log(num,base)
         except:
            raise LispRuntimeFuncError( LP_log, 'Invalid argument.' )

      @LDefPrimitive( 'pow', '<base> <power>')                                 # (pow <base> <power>)
      def LP_pow( env, *args, **keys ):
         if len(args) != 2:
            raise LispRuntimeFuncError( LP_pow, '2 arguments expected.' )

         try:
            base,power = args
            return base ** power
         except:
            raise LispRuntimeFuncError( LP_pow, 'Invalid argument.' )

      @LDefPrimitive( 'sin', '<radians>')                                      # (sin <radians>)
      def LP_sin( env, *args, **kwargs ):
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_sin, '1 argument expected.' )

         try:
            return math.sin(args[0])
         except:
            raise LispRuntimeFuncError( LP_sin, 'Invalid argument.' )

      @LDefPrimitive( 'cos', '<radians>')                                      # (cos <radians>)
      def LP_cos( env, *args, **kwargs ):
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_cos, '1 argument expected.' )

         try:
            return math.cos(args[0])
         except:
            raise LispRuntimeFuncError( LP_cos, 'Invalid argument.' )

      @LDefPrimitive( 'min', '<val1> <val2> ...')                              # (min <val1> <val2> ...)
      def LP_min( env, *args, **kwargs ):
         if len(args) < 1:
            raise LispRuntimeFuncError( LP_min, '1 or more arguments exptected.' )

         try:
            return min( *args )
         except:
            raise LispRuntimeFuncError( LP_min, 'Invalid argument.' )

      @LDefPrimitive( 'max', '<val1> <val2> ...')                              # (max <val1> <val2> ...)
      def LP_max( env, *args, **kwargs ):
         if len(args) < 1:
            raise LispRuntimeFuncError( LP_max, '1 or more arguments exptected.' )

         try:
            return max( *args )
         except:
            raise LispRuntimeFuncError( LP_max, 'Invalid argument.' )

      # ==========
      # Predicates
      # ----------
      @LDefPrimitive( 'isNil?', '<expr>')                                      # (isNil? <expr>)
      def LP_isNil( env, *args, **kwargs ):
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_isNil, '1 argument expected.' )

         arg1 = args[0]
         return L_T if (isinstance(arg1,LList) and (len(arg1) == 0)) else L_NIL  # T or NIL

      @LDefPrimitive( 'isNumber?', '<expr>')                                   # (isNumber?  <expr>)
      def LP_isNumber( env, *args, **kwargs ):
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_isNumber, '1 argument expected.' )

         return L_T if isinstance( args[0], L_NUMBER ) else L_NIL

      @LDefPrimitive( 'isSymbol?', '<expr>')                                   # (isSymbol?  <expr>)
      def LP_isSym( env, *args, **kwargs ):
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_isSym, '1 argument expected.' )

         return L_T if isinstance( args[0], LSymbol ) else L_NIL

      @LDefPrimitive( 'isAtom?', '<expr>')                                     # (isAtom? <expr>) -> 1 if expr in { int, float, fraction, string }
      def LP_isAtom( env, *args, **kwargs ):
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_isAtom, '1 argument expected.' )

         return L_T if isinstance( args[0], L_ATOM ) else L_NIL

      @LDefPrimitive( 'isList?', '<expr>')                                     # (isList? <expr>)
      def LP_isList( env, *args, **kwargs ):
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_isList, '1 argument expected.' )

         return L_T if isinstance( args[0], LList ) else L_NIL

      @LDefPrimitive( 'isMap?', '<expr>')                                      # (isMap?  <expr>)
      def LP_isMap( env, *args, **kwargs ):
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_isMap, '1 argument expected.' )

         return L_T if isinstance( args[0], LMap ) else L_NIL

      @LDefPrimitive( 'isString?', '<expr>')                                   # (isString?  <expr>)
      def LP_isStr( env, *args, **kwargs ):
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_isStr, '1 argument expected.' )

         return L_T if isinstance( args[0], str ) else L_NIL

      @LDefPrimitive( 'isFunction?', '<expr>')                                 # (isFunction? <expr>)
      def LP_isCall( env, *args, **kwargs ):
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_isCall, '1 argument expected.' )

         return L_T if isinstance( args[0], (LPrimitive,LFunction) ) else L_NIL

      # ====================
      # Relational Operators
      # --------------------
      @LDefPrimitive( 'is?', '<expr1> <expr2>')                                # (is? <val1> <val2>)      Are the two values the same object?
      def LP_is( env, *args, **kwargs ):
         try:
            arg1,arg2 = args
         except:
            raise LispRuntimeFuncError( LP_is, '2 arguments exptected.' )

         if isinstance(arg1, (int,float,str)):
            return L_T if (arg1 == arg2) else L_NIL
         else:
            return L_T if (arg1 is arg2) else L_NIL

      @LDefPrimitive( '=', '<expr1> <expr2> ...')                              # (=   <val1> <val2> ...)    Are all the values equal?
      def LP_equal( env, *args, **kwargs ):
         numArgs = len(args)
         if numArgs < 2:
            raise LispRuntimeFuncError( LP_equal, '2 or more arguments expected.' )

         pairs = [ ]
         prior = None
         for mbr in args:
            if prior is not None:
               pairs.append( (prior,mbr) )
            prior = mbr

         try:
            for arg1,arg2 in pairs:
               if not( arg1 == arg2 ):
                  return L_NIL

            return L_T
         except:
            raise LispRuntimeFuncError( LP_equal, 'Unknown error.' )

      @LDefPrimitive( '<>', '<expr1> <expr2> ...')                             # (<>  <val1> <val2> ...)
      def LP_notEqual( env, *args, **kwargs ):
         numArgs = len(args)
         if numArgs < 2:
            raise LispRuntimeFuncError( LP_notEqual, '2 or more arguments expected.' )

         pairs = [ ]
         prior = None
         for mbr in args:
            if prior is not None:
               pairs.append( (prior,mbr) )
            prior = mbr

         try:
            for arg1,arg2 in pairs:
               if not( arg1 != arg2 ):
                  return L_NIL

            return L_T
         except:
            raise LispRuntimeFuncError( LP_notEqual, 'Unknown error.' )

      @LDefPrimitive( '<', '<expr1> <expr2> ...')                              # (<   <val1> <val2> ...)
      def LP_less( env, *args, **kwargs ):
         numArgs = len(args)
         if numArgs < 2:
            raise LispRuntimeFuncError( LP_less, '2 or more arguments expected.' )

         pairs = [ ]
         prior = None
         for mbr in args:
            if prior is not None:
               pairs.append( (prior,mbr) )
            prior = mbr

         try:
            for arg1,arg2 in pairs:
               if not( arg1 < arg2 ):
                  return L_NIL

            return L_T
         except:
            raise LispRuntimeFuncError( LP_less, 'Unknown error.' )

      @LDefPrimitive( '<=', '<expr1> <expr2> ...' )                            # (<=  <val1> <val2> ...)
      def LP_lessOrEqual( env, *args, **kwargs ):
         numArgs = len(args)
         if numArgs < 2:
            raise LispRuntimeFuncError( LP_lessOrEqual, '2 or more arguments expected.' )

         pairs = [ ]
         prior = None
         for mbr in args:
            if prior is not None:
               pairs.append( (prior,mbr) )
            prior = mbr

         try:
            for arg1,arg2 in pairs:
               if not( arg1 <= arg2 ):
                  return L_NIL

            return L_T
         except:
            raise LispRuntimeFuncError( LP_lessOrEqual, 'Unknown error.' )

      @LDefPrimitive( '>', '<expr1> <expr2> ...' )                             # (>   <val1> <val2> ...)
      def LP_greater( env, *args, **kwarg ):
         numArgs = len(args)
         if numArgs < 2:
            raise LispRuntimeFuncError( LP_greater, '2 or more arguments expected.' )

         pairs = [ ]
         prior = None
         for mbr in args:
            if prior is not None:
               pairs.append( (prior,mbr) )
            prior = mbr

         try:
            for arg1,arg2 in pairs:
               if not( arg1 > arg2 ):
                  return L_NIL

            return L_T
         except:
            raise LispRuntimeFuncError( LP_greater, 'Unknown error.' )

      @LDefPrimitive( '>=', '<expr1> <expr2> ...' )                            # (>=  <val1> <val2> ...)
      def LP_greaterOrEqual( env, *args, **kwargs ):
         numArgs = len(args)
         if numArgs < 2:
            raise LispRuntimeFuncError( LP_greaterOrEqual, '2 or more arguments expected.' )

         pairs = [ ]
         prior = None
         for mbr in args:
            if prior is not None:
               pairs.append( (prior,mbr) )
            prior = mbr

         try:
            for arg1,arg2 in pairs:
               if not( arg1 >= arg2 ):
                  return L_NIL

            return L_T
         except:
            raise LispRuntimeFuncError( LP_greaterOrEqual, 'Unknown error.' )

      # =================
      # Logical Operators
      # -----------------
      @LDefPrimitive( 'not', '<expr>')                                         # (not <val>)
      def LP_not( env, *args, **kwargs ):
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_not, '1 argument exptected.' )

         arg1 = args[0]
         return L_T if ((arg1 == 0) or ((isinstance(arg1,LList) and len(arg1)==0)) or (arg1 is None)) else L_NIL

      @LDefPrimitive( 'and', '<expr1> <expr2> ...' )                           # (and <val1> <val2> ...)
      def LP_and( env, *args, **kwargs ):
         if len(args) < 2:
            raise LispRuntimeFuncError( LP_and, '2 or more arguments exptected.' )

         for arg in args:
            if (arg == 0) or (arg is L_NIL) or (arg is None):
               return L_NIL

         return L_T

      @LDefPrimitive( 'or', '<expr1> <expr2> ...' )                            # (or  <val1> <val2> ...)
      def LP_or( env, *args, **kwargs ):
         if len(args) < 2:
            raise LispRuntimeFuncError( LP_or, '2 or more arguments exptected.' )

         for arg in args:
            if (arg != 0) and (arg is not L_NIL) and (arg is not None):
               return L_T

         return L_NIL

      # ===============
      # Type Conversion
      # ---------------
      @LDefPrimitive( 'float', '<sexpr>')                                      # (float <val>)
      def LP_float( env, *args, **kwargs ):
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_float, 'Exactly 1 argument expected.' )

         try:
            return float(args[0])
         except:
            raise LispRuntimeFuncError( LP_float, 'Invalid argument.' )

      @LDefPrimitive( 'int', '<expr>')                                         # (int <val>)
      def LP_int( env, *args, **kwargs ):
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_int, 'Exactly 1 argument expected.' )

         try:
            return int(args[0])
         except:
            raise LispRuntimeFuncError( LP_int, 'Invalid argument.' )

      @LDefPrimitive( 'string', '<expr1> <expr2> ...' )                        # (string <expr1> <expr2> ...)   ; returns the concatenation of the string results of the arguments
      def LP_string( env, *args, **kwargs ):
         if len(args) == 0:
            raise LispRuntimeFuncError( LP_string, '1 or more arguments exptected.' )

         resultStrs = []

         try:
            for arg in args:
               resultStrs.append( prettyPrintSExpr( arg ))
         except:
            raise LispRuntimeFuncError( LP_string, 'Unknown error.' )

         return ''.join(resultStrs)

      # ===============
      # I/O
      # ---------------
      @LDefPrimitive( 'write!', '<object>')                                    # (write! <lispObject>)
      def LP_write( env, *args, **kwargs ):
         numArgs = len(args)
         if numArgs != 1:
            raise LispRuntimeFuncError( LP_write, '1 argument expected' )

         value = args[0]
         valueStr = prettyPrintSExpr(value)
         valueStr = bytes( valueStr, "utf-8" ).decode( "unicode_escape" ) # decode escape sequences
         print( valueStr, sep='', end='', file=LispInterpreter.outStrm )

         return value

      @LDefPrimitive( 'writeLn!', '<object>')                                  # (writeLn! <lispObject>)
      def LP_writeln( env, *args, **kwargs ):
         numArgs = len(args)
         if numArgs != 1:
            raise LispRuntimeFuncError( LP_writeln, '1 argument expected' )

         value  = args[0]
         valueStr = prettyPrintSExpr(value)
         valueStr = bytes( valueStr, "utf-8" ).decode( "unicode_escape" ) # decode escape sequences
         print( valueStr, sep='', end='\n', file=LispInterpreter.outStrm )

         return value

      @LDefPrimitive( 'readLn!', '')                                           # (readLn!)
      def LP_readln( env, *args, **kwargs ):
         numArgs = len(args)
         if numArgs > 0:
            raise LispRuntimeFuncError( LP_readln, '0 arguments expected.' )

         return input()


      return primitiveDict

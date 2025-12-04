from LispAST import ( LSymbol, LList, LMap, LFunction, LPrimitive, LMacro,
                       prettyPrintSExpr )
from LispParser import LispParser
from ltk.Listener import Interpreter
from ltk.Environment import Environment

import functools
import math
import random
import fractions
from typing import Callable, Any


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

class LispInterpreter( Interpreter ):
   outStrm = None
   counter = 1               # Used by gensym to generate unique symbols

   def __init__( self ) -> None:
      self._parser: LispParser = LispParser( )
      primitiveDict: dict[str, Any] = LispInterpreter._constructPrimitives( self._parser.parse )
      self._env:Environment = Environment( parent=None, **primitiveDict )

   def reboot( self ) -> None:
      primitiveDict = LispInterpreter._constructPrimitives( self._parser.parse )
      self._env = self._env.reInitialize( **primitiveDict )

   def eval( self, inputExprStr: str, outStrm=None ) -> str:
      LispInterpreter.outStrm = outStrm
      try:
         ast = self._parser.parse( inputExprStr )
         returnVal = LispInterpreter._lEval( self._env, ast )
      finally:
         LispInterpreter.outStrm = None
      return prettyPrintSExpr( returnVal ).strip()

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
         result = env.getValue( sExpr.strval )
         return sExpr if result is None else result
      elif  isinstance( sExpr, LList ):
         # This code is called when sExpr is an LList and it's contents (a
         # function call usually) need to be evaluated.
         if len(sExpr) == 0:
            return LList( )

         # Break the list contents into a function and a list of args
         try:
            primary, *exprArgs = sExpr
         except ValueError:
            raise LispRuntimeError( 'Badly formed list expression.' )

         # primary is an LPrimitive, LFunction or LMacro or something that
         # evaluates to one of these.
         fnDef = LispInterpreter._lEval( env, primary )
         if not isinstance( fnDef, (LPrimitive, LFunction, LMacro) ):
            raise LispRuntimeError( f'Badly formed list expression \'{primary}\'.  The first element should evaluate to a primitive, function or macro.' )

         # Determine if the function uses the standard evaluation order for arguments
         evaluatedKeys: dict[str, Any] = { }
         if fnDef.specialOp:
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
         env = Environment( env )         # Open a new scope. Auto closes when env goes out of scope.

         # store the arguments as locals
         LispInterpreter._bindArguments( env, sExpr._params, list(args) ) #convert args from a tuple to a list

         # evaluate the body expressions.  Return the result of the last
         # body expression evaluated.
         latestResult = None
         for expr in sExpr._body:
            latestResult = LispInterpreter._lEval( env, expr )

         return latestResult
      elif isinstance( sExpr, LMacro ):
         return LispInterpreter._macroexpand( env, sExpr, list(args) ) #convert args from a tuple to a list
      else:
         raise LispRuntimeError( 'Unknown lisp expression type.' )

   @staticmethod
   def _bindArguments( env: Environment, paramList: list[Any], argsList: list[Any] ) -> None:
      paramNum = 0
      while paramNum < len(paramList):
         paramName = paramList[paramNum]
         if not isinstance(paramName, LSymbol):
            raise LispRuntimeError( 'Param {paramNum} expected to be a symbol.' )

         if paramName == '&REST':
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

         elif paramName == '&OPTIONAL':
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
         elif paramName == '&KEY':
            raise LispRuntimeError( '&KEY parameters not implemented at this time.' )
         else:
            if len(argsList) == 0:
               raise LispRuntimeError( 'Too few arguments.' )
            env.setLocal( str(paramName), argsList[0] )
            argsList = argsList[1:]
            paramNum += 1

      if len(argsList) != 0:
         raise LispRuntimeError( 'Too many arguments passed in.' )

   @staticmethod
   def _macroexpand( env: Environment, expr: Any, args: list[Any] ) -> list[Any]:
      env = Environment( env )      # Open a new scope.  Automatically closes when env goes out of scope

      # store the arguments as locals
      LispInterpreter._bindArguments( env, expr._params, args )

      # Evaluate each body expression; expanding each expr in turn to expand
      # the full macro body.  Place those expanded expressions in resultList.
      resultList = [ ]
      latestResult = None
      for expr in expr._body:
         latestResult = LispInterpreter._lEval( env, expr )
         resultList.append( latestResult )

      return resultList

   @staticmethod
   def _backquote_expand( env: Environment, expr: Any ) -> Any:
      '''Expand a backquote List expression.

      Note: This function is oddly dependent upon COMMA and COMMA-AT.
      '''
      if isinstance( expr, LList ):
         if len(expr) == 0:
            return LList( )

         primary = expr[0]
         if ( (primary == 'COMMA') or (primary == 'COMMA-AT') ):
            result = LispInterpreter._lEval(env, expr)
            return result

         resultList: list[Any] = [ ]
         for listElt in expr:
            resultListElt = LispInterpreter._backquote_expand( env, listElt )
            if ( isinstance( resultListElt, LList ) and
                 (len(resultListElt) > 0) and
                 (resultListElt[0] == LSymbol('COMMA-AT')) ):
               for elt in resultListElt.rest().first():
                  resultList.append( elt )
            else:
               resultList.append( resultListElt )
         return LList( *resultList )
      else:
         return expr

   @staticmethod
   def _constructPrimitives( parseLispString: Callable[[str], Any] ) -> dict[str, Any]:
      primitiveDict: dict[str, Any] = { }
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
      @LDefPrimitive( 'defmacro', '<symbol> (<param1> <param2> ...) <expr1> <expr2> ...', specialOperation=True )
      def LP_defmacro( env: Environment, *args, **kwargs ) -> Any:
         try:
            fnName, funcParams, *funcBody = args
         except:
            raise LispRuntimeFuncError( LP_defmacro, "3 or more arguments expected." )

         if not isinstance( fnName, LSymbol ):
            raise LispRuntimeFuncError( LP_defmacro, "Argument 1 expected to be a symbol." )

         if not isinstance( funcParams, LList ):
            raise LispRuntimeFuncError( LP_defmacro, "Argument 2 expected to be a list of params." )

         if len(funcBody) < 1:
            raise LispRuntimeFuncError( LP_defmacro, "At least one body expression expected." )

         theFunc = LMacro( fnName, funcParams, LList(*funcBody) )
         env.setGlobal( str(fnName), theFunc )
         return theFunc

      @LDefPrimitive( 'macroexpand', '\'(macroName <arg1> <arg2> ...)' )
      def LP_macroexpand( env:  Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_macroexpand, 'Exactly 1 argument expected.' )
         theMacroCall = args[0]

         if not isinstance( theMacroCall, LList ):
            raise LispRuntimeFuncError( LP_macroexpand, 'Argument 1 expected to be a list.' )

         # Break the list contents into a function and a list of args
         try:
            primary, *exprArgs = theMacroCall
         except ValueError:
            raise LispRuntimeError( 'Macro call must be at least two elements in length.' )

         if not isinstance( primary, LSymbol ):
            raise LispRuntimeError( f'Badly formed list expression.  The first element should be a symbol or function.' )

         # fn is an LPrimitive, LFunction or a macro name symbol
         # Use this information to get the function definition
         macroDef = LispInterpreter._lEval( env, primary )
         if not isinstance( macroDef, LMacro ):
            raise LispRuntimeError( 'Badly formed list expression.  The first element should evaluate to a macro.' )

         listOfExpandedMacroBodyExprs = LispInterpreter._lEval( env, macroDef, *exprArgs )
         return LList( *listOfExpandedMacroBodyExprs )

      @LDefPrimitive( 'setf', '<symbol> <sexpr>', specialOperation=True )
      def LP_setf( env: Environment, *args, **kwargs ) -> Any:
         try:
            key,val = args
         except ValueError:
            raise LispRuntimeFuncError( LP_setf, '2 arguments expected.' )

         # Evaluate the value expression
         val = LispInterpreter._lEval(env, val)
         if isinstance( val, (LFunction,LMacro) ):
            val.setName( key )
         key = str(key)

         try:
            # If key exists somewhere in the symbol table hierarchy, set its
            # value to val.  If it doesn't exist, define it in the global
            # symbol table and set its value to val.
            theSymTab = env.findDef( str(key) )
            if theSymTab:
               theSymTab.setLocal( key, val )
            else:
               env.setGlobal( key, val )
            return val
         except:
            raise LispRuntimeFuncError( LP_setf, 'Unknown error.' )

      @LDefPrimitive( 'undef!', '\'<symbol>' )
      def LP_undef( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_undef, '1 argument exptected.' )
         key = args[0]

         try:
            theSymTab =  env.findDef( str(key) )
            if theSymTab:
               theSymTab.undef( str(key) )
         except:
            raise LispRuntimeFuncError( LP_undef, 'Unknown error.' )
         return L_NIL

      @LDefPrimitive( 'symtab!', '')
      def LP_symtab( env: Environment, *args, **kwargs ) -> Any:
         if len(args) > 0:
            raise LispRuntimeFuncError( LP_symtab, '0 arguments expected.' )

         print( 'Symbol Table Dump:  Inner-Most Scope First')
         print( '------------------------------------------')
         scope: (Environment | None) = env
         try:
            while scope:
               symList = scope.localSymbols()
               print( symList )
               scope = scope.parentEnv( )
         except Exception:
            pass

         return L_NIL

      # ==================
      # Control Structures
      # ------------------
      @LDefPrimitive( 'lambda', '(<param1> <param2> ... ) <expr1> <expr2> ...', specialOperation=True )
      def LP_lambda( env: Environment, *args, **kwargs ) -> Any:
         try:
            funcParams, *funcBody = args
         except ValueError:
            raise LispRuntimeFuncError( LP_lambda, '2 arguments expected.' )
         if len(funcBody) < 1:
            raise LispRuntimeFuncError( LP_lambda, '2 arguments expected.' )

         return LFunction( LSymbol(""), funcParams, LList(*funcBody) )

      @LDefPrimitive( 'let', '( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <expr1> <expr2> ...)', specialOperation=True )
      def LP_let( env: Environment, *args, **kwargs ) -> Any:
         try:
            vardefs, *body = args
         except ValueError:
            raise LispRuntimeFuncError( LP_let, '2 or more arguments expected.' )

         if not isinstance( vardefs,  LList ):
            raise LispRuntimeFuncError( LP_let, 'The first argument to let expected to be a list of variable initializations.' )

         if len(body) < 1:
            raise LispRuntimeFuncError( LP_let, 'At least one body expression expected.' )

         # Evaluate the var def initial value exprs in the outer scope.
         varSyms = [ varPair[0] for varPair in vardefs ]   # collect the var names in one list
         varExprs = [ varPair[1] for varPair in vardefs ]  # collect the initial value expressions in another list.
         evaluatedExprs = [ LispInterpreter._lEval(env, expr) for expr in varExprs ]

         # Open the new scope
         env = Environment( env )     # Open a new scope. Auto closes when env goes out of scope.

         # define the local variable symbols and values in the new env/scope.
         for varSym, initialVar in zip( varSyms, evaluatedExprs ):
            if not isinstance( varSym, LSymbol ):
               raise LispRuntimeFuncError( LP_let, 'Variable initialization list expected a symbol.' )
            env.setLocal( str(varSym), initialVar )

         # Evaluate each body sexpr in the new env/scope
         lastResult = L_NIL
         for sexpr in body:
            lastResult = LispInterpreter._lEval( env, sexpr )
         return lastResult

      @LDefPrimitive( 'let*', '<expr1> <expr2> ...)', specialOperation=True )
      def LP_letstar( env: Environment, *args, **kwargs ) -> Any:
         try:
            vardefs, *body = args
         except ValueError:
            raise LispRuntimeFuncError( LP_letstar, '2 or more arguments expected.' )

         if not isinstance( vardefs,  LList ):
            raise LispRuntimeFuncError( LP_letstar, 'The first argument to let expected to be a list of variable initializations.' )

         if len(body) < 1:
            raise LispRuntimeFuncError( LP_letstar, 'At least one body expression expected.' )

         # Open the new scope
         env = Environment( env )    #  Open a new scope. Auto closes when env goes out of scope.

         # Evaluate and bind the ver defs in order
         for varSym, varExpr in vardefs:
            if not isinstance( varSym,  LSymbol ):
               raise LispRuntimeFuncError( LP_letstar, 'Variable initialization list expected a symbol.' )
            evaluatedExpr = LispInterpreter._lEval( env, varExpr )
            env.setLocal( str(varSym),  evaluatedExpr )

         # Evaluate each body sexpr in the new env/scope.
         lastResult = L_NIL
         for sexpr in body:
            lastResult = LispInterpreter._lEval( env, sexpr )
         return lastResult

      @LDefPrimitive( 'progn', '<sexpr1> <sexpr2> ...', specialOperation=True )
      def LP_progn( env: Environment, *args, **kwargs ) -> Any:
         if len(args) < 1:
            raise LispRuntimeFuncError( LP_progn, '1 or more body expressions expected.' )

         lastResult = L_NIL
         for expr in args:
            lastResult = LispInterpreter._lEval( env, expr )
         return lastResult

      @LDefPrimitive( 'if', '<cond> <conseq> [<alt>]', specialOperation=True )
      def LP_if( env: Environment, *args, **kwargs ) -> Any:
         numArgs = len(args)
         if not(2 <= numArgs <= 3):
            raise LispRuntimeFuncError( LP_if, '2 or 3 arguments expected.' )
         condExpr,*rest = args

         condValue = LispInterpreter._lEval( env, condExpr )
         if LispInterpreter._lTrue(condValue):
            return LispInterpreter._lEval( env, rest[0])    # The THEN part
         elif numArgs == 3:
            return LispInterpreter._lEval( env, rest[1])    # The ELSE part
         else:
            return L_NIL

      @LDefPrimitive( 'cond', '(<cond1> <body1>) (<cond2> <body2>)', specialOperation=True )
      def LP_cond( env: Environment, *args, **kwargs ) -> Any:
         if len(args) < 1:
            raise LispRuntimeFuncError( LP_cond, '1 or more argument exptected.' )
         caseList = args

         for caseNum,case in enumerate(caseList):
            try:
               testExpr, *body = case
            except ValueError:
               raise LispRuntimeFuncError( LP_cond, f"Entry {caseNum+1} does not contain a (<cond:expr> <body:expr>) pair." )

            if len(body) < 1:
               raise LispRuntimeFuncError( LP_cond, f'Entry {caseNum+1} expects at least one body expression.' )

            if LispInterpreter._lTrue(LispInterpreter._lEval(env,testExpr)):
               latestResult = None
               for sexpr in body:
                  latestResult = LispInterpreter._lEval( env, sexpr )
               return latestResult

         return LList( )

      @LDefPrimitive( 'case', '<expr> (<val1> <body1>) (<val2> <body2>) ...)', specialOperation=True )
      def LP_case( env: Environment, *args, **kwargs ) -> Any:
         try:
            expr, *caseList = args
         except ValueError:
            raise LispRuntimeFuncError( LP_case, '2 or more arguments exptected.' )
         exprVal = LispInterpreter._lEval( env, expr )

         if len(caseList) < 1:
            raise LispRuntimeFuncError( LP_case, 'At least once case expected.' )

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

      @LDefPrimitive( 'quote', '<expr>', specialOperation=True )
      def LP_quote( env: Environment, *args, **kwargs ) -> Any:
         if (len(args) != 1):
            raise LispRuntimeFuncError( LP_quote, '1 argument exptected.' )
         return args[0]

      @LDefPrimitive( 'backquote', '<expr>', specialOperation=True )
      def LP_backquote( env: Environment, *args, **kwargs ) -> Any:
         nonlocal INSIDE_BACKQUOTE
         if INSIDE_BACKQUOTE:
            raise LispRuntimeFuncError( LP_backquote, 'Cannot nest backquotes.')

         if (len(args) != 1):
            raise LispRuntimeFuncError( LP_backquote, '1 argument exptected.' )
         sExpr = args[0]

         try:
            INSIDE_BACKQUOTE = True
            expandedForm = LispInterpreter._backquote_expand( env, sExpr )
         finally:
            INSIDE_BACKQUOTE = False

         return expandedForm

      @LDefPrimitive( 'comma', '<expr>', specialOperation=True )
      def LP_comma( env: Environment, *args, **kwargs ) -> Any:
         nonlocal INSIDE_BACKQUOTE
         if not INSIDE_BACKQUOTE:
            raise LispRuntimeFuncError( LP_comma, 'COMMA can only occur inside a BACKQUOTE.')

         if (len(args) != 1):
            raise LispRuntimeFuncError( LP_comma, '1 argument exptected.' )
         subordinateExpr = args[0]
         result = LispInterpreter._lEval( env, subordinateExpr )
         return result

      @LDefPrimitive( 'comma-at', '<expr>', specialOperation=True )
      def LP_comma_at( env: Environment, *args, **kwargs ) -> Any:
         nonlocal INSIDE_BACKQUOTE
         if not INSIDE_BACKQUOTE:
            raise LispRuntimeFuncError( LP_comma_at, 'COMMA-AT can only occur inside a BACKQUOTE.')

         if (len(args) != 1):
            raise LispRuntimeFuncError( LP_comma_at, '1 argument exptected.' )
         subordinateExpr = args[0]
         result = LispInterpreter._lEval( env, subordinateExpr )
         if not isinstance(result, LList):
            raise LispRuntimeFuncError( LP_comma_at, 'Argument 1 must evaluate to a List.' )
         retValue = LList( LSymbol('COMMA-AT'), result )
         return retValue

      @LDefPrimitive( 'gensym', '' )
      def LP_gensym( env: Environment, *args, **kwargs ) -> Any:
         if len(args) > 1:
            raise LispRuntimeFuncError( LP_gensym, '0 arguments expected.' )

         try:
            prefix = args[0]
         except IndexError:
            prefix = 'G'

         symstr = f'{prefix}{LispInterpreter.counter}'
         LispInterpreter.counter += 1
         sym = LSymbol( symstr )
         return sym

      @LDefPrimitive( 'while', '<conditionExpr> <body>', specialOperation=True )
      def LP_while( env: Environment, *args, **kwargs ) -> Any:
         try:
            conditionExpr, *body = args
         except ValueError:
            raise LispRuntimeFuncError( LP_while, '2 arguments expected.' )

         if len(body) < 1:
            raise LispRuntimeFuncError( LP_while, 'At least one sexpr expected for the body.' )

         latestResult = None
         condResult = LispInterpreter._lEval(env, conditionExpr)
         while LispInterpreter._lTrue( condResult ):
            for expr in body:
               latestResult = LispInterpreter._lEval( env, expr )
            condResult = LispInterpreter._lEval(env, conditionExpr )
         return latestResult

      @LDefPrimitive( 'doTimes', '(<variable> <integer>) <sexpr1> <sexpr2> ...', specialOperation=True )
      def LP_dotimes( env: Environment, *args, **kwargs ) -> Any:
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

         if len(body) < 1:
            raise LispRuntimeFuncError( LP_dotimes, 'At least one sexpr expected for the loop body.' )

         latestResult = None
         for iterCount in range(count):
            env.setLocal( str(variable), iterCount )
            for sexpr in body:
               latestResult = LispInterpreter._lEval( env, sexpr )
         return latestResult

      @LDefPrimitive( 'foreach', '<variable> <list> <expr1> <expr2> ...', specialOperation=True )
      def LP_foreach( env: Environment, *args, **kwargs ) -> Any:
         try:
            varSymbol, anExpr, *body = args
         except:
            raise LispRuntimeFuncError( LP_foreach, "3 or more arguments expected." )

         if not isinstance( varSymbol, LSymbol ):
            raise LispRuntimeFuncError( LP_foreach, "Argument 1 expected to be a symbol." )

         if len(body) < 1:
            raise LispRuntimeFuncError( LP_foreach, 'At least one sexpr expected for the loop body.' )

         alist = LispInterpreter._lEval( env, anExpr )
         if not isinstance( alist, LList ):
            raise LispRuntimeFuncError( LP_foreach, "Argument 2 expected to evaluate to a list." )

         # Evaluate the body while there are elements left in the list
         latestResult = L_NIL
         for element in alist:
            env.setLocal( str(varSymbol), element )
            for sexpr in body:
               latestResult = LispInterpreter._lEval( env,  sexpr )
         return latestResult

      @LDefPrimitive( 'funcall', '<fnNameSymbol> <arg1> <arg2> ...' )
      def LP_funcall( env: Environment, *args, **kwargs ) -> Any:
         try:
            fnNameSymbol, *fnArgs = args
         except:
            raise LispRuntimeFuncError( LP_funcall, "1 or more arguments expected" )

         newExpr = LList( fnNameSymbol, *fnArgs )
         result = LispInterpreter._lEval( env, newExpr )
         return result

      @LDefPrimitive( 'eval', '<expr>' )
      def LP_eval( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_eval, '1 argument exptected.' )

         return LispInterpreter._lEval( env, args[0] )

      @LDefPrimitive( 'parse', '<sExpressionString>')
      def LP_parse( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_parse, '1 string argument expected.' )
         theExprStr = args[0]
         theExprAST = parseLispString( theExprStr )
         return theExprAST

      @LDefPrimitive( 'pprint', '<sExpr>' )
      def LP_pprint( env: Environment, *args, **kwargs) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_pprint, '1 lisp object argument expected.' )
         theExpr = args[0]
         theExprStr = prettyPrintSExpr( theExpr )
         return theExprStr

      # =======================
      # List & Map Manipulation
      # -----------------------
      @LDefPrimitive( 'list', '<expr1> <expr2> ...')
      def LP_list( env: Environment, *args, **kwargs ) -> Any:
         if len(args) < 1:
            raise LispRuntimeFuncError( LP_list, '1 or more arguments expected.' )
         return LList( *args[:] )

      @LDefPrimitive( 'map', '(<key1> <val1>) (<key2> <val>2) ...', specialOperation=True )
      def LP_map( env: Environment, *args, **kwargs ) -> Any:
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

      @LDefPrimitive( 'car', '<list>' )
      def LP_car( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_car, '1 argument expected.' )
         theList = args[0]

         if not isinstance(theList, LList):
            raise LispRuntimeFuncError( LP_car, '1st argument expected to be a list.' )

         try:
            return theList.first()
         except IndexError:
            return LList( )

      @LDefPrimitive( 'cdr', '<list>' )
      def LP_cdr( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_cdr, '1 argument expected.' )
         theList = args[0]

         if not isinstance(theList, LList):
            raise LispRuntimeFuncError( LP_cdr, '1st argument expected to be a list.' )

         try:
            return theList.rest()
         except IndexError:
            return LList( )

      @LDefPrimitive( 'cons', '\'<obj> \'<list>' )
      def LP_cons( env: Environment, *args, **kwargs ) -> Any:
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

      @LDefPrimitive( 'push!', '\'<list> \'<value>' )
      def LP_push( env: Environment, *args, **kwargs ) -> Any:
         try:
            alist, value = args
         except ValueError:
            raise LispRuntimeFuncError( LP_push, '2 arguments exptected.' )

         try:
            if isinstance(alist, LList):
               alist.append( value )
            else:
               alist = L_NIL
         except:
            raise LispRuntimeFuncError( LP_push, 'Invalid argument.' )
         return alist

      @LDefPrimitive( 'pop!', '\'<list>' )
      def LP_pop( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_pop, '1 argument expected.' )
         alist = args[0]

         try:
            value = alist.pop()
         except:
            raise LispRuntimeFuncError( LP_pop, 'Invalid argument.' )
         return value

      @LDefPrimitive( 'at', '\'<listMapOrStr> \'<keyOrIndex>' )
      def LP_at( env: Environment, *args, **kwargs ) -> Any:
         try:
            keyed,key = args
         except:
            raise LispRuntimeFuncError( LP_at, '2 arguments expected.' )

         if isinstance(keyed, (LList, str) ):
            keyed = keyed
         elif isinstance(keyed, LMap):
            keyed = keyed.dict
         else:
            raise LispRuntimeFuncError( LP_at, 'Invalid argument.  List or Map expected.' )

         if isinstance(key, LSymbol):
            key = key.strval

         try:
            value = keyed[ key ]
         except:
            raise LispRuntimeFuncError( LP_at, 'Invalid argument key/index.' )

         return value

      @LDefPrimitive( 'atSet!', '<listOrMap> <keyOrIndex> <value>' )
      def LP_atSet( env: Environment, *args, **kwargs ) -> Any:
         try:
            keyed,key,value = args
         except:
            raise LispRuntimeFuncError( LP_atSet, '3 arguments expected.' )

         if isinstance(keyed, LList):
            keyed = keyed
         elif isinstance(keyed, LMap):
            keyed = keyed.dict
         else:
            raise LispRuntimeFuncError( LP_atSet, 'Invalid argument.  List or map expeced as first argument.' )

         if isinstance(key, LSymbol):
            key = key.strval

         try:
            keyed[ key ] = value
         except:
            raise LispRuntimeFuncError( LP_atSet, 'Invalid argument key/index.' )

         return value


      @LDefPrimitive( 'append', '\'<list1> \'<list2>' )
      def LP_append( env: Environment, *args, **kwargs ) -> Any:
         if len(args) < 2:
            raise LispRuntimeFuncError( LP_append, 'At least 2 arguments expected.' )

         resultList = LList( )
         for lst in args:
            if not isinstance( lst,  LList ):
               raise LispRuntimeFuncError( LP_append, 'Invalid argument.' )
            for item in lst:
               resultList.append( item )
         return resultList

      @LDefPrimitive( 'hasValue?', '\'<listOrMap> \'<value>' )
      def LP_hasValue( env: Environment, *args, **kwargs ) -> Any:
         try:
            keyed,aVal = args
         except:
            raise LispRuntimeFuncError( LP_hasValue, '2 arguments expected.' )

         if isinstance(keyed, LList):
            keyed = keyed
         elif isinstance(keyed, LMap):
            keyed = keyed.dict.values()
         else:
            raise LispRuntimeFuncError( LP_hasValue, 'Invalid argument.  Argument 1 expected to be a list or map.')

         try:
            return L_T if aVal in keyed else L_NIL    # T or NIL
         except:
            raise LispRuntimeFuncError( LP_hasValue, 'Invalid argument.')

      @LDefPrimitive( 'update!', '<map1> <map2>' )
      def LP_update( env: Environment, *args, **kwargs ) -> Any:
         try:
            map1,map2 = args
         except:
            raise LispRuntimeFuncError( LP_update, '2 arguments exptected.' )

         if not isinstance( map1, LMap ):
            raise LispRuntimeFuncError( LP_update, 'Argument 1 expected to be a map.' )

         if not isinstance( map2, LMap ):
            raise LispRuntimeFuncError( LP_update, 'Argument 2 expected to be a map.' )

         try:
            map1.dict.update( map2.dict )
            return map1
         except:
            raise LispRuntimeFuncError( LP_update, 'Invalid argument.  Both arguments must be maps.' )

      @LDefPrimitive( 'hasKey?', '<map> <key>' )
      def LP_hasKey( env: Environment, *args, **kwargs ) -> Any:
         try:
            aMap,aKey = args
         except:
            raise LispRuntimeFuncError( LP_hasKey, '2 arguments expected.' )

         if isinstance(aMap, LMap):
            aMap = aMap.dict
         else:
            raise LispRuntimeFuncError( LP_hasKey, 'Invalid argument 1.  Map expected.')

         if isinstance(aKey, LSymbol):
            aKey = aKey.strval

         try:
            return L_T if aKey in aMap else L_NIL   # T or NIL
         except:
            raise LispRuntimeFuncError( LP_hasKey, 'Invalid argument.' )

      # =====================
      # Arithmetic Operations
      # ---------------------
      @LDefPrimitive( '+', '<expr1> <expr2> ...')
      def LP_add( env: Environment, *args, **kwargs ) -> Any:
         if len(args) < 1:
            raise LispRuntimeFuncError( LP_add, '1 or more arguments expected.' )

         try:
            return sum(args)
         except:
            raise LispRuntimeFuncError( LP_add, 'Invalid argument.' )

      @LDefPrimitive( '-', '<expr1> <expr2> ...')
      def LP_sub( env: Environment, *args, **kwargs ) -> Any:
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

      @LDefPrimitive( '*', '<expr1> <expr2> ...' )
      def LP_mul( env: Environment, *args, **kwargs ) -> Any:
         if len(args) < 2:
            raise LispRuntimeFuncError( LP_mul, '2 or more arguments exptected.' )

         try:
            return functools.reduce( lambda x,y: x * y, iter(args) )
         except:
            raise LispRuntimeFuncError( LP_mul, 'Invalid argument.' )

      @LDefPrimitive( '/', '<expr1> <expr2> ...' )
      def LP_div( env: Environment, *args, **kwargs ) -> Any:
         if len(args) < 2:
            raise LispRuntimeFuncError( LP_div, '2 or more arguments exptected.' )

         try:
            return functools.reduce( lambda x,y: x / y, iter(args) )
         except:
            raise LispRuntimeFuncError( LP_div, 'Invalid argument.' )

      @LDefPrimitive( '//', '<expr1> <expr>')
      def LP_intdiv( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 2:
            raise LispRuntimeFuncError( LP_intdiv, '2 arguments expected.' )

         try:
            return args[0] // args[1]
         except:
            raise LispRuntimeFuncError( LP_intdiv, 'Invalid argument.' )

      @LDefPrimitive( 'mod', '<expr1> <expr>')
      def LP_moddiv( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 2:
            raise LispRuntimeFuncError( LP_moddiv, '2 arguments expected.' )

         try:
            return args[0] % args[1]
         except:
            raise LispRuntimeFuncError( LP_moddiv, 'Invalid argument.' )

      @LDefPrimitive( 'gcd', '<integer1> <integer2> ...' )
      def LP_gcd( env: Environment,  *args,  **kwargs ) -> Any:
         numArgs = len(args)
         if numArgs == 0:
            return 0
         elif numArgs == 1:
            return abs(args[0])
         else:
            try:
               return math.gcd( *args )
            except:
               raise LispRuntimeFuncError( LP_gcd, 'Invalid argument.' )

      @LDefPrimitive( 'lcm', '<integer1> <integer2> ...' )
      def LP_lcm( env: Environment,  *args,  **kwargs ) -> Any:
         numArgs = len(args)
         if numArgs == 0:
            raise LispRuntimeFuncError( LP_lcm, '1 or more integer arguments expected.' )
         else:
            try:
               return math.lcm( *args )
            except:
               raise LispRuntimeFuncError( LP_lcm, 'Invalid argument.' )

      @LDefPrimitive( 'log', '<expr> [ <base> ]')
      def LP_log( env: Environment, *args, **kwargs ) -> Any:
         numArgs = len(args)
         if not( 1 <= numArgs <= 2 ):
            raise LispRuntimeFuncError( LP_log, '1 or 2 arguments exptected.' )

         try:
            num,*rest = args
            base = math.e if len(rest) == 0 else rest[0]
            return math.log(num,base)
         except:
            raise LispRuntimeFuncError( LP_log, 'Invalid argument.' )

      @LDefPrimitive( 'expt', '<base> <power>')
      def LP_expt( env: Environment, *args, **keys ) -> Any:
         if len(args) != 2:
            raise LispRuntimeFuncError( LP_expt, '2 arguments expected.' )

         try:
            base,power = args
            return base ** power
         except:
            raise LispRuntimeFuncError( LP_expt, 'Invalid argument.' )

      @LDefPrimitive( 'sin', '<radians>')
      def LP_sin( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_sin, '1 argument expected.' )

         try:
            return math.sin(args[0])
         except:
            raise LispRuntimeFuncError( LP_sin, 'Invalid argument.' )

      @LDefPrimitive( 'cos', '<radians>')
      def LP_cos( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_cos, '1 argument expected.' )

         try:
            return math.cos(args[0])
         except:
            raise LispRuntimeFuncError( LP_cos, 'Invalid argument.' )

      @LDefPrimitive( 'asin', '<number>' )
      def LP_asin( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_asin, '1 argument expcted.' )

         try:
            return math.asin(args[0])
         except:
            raise LispRuntimeFuncError( LP_asin, 'Invalid argument.' )

      @LDefPrimitive( 'acos', '<number>' )
      def LP_acos( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_acos, '1 argument expcted.' )

         try:
            return math.asin(args[0])
         except:
            raise LispRuntimeFuncError( LP_acos, 'Invalid argument.' )

      @LDefPrimitive( 'atan', '<number1> &optional <number2>' )
      def LP_atan( env: Environment, *args, **kwargs ) -> Any:
         if len(args) == 1:
            xval = (args[0])
            yval = 1
         elif len(args) == 2:
            xval = args[0]
            yval = args[1]
         else:
            raise LispRuntimeFuncError( LP_atan, '1 or two arguments expcted.' )

         try:
            return math.atan( xval / yval )
         except:
            raise LispRuntimeFuncError( LP_atan, 'Invalid argument.' )

      @LDefPrimitive( 'min', '<val1> <val2> ...')
      def LP_min( env: Environment, *args, **kwargs ) -> Any:
         if len(args) < 1:
            raise LispRuntimeFuncError( LP_min, '1 or more arguments exptected.' )

         try:
            return min( *args )
         except:
            raise LispRuntimeFuncError( LP_min, 'Invalid argument.' )

      @LDefPrimitive( 'max', '<val1> <val2> ...')
      def LP_max( env: Environment, *args, **kwargs ) -> Any:
         if len(args) < 1:
            raise LispRuntimeFuncError( LP_max, '1 or more arguments exptected.' )

         try:
            return max( *args )
         except:
            raise LispRuntimeFuncError( LP_max, 'Invalid argument.' )

      @LDefPrimitive( 'random', 'number' )
      def LP_random( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_random, 'Exactly 1 number argument exptected.' )

         num = args[0]
         if isinstance( num,  int ):
            return random.randint(0, num)
         elif isinstance( num,  float ):
            return random.uniform(0.0, num)

      # ==========
      # Predicates
      # ----------
      @LDefPrimitive( 'numberp', '<expr>')
      def LP_numberp( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_numberp, '1 argument expected.' )
         return L_T if isinstance( args[0], L_NUMBER ) else L_NIL

      @LDefPrimitive( 'integerp', '<expr>' )
      def LP_integerp( env: Environment,  *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_integerp, '1 argument expected.' )
         return L_T if isinstance( args[0], int ) else L_NIL

      @LDefPrimitive( 'rationalp', '<expr>' )
      def LP_rationalp( env: Environment,  *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_rationalp, '1 argument expected.' )
         return L_T if isinstance( args[0], (int, fractions.Fraction) ) else L_NIL

      @LDefPrimitive( 'floatp', '<expr>' )
      def LP_floatp( env: Environment,  *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_floatp, '1 argument expected.' )
         return L_T if isinstance( args[0], (float) ) else L_NIL

      @LDefPrimitive( 'symbolp', '<expr>')
      def LP_symbolp( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_symbolp, '1 argument expected.' )
         return L_T if isinstance( args[0], LSymbol ) else L_NIL

      @LDefPrimitive( 'atom', '<expr>')
      def LP_atom( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_atom, '1 argument expected.' )

         arg = args[0]
         if isinstance(arg, LList):
            return L_T if len(arg) == 0 else L_NIL         # NIL or () is an atom even through it's a list.
         else:
            return L_T

      @LDefPrimitive( 'listp', '<expr>')
      def LP_listp( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_listp, '1 argument expected.' )
         return L_T if isinstance( args[0], LList ) else L_NIL

      @LDefPrimitive( 'isMap?', '<expr>')
      def LP_isMap( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_isMap, '1 argument expected.' )
         return L_T if isinstance( args[0], LMap ) else L_NIL

      @LDefPrimitive( 'stringp', '<expr>')
      def LP_stringp( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_stringp, '1 argument expected.' )
         return L_T if isinstance( args[0], str ) else L_NIL

      @LDefPrimitive( 'functionp', '<expr>')
      def LP_functionp( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_functionp, '1 argument expected.' )
         return L_T if isinstance( args[0], (LPrimitive,LFunction) ) else L_NIL

      # ====================
      # Relational Operators
      # --------------------
      @LDefPrimitive( 'is?', '<expr1> <expr2>')
      def LP_is( env: Environment, *args, **kwargs ) -> Any:
         try:
            arg1,arg2 = args
         except:
            raise LispRuntimeFuncError( LP_is, '2 arguments exptected.' )

         if isinstance(arg1, (int,float,str)):
            return L_T if (arg1 == arg2) else L_NIL
         else:
            return L_T if (arg1 is arg2) else L_NIL

      @LDefPrimitive( '=', '<expr1> <expr2> ...')
      def LP_equal( env: Environment, *args, **kwargs ) -> Any:
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

      @LDefPrimitive( '/=', '<expr1> <expr2> ...')
      def LP_notEqual( env: Environment, *args, **kwargs ) -> Any:
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

      @LDefPrimitive( '<', '<expr1> <expr2> ...')
      def LP_less( env: Environment, *args, **kwargs ) -> Any:
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

      @LDefPrimitive( '<=', '<expr1> <expr2> ...' )
      def LP_lessOrEqual( env: Environment, *args, **kwargs ) -> Any:
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

      @LDefPrimitive( '>', '<expr1> <expr2> ...' )
      def LP_greater( env: Environment, *args, **kwarg ) -> Any:
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

      @LDefPrimitive( '>=', '<expr1> <expr2> ...' )
      def LP_greaterOrEqual( env: Environment, *args, **kwargs ) -> Any:
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
      @LDefPrimitive( 'not', '<expr>')
      def LP_not( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_not, '1 argument exptected.' )

         arg1 = args[0]
         return L_T if (isinstance(arg1,LList) and (len(arg1)==0)) else L_NIL

      @LDefPrimitive( 'and', '<expr1> <expr2> ...' )
      def LP_and( env: Environment, *args, **kwargs ) -> Any:
         if len(args) < 2:
            raise LispRuntimeFuncError( LP_and, '2 or more arguments exptected.' )

         for arg in args:
            if (arg == 0) or (arg is L_NIL) or (arg is None):
               return L_NIL

         return L_T

      @LDefPrimitive( 'or', '<expr1> <expr2> ...' )
      def LP_or( env: Environment, *args, **kwargs ) -> Any:
         if len(args) < 2:
            raise LispRuntimeFuncError( LP_or, '2 or more arguments exptected.' )

         for arg in args:
            if (arg != 0) and (arg is not L_NIL) and (arg is not None):
               return L_T

         return L_NIL

      # ===============
      # Type Conversion
      # ---------------
      @LDefPrimitive( 'float', '<sexpr>')
      def LP_float( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_float, 'Exactly 1 argument expected.' )

         try:
            return float(args[0])
         except:
            raise LispRuntimeFuncError( LP_float, 'Invalid argument.' )

      @LDefPrimitive( 'integer', '<expr> [<base>]')
      def LP_integer( env: Environment, *args, **kwargs ) -> Any:
         numArgs = len(args)
         if (numArgs < 1) or (numArgs > 2):
            raise LispRuntimeFuncError( LP_integer, '1 or two arguments expected.' )

         try:
            return int(*args)
         except:
            raise LispRuntimeFuncError( LP_integer, 'Invalid argument.' )

      @LDefPrimitive( 'rational', '<expr>')
      def LP_rational( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_rational, 'Exactly 1 argument expected.' )

         try:
            return fractions.Fraction(args[0])
         except:
            raise LispRuntimeFuncError( LP_rational, 'Invalid argument.' )

      @LDefPrimitive( 'string', '<expr1> <expr2> ...' )
      def LP_string( env: Environment, *args, **kwargs ) -> Any:
         if len(args) == 0:
            raise LispRuntimeFuncError( LP_string, '1 or more arguments exptected.' )

         resultStrs = [ prettyPrintSExpr(sExpr) for sExpr in args ]
         return ''.join(resultStrs)

      @LDefPrimitive( 'symbol',  '<string1> <string2> ...' )
      def LP_symbol( env: Environment, *args, **kwargs ) -> Any:
         if len(args) <= 1:
            raise LispRuntimeFuncError( LP_symbol, '1 or more string argument expected.' )

         strList = [ str(arg) for arg in args ]
         symstr = ''.join(strList)
         return LSymbol(symstr)

      # ===============
      # I/O
      # ---------------
      @LDefPrimitive( 'write!', '<object>')
      def LP_write( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_write, '1 argument expected' )
         return lwrite( args[0], end='' )

      @LDefPrimitive( 'writeLn!', '<object>')
      def LP_writeln( env: Environment, *args, **kwargs ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_writeln, '1 argument expected' )
         return lwrite( args[0], end='\n' )

      def lwrite( value, end='' ):
         valueStr = prettyPrintSExpr( value )
         valueStr = bytes( valueStr, "utf-8" ).decode( "unicode_escape" ) # decode escape sequences
         print( valueStr, sep='', end='\n', file=LispInterpreter.outStrm )
         return value

      @LDefPrimitive( 'readLn!', '')
      def LP_readln( env: Environment, *args, **kwargs ) -> Any:
         numArgs = len(args)
         if numArgs > 0:
            raise LispRuntimeFuncError( LP_readln, '0 arguments expected.' )
         return input()

      return primitiveDict

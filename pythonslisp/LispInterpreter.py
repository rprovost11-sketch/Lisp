import functools
import math
import random
import time
import sys
from fractions import Fraction
from typing import Callable, Any

from pythonslisp.LispParser import LispParser
from pythonslisp.Listener import Interpreter, retrieveFileList
from pythonslisp.Environment import Environment
from pythonslisp.LispAST import ( LSymbol, LList, LMap, LNUMBER,
                                  LCallable, LFunction, LPrimitive, LMacro,
                                  prettyPrintSExpr, prettyPrint )

class LispExpander( object ):
   @staticmethod
   def expand( env: Environment, sExpr: Any ) -> Any:
      if not isinstance( sExpr, LList ):
         return sExpr
      
      if len(sExpr) == 0:
         return LList( )
      
      primary, *args = sExpr
      if isinstance( primary, LSymbol ):
         if primary == 'DEFMACRO':
            return sExpr
         
         try:
            fnDef = env.getValue( primary.strval )
         except KeyError:
            fnDef = None
         
         if isinstance(fnDef, LMacro):
            return LispExpander._expandMacro( env, fnDef, *args )
         elif isinstance(fnDef, LPrimitive) and (fnDef.name == 'BACKQUOTE'):
            return fnDef.pythonFn( env, *args )             # Invokes LP_backquote
      
      resultList = [ LispExpander.expand(env, subExpr) for subExpr in sExpr ]
      return LList( *resultList )
   
   @staticmethod
   def _expandMacro( env: Environment, macroDef: LMacro, *args ):
      env = Environment( env )
      LispInterpreter._lbindArguments( env, macroDef.params, list(args) )
      expandedBody = [ LispExpander.expand(env, bodySExpr) for bodySExpr in macroDef.body ]
      
      if len(expandedBody) == 0:
         return expandedBody
      elif len(expandedBody) == 1:
         return LispExpander.expand( env, expandedBody[0] )
      else:
         expandedBody.insert( 0, LSymbol('PROGN') )
         return expandedBody
   
class LispRuntimeError( Exception ):
   def __init__( self, *args ) -> None:
      super().__init__( self, *args )


class LispRuntimeFuncError( LispRuntimeError ):
   def __init__( self, lispCallable: LPrimitive, errorMsg: str ) -> None:
      fnName = lispCallable.name
      usage = lispCallable.usageStr
      errStr = f"ERROR '{fnName}': {errorMsg}\nUSAGE: {usage}" if usage else f"ERROR '{fnName}': {errorMsg}"
      super().__init__( errStr )


class LispInterpreter( Interpreter ):
   outStrm = None

   def __init__( self, runtimeLibraryDir: (str|None)=None ) -> None:
      self._libDir = runtimeLibraryDir
      self._parser: LispParser = LispParser( )

   def reboot( self ) -> None:
      # Load in the primitives
      primitiveDict: dict[str, Any] = LispInterpreter._lconstructPrimitives( self._parser.parse )
      self._env:Environment = Environment( parent=None, **primitiveDict )  # Create the GLOBAL environment

      # Load in the runtime library
      if self._libDir:
         filenameList = retrieveFileList( self._libDir )
         for filename in filenameList:
            self.evalFile( filename )

   def eval( self, source: str, outStrm=None ) -> str:
      returnVal = self.rawEval( source, outStrm=outStrm )
      return prettyPrintSExpr( returnVal ).strip()

   def eval_instrumented( self, source: str, outStrm=None ) -> str:
      returnVal,parseTime,execTime = self.rawEval_instrumented( source, outStrm=outStrm )
      return prettyPrintSExpr( returnVal ).strip(), parseTime, execTime

   def evalFile( self, filename: str, outStrm=None ) -> str:
      self.rawEvalFile( filename, outStrm=outStrm )

   def rawEval( self, source: str, outStrm=None ) -> Any:
      LispInterpreter.outStrm = outStrm
      try:
         ast = self._parser.parse( source )
         #ast = LispExpander.expand( self._env, ast )
         returnVal = LispInterpreter._lEval( self._env, ast )
      finally:
         LispInterpreter.outStrm = None
      return returnVal

   def rawEval_instrumented( self, source: str, outStrm=None ) -> Any:
      LispInterpreter.outStrm = outStrm
      try:
         parseStartTime = time.perf_counter()
         ast = self._parser.parse( source )
         parseTime = time.perf_counter() - parseStartTime
         
         #ast = LispExpander.expand( self._env, ast )
         
         startTime = time.perf_counter()
         returnVal = LispInterpreter._lEval( self._env, ast )
         evalTime = time.perf_counter() - startTime
      finally:
         LispInterpreter.outStrm = None
      return returnVal, parseTime, evalTime

   def rawEvalFile( self, filename: str, outStrm=None ) -> Any:
      LispInterpreter.outStrm = outStrm
      try:
         ast = self._parser.parseFile( filename )
         #ast = LispExpander.expand( self._env, ast )
         returnVal = LispInterpreter._lEval( self._env, ast )
      finally:
         LispInterpreter.outStrm = None
      return returnVal

   @staticmethod 
   def _lTrue( sExpr: Any ) -> bool:
      if isinstance(sExpr, list):
         return len(sExpr) != 0
      return True

   @staticmethod
   def _lEval( env: Environment, sExpr: Any ) -> Any:
      '''This is the interpreter's main evaluation function.  Pass it any
      s-expression in the form of a LispAST; eval will return the result.'''
      if isinstance( sExpr, LSymbol ):
         try:
            return env.getValue( sExpr.strval )
         except KeyError:
            if sExpr.isArgKey():
               return sExpr
            raise LispRuntimeError( f'Unbound Variable: {sExpr.strval}.' )
      elif not isinstance( sExpr, LList ):  # atom or map
         return sExpr
      
      # sExpr is a list expression - function call
      
      if len(sExpr) == 0:
         return LList( )  # An empty list always evaluates to an empty list

      # Break the list contents into a primary and a list of args
      primary, *argsToFn = sExpr

      # Primary ought to evaluate to a callable (LPrimitive, LFunction or LMacro)
      fnObj = LispInterpreter._lEval( env, primary )
      if not isinstance( fnObj, LCallable ):
         raise LispRuntimeError( f'Badly formed list expression \'{primary}\'.  The first element should evaluate to a callable.' )
      
      # Do the args need to be evaluated before calling the function?
      if not fnObj.specialForm:
         argsToFn = [ LispInterpreter._lEval(env, argExpr) for argExpr in argsToFn ]
      
      # Call the function and return the results
      return LispInterpreter._lApply( env, fnObj, *argsToFn )
   
   @staticmethod
   def _lApply( env: Environment, lcallable: LCallable, *args ) -> Any:
      if isinstance( lcallable, LPrimitive ):
         return lcallable.pythonFn( env, *args )
      elif isinstance( lcallable, LFunction ):
         #env = Environment( env )         # Open a new scope. Auto closes when env goes out of scope.
         env = Environment( lcallable.closure ) # Open a new scope on the function's closure env to support closures.

         # store the arguments as locals
         LispInterpreter._lbindArguments( env, lcallable.params, args ) #convert args from a tuple to a list

         # evaluate the body expressions.
         latestResult = LList()
         for sexpr in lcallable.body:
            latestResult = LispInterpreter._lEval( env, sexpr )
         return latestResult
      else:                     # LMacro
         listOfExpandedSExprs = LispInterpreter._macroexpand( env, lcallable, *args )

         # Evaluate the expanded macro
         latestResult = LList()
         for sexpr in listOfExpandedSExprs:
            latestResult = LispInterpreter._lEval( env, sexpr )
         return latestResult

   @staticmethod
   def _lbindArguments( env: Environment, paramList: list[Any], argList: tuple[Any] ) -> None:
      paramListLength = len(paramList)
      argListLength = len(argList)

      paramNum, argNum = LispInterpreter._lbindPositionalArgs( env, paramList, 0, argList, 0 )
      
      # Retrieve the next param which should be a symbol
      try:
         nextParam = paramList[paramNum]
      except IndexError:
         # There are no more params to process. So argNum should be == or > argListLength.
         # So, if argNum < argListLength, then there are still unprocessed args.
         if argNum < argListLength:
            raise LispRuntimeError( f'Too many arguments.  Received {argNum+1}.' )
         return          # All params used up.  Return gracefully
      if not isinstance(nextParam, LSymbol):
         raise LispRuntimeError( f"Param {paramNum} expected to be a symbol." )
      
      if nextParam == '&OPTIONAL':
         paramNum, argNum = LispInterpreter._lbindOptionalArgs( env, paramList, paramNum+1, argList, argNum )
      
         # Retrieve the next param which should be a symbol
         try:
            nextParam = paramList[paramNum]
         except IndexError:
            if argNum < argListLength:
               raise LispRuntimeError( f'Too many arguments.  Received {argNum+1}.' )
            return          # All params used up.  Return gracefully
         if not isinstance(nextParam, LSymbol):
            raise LispRuntimeError( f"Param {paramNum} expected to be a symbol." )
      
      if nextParam == '&REST':
         paramNum, argNum = LispInterpreter._lbindRestArgs( env, paramList, paramNum+1, argList, argNum )
      
         # Retrieve the next param which should be a symbol
         try:
            nextParam = paramList[paramNum]
         except IndexError:
            return          # All params used up.  Return gracefully
         if not isinstance(nextParam, LSymbol):
            raise LispRuntimeError( f"Param {paramNum} expected to be a symbol." )
      
      if nextParam == '&KEY':
         paramNum, argNum = LispInterpreter._lbindKeyArgs( env, paramList, paramNum+1, argList, argNum )
      
         # Retrieve the next param which should be a symbol
         try:
            nextParam = paramList[paramNum]
         except IndexError:
            return          # All params used up.  Return gracefully
         if not isinstance(nextParam, LSymbol):
            raise LispRuntimeError( f"Param {paramNum} expected to be a symbol." )
      
      if nextParam == '&AUX':
         paramNum, argNum = LispInterpreter._lbindAuxArgs( env, paramList, paramNum+1, argList, argNum )
   
      if paramNum < paramListLength:
         raise LispRuntimeError( 'Too few parameters.' )

   @staticmethod
   def _lbindPositionalArgs( env: Environment, paramList: list[Any], paramNum: int, argList: tuple[Any], argNum: int ) -> (int, int):
      paramListLength = len(paramList)

      while paramNum < paramListLength:
         # Get the next parameter name.  Insure it's a symbol but doesn't start with '&'.
         paramName = paramList[paramNum]
         if not isinstance(paramName, LSymbol):
            raise LispRuntimeError( f"Positional param {paramNum} expected to be a symbol." )
         if paramName.startswith('&'):
            break
         
         # Get the next argument value
         try:
            argVal = argList[argNum]
         except IndexError:
            raise LispRuntimeError( "Too few positional arguments." )
         
         # Bind paramName to argVal
         env.bindLocal( paramName.strval, argVal )
      
         # Prepare for the next iteration
         paramNum += 1
         argNum += 1
      
      return paramNum, argNum
   
   @staticmethod
   def _lbindOptionalArgs( env: Environment, paramList: list[Any], paramNum: int, argList: tuple[Any], argNum: int ) -> (int, int):
      '''Syntax:  &optional {var | (var [initform [svar]])}*'''
      paramListLength = len(paramList)
      argListLength = len(argList)
      
      # Prepare to loop over the optional parameters and arguments
      if paramNum >= paramListLength:
         raise LispRuntimeError( f'Param expected after &Optional.' )
      
      while (paramNum < paramListLength):
         paramSpec = paramList[paramNum]
         
         # Extract the next parameter's values
         if isinstance( paramSpec, LSymbol ):
            if paramSpec.startswith('&'):
               break
            varName = paramSpec
            initForm = LList( )
            svarName = None
         elif isinstance( paramSpec, LList ):
            paramSpecLen = len(paramSpec)
            if paramSpecLen == 1:
               varName = paramSpec[0]
               initForm = LList()
               svarName = None
            elif paramSpecLen == 2:
               varName, initForm = paramSpec
               svarName = None
            elif paramSpecLen == 3:
               varName, initForm, svarName = paramSpec
            else:
               raise LispRuntimeError( 'Parameter spec following &OPTIONAL must be a list of (<variable> [<defaultvalue> [<svar>]] ).' )

            if svarName and (not isinstance(svarName, LSymbol)):
               raise LispRuntimeFuncError( f'Parameter svar following {varName} must be a symbol.' )
         else:
            raise LispRuntimeError( 'Parameter spec following &OPTIONAL must be a <variable> or a list of (<variable> <defaultvalue>). ' )
         paramNum += 1
        
         # Extract the next arguments's values
         if argNum < argListLength:
            initForm = argList[argNum]
         
         if (argNum >= argListLength) or (isinstance(initForm, LSymbol) and (initForm.startswith(':'))):
            initForm = LispInterpreter._lEval( env, initForm )
            svarVal = LList()   # Nil, False
         else:
            argNum += 1
            svarVal = env.getGlobalValue('T')   # T, True

         # Bind the parameters
         env.bindLocal( varName.strval, initForm )
         
         if svarName:
            env.bindLocal( svarName.strval, svarVal )
      
      return paramNum, argNum

   @staticmethod
   def _lbindRestArgs( env:  Environment, paramList: list[Any], paramNum: int, argList: tuple[Any], argNum: int ) -> (int, int):
      '''Syntax:  &rest var'''
      try:
         paramName = paramList[paramNum]
      except IndexError:
         raise LispRuntimeError( f'Param name expected after &rest.' )

      if not isinstance(paramName, LSymbol ):
         raise LispRuntimeError( 'Symbol expected after &rest.' )
      
      theRestArgs = argList[argNum:]
      env.bindLocal( paramName.strval, LList(*theRestArgs) )
      
      return paramNum + 1, argNum

   @staticmethod
   def _lbindKeyArgs( env: Environment, paramList: list[Any], paramNum: int, argList: tuple[Any], argNum: int ) -> (int, int):
      '''syntax:  &key {var | ( {var | ( keyword var )} [initForm [svar]])}* [&allow-other-keys]'''
      paramListLength = len(paramList)
      argListLength = len(argList)
      
      # Prepare to iterate over the parameters
      if paramNum >= paramListLength:
         raise LispRuntimeError( f'Param name expected after &key.' )
      keysDict = {}       # Mapping: parameterKeyWord -> (varName,svarName)
      varsDict = {}       # Mapping: varName -> value
      
      # Iterate throught the key parameters adding them to keysDict and varsDict
      while paramNum < paramListLength:
         paramSpec = paramList[paramNum]
         if isinstance(paramSpec, LSymbol):
            if paramSpec == '&ALLOW-OTHER-KEYS':
               break
            keyName = paramSpec
            varName = paramSpec
            initForm = LList()
            svarName = None
         elif isinstance(paramSpec, list):
            try:
               keyVarSpec, *initFormSpec = paramSpec
            except ValueError:
               raise LispRuntimeError( f'Too many default values for key parameter {paramSpec[0]}.' )
            
            # Extract the keyName and varName from keyVarSpec
            if isinstance(keyVarSpec, LSymbol):
               keyName = keyVarSpec
               varName = keyVarSpec
            elif isinstance(keyVarSpec, list):
               try:
                  keyName, varName = keyVarSpec
               except ValueError:
                  raise LispRuntimeError( f'Key Var pair following &key must contain exactly two elements.' )
            else:
               raise LispRuntimeError( f'&key key var pair must be either a symbol of a list (keySymbol varSymbol)' )
            
            # Extract initForm and svarName from initFormSpec
            initFormSpecLen = len(initFormSpec)
            if initFormSpec == 0:
               initForm = LList()
               svarName = None
            elif initFormSpecLen == 1:
               initForm = initFormSpec[0]
               svarName = None
            elif initFormSpecLen == 2:
               initForm, svarName = initFormSpec
            else:
               raise LispRuntimeError( f'Too many arguments specified in a parameter keyword initialization list.' )
         
         # Record the names and values into the appropriate dicts
         keysDict[keyName.strval] = (varName, svarName)
         varsDict[varName.strval] = LispInterpreter._lEval(env, initForm)
         paramNum += 1
      
      allowOtherKeys = False
      if (paramNum < paramListLength):
         nextParam = paramList[paramNum]
         if isinstance(nextParam, LSymbol) and (nextParam == '&ALLOW-OTHER-KEYS'):
            allowOtherKeys = True
            paramNum += 1
      
      # Iterate through the key args updating varsDict
      while argNum < argListLength:
         keyArg = argList[argNum]
         svarName = None
         svarVal = LList()
         if not isinstance(keyArg, LSymbol):
            raise LispRuntimeError( f'Keyword expected, found {keyArg}.' )
         if not keyArg.startswith(':'):
            raise LispRuntimeError( f'Keyword expected, found {keyArg}.' )
         keyArg = keyArg.strval[1:]  # Strip argKey of the leading colon :
         if (not allowOtherKeys) and (keyArg not in keysDict):
            raise LispRuntimeError( f'Unexpected keyword found {keyArg}.' )
         
         argNum += 1
         try:
            argVal = argList[argNum]
         except IndexError:
            raise LispRuntimeError( f'Keyword {keyArg} expected to be followed by a value.' )
         argNum += 1
         try:
            if allowOtherKeys:
               varName,svarName = keysDict.get(keyArg, (keyArg, None))
            else:
               varName,svarName = keysDict[keyArg]
            svarVal = env.getGlobalValue('T')
         except KeyError:
            raise LispRuntimeError( 'Invalid key in argument list :{argKey}.' )
         
         # Record the bindings
         varsDict[varName.strval] = argVal
         
         if svarName:
            varsDict[svarName.strval] = svarVal
      
      # Update env's locals with varsDict
      env.updateLocals( varsDict )
      
      return paramNum, argNum

   @staticmethod
   def _lbindAuxArgs( env: Environment, paramList: list[Any], paramNum: int, argList: tuple[Any], argNum: int ) -> (int, int):
      '''Syntax:  &aux {var | (var [initForm])}*
      These are not really arguments.  At least: these parameters get no corresponding arguments.
      These parameters are strictly local variables for the function.'''
      paramListLength = len(paramList)
      
      # Prepare to loop over the optional parameters and arguments
      if paramNum >= paramListLength:
         raise LispRuntimeError( 'Param expected after &aux.' )
      
      while (paramNum < paramListLength):
         paramSpec = paramList[paramNum]
         
         # Extract the next parameter's name and value
         if isinstance( paramSpec, LSymbol ):
            if paramSpec.startswith('&'):
               raise LispRuntimeError( f'{paramSpec} occurs after &aux.' )
            varName = paramSpec
            initForm = LList( )
         elif isinstance( paramSpec, LList ):
            try:
               varName, initForm = paramSpec
            except:
               raise LispRuntimeError( 'Parameter spec following &OPTIONAL must be a list of (<variable> <defaultvalue> [<svar>] ).' )

            if not isinstance(varName, LSymbol):
               raise LispRuntimeError( 'Parameter spec following &OPTIONAL must be a list of (<variable> <defaultvalue> [<svar>] ).' )
            
            initForm = LispInterpreter._lEval( env, initForm )
         else:
            raise LispRuntimeError( 'Parameter spec following &OPTIONAL must be a <variable> or a list of (<variable> <defaultvalue>). ' )
         
         # Bind the parameters
         env.bindLocal( varName.strval, initForm )
         
         # Prepare for the next iteration
         paramNum += 1
      
      return paramNum, argNum
   
   @staticmethod
   def _macroexpand( env: Environment, macroDef: LMacro, *args ) -> list[Any]:
      env = Environment( env )      # Open a new scope.  Automatically closes when env goes out of scope

      # store the arguments as locals
      LispInterpreter._lbindArguments( env, macroDef.params, args ) #convert args from a tuple to a list

      # Evaluate each body expression; expanding each expr in turn to expand
      # the full macro body.  Place those expanded expressions in resultList.
      return [ LispInterpreter._lEval(env, bodySExpr) for bodySExpr in macroDef.body ]

   @staticmethod
   def _lbackquoteExpand( env: Environment, expr: Any ) -> Any:
      '''Expand a backquote List expression.'''
      if isinstance( expr, LList ):
         if len(expr) == 0:
            return LList( )

         primary = expr[0]
         if ( (primary == 'COMMA') or (primary == 'COMMA-AT') ):
            result = LispInterpreter._lEval(env, expr)
            return result

         resultList: list[Any] = [ ]
         for listElt in expr:
            resultListElt = LispInterpreter._lbackquoteExpand( env, listElt )
            if ( isinstance( resultListElt, LList ) and
                 (len(resultListElt) > 0) and
                 (resultListElt[0] == LSymbol('COMMA-AT')) ):
               for elt in resultListElt[1]:
                  resultList.append( elt )
            else:
               resultList.append( resultListElt )
         return LList( *resultList )
      else:
         return expr

   @staticmethod
   def _lconstructPrimitives( parseLispString: Callable[[str], Any] ) -> dict[str, Any]:
      primitiveDict: dict[str, Any] = { }
      INSIDE_BACKQUOTE = False

      # ###################################
      # Lisp Object & Primitive Definitions
      # ###################################
      L_T = LSymbol( 'T' )
      L_NIL = LList( )
      primitiveDict[ 'T'    ] = L_T
      primitiveDict[ 'NIL'  ] = L_NIL
      primitiveDict[ 'PI'   ] = math.pi
      primitiveDict[ 'E'    ] = math.e

      class LDefPrimitive( object ):
         '''Decorator to assist in the definition of a lisp primitive.'''
         def __init__( self, primitiveSymbol: str, params: str = '', specialForm: bool=False ) -> None:
            '''Arguments:
            primitiveSymbol, the string representation of the lisp symbol used
               to name the primitive in the interpreter.
            params, a documentation string listing the arguments taken by this
               primitive.
            specialForm, Defaults to False.  A true value indicates to the
               interpreter that that this primitive wants its arguments
               unevaluated prior to its call. (Otherwise all arguments are
               evaluated prior to calling this function.)
            '''
            self._name:str  = primitiveSymbol.upper( )
            self._usage:str = f'({primitiveSymbol} {params})' if params else ''
            self._params:str = params
            self._specialForm:bool = specialForm

         def __call__( self, primitiveDef ):
            '''primitiveDef is a python function to implmenet the lisp primitive.'''
            nonlocal primitiveDict
            lPrimitivObj = LPrimitive( primitiveDef, self._name,
                                       self._usage, self._params,
                                       specialForm=self._specialForm )
            primitiveDict[ self._name ] = lPrimitivObj
            return lPrimitivObj

      # =================
      # Symbol Definition
      # -----------------
      @LDefPrimitive( 'defmacro', '<symbol> ( <paramList> ) <sexpr1> <sexpr2> ...', specialForm=True )
      def LP_defmacro( env: Environment, *args ) -> Any:
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
         return env.bindGlobal( fnName.strval, theFunc )

      @LDefPrimitive( 'macroexpand', '\'(<macroName> <arg1> <arg2> ...)' )
      def LP_macroexpand( env: Environment, *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_macroexpand, 'Exactly 1 argument expected.' )
         theMacroCall = args[0]

         if not isinstance( theMacroCall, LList ):
            raise LispRuntimeFuncError( LP_macroexpand, 'Argument 1 expected to be a list.' )

         if len(theMacroCall) < 2:
            raise LispRuntimeError( 'Macro call must be at least two elements in length.' )
         
         # Break the list contents into a function and a list of args
         primary, *exprArgs = theMacroCall

         # fn is an LPrimitive, LFunction or a macro name symbol
         # Use this information to get the function definition
         macroDef = LispInterpreter._lEval( env, primary )
         if not isinstance( macroDef, LMacro ):
            raise LispRuntimeError( 'Badly formed list expression.  The first element should evaluate to a macro.' )

         expandedMacroBody = LispInterpreter._macroexpand( env, macroDef, *exprArgs )
         return LList( *expandedMacroBody )

      @LDefPrimitive( 'setf', '<symbol> <sexpr>', specialForm=True )
      def LP_setf( env: Environment, *args ) -> Any:
         try:
            lval,rval = args
         except ValueError:
            raise LispRuntimeFuncError( LP_setf, '2 arguments expected.' )

         rval = LispInterpreter._lEval(env, rval)
         
         if isinstance(lval, LList):       # s-expression
            if len(lval) == 0:
               raise LispRuntimeFuncError( LP_setf, 'lvalue cannot be NIL.' )
            
            primitive = lval[0]
            if primitive == 'AT':
               try:
                  primitive, keyOrIndex, mapOrLst = lval
               except ValueError:
                  raise LispRuntimeFuncError( LP_setf, 'lvalue expected 3 elements.' )
               
               theSelector = LispInterpreter._lEval(env,keyOrIndex)
               theContainer = LispInterpreter._lEval(env,mapOrLst)
               
               if not isinstance(theContainer, (LList, LMap)):
                  raise LispRuntimeFuncError( LP_setf, 'Invalid container type following \'AT\' primitive.' )

               try:
                  theContainer[theSelector] = rval
               except (KeyError, IndexError):
                  raise LispRuntimeFuncError( LP_setf, 'Invalid key or index.')
               
               return rval
            else:
               lval = LispInterpreter._lEval(env,lval)

         if isinstance(lval, LSymbol ):
            sym = lval
            if isinstance(rval,(LFunction,LMacro)) and (rval.name == ''):
               rval.name = sym
            sym = str(sym)
   
            # If sym exists somewhere in the symbol table hierarchy, set its
            # value to rval.  If it doesn't exist, define it in the global
            # symbol table and set its value to rval.
            theSymTab = env.findDef( sym )
            if theSymTab:
               theSymTab.bindLocal( sym, rval )
            else:
               env.bindGlobal( sym, rval )
         
         return rval
         
      @LDefPrimitive( 'undef!', '<symbol>', specialForm=True)
      def LP_undef( env: Environment, *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_undef, '1 argument exptected.' )
         key = args[0]
         if not isinstance(key, LSymbol):
            raise LispRuntimeFuncError( LP_undef, 'Argument expected to be a symbol.' )
         env.getGlobalEnv().undef( key.strval )
         return L_NIL

      @LDefPrimitive( 'symtab!' )
      def LP_symtab( env: Environment, *args ) -> Any:
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
      @LDefPrimitive( 'lambda', '( <paramList> ) <sexpr1> <sexpr2> ...', specialForm=True )
      def LP_lambda( env: Environment, *args ) -> Any:
         try:
            funcParams, *funcBody = args
         except ValueError:
            raise LispRuntimeFuncError( LP_lambda, '2 arguments expected.' )

         return LFunction( LSymbol(""), funcParams, funcBody, closure=env )

      @LDefPrimitive( 'let', '( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <sexpr1> <sexpr2> ...)', specialForm=True )
      def LP_let( env: Environment, *args ) -> Any:
         try:
            vardefs, *body = args
         except ValueError:
            raise LispRuntimeFuncError( LP_let, '2 or more arguments expected.' )

         if not isinstance( vardefs,  LList ):
            raise LispRuntimeFuncError( LP_let, 'The first argument to let expected to be a list of variable initializations.' )

         # Evaluate the var def initial value exprs in the outer scope.
         initDict = { }
         for varSpec in vardefs:
            if isinstance(varSpec, LSymbol):
               varName = varSpec
               initForm = LList()
            elif isinstance(varSpec, list):
               varSpecLen = len(varSpec)
               if varSpecLen == 1:
                  varName = varSpec[0]
                  initForm = LList()
               elif varSpecLen == 2:
                  varName, initForm = varSpec
               else:
                  raise LispRuntimeFuncError( LP_let, 'Variable initializer spec expected to be 1 or 2 elements long.' )
                  
               if not isinstance(varName, LSymbol):
                  raise LispRuntimeFuncError( LP_let, 'First element of a variable initializer pair expected to be a symbol.' )
            
            initDict[varName.strval] = LispInterpreter._lEval(env, initForm)

         # Open the new scope
         env = Environment( env, **initDict )     # Open a new scope. Auto closes when env goes out of scope.

         # Evaluate each body sexpr in the new env/scope
         lastResult = L_NIL
         for sexpr in body:
            lastResult = LispInterpreter._lEval( env, sexpr )
         return lastResult

      @LDefPrimitive( 'let*', '( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <sexpr1> <sexpr2> ...)', specialForm=True )
      def LP_letstar( env: Environment, *args ) -> Any:
         try:
            vardefs, *body = args
         except ValueError:
            raise LispRuntimeFuncError( LP_letstar, '2 or more arguments expected.' )

         if not isinstance( vardefs,  LList ):
            raise LispRuntimeFuncError( LP_letstar, 'The first argument to let expected to be a list of variable initializations.' )

         # Open the new scope
         env = Environment( env )    #  Open a new scope. Auto closes when env goes out of scope.

         for varSpec in vardefs:
            if isinstance(varSpec, LSymbol):
               varName = varSpec
               initForm = LList()
            elif isinstance(varSpec, list):
               varSpecLen = len(varSpec)
               if varSpecLen == 1:
                  varName = varSpec[0]
                  initForm = LList()
               elif varSpecLen == 2:
                  varName, initForm = varSpec
               else:
                  raise LispRuntimeFuncError( LP_letstar, 'Variable initializer spec expected to be 1 or 2 elements long.' )
                  
               if not isinstance(varName, LSymbol):
                  raise LispRuntimeFuncError( LP_letstar, 'First element of a variable initializer pair expected to be a symbol.' )
            
            env.bindLocal( varName.strval, LispInterpreter._lEval(env, initForm) )

         # Evaluate each body sexpr in the new env/scope.
         lastResult = L_NIL
         for sexpr in body:
            lastResult = LispInterpreter._lEval( env, sexpr )
         return lastResult

      @LDefPrimitive( 'progn', '<sexpr1> <sexpr2> ...', specialForm=True )
      def LP_progn( env: Environment, *args ) -> Any:
         lastResult = L_NIL
         for expr in args:
            lastResult = LispInterpreter._lEval( env, expr )
         return lastResult

      @LDefPrimitive( 'if', '<cond> <conseq> &optional <alt>', specialForm=True )
      def LP_if( env: Environment, *args ) -> Any:
         numArgs = len(args)
         if not(2 <= numArgs <= 3):
            raise LispRuntimeFuncError( LP_if, '2 or 3 arguments expected.' )
         condExpr,conseq,*alt = args

         condValue = LispInterpreter._lEval( env, condExpr )
         if LispInterpreter._lTrue(condValue):
            return LispInterpreter._lEval( env, conseq)    # The THEN part
         elif numArgs == 3:
            return LispInterpreter._lEval( env, alt[0])    # The ELSE part
         else:
            return L_NIL

      @LDefPrimitive( 'cond', '(<cond1> <body1>) (<cond2> <body2>) ...', specialForm=True )
      def LP_cond( env: Environment, *args ) -> Any:
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
               latestResult = LList( )
               for sexpr in body:
                  latestResult = LispInterpreter._lEval( env, sexpr )
               return latestResult

         return LList( )

      @LDefPrimitive( 'case', '<sexpr> (<val1> <body1>) (<val2> <body2>) ...', specialForm=True )
      def LP_case( env: Environment, *args ) -> Any:
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

      @LDefPrimitive( 'quote', '<sexpr>', specialForm=True )
      def LP_quote( env: Environment, *args ) -> Any:
         if (len(args) != 1):
            raise LispRuntimeFuncError( LP_quote, '1 argument exptected.' )
         return args[0]

      @LDefPrimitive( 'backquote', '<sexpr>', specialForm=True )
      def LP_backquote( env: Environment, *args ) -> Any:
         nonlocal INSIDE_BACKQUOTE
         if INSIDE_BACKQUOTE:
            raise LispRuntimeFuncError( LP_backquote, 'Cannot nest backquotes.')

         if (len(args) != 1):
            raise LispRuntimeFuncError( LP_backquote, '1 argument exptected.' )
         sExpr = args[0]

         try:
            INSIDE_BACKQUOTE = True
            expandedForm = LispInterpreter._lbackquoteExpand( env, sExpr )
         finally:
            INSIDE_BACKQUOTE = False

         return expandedForm

      @LDefPrimitive( 'comma', '<sexpr>', specialForm=True )
      def LP_comma( env: Environment, *args ) -> Any:
         nonlocal INSIDE_BACKQUOTE
         if not INSIDE_BACKQUOTE:
            raise LispRuntimeFuncError( LP_comma, 'COMMA can only occur inside a BACKQUOTE.')

         if (len(args) != 1):
            raise LispRuntimeFuncError( LP_comma, '1 argument exptected.' )
         subordinateExpr = args[0]
         result = LispInterpreter._lEval( env, subordinateExpr )
         return result

      @LDefPrimitive( 'comma-at', '<sexpr>', specialForm=True )
      def LP_comma_at( env: Environment, *args ) -> Any:
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

      @LDefPrimitive( 'while', '<cond> <sexpr1> <sexpr2> ...', specialForm=True )
      def LP_while( env: Environment, *args ) -> Any:
         try:
            conditionExpr, *body = args
         except ValueError:
            raise LispRuntimeFuncError( LP_while, '2 arguments expected.' )

         if len(body) < 1:
            raise LispRuntimeFuncError( LP_while, 'At least one sexpr expected for the body.' )

         latestResult = LList()
         condResult = LispInterpreter._lEval(env, conditionExpr)
         while LispInterpreter._lTrue( condResult ):
            for expr in body:
               latestResult = LispInterpreter._lEval( env, expr )
            condResult = LispInterpreter._lEval(env, conditionExpr )
         return latestResult

      @LDefPrimitive( 'doTimes', '(<var> <integer>) <sexpr1> <sexpr2> ...', specialForm=True )
      def LP_dotimes( env: Environment, *args ) -> Any:
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

         latestResult = LList()
         for iterCount in range(count):
            env.bindLocal( variable.strval, iterCount )
            for sexpr in body:
               latestResult = LispInterpreter._lEval( env, sexpr )
         return latestResult

      @LDefPrimitive( 'foreach', '<variable> <list> <sexpr1> <sexpr2> ...', specialForm=True )
      def LP_foreach( env: Environment, *args ) -> Any:
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
         latestResult = LList()
         for element in alist:
            env.bindLocal( str(varSymbol), element )
            for sexpr in body:
               latestResult = LispInterpreter._lEval( env,  sexpr )
         return latestResult

      @LDefPrimitive( 'funcall', '<fnNameSymbol> <arg1> <arg2> ...' )
      def LP_funcall( env: Environment, *args ) -> Any:
         try:
            fnNameSymbol, *fnArgs = args
         except:
            raise LispRuntimeFuncError( LP_funcall, "1 or more arguments expected" )

         newExpr = LList( fnNameSymbol, *fnArgs )
         result = LispInterpreter._lEval( env, newExpr )
         return result

      @LDefPrimitive( 'eval', '<sexpr>' )
      def LP_eval( env: Environment, *args ) -> Any:
         try:
            expr = args[0]
         except IndexError:
            raise LispRuntimeFuncError( LP_eval, '1 argument exptected.' )
         return LispInterpreter._lEval( env, expr )

      @LDefPrimitive( 'apply', '<function> &rest <args> <argsList>' )
      def LP_apply( env: Environment, *args ) -> Any:
         if len(args) < 2:
            raise LispRuntimeFuncError( LP_apply, "At least 2 arguments expected to apply." )
         
         listArg = args[-1]
         if not isinstance( listArg, LList ):
            raise LispRuntimeFuncError( LP_apply, "Last argument expected to be a list." )
         
         fn = args[0]
         if isinstance( fn, LSymbol ):
            fn = env.getValue( fn.strval )
         
         if not isinstance( fn, (LPrimitive, LFunction) ):
            raise LispRuntimeFuncError( LP_apply, "First argument must be a primitive or function." )
         
         if fn.specialForm:
            raise LispRuntimeFuncError( LP_apply, "First argument may not be a special form." )
         
         fnArgs = list( args[1:-1] )
         fnArgs.extend( listArg )
         
         return LispInterpreter._lApply( env, fn, *fnArgs )

      @LDefPrimitive( 'parse', '<string>' )
      def LP_parse( env: Environment, *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_parse, '1 string argument expected.' )
         theExprStr = args[0]
         theExprAST = parseLispString( theExprStr )
         return theExprAST

      @LDefPrimitive( 'python', '<string>' )
      def LP_python( env: Environment, *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_python, '1 string argument expected by python.' )
         thePythonCode = args[0]
         if not isinstance(thePythonCode, str):
            raise LispRuntimeFuncError( LP_python, 'Argument expected to be a string.' )
         theReturnVal = eval( thePythonCode, globals(), locals() )
         return theReturnVal

      # =======================
      # List & Map Manipulation
      # -----------------------
      @LDefPrimitive( 'map', '(<key1> <val1>) (<key2> <val2>) ...', specialForm=True )
      def LP_map( env: Environment, *args ) -> Any:
         theMapping = LMap( )
         for entryNum,key_expr_pair in enumerate(args):
            try:
               key,expr = key_expr_pair
            except:
               raise LispRuntimeFuncError( LP_map, f'Entry {entryNum + 1} does not contain a (key value) pair.' )

            if isinstance( key,  LSymbol ):
               key = str(key)

            if isinstance( key, (int,float,str) ):
               theMapping[ key ] = LispInterpreter._lEval( env, expr )
            else:
               raise LispRuntimeFuncError( LP_map, f'Entry {entryNum+1} has an invalid <key> type.' )
         return theMapping

      @LDefPrimitive( 'car', '<list>' )
      def LP_car( env: Environment, *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_car, '1 argument expected.' )
         theList = args[0]

         if not isinstance(theList, LList):
            raise LispRuntimeFuncError( LP_car, '1st argument expected to be a list.' )

         try:
            return theList[0]
         except IndexError:
            return LList( )

      @LDefPrimitive( 'cdr', '<list>' )
      def LP_cdr( env: Environment, *args ) -> Any:
         numArgs = len(args)
         if numArgs != 1:
            raise LispRuntimeFuncError( LP_cdr, '1 argument expected.' )
         theList = args[0]

         if not isinstance(theList, LList):
            raise LispRuntimeFuncError( LP_cdr, '1st argument expected to be a list.' )

         try:
            return LList( *theList[1:] )
         except IndexError:
            return LList( )

      @LDefPrimitive( 'cons', '<obj> <list>' )
      def LP_cons( env: Environment, *args ) -> Any:
         try:
            arg1,arg2 = args
         except:
            raise LispRuntimeFuncError( LP_cons, '2 arguments exptected.' )

         try:
            copiedList = LList( *arg2 ) # copy LList arg2
            copiedList.insert( 0, arg1 )
         except:
            raise LispRuntimeFuncError( LP_cons, 'Invalid argument.' )

         return copiedList

      @LDefPrimitive( 'push!', '<list> <value>' )
      def LP_push( env: Environment, *args ) -> Any:
         try:
            alist, value = args
         except ValueError:
            raise LispRuntimeFuncError( LP_push, '2 arguments exptected.' )

         try:
            if isinstance(alist, LList):
               alist.append( value )
            else:
               alist = LList()
         except:
            raise LispRuntimeFuncError( LP_push, 'Invalid argument.' )
         return alist

      @LDefPrimitive( 'pop!', '<list>' )
      def LP_pop( env: Environment, *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_pop, '1 argument expected.' )
         alist = args[0]

         try:
            value = alist.pop()
         except:
            raise LispRuntimeFuncError( LP_pop, 'Invalid argument.' )
         return value

      @LDefPrimitive( 'at', '<keyOrIndex> <mapListOrStr>' )
      def LP_at( env: Environment, *args ) -> Any:
         try:
            key,keyed = args
         except:
            raise LispRuntimeFuncError( LP_at, '2 arguments expected.' )

         if not isinstance(keyed, (LList, LMap, str) ):
            raise LispRuntimeFuncError( LP_at, 'Invalid argument.  List or Map expected.' )

         if isinstance( key, LSymbol ):
            key = key.strval

         try:
            return keyed[ key ]
         except ( KeyError, IndexError ):
            raise LispRuntimeFuncError( LP_at, 'Invalid argument key/index.' )

      @LDefPrimitive( 'at-delete', '<keyOrIndex> <mapOrList>' )
      def LP_atDelete( env: Environment, *args ) -> bool:
         try:
            key, keyed = args
         except:
            raise LispRuntimeFuncError( LP_atDelete, "Exactly 2 arguments expected." )
         
         if not isinstance( keyed, (LList, LMap) ):
            raise LispRuntimeFuncError( LP_atDelete, "Argument 2 expected to be a list or map." )

         try:
            del keyed[key]
         except ( IndexError, KeyError ):
            raise LispRuntimeFuncError( LP_atDelete, "Bad index or key into collection." )
         
         return L_T
      
      @LDefPrimitive( 'at-insert', '<index> <list> <newItem>' )
      def LP_atInsert( env: Environment, *args ) -> bool:
         try:
            index, lst, newItem = args
         except:
            raise LispRuntimeFuncError( LP_atInsert, "Exactly 3 arguments expected." )
         
         if not isinstance(index, int):
            raise LispRuntimeFuncError( LP_atInsert, "Argument 1 expected to be an integer index." )
         
         if not isinstance( lst, list ):
            raise LispRuntimeFuncError( LP_atInsert, "Argument 2 expected to be a list." )
         
         lst.insert( index, newItem )
         return newItem
      
      @LDefPrimitive( 'append', '<list1> <list2> ...' )
      def LP_append( env: Environment, *args ) -> Any:
         if len(args) < 2:
            raise LispRuntimeFuncError( LP_append, 'At least 2 arguments expected.' )

         resultList = LList( )
         for lst in args:
            if not isinstance( lst,  LList ):
               raise LispRuntimeFuncError( LP_append, 'Invalid argument.' )
            for item in lst:
               resultList.append( item )
         return resultList

      @LDefPrimitive( 'hasValue?', '<listOrMap> <value>' )
      def LP_hasValue( env: Environment, *args ) -> Any:
         try:
            keyed,aVal = args
         except:
            raise LispRuntimeFuncError( LP_hasValue, '2 arguments expected.' )

         if isinstance(keyed, LList):
            keyed = keyed
         elif isinstance(keyed, LMap):
            keyed = keyed.values()
         else:
            raise LispRuntimeFuncError( LP_hasValue, 'Invalid argument.  Argument 1 expected to be a list or map.')

         try:
            return L_T if aVal in keyed else L_NIL    # T or NIL
         except:
            raise LispRuntimeFuncError( LP_hasValue, 'Invalid argument.')

      @LDefPrimitive( 'update!', '<map1> <map2>' )
      def LP_update( env: Environment, *args ) -> Any:
         try:
            map1,map2 = args
         except:
            raise LispRuntimeFuncError( LP_update, '2 arguments exptected.' )

         if not isinstance( map1, LMap ):
            raise LispRuntimeFuncError( LP_update, 'Argument 1 expected to be a map.' )

         if not isinstance( map2, LMap ):
            raise LispRuntimeFuncError( LP_update, 'Argument 2 expected to be a map.' )

         map1.update( map2 )
         return map1

      @LDefPrimitive( 'hasKey?', '<map> <key>' )
      def LP_hasKey( env: Environment, *args ) -> Any:
         try:
            aMap,aKey = args
         except:
            raise LispRuntimeFuncError( LP_hasKey, '2 arguments expected.' )

         if not isinstance(aMap, LMap):
            raise LispRuntimeFuncError( LP_hasKey, 'Invalid argument 1.  Map expected.')

         if isinstance( aKey, LSymbol ):
            aKey = aKey.strval

         return L_T if aKey in aMap else L_NIL   # T or NIL

      @LDefPrimitive( 'sorted', '<list>' )
      def LP_sorted( env: Environment, *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_sorted, "Exactly 1 argument exptected." )
         
         theList = args[0]
         if not isinstance(theList, list):
            raise LispRuntimeFuncError( LP_sorted, "Argument 1 expected to be a list." )
         
         theSortedList = sorted( theList )
         return LList( *theSortedList )
      
      # =====================
      # Arithmetic Operations
      # ---------------------
      @LDefPrimitive( '+', '<number1> <number2> ...' )
      def LP_add( env: Environment, *args ) -> Any:
         try:
            return sum(args)
         except TypeError:
            raise LispRuntimeFuncError( LP_add, 'Invalid argument.' )

      @LDefPrimitive( '-', '<number1> <number2> ...' )
      def LP_sub( env: Environment, *args ) -> Any:
         try:
            if len(args) == 1:
               arg = args[0]
               if not isinstance( arg, LNUMBER ):
                  raise TypeError( )
               return -1 * arg
            else:
               return functools.reduce( lambda x,y: x - y, args )
         except TypeError:
            raise LispRuntimeFuncError( LP_sub, 'Invalid argument.' )

      @LDefPrimitive( '*', '<number1> <number2> ...' )
      def LP_mul( env: Environment, *args ) -> Any:
         try:
            return functools.reduce( lambda x,y: x * y, iter(args) )
         except:
            raise LispRuntimeFuncError( LP_mul, 'Invalid argument.' )

      @LDefPrimitive( '/', '<number1> <number2> ...' )
      def LP_div( env: Environment, *args ) -> Any:
         try:
            return functools.reduce( lambda x,y: x / y, iter(args) )
         except:
            raise LispRuntimeFuncError( LP_div, 'Invalid argument.' )

      @LDefPrimitive( '//', '<number1> <number2>' )
      def LP_intdiv( env: Environment, *args ) -> Any:
         try:
            return args[0] // args[1]
         except:
            raise LispRuntimeFuncError( LP_intdiv, 'Invalid argument.' )

      @LDefPrimitive( 'mod', '<number1> <number2>' )
      def LP_moddiv( env: Environment, *args ) -> Any:
         try:
            return args[0] % args[1]
         except:
            raise LispRuntimeFuncError( LP_moddiv, 'Invalid argument.' )

      @LDefPrimitive( 'gcd', '<integer1> <integer2> ...' )
      def LP_gcd( env: Environment,  *args ) -> Any:
         try:
            return math.gcd( *args )
         except:
            raise LispRuntimeFuncError( LP_gcd, 'Invalid argument.' )

      @LDefPrimitive( 'lcm', '<integer1> <integer2> ...' )
      def LP_lcm( env: Environment,  *args ) -> Any:
         try:
            return math.lcm( *args )
         except:
            raise LispRuntimeFuncError( LP_lcm, 'Invalid argument.' )

      @LDefPrimitive( 'log', '<number> &optional (<base> 10)' )
      def LP_log( env: Environment, *args ) -> Any:
         numArgs = len(args)
         if not( 1 <= numArgs <= 2 ):
            raise LispRuntimeFuncError( LP_log, '1 or 2 arguments exptected.' )

         try:
            num,*rest = args
            base = math.e if len(rest) == 0 else rest[0]
            return math.log(num,base)
         except:
            raise LispRuntimeFuncError( LP_log, 'Invalid argument.' )

      @LDefPrimitive( 'expt', '<base> <power>' )
      def LP_expt( env: Environment, *args ) -> Any:
         try:
            base,power = args
            return base ** power
         except:
            raise LispRuntimeFuncError( LP_expt, 'Invalid argument.' )

      @LDefPrimitive( 'sin', '<radians>' )
      def LP_sin( env: Environment, *args ) -> Any:
         try:
            return math.sin(args[0])
         except:
            raise LispRuntimeFuncError( LP_sin, 'Invalid argument.' )

      @LDefPrimitive( 'cos', '<radians>' )
      def LP_cos( env: Environment, *args ) -> Any:
         try:
            return math.cos(args[0])
         except:
            raise LispRuntimeFuncError( LP_cos, 'Invalid argument.' )

      @LDefPrimitive( 'asin', '<number>' )
      def LP_asin( env: Environment, *args ) -> Any:
         try:
            return math.asin(args[0])
         except:
            raise LispRuntimeFuncError( LP_asin, 'Invalid argument.' )

      @LDefPrimitive( 'acos', '<number>' )
      def LP_acos( env: Environment, *args ) -> Any:
         try:
            return math.acos(args[0])
         except:
            raise LispRuntimeFuncError( LP_acos, 'Invalid argument.' )

      @LDefPrimitive( 'atan', '<number1> &optional <number2>' )
      def LP_atan( env: Environment, *args ) -> Any:
         numArgs = len(args)
         if numArgs == 1:
            nval = args[0]
            dval = 1
         elif numArgs == 2:
            nval,dval = args
         else:
            raise LispRuntimeFuncError( LP_atan, '1 or two arguments expcted.' )

         try:
            return math.atan( nval / dval )
         except:
            raise LispRuntimeFuncError( LP_atan, 'Invalid argument.' )

      @LDefPrimitive( 'min', '<number1> <number2> ...' )
      def LP_min( env: Environment, *args ) -> Any:
         try:
            return min( *args )
         except:
            raise LispRuntimeFuncError( LP_min, 'Invalid argument.' )

      @LDefPrimitive( 'max', '<number1> <number2> ...' )
      def LP_max( env: Environment, *args ) -> Any:
         try:
            return max( *args )
         except:
            raise LispRuntimeFuncError( LP_max, 'Invalid argument.' )

      @LDefPrimitive( 'random', '<integerOrFloat>' )
      def LP_random( env: Environment, *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_random, 'Exactly 1 number argument exptected.' )
         num = args[0]

         if isinstance( num,  int ):
            return random.randint(0, num)
         elif isinstance( num,  float ):
            return random.uniform(0.0, num)
         else:
            raise LispRuntimeError( LP_random, 'Invalid argument type.' )

      # ==========
      # Predicates
      # ----------
      @LDefPrimitive( 'numberp', '<sexpr>' )
      def LP_numberp( env: Environment, *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_numberp, '1 argument expected.' )
         return L_T if isinstance( args[0], LNUMBER ) else L_NIL

      @LDefPrimitive( 'integerp', '<sexpr>' )
      def LP_integerp( env: Environment,  *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_integerp, '1 argument expected.' )
         return L_T if isinstance( args[0], int ) else L_NIL

      @LDefPrimitive( 'rationalp', '<sexpr>' )
      def LP_rationalp( env: Environment,  *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_rationalp, '1 argument expected.' )
         return L_T if isinstance( args[0], (int,Fraction) ) else L_NIL

      @LDefPrimitive( 'floatp', '<sexpr>' )
      def LP_floatp( env: Environment,  *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_floatp, '1 argument expected.' )
         return L_T if isinstance( args[0], float ) else L_NIL

      @LDefPrimitive( 'symbolp', '<sexpr>' )
      def LP_symbolp( env: Environment, *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_symbolp, '1 argument expected.' )
         return L_T if isinstance( args[0], LSymbol ) else L_NIL

      @LDefPrimitive( 'atom', '<sexpr>' )
      def LP_atom( env: Environment, *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_atom, '1 argument expected.' )
         arg = args[0]
         if isinstance(arg, LList):
            return L_T if len(arg) == 0 else L_NIL         # NIL or () is an atom even through it's a list.
         return L_T

      @LDefPrimitive( 'listp', '<sexpr>' )
      def LP_listp( env: Environment, *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_listp, '1 argument expected.' )
         return L_T if isinstance( args[0], LList ) else L_NIL

      @LDefPrimitive( 'isMap?', '<sexpr>' )
      def LP_isMap( env: Environment, *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_isMap, '1 argument expected.' )
         return L_T if isinstance( args[0], LMap ) else L_NIL

      @LDefPrimitive( 'stringp', '<sexpr>' )
      def LP_stringp( env: Environment, *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_stringp, '1 argument expected.' )
         return L_T if isinstance( args[0], str ) else L_NIL

      @LDefPrimitive( 'functionp', '<sexpr>' )
      def LP_functionp( env: Environment, *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_functionp, '1 argument expected.' )
         return L_T if isinstance( args[0], (LPrimitive,LFunction) ) else L_NIL

      # ====================
      # Relational Operators
      # --------------------
      @LDefPrimitive( 'is?', '<expr1> <expr2>' )
      def LP_is( env: Environment, *args ) -> Any:
         try:
            arg1,arg2 = args
         except:
            raise LispRuntimeFuncError( LP_is, '2 arguments exptected.' )

         if isinstance(arg1, (int,float,str)):
            return L_T if (arg1 == arg2) else L_NIL
         else:
            return L_T if (arg1 is arg2) else L_NIL

      @LDefPrimitive( '=', '<expr1> <expr2> ...' )
      def LP_equal( env: Environment, *args ) -> Any:
         numArgs = len(args)
         if numArgs < 2:
            raise LispRuntimeFuncError( LP_equal, '2 or more arguments expected.' )

         pairs = [ ]
         prior = None
         for mbr in args:
            if prior is not None:
               pairs.append( (prior,mbr) )
            prior = mbr

         for arg1,arg2 in pairs:
            if not( arg1 == arg2 ):
               return L_NIL

         return L_T

      @LDefPrimitive( '/=', '<expr1> <expr2> ...' )
      def LP_notEqual( env: Environment, *args ) -> Any:
         numArgs = len(args)
         if numArgs < 2:
            raise LispRuntimeFuncError( LP_notEqual, '2 or more arguments expected.' )

         pairs = [ ]
         prior = None
         for mbr in args:
            if prior is not None:
               pairs.append( (prior,mbr) )
            prior = mbr

         for arg1,arg2 in pairs:
            if not( arg1 != arg2 ):
               return L_NIL

         return L_T

      @LDefPrimitive( '<', '<expr1> <expr2> ...' )
      def LP_less( env: Environment, *args ) -> Any:
         numArgs = len(args)
         if numArgs < 2:
            raise LispRuntimeFuncError( LP_less, '2 or more arguments expected.' )

         pairs = [ ]
         prior = None
         for mbr in args:
            if prior is not None:
               pairs.append( (prior,mbr) )
            prior = mbr

         for arg1,arg2 in pairs:
            if not( arg1 < arg2 ):
               return L_NIL

         return L_T

      @LDefPrimitive( '<=', '<expr1> <expr2> ...' )
      def LP_lessOrEqual( env: Environment, *args ) -> Any:
         numArgs = len(args)
         if numArgs < 2:
            raise LispRuntimeFuncError( LP_lessOrEqual, '2 or more arguments expected.' )

         pairs = [ ]
         prior = None
         for mbr in args:
            if prior is not None:
               pairs.append( (prior,mbr) )
            prior = mbr

         for arg1,arg2 in pairs:
            if not( arg1 <= arg2 ):
               return L_NIL

         return L_T

      @LDefPrimitive( '>', '<expr1> <expr2> ...' )
      def LP_greater( env: Environment, *args ) -> Any:
         numArgs = len(args)
         if numArgs < 2:
            raise LispRuntimeFuncError( LP_greater, '2 or more arguments expected.' )

         pairs = [ ]
         prior = None
         for mbr in args:
            if prior is not None:
               pairs.append( (prior,mbr) )
            prior = mbr

         for arg1,arg2 in pairs:
            if not( arg1 > arg2 ):
               return L_NIL

         return L_T

      @LDefPrimitive( '>=', '<expr1> <expr2> ...' )
      def LP_greaterOrEqual( env: Environment, *args ) -> Any:
         numArgs = len(args)
         if numArgs < 2:
            raise LispRuntimeFuncError( LP_greaterOrEqual, '2 or more arguments expected.' )

         pairs = [ ]
         prior = None
         for mbr in args:
            if prior is not None:
               pairs.append( (prior,mbr) )
            prior = mbr

         for arg1,arg2 in pairs:
            if not( arg1 >= arg2 ):
               return L_NIL

         return L_T

      # =================
      # Logical Operators
      # -----------------
      @LDefPrimitive( 'not', '<boolean>' )
      def LP_not( env: Environment, *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_not, '1 argument exptected.' )
         arg1 = args[0]
         return L_T if (isinstance(arg1,LList) and (len(arg1)==0)) else L_NIL

      @LDefPrimitive( 'and', '<boolean1> <boolean2> ...' )
      def LP_and( env: Environment, *args ) -> Any:
         if len(args) < 2:
            raise LispRuntimeFuncError( LP_and, '2 or more arguments exptected.' )

         for arg in args:
            if not LispInterpreter._lTrue(arg):
               return LList()

         return L_T

      @LDefPrimitive( 'or', '<boolean1> <boolean2> ...' )
      def LP_or( env: Environment, *args ) -> Any:
         if len(args) < 2:
            raise LispRuntimeFuncError( LP_or, '2 or more arguments exptected.' )

         for arg in args:
            if LispInterpreter._lTrue(arg):
               return L_T

         return LList()

      # ===============
      # Type Conversion
      # ---------------
      @LDefPrimitive( 'float', '<number>' )
      def LP_float( env: Environment, *args ) -> Any:
         try:
            return float(args[0])
         except (ValueError, IndexError):
            raise LispRuntimeFuncError( LP_float, 'Invalid argument.' )

      @LDefPrimitive( 'integer', '<number> &optional (<base> 10)' )
      def LP_integer( env: Environment, *args ) -> Any:
         numArgs = len(args)
         if (numArgs < 1) or (numArgs > 2):
            raise LispRuntimeFuncError( LP_integer, '1 or two arguments expected.' )

         try:
            return int(*args)
         except TypeError:
            raise LispRuntimeFuncError( LP_integer, 'Invalid argument.' )

      @LDefPrimitive( 'rational', '<number>' )
      def LP_rational( env: Environment, *args ) -> Any:
         if len(args) != 1:
            raise LispRuntimeFuncError( LP_rational, 'Exactly 1 argument expected.' )

         try:
            return Fraction(args[0])
         except (IndexError, TypeError):
            raise LispRuntimeFuncError( LP_rational, 'Invalid argument.' )

      @LDefPrimitive( 'string', '<object1> <object2> ...' )
      def LP_string( env: Environment, *args ) -> Any:
         if len(args) == 0:
            raise LispRuntimeFuncError( LP_string, '1 or more arguments exptected.' )

         resultStrs = [ prettyPrintSExpr(sExpr) for sExpr in args ]
         return ''.join(resultStrs)

      @LDefPrimitive( 'ustring', '<object1> <object2> ...'  )
      def LP_ustring( env: Environment, *args ) -> Any:
         if len(args) == 0:
            raise LispRuntimeFuncError( LP_ustring, '1 or more arguments exptected.' )

         resultStrs = [ prettyPrint(sExpr) for sExpr in args ]
         return ''.join(resultStrs)

      @LDefPrimitive( 'symbol', '<string1> <string2> ...'  )
      def LP_symbol( env: Environment, *args ) -> Any:
         if len(args) == 0:
            raise LispRuntimeFuncError( LP_symbol, '1 or more string argument expected.' )

         strList = [ str(arg) for arg in args ]
         symstr = ''.join(strList)
         return LSymbol(symstr)

      # ===============
      # I/O
      # ---------------
      @LDefPrimitive( 'writef', '<formatString> <MapOrList>' )
      def LP_writef( env: Environment, *args ) -> str:
         try:
            formatString, mapOrList = args
         except ValueError:
            raise LispRuntimeFuncError( LP_writef, "2 arguments expected." )
         
         if not isinstance( formatString, str ):
            raise LispRuntimeFuncError( LP_writef, "1st argument expected to be a format string." )
         
         if isinstance( mapOrList, list ):
            formattedStr = formatString.format( *mapOrList )
         elif isinstance( mapOrList, dict ):
            formattedStr = formatString.format( **mapOrList )
         else:
            raise LispRuntimeFuncError( LP_writef, "2nd argument expected to be a list or map." )
         
         outputStr = bytes( formattedStr, "utf-8" ).decode( "unicode_escape" ) # decode escape sequences
         print( outputStr, end='', file=LispInterpreter.outStrm )
         return outputStr
      
      @LDefPrimitive( 'write!', '<obj1> <obj2> ...' )
      def LP_write( env: Environment, *args ) -> Any:
         if len(args) == 0:
            raise LispRuntimeFuncError( LP_write, '1 or more arguments expected.' )
         return lwrite( *args, end='' )

      @LDefPrimitive( 'writeLn!', '<obj1> <obj2> ...' )
      def LP_writeln( env: Environment, *args ) -> Any:
         if len(args) == 0:
            raise LispRuntimeFuncError( LP_writeln, '1 or more arguments expected.' )
         return lwrite( *args, end='\n' )

      def lwrite( *values, end='' ):
         for value in values:
            valueStr = prettyPrintSExpr( value )
            valueStr = bytes( valueStr, "utf-8" ).decode( "unicode_escape" ) # decode escape sequences
            print( valueStr, end='', file=LispInterpreter.outStrm )
         if end:
            print( end=end, file=LispInterpreter.outStrm )
         return values[-1]

      @LDefPrimitive( 'uwrite!', '<obj1> <obj2> ...' )
      def LP_uwrite( env: Environment, *args ) -> Any:
         if len(args) == 0:
            raise LispRuntimeFuncError( LP_uwrite, '1 or more arguments expected.' )
         return luwrite( *args, end='' )

      @LDefPrimitive( 'uwriteLn!', '<obj1> <obj2> ...' )
      def LP_uwriteln( env: Environment, *args ) -> Any:
         if len(args) == 0:
            raise LispRuntimeFuncError( LP_uwriteln, '1 or more arguments expected.' )
         return luwrite( *args, end='\n' )

      def luwrite( *values, end='' ):
         for value in values:
            valueStr = prettyPrint( value )
            valueStr = bytes( valueStr, "utf-8" ).decode( "unicode_escape" ) # decode escape sequences
            print( valueStr, end='', file=LispInterpreter.outStrm )
         if end:
            print( end=end, file=LispInterpreter.outStrm )
         return values[-1]

      @LDefPrimitive( 'readLn!' )
      def LP_readln( env: Environment, *args ) -> Any:
         if len(args) > 0:
            raise LispRuntimeFuncError( LP_readln, '0 arguments expected.' )
         return input()

      # ===============
      # System Level
      # ---------------
      @LDefPrimitive( 'recursion-limit', '&optional <newLimit>')
      def LP_recursionlimit( env: Environment, *args ) -> Any:
         numArgs = len(args)
         if numArgs == 0:
            return sys.getrecursionlimit()
         elif numArgs == 1:
            newLimit = int(args[0])
            try:
               sys.setrecursionlimit(newLimit)
               return newLimit
            except RecursionError:
               return LList()
         else:
            raise LispRuntimeFuncError( LP_recursionlimit, 'Only one optional arg is allowed.' )
      
      return primitiveDict

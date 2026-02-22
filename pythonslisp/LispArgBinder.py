"""LispArgBinder — argument binding for lambda/function/macro calls.

Extracted from LispInterpreter so that LispExpander can use it without
depending on the full interpreter.  evalFn (LispInterpreter._lEval) is
passed in by the caller so this module stays free of LispInterpreter deps.
"""

from typing import Any, Callable, Sequence

from pythonslisp.Environment import Environment
from pythonslisp.LispAST import LSymbol
from pythonslisp.LispExceptions import LispArgBindingError


def validateNoDuplicateParams( lambdaListAST: list[Any] ) -> None:
   '''Pre-pass: verify no variable name appears more than once in a lambda list.'''
   seen: set[str] = set()

   def _check( name: Any, context: str ) -> None:
      if not isinstance(name, LSymbol):
         return     # other validation will catch non-symbols
      if name.startswith('&'):
         return     # &-keywords are not parameter names
      if name.strval in seen:
         raise LispArgBindingError( f'Duplicate parameter name {name.strval} in {context}.' )
      seen.add( name.strval )

   index = 0
   lambdaListLen = len( lambdaListAST )
   while index < lambdaListLen:
      spec = lambdaListAST[index]

      if isinstance( spec, LSymbol ):
         if spec == '&REST':
            index += 1
            if index < lambdaListLen:
               _check( lambdaListAST[index], '&REST' )
         elif not spec.startswith('&'):
            _check( spec, 'positional parameters' )

      elif isinstance( spec, list ) and len(spec) > 0:
         # First element is either a symbol (var) or a list ((keyword var))
         keyVarSpec = spec[0]
         if isinstance( keyVarSpec, LSymbol ):
            _check( keyVarSpec, 'lambda list' )
         elif isinstance( keyVarSpec, list ) and len(keyVarSpec) >= 2:
            _check( keyVarSpec[1], '&KEY (keyword var) pair' )
         # Check svar: 3rd element if present (for &optional and &key)
         if len(spec) >= 3:
            _check( spec[2], 'supplied-p variable' )

      index += 1


def bindArguments( env: Environment, lambdaListAST: list[Any], argList: Sequence[Any],
                   evalFn: Callable ) -> None:
   validateNoDuplicateParams( lambdaListAST )
   paramListLength = len(lambdaListAST)
   argListLength = len(argList)

   paramNum, argNum = _bindPositionalArgs( env, lambdaListAST, 0, argList, 0 )

   # Retrieve the next param which should be a symbol
   try:
      nextParam = lambdaListAST[paramNum]
   except IndexError:
      # There are no more params to process. So argNum should be == or > argListLength.
      # So, if argNum < argListLength, then there are still unprocessed args.
      if argNum < argListLength:
         raise LispArgBindingError( f'Too many arguments.  Received {argListLength}.' )
      return          # All params used up.  Return gracefully
   if not isinstance(nextParam, LSymbol):
      raise LispArgBindingError( f"Param {paramNum} expected to be a symbol." )

   if nextParam == '&OPTIONAL':
      paramNum, argNum = _bindOptionalArgs( env, lambdaListAST, paramNum+1, argList, argNum, evalFn )

      # Retrieve the next param which should be a symbol
      try:
         nextParam = lambdaListAST[paramNum]
      except IndexError:
         if argNum < argListLength:
            raise LispArgBindingError( f'Too many arguments.  Received {argListLength}.' )
         return          # All params used up.  Return gracefully
      if not isinstance(nextParam, LSymbol):
         raise LispArgBindingError( f"Param {paramNum} expected to be a symbol." )

   if nextParam == '&REST':
      paramNum, argNum = _bindRestArgs( env, lambdaListAST, paramNum+1, argList, argNum )

      # Retrieve the next param which should be a symbol
      try:
         nextParam = lambdaListAST[paramNum]
      except IndexError:
         return          # All params used up.  Return gracefully
      if not isinstance(nextParam, LSymbol):
         raise LispArgBindingError( f"Param {paramNum} expected to be a symbol." )

   if nextParam == '&KEY':
      paramNum, argNum = _bindKeyArgs( env, lambdaListAST, paramNum+1, argList, argNum, evalFn )

      # Retrieve the next param which should be a symbol
      try:
         nextParam = lambdaListAST[paramNum]
      except IndexError:
         return          # All params used up.  Return gracefully
      if not isinstance(nextParam, LSymbol):
         raise LispArgBindingError( f"Param {paramNum} expected to be a symbol." )

   if nextParam == '&AUX':
      paramNum, argNum = _bindAuxArgs( env, lambdaListAST, paramNum+1, argList, argNum, evalFn )
   elif nextParam.startswith('&'):
      _KNOWN_KEYWORDS = {'&OPTIONAL', '&REST', '&KEY', '&AUX', '&ALLOW-OTHER-KEYS'}
      if nextParam.strval in _KNOWN_KEYWORDS:
         raise LispArgBindingError( f'{nextParam} is misplaced in the lambda list.  Valid order: &optional, &rest, &key, &aux.' )
      else:
         raise LispArgBindingError( f'Unknown lambda list keyword: {nextParam}.' )

   if paramNum < paramListLength:
      raise LispArgBindingError( f'Unexpected content at position {paramNum} in lambda list.' )


def _bindPositionalArgs( env: Environment, lambdaListAST: list[Any], paramNum: int, argList: Sequence[Any], argNum: int ) -> tuple[int, int]:
   paramListLength = len(lambdaListAST)

   while paramNum < paramListLength:
      # Get the next parameter name.  Ensure it's a symbol but doesn't start with '&'.
      paramName = lambdaListAST[paramNum]
      if not isinstance(paramName, LSymbol):
         raise LispArgBindingError( f"Positional param {paramNum} expected to be a symbol." )
      if paramName.startswith('&'):
         break

      # Get the next argument value
      try:
         argVal = argList[argNum]
      except IndexError:
         raise LispArgBindingError( "Too few positional arguments." )

      # Bind paramName to argVal
      env.bindLocal( paramName.strval, argVal )

      # Prepare for the next iteration
      paramNum += 1
      argNum += 1

   return paramNum, argNum


def _bindOptionalArgs( env: Environment, lambdaListAST: list[Any], paramNum: int, argList: Sequence[Any], argNum: int,
                       evalFn: Callable ) -> tuple[int, int]:
   '''Syntax:  &optional {var | (var [initform [svar]])}*'''
   paramListLength = len(lambdaListAST)
   argListLength = len(argList)

   # Loop over the optional parameters and arguments
   while (paramNum < paramListLength):
      paramSpec = lambdaListAST[paramNum]

      # Extract the next parameter's values
      if isinstance( paramSpec, LSymbol ):
         if paramSpec.startswith('&'):
            break
         varName = paramSpec
         initForm = list( )
         svarName = None
      elif isinstance(paramSpec, list):
         paramSpecLen = len(paramSpec)
         if paramSpecLen == 1:
            varName = paramSpec[0]
            initForm = list()
            svarName = None
         elif paramSpecLen == 2:
            varName, initForm = paramSpec
            svarName = None
         elif paramSpecLen == 3:
            varName, initForm, svarName = paramSpec
         else:
            raise LispArgBindingError( 'Parameter spec following &OPTIONAL must be a list of (<variable> [<defaultvalue> [<svar>]] ).' )

         if not isinstance(varName, LSymbol):
            raise LispArgBindingError( 'Parameter variable in &OPTIONAL spec must be a symbol.' )
         if varName.startswith('&'):
            raise LispArgBindingError( f'Lambda list keyword {varName} cannot be used as a variable name in &OPTIONAL spec.' )
         if svarName and (not isinstance(svarName, LSymbol)):
            raise LispArgBindingError( f'Parameter svar following {varName} must be a symbol.' )
         if isinstance(svarName, LSymbol) and svarName.startswith('&'):
            raise LispArgBindingError( f'Lambda list keyword {svarName} cannot be used as a supplied-p variable in &OPTIONAL spec.' )
      else:
         raise LispArgBindingError( 'Parameter spec following &OPTIONAL must be a <variable> or a list of (<variable> <defaultvalue>). ' )
      paramNum += 1

      # Extract the next argument's value
      originalInitForm = initForm                  # save before potential overwrite
      if argNum < argListLength:
         initForm = argList[argNum]

      if (argNum >= argListLength) or (isinstance(initForm, LSymbol) and (initForm.startswith(':'))):
         initForm = evalFn( env, originalInitForm )   # evaluate the spec default
         svarVal = list()   # Nil, False
      else:
         argNum += 1
         svarVal = env.lookupGlobal('T')   # T, True

      # Bind the parameters
      env.bindLocal( varName.strval, initForm )

      if svarName:
         env.bindLocal( svarName.strval, svarVal )

   return paramNum, argNum


def _bindRestArgs( env: Environment, lambdaListAST: list[Any], paramNum: int, argList: Sequence[Any], argNum: int ) -> tuple[int, int]:
   '''Syntax:  &rest var'''
   try:
      paramName = lambdaListAST[paramNum]
   except IndexError:
      raise LispArgBindingError( f'Param name expected after &rest.' )

   if not isinstance(paramName, LSymbol ) or paramName.startswith('&'):
      raise LispArgBindingError( 'Symbol expected after &rest.' )

   theRestArgs = argList[argNum:]
   env.bindLocal( paramName.strval, list(theRestArgs) )

   return paramNum + 1, argNum


def _bindKeyArgs( env: Environment, lambdaListAST: list[Any], paramNum: int, argList: Sequence[Any], argNum: int,
                  evalFn: Callable ) -> tuple[int, int]:
   '''syntax:  &key {var | ( {var | ( keyword var )} [initForm [svar]])}* [&allow-other-keys]'''
   paramListLength = len(lambdaListAST)
   argListLength = len(argList)

   keysDict = {}         # keyStr -> (varName, svarName, initForm)
   keyParamOrder = []    # ordered list of keyStr for incremental evaluation

   # ---- Phase 1: Parse key parameter specs (store initForms, don't evaluate yet) ----
   while paramNum < paramListLength:
      paramSpec = lambdaListAST[paramNum]
      if isinstance(paramSpec, LSymbol):
         if paramSpec.startswith('&'):
            break
         keyName = paramSpec
         varName = paramSpec
         initForm = list()
         svarName = None
      elif isinstance(paramSpec, list):
         if len(paramSpec) == 0:
            raise LispArgBindingError( f'Empty parameter spec () in &KEY lambda list.' )
         keyVarSpec, *initFormSpec = paramSpec

         # Extract the keyName and varName from keyVarSpec
         if isinstance(keyVarSpec, LSymbol):
            if keyVarSpec.startswith('&'):
               raise LispArgBindingError( f'Lambda list keyword {keyVarSpec} cannot be used as a variable name in &KEY spec.' )
            keyName = keyVarSpec
            varName = keyVarSpec
         elif isinstance(keyVarSpec, list):
            try:
               keyName, varName = keyVarSpec
            except ValueError:
               raise LispArgBindingError( f'Key Var pair following &KEY must contain exactly two elements.' )
            if not isinstance(keyName, LSymbol) or not keyName.startswith(':'):
               raise LispArgBindingError( f'The key in a &KEY (keyword var) pair must be a keyword symbol (e.g. :mykey).' )
            if not isinstance(varName, LSymbol):
               raise LispArgBindingError( f'Variable in &KEY (keyword var) pair must be a symbol.' )
         else:
            raise LispArgBindingError( f'&KEY key/var spec must be either a symbol or a list (:keySymbol varSymbol).' )

         # Extract initForm and svarName from initFormSpec
         initFormSpecLen = len(initFormSpec)
         if initFormSpecLen == 0:
            initForm = list()
            svarName = None
         elif initFormSpecLen == 1:
            initForm = initFormSpec[0]
            svarName = None
         elif initFormSpecLen == 2:
            initForm, svarName = initFormSpec
            if svarName and not isinstance(svarName, LSymbol):
               raise LispArgBindingError( f'svar for &KEY parameter {varName} must be a symbol.' )
            if isinstance(svarName, LSymbol) and svarName.startswith('&'):
               raise LispArgBindingError( f'Lambda list keyword {svarName} cannot be used as a supplied-p variable in &KEY spec.' )
         else:
            raise LispArgBindingError( f'Too many arguments specified in a parameter keyword initialization list.' )
      else:
         raise LispArgBindingError( f'Parameter spec following &KEY must be a symbol or a list.' )

      keyStr = keyName.strval[1:] if keyName.startswith(':') else keyName.strval
      keysDict[keyStr] = (varName, svarName, initForm)
      keyParamOrder.append( keyStr )
      paramNum += 1

   # ---- Phase 2: Handle &allow-other-keys in lambda list ----
   allowOtherKeys = False
   if (paramNum < paramListLength):
      nextParam = lambdaListAST[paramNum]
      if isinstance(nextParam, LSymbol) and (nextParam == '&ALLOW-OTHER-KEYS'):
         allowOtherKeys = True
         paramNum += 1

   # ---- Phase 3: Scan supplied keyword arguments ----
   # Pre-scan for :allow-other-keys in args (CL 3.4.1.4.1)
   scanIdx = argNum
   while scanIdx + 1 < argListLength:
      scanArg = argList[scanIdx]
      if isinstance(scanArg, LSymbol) and scanArg.strval == ':ALLOW-OTHER-KEYS':
         if argList[scanIdx + 1] != list():   # non-NIL value enables it
            allowOtherKeys = True
         break
      scanIdx += 2

   # Collect supplied keyword arguments (first occurrence wins per CL 3.4.1.4.1)
   suppliedArgs = {}    # keyStr -> argVal
   while argNum < argListLength:
      keyArg = argList[argNum]
      if (not isinstance(keyArg, LSymbol)) or (not keyArg.startswith(':')):
         raise LispArgBindingError( f'Keyword expected, found {keyArg}.' )
      keyArgStr = keyArg.strval[1:]  # Strip the leading colon
      argNum += 1

      try:
         argVal = argList[argNum]
      except IndexError:
         raise LispArgBindingError( f'Keyword {keyArgStr} expected to be followed by a value.' )
      argNum += 1

      # :allow-other-keys is always accepted; skip binding if not a declared key param
      if keyArgStr == 'ALLOW-OTHER-KEYS' and keyArgStr not in keysDict:
         continue

      if (not allowOtherKeys) and (keyArgStr not in keysDict):
         raise LispArgBindingError( f'Unexpected keyword found {keyArgStr}.' )

      # First occurrence wins; skip unknown keys when allowOtherKeys
      if keyArgStr in keysDict and keyArgStr not in suppliedArgs:
         suppliedArgs[keyArgStr] = argVal

   # ---- Phase 4: Bind key parameters incrementally (CL 3.4.1 eval order) ----
   nilVal = list()
   tVal = env.lookupGlobal('T')
   for keyStr in keyParamOrder:
      varName, svarName, initForm = keysDict[keyStr]
      if keyStr in suppliedArgs:
         env.bindLocal( varName.strval, suppliedArgs[keyStr] )
         if svarName:
            env.bindLocal( svarName.strval, tVal )
      else:
         env.bindLocal( varName.strval, evalFn( env, initForm ) )
         if svarName:
            env.bindLocal( svarName.strval, nilVal )

   return paramNum, argNum


def _bindAuxArgs( env: Environment, lambdaListAST: list[Any], paramNum: int, argList: Sequence[Any], argNum: int,
                  evalFn: Callable ) -> tuple[int, int]:
   '''Syntax:  &aux {var | (var [initForm])}*
   These are not really arguments.  At least: these parameters get no corresponding arguments.
   These parameters are strictly local variables for the function.'''
   paramListLength = len(lambdaListAST)

   while (paramNum < paramListLength):
      paramSpec = lambdaListAST[paramNum]

      # Extract the next parameter's name and value
      if isinstance( paramSpec, LSymbol ):
         if paramSpec.startswith('&'):
            raise LispArgBindingError( f'{paramSpec} occurs after &AUX.' )
         varName = paramSpec
         initForm = list( )
      elif isinstance(paramSpec, list):
         paramSpecLen = len(paramSpec)
         if paramSpecLen == 1:
            varName = paramSpec[0]
            initForm = list()
         elif paramSpecLen == 2:
            varName, initForm = paramSpec
         else:
            raise LispArgBindingError( 'Parameter spec following &AUX must be a list of (<variable> [<defaultvalue>]).' )

         if not isinstance(varName, LSymbol) or varName.startswith('&'):
            raise LispArgBindingError( 'Parameter spec following &AUX must be a list of (<variable> [<defaultvalue>]).' )

         initForm = evalFn( env, initForm )
      else:
         raise LispArgBindingError( 'Parameter spec following &AUX must be a <variable> or a list of (<variable> [<defaultvalue>]).' )

      # Bind the parameters
      env.bindLocal( varName.strval, initForm )

      # Prepare for the next iteration
      paramNum += 1

   return paramNum, argNum

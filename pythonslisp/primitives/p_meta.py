from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.LispAST import LSymbol, LFunction, LMacro
from pythonslisp.LispAST import L_T, L_NIL
from pythonslisp.LispExceptions import LispRuntimeFuncError
from pythonslisp.LispInterpreter import LispInterpreter


def register(primitive) -> None:

   @primitive( 'defmacro', '<symbol> <lambda-list> &optional <sexpr1> <sexpr2> ...', specialForm=True )
   def LP_defmacro( env: Environment, *args ) -> Any:
      """Defines and returns a new globally named macro.  The first expr of the body
can be an optional documentation string."""
      try:
         fnName, funcParams, *funcBody = args
      except ValueError:
         raise LispRuntimeFuncError( LP_defmacro, "3 or more arguments expected." )

      if not isinstance(fnName, LSymbol):
         raise LispRuntimeFuncError( LP_defmacro, "Argument 1 expected to be a symbol." )

      if not isinstance(funcParams, list):
         raise LispRuntimeFuncError( LP_defmacro, "Argument 2 expected to be a list of params." )

      if len(funcBody) < 1:
         raise LispRuntimeFuncError( LP_defmacro, "At least one body expression expected." )

      if isinstance(funcBody[0], str):
         docString = funcBody[0]
         funcBody = funcBody[1:]
      else:
         docString = ''

      if len(funcBody) < 1:
         raise LispRuntimeFuncError( LP_defmacro, "At least one body expression expected after docstring." )

      theFunc = LMacro( fnName, funcParams, docString, funcBody )
      return env.bindGlobal( fnName.strval, theFunc )

   @primitive( 'macroexpand', '\'(<macroName> <arg1> <arg2> ...)' )
   def LP_macroexpand( env: Environment, *args ) -> Any:
      """Fully expands a macro call at the top level, looping until the form is no
longer headed by a macro.  Non-macro and non-list forms are returned unchanged."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_macroexpand, 'Exactly 1 argument expected.' )

      from pythonslisp.LispExpander import LispExpander
      form = args[0]
      while isinstance(form, list) and len(form) >= 1:
         primary = form[0]
         if not isinstance(primary, LSymbol):
            break
         try:
            macroDef = env.lookup( primary.strval )
         except KeyError:
            break
         if not isinstance( macroDef, LMacro ):
            break
         form = LispExpander._expandMacroCall( env, macroDef, form[1:] )
      return form

   @primitive( 'macroexpand-1', '\'(<macroName> <arg1> <arg2> ...)' )
   def LP_macroexpand_1( env: Environment, *args ) -> Any:
      """Expands a macro call exactly once.  Returns the form unchanged if it is
not a macro call."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_macroexpand_1, 'Exactly 1 argument expected.' )

      form = args[0]
      if not isinstance(form, list) or len(form) < 1:
         return form
      primary = form[0]
      if not isinstance(primary, LSymbol):
         return form
      try:
         macroDef = env.lookup( primary.strval )
      except KeyError:
         return form
      if not isinstance( macroDef, LMacro ):
         return form

      from pythonslisp.LispExpander import LispExpander
      return LispExpander._expandMacroCall( env, macroDef, form[1:] )

   @primitive( 'defsetf-internal', '<accessor-symbol> <field-symbol>' )
   def LP_defsetf_internal( env: Environment, *args ) -> Any:
      """Register a struct field accessor as a valid setf target."""
      if len(args) != 2:
         raise LispRuntimeFuncError( LP_defsetf_internal, '2 arguments expected.' )
      accessor_sym, field_sym = args
      if not isinstance(accessor_sym, LSymbol) or not isinstance(field_sym, LSymbol):
         raise LispRuntimeFuncError( LP_defsetf_internal, 'Both arguments must be symbols.' )
      LispInterpreter._setf_registry[accessor_sym.strval] = field_sym.strval
      return accessor_sym

   @primitive( 'setf', '<symbol> <sexpr>', specialForm=True )
   def LP_setf( env: Environment, *args ) -> Any:
      """Updates a variable's value, returns value.  The search for the variable begins
locally and proceeds to search ever less local scopes until the global scope
is searched.  If the variable is located in this search its value is updated.
If it's not located a new global is defined and set the value.

Alternate usage: (setf (at <keyOrIndex> <mapOrList>) <newValue>)"""
      numArgs = len(args)

      if numArgs == 0:
         raise LispRuntimeFuncError( LP_setf, 'At least 2 arguments expected.' )

      if (numArgs % 2) != 0:
         raise LispRuntimeFuncError( LP_setf, f'An even number of arguments is expected.  Received {numArgs}.' )

      rval = list()
      while len(args) > 0:
         lval,rval,*args = args

         rval = LispInterpreter._lEval(env, rval)

         # Case where the lvalue is a symbol form:  (setf variable newValue)
         if isinstance(lval, LSymbol ):
            sym = lval
            if isinstance(rval,(LFunction,LMacro)) and (rval.name == ''):
               rval.name = sym.strval
            sym = sym.strval

            theSymTab = env.findDef( sym )
            if theSymTab:
               theSymTab.bindLocal( sym, rval )
            else:
               env.bindGlobal( sym, rval )

         # Case where the lvalue is an 'at' form:  (setf (at key collection) newValue)
         elif isinstance(lval, list):
            if len(lval) == 0:
               raise LispRuntimeFuncError( LP_setf, 'lvalue cannot be NIL or ().' )

            prim = lval[0]
            if prim == 'AT':
               try:
                  prim, keyOrIndex, mapOrLst = lval
               except ValueError:
                  raise LispRuntimeFuncError( LP_setf, 'lvalue \'at\' form expected 3 elements.' )

               theSelector  = LispInterpreter._lEval(env, keyOrIndex)
               theContainer = LispInterpreter._lEval(env, mapOrLst)

               if not isinstance(theContainer, (list, dict)):
                  raise LispRuntimeFuncError( LP_setf, 'Invalid container type following \'AT\' primitive.  Expected list or map.' )

               try:
                  theContainer[theSelector] = rval
               except (KeyError, IndexError, TypeError):
                  raise LispRuntimeFuncError( LP_setf, f'Invalid key or index supplied to \'AT\' form.  Received {theSelector}.' )
            elif isinstance(prim, LSymbol) and prim.strval in LispInterpreter._setf_registry:
               field_key = LispInterpreter._setf_registry[prim.strval]
               if len(lval) != 2:
                  raise LispRuntimeFuncError( LP_setf,
                     f'Struct accessor setf expects exactly 1 instance argument, got {len(lval)-1}.' )
               instance = LispInterpreter._lEval(env, lval[1])
               if not isinstance(instance, dict):
                  raise LispRuntimeFuncError( LP_setf,
                     f'Expected a struct instance as argument to ({prim.strval} ...).' )
               instance[field_key] = rval
            else:
               raise LispRuntimeFuncError( LP_setf, 'Unrecognized setf place.' )

      return rval

   @primitive( 'undef!', '<symbol>', specialForm=True )
   def LP_undef( env: Environment, *args ) -> Any:
      """Undefines the global definition for a symbol and returns nil."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_undef, '1 argument expected.' )
      key = args[0]
      if not isinstance(key, LSymbol):
         raise LispRuntimeFuncError( LP_undef, 'Argument expected to be a symbol.' )
      env.getGlobalEnv().unbind( key.strval )
      return L_NIL

   @primitive( 'symtab!', '' )
   def LP_symtab( env: Environment, *args ) -> Any:
      """Prints the entire environment stack and returns nil.  Each scope is printed
in a separate list and begins on a new line.  The local scope is first; global
is last."""
      if len(args) > 0:
         raise LispRuntimeFuncError( LP_symtab, '0 arguments expected.' )

      print( 'Symbol Table Dump:  Inner-Most Scope First')
      print( '------------------------------------------')
      scope: (Environment | None) = env
      while scope:
         symList = scope.localSymbols()
         print( symList )
         scope = scope.parentEnv( )

      return L_NIL

   @primitive( 'trace', '&rest <fn-names>', specialForm=True )
   def LP_trace( env: Environment, *args ) -> Any:
      """Enables call tracing for the named functions and returns the updated
trace list.  With no arguments, returns the list of currently traced functions."""
      if len(args) == 0:
         return [ LSymbol(name) for name in sorted(LispInterpreter._traced) ]
      for sym in args:
         if not isinstance(sym, LSymbol):
            raise LispRuntimeFuncError( LP_trace, 'Arguments must be symbols.' )
         LispInterpreter._traced.add( sym.strval )
      LispInterpreter._set_trace_hook()
      return [ LSymbol(name) for name in sorted(LispInterpreter._traced) ]

   @primitive( 'untrace', '&rest <fn-names>', specialForm=True )
   def LP_untrace( env: Environment, *args ) -> Any:
      """Disables call tracing for the named functions and returns the updated
trace list.  With no arguments, clears all named function tracing."""
      if len(args) == 0:
         LispInterpreter._traced.clear()
      else:
         for sym in args:
            if not isinstance(sym, LSymbol):
               raise LispRuntimeFuncError( LP_untrace, 'Arguments must be symbols.' )
            LispInterpreter._traced.discard( sym.strval )
      LispInterpreter._set_trace_hook()
      return [ LSymbol(name) for name in sorted(LispInterpreter._traced) ]

   @primitive( 'boundp', '<symbol>' )
   def LP_boundp( env: Environment, *args ) -> Any:
      """Returns T if the symbol has a value bound in the environment, NIL otherwise."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_boundp, '1 argument expected.' )
      sym = args[0]
      if not isinstance( sym, LSymbol ):
         raise LispRuntimeFuncError( LP_boundp, 'Argument 1 must be a Symbol.' )
      try:
         env.lookup( sym.strval )
         return L_T
      except KeyError:
         return L_NIL

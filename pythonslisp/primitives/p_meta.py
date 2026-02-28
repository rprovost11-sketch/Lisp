from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.LispAST import LSymbol, LMacro, LContinuation, LCallable
from pythonslisp.LispAST import L_T, L_NIL
from pythonslisp.LispContext import LispContext
from pythonslisp.LispExceptions import LispRuntimeFuncError, ContinuationInvoked
from pythonslisp.LispExpander import LispExpander


def register(primitive) -> None:

   @primitive( 'defmacro', 'symbol lambda-list &rest body', specialForm=True )
   def LP_defmacro( ctx: LispContext, env: Environment, *args ) -> Any:
      """Defines and returns a new globally named macro.  The first sexpr of the body
can be an optional documentation string."""
      fnName, funcParams, *funcBody = args   # analyzer guarantees structure
      if funcBody and isinstance(funcBody[0], str):
         docString = funcBody[0]
         funcBody  = funcBody[1:]
      else:
         docString = ''
      theFunc = LMacro( fnName, funcParams, docString, funcBody )
      return env.bindGlobal( fnName.strval, theFunc )

   @primitive( 'macroexpand', '\'(macroName &rest args)',
               min_args=1, max_args=1, arity_msg='Exactly 1 argument expected.' )
   def LP_macroexpand( ctx: LispContext, env: Environment, *args ) -> Any:
      """Fully expands a macro call at the top level, looping until the form is no
longer headed by a macro.  Non-macro and non-list forms are returned unchanged."""

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
         form = LispExpander._expandMacroCall( ctx, env, macroDef, form[1:] )
      return form

   @primitive( 'macroexpand-1', '\'(macroName &rest args)',
               min_args=1, max_args=1, arity_msg='Exactly 1 argument expected.' )
   def LP_macroexpand_1( ctx: LispContext, env: Environment, *args ) -> Any:
      """Expands a macro call exactly once.  Returns the form unchanged if it is
not a macro call."""

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

      return LispExpander._expandMacroCall( ctx, env, macroDef, form[1:] )

   @primitive( 'defsetf-internal', 'accessor-symbol field-symbol',
               min_args=2, max_args=2, arity_msg='2 arguments expected.' )
   def LP_defsetf_internal( ctx: LispContext, env: Environment, *args ) -> Any:
      """Register a struct field accessor as a valid setf target."""
      accessor_sym, field_sym = args
      if not isinstance(accessor_sym, LSymbol) or not isinstance(field_sym, LSymbol):
         raise LispRuntimeFuncError( LP_defsetf_internal, 'Both arguments must be symbols.' )
      ctx.setfRegistry[accessor_sym.strval] = field_sym.strval
      return accessor_sym

   @primitive( 'set-accessor!', 'accessor-symbol instance newValue',
               min_args=3, max_args=3, arity_msg='3 arguments expected.' )
   def LP_set_accessor( ctx: LispContext, env: Environment, *args ) -> Any:
      """Internal: write a struct field value via the defsetf registry."""
      accessor, instance, newval = args
      if not isinstance( accessor, LSymbol ):
         raise LispRuntimeFuncError( LP_set_accessor, 'Argument 1 must be a symbol.' )
      field_key = ctx.setfRegistry.get( accessor.strval )
      if field_key is None:
         raise LispRuntimeFuncError( LP_set_accessor,
                                     f'No setf expander registered for {accessor.strval}.' )
      if not isinstance( instance, dict ):
         raise LispRuntimeFuncError( LP_set_accessor, 'Argument 2 must be a struct instance.' )
      instance[ field_key ] = newval
      return newval

   @primitive( 'setq', 'symbol1 sexpr1 symbol2 sexpr2 ...', specialForm=True )
   def LP_setq( ctx: LispContext, env: Environment, *args ) -> Any:
      """Updates one or more variables' values', returns value.  The search for
the variable begins locally and proceeds to search ever less local scopes until
the global scope is searched.  If the variable is located in this search its
value is updated.  If it's not located a new global is defined and set the
value.

Alternate usage: (setf (at keyOrIndex dictOrList) newValue)"""
      raise LispRuntimeFuncError( LP_setq, 'Handled by main eval loop.' )

   @primitive( 'makunbound', 'symbol',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_makunbound( ctx: LispContext, env: Environment, *args ) -> Any:
      """Undefines the global definition for a symbol and returns nil.
The argument is evaluated: (makunbound 'x) unbinds X."""
      key = args[0]
      if not isinstance(key, LSymbol):
         raise LispRuntimeFuncError( LP_makunbound, 'Argument expected to be a symbol.' )
      env.getGlobalEnv().unbind( key.strval )
      return L_NIL

   @primitive( 'symtab!', '',
               min_args=0, max_args=0, arity_msg='0 arguments expected.' )
   def LP_symtab( ctx: LispContext, env: Environment, *args ) -> Any:
      """Prints the entire environment stack and returns nil.  Each scope is printed
in a separate list and begins on a new line.  The local scope is first; global
is last."""
      print( 'Symbol Table Dump:  Inner-Most Scope First')
      print( '------------------------------------------')
      scope: (Environment | None) = env
      while scope:
         symList = scope.localSymbols()
         print( symList )
         scope = scope.parentEnv( )

      return L_NIL

   @primitive( 'trace', '&rest fn-names', specialForm=True )
   def LP_trace( ctx: LispContext, env: Environment, *args ) -> Any:
      """Enables call tracing for the named functions and returns the updated
trace list.  With no arguments, returns the list of currently traced functions."""
      tracer = ctx.tracer
      if len(args) == 0:
         return [ LSymbol(name) for name in sorted(tracer.getFnsToTrace()) ]
      for sym in args:
         if not isinstance(sym, LSymbol):
            raise LispRuntimeFuncError( LP_trace, 'Arguments must be symbols.' )
         tracer.addFnTrace( sym.strval )
      return [ LSymbol(name) for name in sorted(tracer.getFnsToTrace()) ]

   @primitive( 'untrace', '&rest fn-names', specialForm=True )
   def LP_untrace( ctx: LispContext, env: Environment, *args ) -> Any:
      """Disables call tracing for the named functions and returns the updated
trace list.  With no arguments, clears all named function tracing."""
      tracer = ctx.tracer
      if len(args) == 0:
         tracer.removeAll()
      else:
         for sym in args:
            if not isinstance(sym, LSymbol):
               raise LispRuntimeFuncError( LP_untrace, 'Arguments must be symbols.' )
            tracer.removeFnTrace( sym.strval )
      return [ LSymbol(name) for name in sorted(tracer.getFnsToTrace()) ]

   @primitive( 'call/cc', 'procedure',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_callcc( ctx: LispContext, env: Environment, *args ) -> Any:
      """Calls procedure with one argument: an escape continuation object.
Invoking the continuation with a value causes call/cc to immediately return
that value, unwinding any intermediate computation.  Only upward (escape)
continuations are supported; invoking a stale continuation is an error."""
      proc = args[0]
      if not isinstance( proc, LCallable ):
         raise LispRuntimeFuncError( LP_callcc, 'Argument must be a callable.' )
      if proc.specialForm:
         raise LispRuntimeFuncError( LP_callcc, 'Argument may not be a special form.' )

      token = object()          # unique identity token for this continuation
      cont  = LContinuation( token )

      try:
         return ctx.lApply( env, proc, [cont] )
      except ContinuationInvoked as ci:
         if ci.token is token:
            return ci.value
         raise   # re-raise so an outer call/cc can catch it

   @primitive( 'boundp', 'symbol',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_boundp( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns T if the symbol has a value bound in the environment, NIL otherwise."""
      sym = args[0]
      if not isinstance( sym, LSymbol ):
         raise LispRuntimeFuncError( LP_boundp, 'Argument 1 must be a Symbol.' )
      try:
         env.lookup( sym.strval )
         return L_T
      except KeyError:
         return L_NIL

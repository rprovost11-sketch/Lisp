import time
from typing import Any

#from pythonslisp.ltk.EnvironmentBase import EnvironmentBase
from pythonslisp.Environment import Environment
from pythonslisp.AST import LSymbol, LMacro, LContinuation, LCallable
from pythonslisp.AST import L_T, L_NIL, prettyPrint, prettyPrintSExpr
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimePrimError, ContinuationInvoked
from pythonslisp.Expander import Expander
from pythonslisp.primitives import LambdaListMode


def register(primitive) -> None:

   @primitive( 'defmacro', '(symbol lambda-list &rest body)', preEvalArgs=False )
   def LP_defmacro( ctx: Context, env: Environment, *args ) -> Any:
      """Defines and returns a new globally named macro.  The first sexpr of the body
can be an optional documentation string."""
      fnName, funcParams, *funcBody = args   # analyzer guarantees structure
      if funcBody and isinstance(funcBody[0], str):
         docString = funcBody[0]
         funcBody  = funcBody[1:]
      else:
         docString = ''
      theFunc = LMacro( fnName, funcParams, docString, funcBody )
      return env.bindGlobal( fnName.name, theFunc )

   @primitive( 'macroexpand', '(\'form)',
               mode=LambdaListMode.DOC_ONLY, min_args=1, max_args=1 )
   def LP_macroexpand( ctx: Context, env: Environment, *args ) -> Any:
      """Fully expands a macro call at the top level, looping until the form is no
longer headed by a macro.  Non-macro and non-list forms are returned unchanged."""

      form = args[0]
      while isinstance(form, list) and len(form) >= 1:
         head = form[0]
         if not isinstance(head, LSymbol):
            break
         try:
            macroDef = env.lookup( head.name )
         except KeyError:
            break
         if not isinstance( macroDef, LMacro ):
            break
         form = Expander._expandMacroCall( ctx, env, macroDef, form[1:] )
      return form

   @primitive( 'macroexpand-1', '(\'form)',
               mode=LambdaListMode.DOC_ONLY, min_args=1, max_args=1 )
   def LP_macroexpand_1( ctx: Context, env: Environment, *args ) -> Any:
      """Expands a macro call exactly once.  Returns the form unchanged if it is
not a macro call."""

      form = args[0]
      if not isinstance(form, list) or len(form) < 1:
         return form
      head = form[0]
      if not isinstance(head, LSymbol):
         return form
      try:
         macroDef = env.lookup( head.name )
      except KeyError:
         return form
      if not isinstance( macroDef, LMacro ):
         return form

      return Expander._expandMacroCall( ctx, env, macroDef, form[1:] )

   @primitive( 'defsetf-internal', '(accessor-symbol field-symbol)' )
   def LP_defsetf_internal( ctx: Context, env: Environment, *args ) -> Any:
      """Register a struct field accessor as a valid setf target."""
      accessor_sym, field_sym = args
      if not isinstance(accessor_sym, LSymbol) or not isinstance(field_sym, LSymbol):
         raise LRuntimePrimError( LP_defsetf_internal, 'Both arguments must be symbols.' )
      ctx.setfRegistry[accessor_sym.name] = field_sym.name
      return accessor_sym

   @primitive( 'set-accessor!', '(accessor-symbol instance newValue)' )
   def LP_set_accessor( ctx: Context, env: Environment, *args ) -> Any:
      """Internal: write a struct field value via the defsetf registry."""
      accessor, instance, newval = args
      if not isinstance( accessor, LSymbol ):
         raise LRuntimePrimError( LP_set_accessor, 'Argument 1 must be a symbol.' )
      field_key = ctx.setfRegistry.get( accessor.name )
      if field_key is None:
         raise LRuntimePrimError( LP_set_accessor,
                                     f'No setf expander registered for {accessor.name}.' )
      if not isinstance( instance, dict ):
         raise LRuntimePrimError( LP_set_accessor, 'Argument 2 must be a struct instance.' )
      instance[ field_key ] = newval
      return newval

   @primitive( 'setq', '(symbol1 sexpr1 symbol2 sexpr2 ...)',
               mode=LambdaListMode.DOC_ONLY, preEvalArgs=False )
   def LP_setq( ctx: Context, env: Environment, *args ) -> Any:
      """Updates one or more variables' values', returns value.  The search for
the variable begins locally and proceeds to search ever less local scopes until
the global scope is searched.  If the variable is located in this search its
value is updated.  If it's not located a new global is defined and set the
value.

Alternate usage: (setf (at keyOrIndex dictOrList) newValue)"""
      raise LRuntimePrimError( LP_setq, 'Handled by main eval loop.' )

   @primitive( 'makunbound', '(symbol)' )
   def LP_makunbound( ctx: Context, env: Environment, *args ) -> Any:
      """Undefines the global definition for a symbol and returns nil.
The argument is evaluated: (makunbound 'x) unbinds X."""
      key = args[0]
      if not isinstance(key, LSymbol):
         raise LRuntimePrimError( LP_makunbound, 'Argument expected to be a symbol.' )
      env.getGlobalEnv().unbind( key.name )
      return L_NIL

   @primitive( 'symtab!', '()', max_args=0 )
   def LP_symtab( ctx: Context, env: Environment, *args ) -> Any:
      """Prints the entire environment stack and returns nil.  Each scope is printed
in a separate list and begins on a new line.  The local scope is first; global
is last."""
      print( 'Symbol Table Dump:  Inner-Most Scope First')
      print( '------------------------------------------')
      scope: (Environment | None) = env
      while scope:
         symList = scope.localSymbols()
         print( '   ', prettyPrint( symList ) )
         scope = scope.parentEnv( )
      return L_NIL

   @primitive( 'trace', '(&rest fn-names)', preEvalArgs=False )
   def LP_trace( ctx: Context, env: Environment, *args ) -> Any:
      """Enables call tracing for the named functions and returns the updated
trace list.  With no arguments, returns the list of currently traced functions."""
      tracer = ctx.tracer
      if len(args) == 0:
         return [ LSymbol(name) for name in sorted(tracer.getFnsToTrace()) ]
      for sym in args:
         if not isinstance(sym, LSymbol):
            raise LRuntimePrimError( LP_trace, 'Arguments must be symbols.' )
         tracer.addFnTrace( sym.name )
      return [ LSymbol(name) for name in sorted(tracer.getFnsToTrace()) ]

   @primitive( 'untrace', '(&rest fn-names)', preEvalArgs=False )
   def LP_untrace( ctx: Context, env: Environment, *args ) -> Any:
      """Disables call tracing for the named functions and returns the updated
trace list.  With no arguments, clears all named function tracing."""
      tracer = ctx.tracer
      if len(args) == 0:
         tracer.removeAll()
      else:
         for sym in args:
            if not isinstance(sym, LSymbol):
               raise LRuntimePrimError( LP_untrace, 'Arguments must be symbols.' )
            tracer.removeFnTrace( sym.name )
      return [ LSymbol(name) for name in sorted(tracer.getFnsToTrace()) ]

   @primitive( 'call/cc', '(procedure)' )
   def LP_callcc( ctx: Context, env: Environment, *args ) -> Any:
      """Calls procedure with one argument: an escape continuation object.
Invoking the continuation with a value causes call/cc to immediately return
that value, unwinding any intermediate computation.  Only upward (escape)
continuations are supported; invoking a stale continuation is an error."""
      proc = args[0]
      if not isinstance( proc, LCallable ):
         raise LRuntimePrimError( LP_callcc, 'Argument must be a callable.' )
      if not proc.preEvalArgs:
         raise LRuntimePrimError( LP_callcc, 'Argument may not be a special form.' )

      token = object()          # unique identity token for this continuation
      cont  = LContinuation( token )

      try:
         return ctx.lApply( ctx, env, proc, [cont] )
      except ContinuationInvoked as ci:
         if ci.token is token:
            return ci.value
         raise   # re-raise so an outer call/cc can catch it

   @primitive( 'boundp', '(symbol)' )
   def LP_boundp( ctx: Context, env: Environment, *args ) -> Any:
      """Returns T if the symbol has a value bound in the environment, NIL otherwise."""
      sym = args[0]
      if not isinstance( sym, LSymbol ):
         raise LRuntimePrimError( LP_boundp, 'Argument 1 must be a Symbol.' )
      try:
         env.lookup( sym.name )
         return L_T
      except KeyError:
         return L_NIL

   @primitive( 'gensym', '(&optional x)' )
   def LP_gensym( ctx: Context, env: Environment, *args ) -> Any:
      """Generate and return a new, unique symbol.
If x is a string it is used as the prefix (default \"G\").  The current
value of *gensym-counter* is appended and then *gensym-counter* is
incremented.  If x is a non-negative integer it is used directly as the
numeric suffix and *gensym-counter* is left unchanged."""
      counter = env.lookupGlobalWithDefault( '*GENSYM-COUNTER*', 0 )
      if not args:
         env.bindGlobal( '*GENSYM-COUNTER*', counter + 1 )
         return LSymbol( f'G{counter}' )
      x = args[0]
      if isinstance( x, str ):
         env.bindGlobal( '*GENSYM-COUNTER*', counter + 1 )
         return LSymbol( f'{x}{counter}' )
      if isinstance( x, int ) and not isinstance( x, bool ) and x >= 0:
         return LSymbol( f'G{x}' )
      raise LRuntimePrimError( LP_gensym, 'Argument must be a string prefix or non-negative integer.' )

   _ITUPS = 1_000_000

   @primitive( 'internal-time-units-per-second', '()' )
   def LP_itups( ctx: Context, env: Environment, *args ) -> Any:
      """Returns the number of internal time units per second (1,000,000)."""
      return _ITUPS

   @primitive( 'get-internal-real-time', '()' )
   def LP_get_internal_real_time( ctx: Context, env: Environment, *args ) -> Any:
      """Returns a high-resolution timestamp as an integer in units of
internal-time-units-per-second (microseconds).  Backed by time.perf_counter()."""
      return int( time.perf_counter() * _ITUPS )

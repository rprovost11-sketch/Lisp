from __future__ import annotations
import datetime
import time
from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.AST import LSymbol, LMacro, got_str
from pythonslisp.AST import ( T_SYM, L_NIL, LSymbol,
                               prettyPrint, prettyPrintSExpr )
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimeUsageError
from pythonslisp.Parser import ParseError
from pythonslisp.Expander import Expander
from pythonslisp.extensions import LambdaListMode, primitive
from pythonslisp.extensions.modules import resolve_module_path


@primitive( 'defmacro', '(symbol lambda-list &rest body)' )
def LP_defmacro( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Defines and returns a new globally named macro.  The first sexpr of the body
can be an optional documentation string."""
   raise LRuntimeUsageError( LP_defmacro, 'Handled by CEK machine.' )

@primitive( 'macroexpand', '(\'form)',
            mode=LambdaListMode.DOC_ONLY, min_args=1, max_args=1 )
def LP_macroexpand( ctx: Context, env: Environment, args: list[Any] ) -> Any:
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
      form = Expander.expandMacroCall( ctx, env, macroDef, form[1:] )
   return form

@primitive( 'macroexpand-1', '(\'form)',
            mode=LambdaListMode.DOC_ONLY, min_args=1, max_args=1 )
def LP_macroexpand_1( ctx: Context, env: Environment, args: list[Any] ) -> Any:
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

   return Expander.expandMacroCall( ctx, env, macroDef, form[1:] )

@primitive( 'defsetf-internal', '(accessor-symbol field-symbol)' )
def LP_defsetf_internal( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Register a struct field accessor as a valid setf target."""
   accessor_sym, field_sym = args
   if not isinstance(accessor_sym, LSymbol):
      raise LRuntimeUsageError( LP_defsetf_internal, f'Invalid argument 1. SYMBOL expected{got_str(accessor_sym)}.' )
   if not isinstance(field_sym, LSymbol):
      raise LRuntimeUsageError( LP_defsetf_internal, f'Invalid argument 2. SYMBOL expected{got_str(field_sym)}.' )
   ctx.setfRegistry[accessor_sym.name] = field_sym
   return accessor_sym

@primitive( 'set-accessor!', '(accessor-symbol instance newValue)' )
def LP_set_accessor( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Internal: write a struct field value via the defsetf registry."""
   accessor, instance, newval = args
   if not isinstance( accessor, LSymbol ):
      raise LRuntimeUsageError( LP_set_accessor, f'Invalid argument 1. SYMBOL expected{got_str(accessor)}.' )
   field_key = ctx.setfRegistry.get( accessor.name )
   if field_key is None:
      raise LRuntimeUsageError( LP_set_accessor,
                                  f'No setf expander registered for {accessor.name}.' )
   if not isinstance( instance, dict ):
      raise LRuntimeUsageError( LP_set_accessor, f'Invalid argument 2. STRUCT INSTANCE expected{got_str(instance)}.' )
   instance[ field_key ] = newval
   return newval

@primitive( 'setq', '(symbol1 sexpr1 symbol2 sexpr2 ...)',
            mode=LambdaListMode.DOC_ONLY )
def LP_setq( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Updates one or more variables' values', returns value.  The search for
the variable begins locally and proceeds to search ever less local scopes until
the global scope is searched.  If the variable is located in this search its
value is updated.  If it's not located a new global is defined and set the
value.

Alternate usage: (setf (at keyOrIndex dictOrList) newValue)"""
   raise LRuntimeUsageError( LP_setq, 'Handled by main eval loop.' )

@primitive( 'makunbound', '(symbol)' )
def LP_makunbound( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Undefines the global definition for a symbol and returns nil.
The argument is evaluated: (makunbound 'x) unbinds X."""
   key = args[0]
   if not isinstance(key, LSymbol):
      raise LRuntimeUsageError( LP_makunbound, f'Invalid argument 1. SYMBOL expected{got_str(key)}.' )
   env.getGlobalEnv().unbind( key.name )
   return L_NIL

@primitive( 'symtab!', '()', max_args=0 )
def LP_symtab( ctx: Context, env: Environment, args: list[Any] ) -> Any:
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

@primitive( 'trace', '(&rest fn-names)' )
def LP_trace( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Enables call tracing for the named functions and returns the updated
trace list.  With no arguments, returns the list of currently traced functions."""
   raise LRuntimeUsageError( LP_trace, 'Handled by CEK machine.' )

@primitive( 'untrace', '(&rest fn-names)' )
def LP_untrace( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Disables call tracing for the named functions and returns the updated
trace list.  With no arguments, clears all named function tracing."""
   raise LRuntimeUsageError( LP_untrace, 'Handled by CEK machine.' )

@primitive( 'toggle-global-trace', '()' )
def LP_toggle_global_trace( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Toggles global function tracing on or off.  When global tracing is on,
every call to a user-defined function is reported with its arguments and
return value.  Returns T if tracing is now on, NIL if now off."""
   return T_SYM if ctx.tracer.toggle_global() else L_NIL

@primitive( 'call/cc', '(procedure)' )
def LP_callcc( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Calls procedure with one argument: a first-class continuation object.
Invoking the continuation with a value restores the captured computation
state and delivers that value as the result of the original call/cc expression.
Continuations are fully re-invocable: the same continuation may be called
multiple times, reinstating the saved state each time."""
   raise LRuntimeUsageError( LP_callcc, 'Handled by CEK machine.' )

@primitive( 'boundp', '(symbol)' )
def LP_boundp( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns T if the symbol has a value bound in the environment, NIL otherwise."""
   sym = args[0]
   if not isinstance( sym, LSymbol ):
      raise LRuntimeUsageError( LP_boundp, f'Invalid argument 1. SYMBOL expected{got_str(sym)}.' )
   try:
      env.lookup( sym.name )
      return T_SYM
   except KeyError:
      return L_NIL

@primitive( 'gensym', '(&optional x)' )
def LP_gensym( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Generate and return a new, unique valid symbol.
With no argument, uses prefix \"G\" and appends the current *gensym-counter*,
then increments it.  If x is a string, it is used as the prefix and validated
as a legal symbol prefix; an error is raised if it is not valid.
If x is a symbol, its name is used as the prefix directly.  If x is a
non-negative integer it is used as the numeric suffix directly and
*gensym-counter* is left unchanged."""
   counter = env.lookupGlobalWithDefault( '*GENSYM-COUNTER*', 0 )
   if not args:
      env.bindGlobal( '*GENSYM-COUNTER*', counter + 1 )
      return LSymbol.makeSymbol( f'G{counter}' )
   x = args[0]
   if isinstance( x, str ):
      combined = f'{x}{counter}'
      try:
         sym, _ = ctx.parseOne( combined )
      except ParseError:
         raise LRuntimeUsageError( LP_gensym, 'Invalid argument 1. Valid SYMBOL PREFIX expected.' )
      if not isinstance( sym, LSymbol ):
         raise LRuntimeUsageError( LP_gensym, 'Invalid argument 1. Valid SYMBOL PREFIX expected.' )
      env.bindGlobal( '*GENSYM-COUNTER*', counter + 1 )
      return sym
   elif isinstance( x, LSymbol ):
      env.bindGlobal( '*GENSYM-COUNTER*', counter + 1 )
      return LSymbol.makeSymbol( f'{x.name}{counter}' )
   elif isinstance( x, int ) and not isinstance( x, bool ) and x >= 0:
      return LSymbol.makeSymbol( f'G{x}' )
   raise LRuntimeUsageError( LP_gensym, 'Invalid argument 1. STRING, SYMBOL, or NON-NEGATIVE INTEGER expected.' )

_ITUPS = 1_000_000

@primitive( 'internal-time-units-per-second', '()' )
def LP_itups( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the number of internal time units per second (1,000,000)."""
   return _ITUPS

@primitive( 'get-internal-real-time', '()' )
def LP_get_internal_real_time( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a high-resolution timestamp as an integer in units of
internal-time-units-per-second (microseconds).  Backed by time.perf_counter()."""
   return int( time.perf_counter() * _ITUPS )

@primitive( 'now-string', '(&optional format)' )
def LP_now_string( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the current date and time as a string.
With no argument, returns an ISO 8601 string (e.g. \"2026-03-14T10:30:00.123456\").
With a format string, uses strftime formatting (e.g. \"%Y-%m-%d-%H%M%S\")."""
   now = datetime.datetime.now()
   if len(args) == 0:
      return now.isoformat()
   fmt = args[0]
   if not isinstance( fmt, str ):
      raise LRuntimeUsageError( LP_now_string, 'Invalid argument 1. FORMAT STRING expected.' )
   return now.strftime( fmt )

@primitive( 'load-extension', '(&rest files &key name)',
               mode=LambdaListMode.DOC_ONLY, min_args=0 )
def LP_load_extensions( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Loads each file in the argument list.  A .lisp file is parsed and
evaluated; a .py file uses the @primitive decorator from
pythonslisp.extensions to register its primitives.
All FILES must be string paths.
:name optionally specifies a module or package path (symbol or 'pkg:submod
notation) into which primitives are bound instead of the global environment.
Returns T if all files loaded successfully."""
   # Manually separate file paths from :name keyword argument.
   files    = []
   name_arg = L_NIL
   i = 0
   while i < len(args):
      arg = args[i]
      if isinstance( arg, LSymbol ) and arg.name == ':NAME':
         if i + 1 >= len(args):
            raise LRuntimeUsageError( LP_load_extensions, 'Invalid keyword :name. Value required.' )
         name_arg = args[i + 1]
         i += 2
      else:
         files.append( arg )
         i += 1
   for i, path in enumerate(files, 1):
      if not isinstance( path, str ):
         raise LRuntimeUsageError( LP_load_extensions, f'Invalid argument {i}. STRING PATH expected.' )
   target_env = resolve_module_path( name_arg, env.getGlobalEnv(), ctx, LP_load_extensions )
   for path in files:
      ctx.loadExt( path, target_env )
   return T_SYM

@primitive( 'load-extension-dirs', '(&rest dirs)',
               mode=LambdaListMode.DOC_ONLY, min_args=0 )
def LP_load_extension_dirs( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Loads each directory in the argument list.  Within each directory all
.py files are loaded alphabetically, then all .lisp files alphabetically.
All arguments must be string paths.  Returns T if all directories loaded
successfully."""
   for i, path in enumerate(args, 1):
      if not isinstance( path, str ):
         raise LRuntimeUsageError( LP_load_extension_dirs, f'Invalid argument {i}. STRING PATH expected.' )
   for path in args:
      ctx.loadExtDir( path )
   return T_SYM

@primitive( 'interpreter-reboot', '()' )
def LP_interpreter_reboot( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Reboots the interpreter: clears all user-defined functions and variables,
reloads the standard library, and re-runs startup.lisp.  Equivalent to the
]reboot listener command.  Returns T."""
   ctx.reboot()
   return T_SYM

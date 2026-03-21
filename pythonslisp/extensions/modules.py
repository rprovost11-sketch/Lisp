from __future__ import annotations
from pathlib import Path
from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.AST import LSymbol, prettyPrint
from pythonslisp.Context import Context
from pythonslisp.Environment import ModuleEnvironment
from pythonslisp.Exceptions import LRuntimePrimError
from pythonslisp.extensions import LambdaListMode, primitive
from pythonslisp.AST import L_NIL


def _navigate_or_create_path( path_syms, global_env, evalFn ):
   """Walk/create a chain of ModuleEnvironments, returning the leaf.
   Raises ValueError(name) if a name already exists as a non-module binding."""
   container = global_env
   for sym in path_syms:
      name     = sym.name
      existing = container._bindings.get( name )
      if existing is None:
         mod = ModuleEnvironment( name=name, parent=global_env, evalFn=evalFn )
         container._bindings[name] = mod
      elif isinstance( existing, ModuleEnvironment ):
         mod = existing
      else:
         raise ValueError( name )
      container = mod
   return container


def resolve_module_path( name_arg, global_env, ctx, prim_for_error ):
   """Resolve a :name argument to an existing-or-new ModuleEnvironment.
   Returns None if name_arg is NIL (caller should bind globally).
   Raises LRuntimePrimError for bad types or conflicts."""
   try:
      if isinstance( name_arg, list ) and len(name_arg) == 0:
         return None
      elif isinstance( name_arg, LSymbol ):
         return _navigate_or_create_path( [name_arg], global_env, ctx.lEval )
      elif isinstance( name_arg, str ):
         return _navigate_or_create_path( [LSymbol( name_arg.upper() )],
                                          global_env, ctx.lEval )
      elif ( isinstance( name_arg, list )
             and len(name_arg) >= 2
             and isinstance( name_arg[0], LSymbol )
             and name_arg[0].name == ':' ):
         path = name_arg[1:]
         if not all( isinstance(s, LSymbol) for s in path ):
            raise LRuntimePrimError( prim_for_error,
               ':name path elements must all be symbols.' )
         return _navigate_or_create_path( path, global_env, ctx.lEval )
      else:
         raise LRuntimePrimError( prim_for_error,
            ':name must be a symbol, string, or quoted (: ...) path.' )
   except ValueError as exc:
      raise LRuntimePrimError( prim_for_error,
         f'{exc.args[0]} already exists but is not a module.' )


@primitive( ':', '(module-or-pkg &rest path)',
            mode=LambdaListMode.DOC_ONLY, min_args=2 )
def LP_colon( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Navigate a module or package hierarchy and return the named value.
The first argument is evaluated to obtain the root module or package.
Each subsequent argument must be a symbol naming the next level.
(: mymodule myfn)          -- returns myfn from mymodule
(: mypkg mymodule myfn)    -- navigates pkg -> module -> symbol"""
   raise LRuntimePrimError( LP_colon, 'Handled by CEK machine.' )

@primitive( 'module-set!', '(module symbol value)' )
def LP_module_set( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Set the value of a symbol in a module.  Called by setf for (: ...) place forms.
MODULE must be a module object.  SYMBOL must be a symbol naming the binding.
Returns VALUE."""
   module, sym, value = args
   if not isinstance( module, ModuleEnvironment ):
      raise LRuntimePrimError( LP_module_set, '1st argument must be a module.' )
   if not isinstance( sym, LSymbol ):
      raise LRuntimePrimError( LP_module_set, '2nd argument must be a symbol.' )
   module._bindings[ sym.name ] = value
   return value

@primitive( 'load-module', '(filespec &key name)', mode=LambdaListMode.FULL_BINDING )
def LP_load_module( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Load a .lisp file into a new module and bind it in the global environment.
FILESPEC is the path to the .lisp file as a string.
:name optionally specifies the module name as a symbol or string; otherwise
the name is derived from the filename (stem uppercased).
Returns the new module object."""
   filespec = env.lookup( 'FILESPEC' )
   name_arg = env.lookup( 'NAME' )
   if not isinstance( filespec, str ):
      raise LRuntimePrimError( LP_load_module, 'filespec must be a string.' )
   global_env = env.getGlobalEnv()
   # Determine module name and binding location from :name argument.
   # :name may be NIL, a symbol, a string, or a quoted (: ...) path form.
   try:
      if isinstance( name_arg, list ) and len(name_arg) == 0:
         # NIL — derive name from filename, bind in global env
         module_name = Path(filespec).stem.upper()
         container   = global_env
      elif ( isinstance( name_arg, list )
             and len(name_arg) >= 3
             and isinstance( name_arg[0], LSymbol )
             and name_arg[0].name == ':' ):
         # Quoted (: pkg subpkg ... module) path — navigate/create intermediate packages
         path = name_arg[1:]
         if not all( isinstance(s, LSymbol) for s in path ):
            raise LRuntimePrimError( LP_load_module,
               ':name path elements must all be symbols.' )
         module_name = path[-1].name
         container   = _navigate_or_create_path( path[:-1], global_env, ctx.lEval )
      elif isinstance( name_arg, LSymbol ):
         module_name = name_arg.name
         container   = global_env
      elif isinstance( name_arg, str ):
         module_name = name_arg.upper()
         container   = global_env
      else:
         raise LRuntimePrimError( LP_load_module,
            ':name must be a symbol, string, or quoted (: ...) path.' )
   except ValueError as exc:
      raise LRuntimePrimError( LP_load_module,
         f'{exc.args[0]} already exists but is not a module.' )
   module_env = ModuleEnvironment( name=module_name, parent=global_env, evalFn=ctx.lEval )
   ast = ctx.parseFile( filespec )
   ctx.lEval( module_env, ast )
   container._bindings[module_name] = module_env
   return module_env

@primitive( 'make-module', '(name)' )
def LP_make_module( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Create and return a new empty module with the given name.
NAME may be a symbol or a string.  The module is bound in the global environment."""
   name_arg = args[0]
   if isinstance( name_arg, LSymbol ):
      module_name = name_arg.name
   elif isinstance( name_arg, str ):
      module_name = name_arg.upper()
   else:
      raise LRuntimePrimError( LP_make_module, 'name must be a symbol or string.' )
   global_env = env.getGlobalEnv()
   module_env = ModuleEnvironment( name=module_name, parent=global_env, evalFn=ctx.lEval )
   env.bindGlobal( module_name, module_env )
   return module_env

@primitive( 'module-name', '(module)' )
def LP_module_name( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the name of a module as a string."""
   module = args[0]
   if not isinstance( module, ModuleEnvironment ):
      raise LRuntimePrimError( LP_module_name, 'argument must be a module.' )
   return module.name

@primitive( 'module-symbols', '(module)' )
def LP_module_symbols( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a sorted list of symbols defined directly in the module."""
   module = args[0]
   if not isinstance( module, ModuleEnvironment ):
      raise LRuntimePrimError( LP_module_symbols, 'argument must be a module.' )
   return [ LSymbol(name) for name in sorted( module._bindings.keys() ) ]

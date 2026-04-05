"""Debug extension: (break) primitive for dropping into a nested REPL."""
from __future__ import annotations
import sys
from typing import Any

from pythonslisp.AST import L_NIL, prettyPrint, prettyPrintSExpr
from pythonslisp.Context import Context
from pythonslisp.Environment import Environment
from pythonslisp.Exceptions import LRuntimeError, LRuntimeUsageError
from pythonslisp.Utils import paren_state
from pythonslisp.extensions import primitive


def _collect_locals( env: Environment ) -> dict:
   """Return all non-global bindings visible from env, innermost scope first."""
   result     = {}
   current    = env
   global_env = env._GLOBAL_ENV
   while current is not None and current is not global_env:
      for name, val in current._bindings.items():
         if name not in result:
            result[name] = val
      current = current._parent
   return result


@primitive( 'break', '(&optional message)', min_args=0, max_args=1 )
def LP_break( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Drop into a nested debug REPL at the current call site.
Displays all local variable bindings in scope.  Commands:
  ]continue [expr]  - resume execution returning expr (default NIL)
  ]abort            - abort execution to top level"""

   if args:
      print( f'\n*** Break: {prettyPrintSExpr( args[0] )}' )
   else:
      print( '\n*** Break ***' )

   locals_map = _collect_locals( env )
   if locals_map:
      print( 'Locals:' )
      for name, val in sorted( locals_map.items() ):
         print( f'  {name} = {prettyPrintSExpr( val )}' )
   else:
      print( 'Locals: (none)' )

   print()
   print( '  ]continue [expr]  resume execution (returning expr or NIL)' )
   print( '  ]abort            abort to top level' )
   print()

   if ctx.nested_repl is not None:
      return ctx.nested_repl( env )

   # Fallback: plain input() loop for use outside a Listener session.
   expr_lines: list[str] = []

   while True:
      try:
         prompt   = 'brk>>> ' if not expr_lines else 'brk... '
         line     = input( prompt ).rstrip()
      except EOFError:
         print()
         raise LRuntimeError( 'break: end of input in debug REPL' )
      except KeyboardInterrupt:
         print()
         expr_lines = []
         continue

      # ]continue and ]abort commands
      if not expr_lines and line.startswith( ']' ):
         parts = line[1:].split( None, 1 )
         cmd   = parts[0] if parts else ''
         rest  = parts[1].strip() if len(parts) > 1 else ''
         if cmd == 'continue':
            if rest:
               try:
                  ast    = ctx.parse( rest )
                  result = L_NIL
                  for form in ast[1:]:
                     form   = ctx.expand( env, form )
                     ctx.analyze( env, form )
                     result = ctx.lEval( env, form )
                  return result
               except Exception as ex:
                  print( f'%%% {ex}' )
                  continue
            return L_NIL
         elif cmd == 'abort':
            raise LRuntimeError( 'Aborted from (break).' )
         else:
            print( f"Unknown command '{cmd}'.  Use ]continue or ]abort." )
            continue

      # Super-bracket support
      if line.endswith( ']' ) and not (line.startswith( ']' ) and len(line) > 1):
         tentative = line[:-1]
         combined  = '\n'.join( expr_lines + ([tentative] if tentative else []) )
         sb_depth, sb_in_str = paren_state( combined )
         if sb_depth > 0 and not sb_in_str:
            line = tentative + ')' * sb_depth
         elif line == ']' and sb_depth == 0 and not sb_in_str:
            continue

      if line == '' and not expr_lines:
         continue

      expr_lines.append( line )
      depth, _ = paren_state( '\n'.join( expr_lines ) )

      if line == '' or depth == 0:
         src        = '\n'.join( expr_lines ).strip()
         expr_lines = []
         if not src:
            continue
         try:
            ast    = ctx.parse( src )
            result = L_NIL
            for form in ast[1:]:
               form   = ctx.expand( env, form )
               ctx.analyze( env, form )
               result = ctx.lEval( env, form )
            print( f'\n==> {prettyPrintSExpr( result )}\n' )
         except Exception as ex:
            print( f'%%% {ex}\n' )


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

@primitive( 'trace', '(&rest fn-names)', special=True )
def LP_trace( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Enables call tracing for the named functions and returns the updated
trace list.  With no arguments, returns the list of currently traced functions."""
   raise LRuntimeUsageError( LP_trace, 'Handled by CEK machine.' )

@primitive( 'untrace', '(&rest fn-names)', special=True )
def LP_untrace( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Disables call tracing for the named functions and returns the updated
trace list.  With no arguments, clears all named function tracing."""
   raise LRuntimeUsageError( LP_untrace, 'Handled by CEK machine.' )

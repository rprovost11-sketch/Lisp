"""Debug extension: break, trace, timing, inspection primitives."""
from __future__ import annotations

LISP_DOCUMENTATION_TITLE = 'Debugging'
import sys
from fractions import Fraction
from io import IOBase, StringIO
from typing import Any

from pythonslisp.AST import ( L_NIL, L_T, LSymbol, LCallable, LPrimitive, LSpecialOperator,
                               LFunction, LMacro, LContinuation, LMultipleValues,
                               lisp_type_name, prettyPrint, prettyPrintSExpr )
from pythonslisp.Context import Context
from pythonslisp.Environment import Environment, ModuleEnvironment
from pythonslisp.Exceptions import LRuntimeError, LRuntimeUsageError
from pythonslisp.Utils import paren_state
from pythonslisp.extensions import primitive


def _resolve_trace_output( env: Environment, ctx: Context ):
   """Resolve *TRACE-OUTPUT* stream for output.
   Uses the current binding of *trace-output* (global or locally rebound).
   Falls back to ctx.outStrm only during early startup before io.lisp
   has defined *trace-output*."""
   try:
      return env.lookup( '*TRACE-OUTPUT*' )
   except Exception:
      return ctx.outStrm


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
  ]cont [expr]  - resume execution returning expr (default NIL)
  ]abort        - abort execution to top level"""

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
   print( '  ]cont [expr]  resume execution (returning expr or NIL)' )
   print( '  ]abort        abort to top level' )
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

      # ]cont and ]abort commands
      if not expr_lines and line.startswith( ']' ):
         parts = line[1:].split( None, 1 )
         cmd   = parts[0] if parts else ''
         rest  = parts[1].strip() if len(parts) > 1 else ''
         if cmd == 'cont':
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
            print( f"Unknown command '{cmd}'.  Use ]cont or ]abort." )
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


@primitive( '%time-report', '(elapsed-usec)' )
def LP_time_report( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Print timing report to *trace-output*.  Used internally by the (time) macro."""
   elapsed_usec = args[0]
   if not isinstance( elapsed_usec, (int, float) ):
      raise LRuntimeUsageError( LP_time_report, 'Invalid argument 1. NUMBER expected.' )
   elapsed_sec = elapsed_usec / 1_000_000
   trace_out = _resolve_trace_output( env, ctx )
   print( f'; Evaluation took {elapsed_sec:.6f} seconds of real time.', file=trace_out )
   return L_NIL


@primitive( 'trace-locals', '(&optional label-or-depth depth)', min_args=0, max_args=2 )
def LP_trace_locals( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Print local variable bindings to *trace-output*, grouped by scope.
Innermost scope is shown first.  Optional DEPTH limits how many scopes
to display.  An optional string LABEL is printed as a header.
  (trace-locals)            - all scopes, no label
  (trace-locals 2)          - 2 scopes, no label
  (trace-locals \"tag\")      - all scopes, with label
  (trace-locals \"tag\" 2)    - 2 scopes, with label"""
   label     = None
   max_depth = None
   if len(args) == 2:
      label     = args[0]
      max_depth = args[1]
   elif len(args) == 1:
      if isinstance( args[0], str ):
         label = args[0]
      else:
         max_depth = args[0]
   if max_depth is not None and not isinstance( max_depth, int ):
      raise LRuntimeUsageError( LP_trace_locals, 'DEPTH must be an integer.' )
   trace_out  = _resolve_trace_output( env, ctx )
   if label is not None:
      print( f'--- {label} ---', file=trace_out )
   current    = env
   global_env = env._GLOBAL_ENV
   scope_num  = 0
   while current is not None and current is not global_env:
      if max_depth is not None and scope_num >= max_depth:
         break
      bindings = current._bindings
      if bindings:
         print( f'--- scope {scope_num} ---', file=trace_out )
         for name in sorted( bindings ):
            print( f'{name}:   {prettyPrintSExpr( bindings[name] )}', file=trace_out )
         scope_num += 1
      current = current._parent
   return L_NIL


@primitive( '%trace-vars', '(label names values)' )
def LP_pct_trace_vars( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Print named variable values to *trace-output*.  Used internally by
the (trace-vars) macro."""
   label     = args[0]
   names     = args[1]
   values    = args[2]
   trace_out = _resolve_trace_output( env, ctx )
   if isinstance( label, str ):
      print( f'--- {label} ---', file=trace_out )
   for name, val in zip( names, values ):
      print( f'{prettyPrintSExpr( name )}:   {prettyPrintSExpr( val )}', file=trace_out )
   return L_NIL


@primitive( '%trace-eval', '(exprs values)' )
def LP_pct_trace_eval( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Print expressions and their values to *trace-output*.  Used internally
by the (trace-eval) macro."""
   exprs     = args[0]
   values    = args[1]
   trace_out = _resolve_trace_output( env, ctx )
   for expr, val in zip( exprs, values ):
      print( f'{prettyPrintSExpr( expr )}:   {prettyPrintSExpr( val )}', file=trace_out )
   return L_NIL


# ── Describe ─────────────────────────────────────────────────────────────

def _describe_object( obj: Any, out ) -> None:
   """Print a structured description of obj to out."""
   if isinstance( obj, bool ):
      print( f'{"T" if obj else "NIL"} is a SYMBOL', file=out )
   elif isinstance( obj, LSymbol ):
      print( f'{obj.name} is a SYMBOL', file=out )
      if obj.isKeyword():
         print( '  Keyword symbol', file=out )
   elif isinstance( obj, list ):
      if not obj:
         print( 'NIL is the empty list (NULL)', file=out )
      else:
         preview = prettyPrintSExpr( obj )
         if len( preview ) > 60:
            preview = preview[:57] + '...'
         print( f'{preview} is a CONS', file=out )
         print( f'  Length: {len( obj )}', file=out )
   elif isinstance( obj, int ):
      print( f'{obj} is an INTEGER', file=out )
   elif isinstance( obj, float ):
      print( f'{obj} is a FLOAT', file=out )
   elif isinstance( obj, complex ):
      print( f'{prettyPrintSExpr( obj )} is a COMPLEX', file=out )
      print( f'  Real part: {prettyPrintSExpr( obj.real )}', file=out )
      print( f'  Imaginary part: {prettyPrintSExpr( obj.imag )}', file=out )
   elif isinstance( obj, Fraction ):
      print( f'{obj} is a RATIO', file=out )
   elif isinstance( obj, str ):
      print( f'{prettyPrintSExpr( obj )} is a STRING', file=out )
      print( f'  Length: {len( obj )}', file=out )
   elif isinstance( obj, LSpecialOperator ):
      print( f'{obj.name} is a SPECIAL OPERATOR', file=out )
      print( f'  Usage: {obj.callForm()}', file=out )
      if obj.docString:
         print( f'  Documentation:', file=out )
         for line in obj.docString.split( '\n' ):
            print( f'    {line}', file=out )
   elif isinstance( obj, LPrimitive ):
      print( f'{obj.name} is a PRIMITIVE', file=out )
      print( f'  Usage: {obj.callForm()}', file=out )
      arity = f'{obj.min_args}'
      if obj.max_args is None:
         arity += '+'
      elif obj.max_args != obj.min_args:
         arity += f'-{obj.max_args}'
      print( f'  Arity: {arity}', file=out )
      if obj.docString:
         print( f'  Documentation:', file=out )
         for line in obj.docString.split( '\n' ):
            print( f'    {line}', file=out )
   elif isinstance( obj, LFunction ):
      print( f'{obj.name} is a FUNCTION', file=out )
      print( f'  Usage: {obj.callForm()}', file=out )
      if obj.source_file:
         print( f'  Source: {obj.source_file}', file=out )
      if obj.docString:
         print( f'  Documentation:', file=out )
         for line in obj.docString.split( '\n' ):
            print( f'    {line}', file=out )
   elif isinstance( obj, LMacro ):
      print( f'{obj.name} is a MACRO', file=out )
      print( f'  Usage: {obj.callForm()}', file=out )
      if obj.source_file:
         print( f'  Source: {obj.source_file}', file=out )
      if obj.docString:
         print( f'  Documentation:', file=out )
         for line in obj.docString.split( '\n' ):
            print( f'    {line}', file=out )
   elif isinstance( obj, LContinuation ):
      print( '#<CONTINUATION> is a CONTINUATION', file=out )
   elif isinstance( obj, LMultipleValues ):
      print( f'#<VALUES> with {len( obj.values )} values', file=out )
      for i, v in enumerate( obj.values ):
         print( f'  {i}: {prettyPrintSExpr( v )}', file=out )
   elif isinstance( obj, StringIO ):
      status = 'closed' if obj.closed else 'open'
      print( f'#<STRING-STREAM> is a STRING-STREAM', file=out )
      print( f'  Status: {status}', file=out )
   elif isinstance( obj, IOBase ):
      status = 'closed' if obj.closed else 'open'
      name   = getattr( obj, 'name', None )
      mode   = getattr( obj, 'mode', None )
      print( f'#<FILE-STREAM> is a FILE-STREAM', file=out )
      print( f'  Status: {status}', file=out )
      if name is not None:
         print( f'  Name: {name}', file=out )
      if mode is not None:
         print( f'  Mode: {mode}', file=out )
   elif isinstance( obj, ModuleEnvironment ):
      print( f'#<MODULE {obj.name}> is a MODULE', file=out )
      syms = sorted( obj.localSymbols() )
      if syms:
         print( f'  Symbols: {", ".join( syms )}', file=out )
   elif isinstance( obj, dict ):
      struct_type = obj.get( LSymbol( 'STRUCT-TYPE' ) )
      if isinstance( struct_type, LSymbol ) and struct_type.name != '%STRUCT-DESCRIPTOR%':
         print( f'{prettyPrintSExpr( obj )} is a {struct_type.name} (struct)', file=out )
         for k, v in obj.items():
            if isinstance( k, LSymbol ) and k.name in ('STRUCT-TYPE', 'STRUCT-INCLUDES'):
               continue
            key_str = k.name if isinstance( k, LSymbol ) else prettyPrintSExpr( k )
            print( f'  {key_str}: {prettyPrintSExpr( v )}', file=out )
      else:
         print( f'{prettyPrintSExpr( obj )} is a DICT', file=out )
         print( f'  Size: {len( obj )}', file=out )
         if obj:
            keys_strs = [ prettyPrintSExpr( k ) for k in obj.keys() ]
            print( f'  Keys: {", ".join( keys_strs )}', file=out )
   else:
      print( f'{prettyPrintSExpr( obj )} is of type {lisp_type_name( obj )}', file=out )


@primitive( 'describe', '(object)' )
def LP_describe( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Prints a structured description of OBJECT: its type, attributes,
and documentation (for callables).  Returns NIL."""
   _describe_object( args[0], ctx.outStrm )
   return L_NIL


# ── Inspect ──────────────────────────────────────────────────────────────

def _inspectable_children( obj: Any ) -> list[tuple[str, Any]] | None:
   """Return numbered children for navigation, or None if not structured."""
   if isinstance( obj, list ) and obj:
      return [ (str(i), v) for i, v in enumerate( obj ) ]
   if isinstance( obj, dict ):
      result = []
      for i, (k, v) in enumerate( obj.items() ):
         key_str = k.name if isinstance( k, LSymbol ) else prettyPrintSExpr( k )
         result.append( (f'{i}: {key_str}', v) )
      return result if result else None
   if isinstance( obj, LMultipleValues ):
      return [ (str(i), v) for i, v in enumerate( obj.values ) ]
   return None


def _print_inspect( obj: Any, out ) -> None:
   """Print inspect-style view with numbered children."""
   children = _inspectable_children( obj )
   if children is None:
      _describe_object( obj, out )
      return
   type_name = lisp_type_name( obj )
   if isinstance( obj, list ):
      print( f'{type_name}, {len( obj )} elements', file=out )
      for i, v in enumerate( obj ):
         print( f'  {i}: {prettyPrintSExpr( v )}', file=out )
   elif isinstance( obj, dict ):
      struct_type = obj.get( LSymbol( 'STRUCT-TYPE' ) )
      if isinstance( struct_type, LSymbol ) and struct_type.name != '%STRUCT-DESCRIPTOR%':
         print( f'{struct_type.name} (struct)', file=out )
      else:
         print( f'{type_name}, {len( obj )} entries', file=out )
      for i, (k, v) in enumerate( obj.items() ):
         key_str = k.name if isinstance( k, LSymbol ) else prettyPrintSExpr( k )
         print( f'  {i}: {key_str} = {prettyPrintSExpr( v )}', file=out )
   elif isinstance( obj, LMultipleValues ):
      print( f'VALUES, {len( obj.values )} values', file=out )
      for i, v in enumerate( obj.values ):
         print( f'  {i}: {prettyPrintSExpr( v )}', file=out )


@primitive( 'inspect', '(object)' )
def LP_inspect( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Interactively inspect a structured object.  For lists, dicts, and
structs, displays numbered elements and lets you navigate into them.
Commands:
  <number>   - dive into the element at that index
  ]up        - go back to the parent object
  ]quit      - exit the inspector
For non-structured objects, prints a description and returns immediately."""
   obj     = args[0]
   out     = ctx.outStrm
   history = []

   children = _inspectable_children( obj )
   if children is None:
      _describe_object( obj, out )
      return L_NIL

   while True:
      _print_inspect( obj, out )
      children = _inspectable_children( obj )
      if children is None:
         if history:
            obj = history.pop()
            continue
         break

      try:
         line = input( 'inspect> ' ).strip()
      except (EOFError, KeyboardInterrupt):
         print( file=out )
         break

      if line in ('', ']quit', ']q', 'q'):
         break
      elif line in (']up', 'u'):
         if history:
            obj = history.pop()
         else:
            break
      elif line.isdigit():
         idx = int( line )
         if 0 <= idx < len( children ):
            history.append( obj )
            obj = children[idx][1]
         else:
            print( f'No element at index {idx}', file=out )
      else:
         print( 'Enter a number, ]up, or ]quit', file=out )

   return L_NIL

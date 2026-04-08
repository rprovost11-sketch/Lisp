"""Debug extension: debugger, trace, timing, profiling, inspection primitives."""
from __future__ import annotations

LISP_DOCUMENTATION_TITLE = 'Debugging & Profiling'
from fractions import Fraction
from io import IOBase, StringIO
import time as time_mod
from typing import Any

from pythonslisp.AST import ( L_NIL, L_T, LSymbol, LPrimitive, LSpecialOperator,
                               LFunction, LMacro, LContinuation, LMultipleValues,
                               LCallable, lisp_type_name, prettyPrint, prettyPrintSExpr )
from pythonslisp.Context import Context
from pythonslisp.Environment import Environment, ModuleEnvironment
from pythonslisp.Exceptions import LRuntimeUsageError
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



@primitive( 'debug', '()', max_args=0 )
def LP_debug( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Opens the interactive debugger.  Equivalent to the ]debug listener
command.  Set breakpoints and watches, then use rd to run expressions
with debugging active.  Type h at the debug> prompt for help."""
   ctx.debugger.run_debugger_repl( ctx, env )
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


# ── Timing ──────────────────────────────────────────────────────────────

@primitive( '%time-setup', '()' )
def LP_time_setup( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Capture GC generation counts before a timed evaluation.
   Returns a list of three integers (gen0, gen1, gen2).
   Used internally by the (time) macro."""
   import gc
   counts = gc.get_count()
   return [ counts[0], counts[1], counts[2] ]

@primitive( '%time-report', '(elapsed-usec gc-before)' )
def LP_time_report( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Print timing report to *trace-output*.  Used internally by the (time) macro."""
   import gc
   elapsed_usec = args[0]
   gc_before    = args[1]
   if not isinstance( elapsed_usec, (int, float) ):
      raise LRuntimeUsageError( LP_time_report, 'Invalid argument 1. NUMBER expected.' )
   elapsed_sec = elapsed_usec / 1_000_000
   trace_out = _resolve_trace_output( env, ctx )
   print( f'; Evaluation took {elapsed_sec:.6f} seconds of real time.', file=trace_out )
   if isinstance( gc_before, list ) and len( gc_before ) == 3:
      gc_after = gc.get_count()
      gen0 = gc_after[0] - gc_before[0]
      gen1 = gc_after[1] - gc_before[1]
      gen2 = gc_after[2] - gc_before[2]
      total = gen0 + gen1 + gen2
      if total > 0:
         print( f'; {total} GC collections during evaluation (gen0={gen0}, gen1={gen1}, gen2={gen2}).', file=trace_out )
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


# ── Benchmarking ────────────────────────────────────────────────────────

@primitive( '%benchmark-report', '(n total min-t max-t)' )
def LP_benchmark_report( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Print benchmark results to *trace-output*.  Used internally by
the (benchmark) macro.  Times are in internal time units (microseconds)."""
   n     = args[0]
   total = args[1]
   min_t = args[2]
   max_t = args[3]
   trace_out = _resolve_trace_output( env, ctx )
   total_sec = total / 1_000_000
   avg_us    = total / n if n > 0 else 0
   min_us    = min_t
   max_us    = max_t
   # Pick readable unit for per-iteration stats
   def _fmt( us ):
      if us >= 1_000_000:
         return f'{us / 1_000_000:.4f} s'
      if us >= 1_000:
         return f'{us / 1_000:.4f} ms'
      return f'{us:.1f} us'
   print( f'; {n:,} iterations in {total_sec:.6f} seconds.', file=trace_out )
   print( f'; Per iteration: avg {_fmt( avg_us )}, min {_fmt( min_us )}, max {_fmt( max_us )}.', file=trace_out )
   return L_NIL


# ── Profiling ───────────────────────────────────────────────────────────

_profile_registry: dict[str, dict] = {}
# name -> { 'original': LCallable, 'calls': int, 'total_time': float }


def _make_profile_wrapper( name: str, original: LCallable, entry: dict ) -> LPrimitive:
   """Create a wrapper primitive that times calls to *original*."""
   def wrapper_fn( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      start = time_mod.perf_counter()
      try:
         return ctx.lApply( ctx, env, original, args )
      finally:
         entry['calls'] += 1
         entry['total_time'] += time_mod.perf_counter() - start
   params = ''
   if isinstance( original, LPrimitive ):
      params = original.paramsString
   elif isinstance( original, LFunction ):
      if original.lambdaListAST:
         params = prettyPrintSExpr( original.lambdaListAST )[1:-1]
   return LPrimitive( wrapper_fn, name, params, min_args=0, max_args=None )


@primitive( 'profile', '(&rest fn-names)', min_args=0 )
def LP_profile( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Enables profiling for the named functions.  Each argument must be a
symbol naming a function or primitive.  With no arguments, returns the
list of currently profiled function names.
  (profile 'fibonacci 'helper)"""
   if not args:
      return [ LSymbol( name ) for name in sorted( _profile_registry ) ]
   for arg in args:
      if not isinstance( arg, LSymbol ):
         raise LRuntimeUsageError( LP_profile, f'Arguments must be symbols, got {lisp_type_name( arg )}.' )
      name = arg.name
      if name in _profile_registry:
         continue
      try:
         fn = env.lookup( name )
      except KeyError:
         raise LRuntimeUsageError( LP_profile, f'Unbound function: {name}.' )
      if not isinstance( fn, (LPrimitive, LFunction) ):
         raise LRuntimeUsageError( LP_profile, f'{name} is not a function ({lisp_type_name( fn )}).' )
      entry = { 'original': fn, 'calls': 0, 'total_time': 0.0 }
      wrapper = _make_profile_wrapper( name, fn, entry )
      _profile_registry[name] = entry
      env.getGlobalEnv().bind( name, wrapper )
   return [ LSymbol( name ) for name in sorted( _profile_registry ) ]


@primitive( 'unprofile', '(&rest fn-names)', min_args=0 )
def LP_unprofile( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Removes profiling from the named functions and restores the originals.
With no arguments, unprofiles all currently profiled functions.
  (unprofile 'fibonacci)"""
   if not args:
      names = list( _profile_registry.keys() )
   else:
      names = []
      for arg in args:
         if not isinstance( arg, LSymbol ):
            raise LRuntimeUsageError( LP_unprofile, f'Arguments must be symbols, got {lisp_type_name( arg )}.' )
         names.append( arg.name )
   global_env = env.getGlobalEnv()
   for name in names:
      entry = _profile_registry.pop( name, None )
      if entry is not None:
         global_env.bind( name, entry['original'] )
   return [ LSymbol( name ) for name in sorted( _profile_registry ) ]


@primitive( 'profile-report', '()', max_args=0 )
def LP_profile_report( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Prints a profiling report to *trace-output* showing call counts and
timing for all profiled functions.  Returns NIL."""
   trace_out = _resolve_trace_output( env, ctx )
   if not _profile_registry:
      print( '; No functions are currently profiled.', file=trace_out )
      return L_NIL
   # Sort by total time descending
   entries = sorted( _profile_registry.items(), key=lambda kv: kv[1]['total_time'], reverse=True )
   grand_total = sum( e['total_time'] for _, e in entries )
   # Column widths
   name_w = max( len( name ) for name, _ in entries )
   name_w = max( name_w, 8 )
   print( f'; {"Function":<{name_w}}  {"Calls":>8}  {"Total (s)":>12}  {"Avg (ms)":>10}  {"%":>6}', file=trace_out )
   print( f'; {"-" * name_w}  {"-" * 8}  {"-" * 12}  {"-" * 10}  {"-" * 6}', file=trace_out )
   total_calls = 0
   for name, entry in entries:
      calls = entry['calls']
      total = entry['total_time']
      avg_ms = (total / calls * 1000) if calls > 0 else 0.0
      pct = (total / grand_total * 100) if grand_total > 0 else 0.0
      total_calls += calls
      print( f'; {name:<{name_w}}  {calls:>8}  {total:>12.6f}  {avg_ms:>10.4f}  {pct:>5.1f}%', file=trace_out )
   print( f'; {"-" * name_w}  {"-" * 8}  {"-" * 12}  {"-" * 10}  {"-" * 6}', file=trace_out )
   print( f'; {"Total":<{name_w}}  {total_calls:>8}  {grand_total:>12.6f}', file=trace_out )
   return L_NIL


@primitive( 'profile-reset', '()', max_args=0 )
def LP_profile_reset( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Resets all profiling counters to zero without removing instrumentation.
Returns T."""
   for entry in _profile_registry.values():
      entry['calls'] = 0
      entry['total_time'] = 0.0
   return L_T


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


def _run_inspect( obj, out ):
   """Run the interactive inspect loop on obj.  Returns when the user exits."""
   history = []

   children = _inspectable_children( obj )
   if children is None:
      _describe_object( obj, out )
      return

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


@primitive( 'inspect', '(object)' )
def LP_inspect( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Interactively inspect a structured object.  For lists, dicts, and
structs, displays numbered elements and lets you navigate into them.
Commands:
  <number>   - dive into the element at that index
  ]up        - go back to the parent object
  ]quit      - exit the inspector
For non-structured objects, prints a description and returns immediately."""
   _run_inspect( args[0], ctx.outStrm )
   return L_NIL

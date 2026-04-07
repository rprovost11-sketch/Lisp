"""Debugger: interactive step debugger, breakpoints, watches."""
from __future__ import annotations

import re
from typing import Any

from pythonslisp.AST import ( L_NIL, LSymbol, LCallable, LFunction, prettyPrintSExpr )
from pythonslisp.Environment import Environment
from pythonslisp.Exceptions import LRuntimeError, RestartInvoked


class _RestartRd( Exception ):
   """Raised to restart the current rd session from any debug prompt."""
   pass


# ── Utility helpers ─────────────────────────────────────────────────────

def _is_callable( val: Any ) -> bool:
   """True if val is a function, primitive, special operator, or macro."""
   return isinstance( val, LCallable )


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


def _print_scoped_locals( env, max_depth=None ):
   """Print local variable bindings grouped by scope.
   Filters out bindings whose values are callables."""
   current    = env
   global_env = env._GLOBAL_ENV
   scope_num  = 0
   while current is not None and current is not global_env:
      if max_depth is not None and scope_num >= max_depth:
         break
      bindings = { k: v for k, v in current._bindings.items() if not _is_callable( v ) }
      if bindings:
         print( f'--- scope {scope_num} ---' )
         for name in sorted( bindings ):
            print( f'{name}:   {prettyPrintSExpr( bindings[name] )}' )
         scope_num += 1
      current = current._parent


def _print_named_vars( env, names ):
   """Print specific named variables looked up in env.
   Shows values even if callable, since explicitly requested."""
   for name in names:
      uname = name.upper()
      try:
         val = env.lookup( uname )
         print( f'{uname}:   {prettyPrintSExpr( val )}' )
      except KeyError:
         print( f'{uname}:   <unbound>' )


def _cmd_word( line ):
   """Return the first whitespace-delimited token of line."""
   return line.split( None, 1 )[0] if line else ''


def _cmd_rest( line ):
   """Return everything after the first token, stripped."""
   parts = line.split( None, 1 )
   return parts[1].strip() if len( parts ) > 1 else ''


def _find_nth_call( forms, callable_name, target_n ):
   """Walk an AST (list of forms) to find the Nth call to callable_name.
   Returns the subexpression (list object) or None.  target_n is 1-based."""
   count = [0]   # mutable so nested function can update it

   def _walk( form ):
      if not isinstance( form, list ) or len( form ) == 0:
         return None
      head = form[0]
      if isinstance( head, LSymbol ) and head.name == callable_name:
         count[0] += 1
         if count[0] == target_n:
            return form
      # Recurse into all sub-expressions
      for sub in form[1:]:
         result = _walk( sub )
         if result is not None:
            return result
      return None

   for form in forms:
      result = _walk( form )
      if result is not None:
         return result
   return None


# ── ANSI helpers ───────────────────────────────────────────────────────

_BOLD  = '\033[1m'
_DIM   = '\033[2m'
_RESET = '\033[0m'


# ── Help formatting ────────────────────────────────────────────────────

def _parse_help_entries( docstring ):
   """Parse docstring lines into (usage, description) pairs.
   Each line should be: <usage><2+ spaces><description>."""
   entries = []
   for line in docstring.strip().splitlines():
      line = line.strip()
      if not line:
         continue
      parts = re.split( r'\s{2,}', line, maxsplit=1 )
      if len( parts ) == 2:
         entries.append( ( parts[0], parts[1] ) )
   return entries


def _print_help_entries( entries ):
   """Print help entries with vertically aligned columns."""
   if not entries:
      return
   max_w = max( len( u ) for u, d in entries )
   for usage, desc in entries:
      print( f'  {usage:<{max_w}}  {desc}' )


# ── Debugger ────────────────────────────────────────────────────────────

_DEFAULT_BREAK_ON = frozenset({ 'ERROR', 'WARN', 'SIGNAL', 'THROW' })


class Debugger:
   """Holds breakpoints, watch list, and implements debug REPLs."""

   def __init__( self ):
      self.breakpoints:     dict = {}      # name -> condition_str or None
      self._inner_targets:  dict = {}      # id(ast_node) -> (display_name, cond_src, ast_node, fn_name, fn_obj)
      self._disabled:       set  = set()   # names/display_names of disabled breakpoints
      self.watch_list:      list = []      # canonical expression strings
      self.break_on:        set  = set( _DEFAULT_BREAK_ON )   # auto-break during rd
      self.breakpoint_hook: BreakpointHook = BreakpointHook( self )
      self.input_fn = input                # overridden by Listener for readline
      self._rl      = None                 # readline module ref, set by Listener
      self._history: list[str] = []        # separate debug command history
      self._last_rd_expr: (str | None) = None   # last rd expression for restart
      self._current_K: (list | None) = None     # K reference for backtrace

   # ── History swapping ─────────────────────────────────────────────────

   def _swap_history_in( self ):
      """Save the main REPL history and install the debug history."""
      if self._rl is None:
         return
      rl = self._rl
      if hasattr( rl, 'get_history' ):
         # readline_win
         self._saved_history = rl.get_history()
         rl.set_history( self._history )
      elif hasattr( rl, 'get_current_history_length' ):
         # GNU readline
         n = rl.get_current_history_length()
         self._saved_history = [ rl.get_history_item( i ) for i in range( 1, n + 1 ) ]
         for i in range( n, 0, -1 ):
            rl.remove_history_item( i - 1 )
         for entry in self._history:
            rl.add_history( entry )

   def _swap_history_out( self ):
      """Save the debug history and restore the main REPL history."""
      if self._rl is None:
         return
      rl = self._rl
      if hasattr( rl, 'get_history' ):
         # readline_win
         self._history = rl.get_history()
         rl.set_history( self._saved_history )
      elif hasattr( rl, 'get_current_history_length' ):
         # GNU readline
         n = rl.get_current_history_length()
         self._history = [ rl.get_history_item( i ) for i in range( 1, n + 1 ) ]
         for i in range( n, 0, -1 ):
            rl.remove_history_item( i - 1 )
         for entry in self._saved_history:
            rl.add_history( entry )

   # ── Command handlers ─────────────────────────────────────────────────
   #
   # All _cmd_X methods share the signature (self, cmd, rest, ctx, env).
   #   cmd  = the full command word (e.g. 'w-', 'bo+', 'b!')
   #   rest = everything after the command word, stripped
   #   ctx  = Context     (may be unused by some commands)
   #   env  = Environment (may be unused by some commands)
   #
   # The _dispatch method routes to these via getattr, normalising the
   # command word: ':r' -> 'r', 'w-' -> 'w', 'bo+' -> 'bo', 'b!' -> 'b'.
   # Adding a new _cmd_X method automatically registers the command.
   #
   # Methods marked with _doc_only = True exist solely for their
   # docstrings (used by _cmd_h).  They are skipped by _dispatch
   # because the REPL loops handle them inline for flow control.

   def _cmd_abort( self, cmd, rest, ctx, env ):
      """abort  abort execution to top level"""
      pass
   _cmd_abort._doc_only = True

   def _cmd_b( self, cmd, rest, ctx, env ):
      """b [*]          list all breakpoints
      b name         break on function entry
      b name (cond)  conditional breakpoint
      b fn:call[:n]  break at nth call inside fn's body
      b! name        toggle enable/disable
      b- name        remove breakpoint
      b- *           clear all breakpoints"""

      if cmd == 'b!':
         if rest == '':
            print( 'Usage: b! <name>' )
            return
         uname = rest.upper()
         if uname not in self.breakpoints:
            found = False
            for _nid, (display, *_r) in self._inner_targets.items():
               if display == uname:
                  found = True
                  break
            if not found:
               print( f'No breakpoint on {uname}.' )
               return
         if uname in self._disabled:
            self._disabled.discard( uname )
            print( f'Breakpoint on {uname} enabled.' )
         else:
            self._disabled.add( uname )
            print( f'Breakpoint on {uname} disabled.' )
         return

      if cmd == 'b':
         if rest == '' or rest == '*':
            self._prune_stale_inner( env )
            has_any = False
            if self.breakpoints:
               for name, cond in sorted( self.breakpoints.items() ):
                  has_any = True
                  disabled = name in self._disabled
                  tag = '  :when ' + cond if cond else ''
                  if disabled:
                     print( f'  {_DIM}{name}{tag}  [disabled]{_RESET}' )
                  else:
                     print( f'  {_BOLD}{name}{_RESET}{tag}' )
            for node_id, (display_name, cond, _node, _fn_name, _fn_obj) in sorted(
                  self._inner_targets.items(), key=lambda x: x[1][0] ):
               has_any = True
               disabled = display_name in self._disabled
               tag = '  :when ' + cond if cond else ''
               if disabled:
                  print( f'  {_DIM}{display_name}{tag}  [disabled]{_RESET}' )
               else:
                  print( f'  {_BOLD}{display_name}{_RESET}{tag}' )
            if self.break_on:
               has_any = True
               for name in sorted( self.break_on ):
                  print( f'  {name}  [break-on]' )
            if not has_any:
               print( 'No breakpoints set.' )
            return

         # b name or b name (cond)
         paren_pos = rest.find( '(' )
         if paren_pos > 0:
            name = rest[:paren_pos].strip().upper()
            cond = rest[paren_pos:].strip()
         else:
            name = rest.upper()
            cond = None
         if ':' in name:
            self._set_inner_breakpoint( name, cond, env )
         else:
            self.breakpoints[name] = cond
            self._disabled.discard( name )
            if cond is None:
               print( f'Breakpoint set on {name}.' )
            else:
               print( f'Breakpoint set on {name} :when {cond}' )
         return

      # cmd == 'b-'
      if rest == '':
         print( 'Usage: b- <name> or b- *' )
      elif rest == '*':
         self.breakpoints.clear()
         self._inner_targets.clear()
         self._disabled.clear()
         print( 'All breakpoints cleared.' )
      else:
         uname = rest.upper()
         if uname in self.breakpoints:
            del self.breakpoints[uname]
            self._disabled.discard( uname )
            print( f'Breakpoint on {uname} removed.' )
         else:
            removed = False
            for node_id, (display_name, *_rest) in list( self._inner_targets.items() ):
               if display_name == uname:
                  del self._inner_targets[node_id]
                  self._disabled.discard( uname )
                  print( f'Breakpoint on {uname} removed.' )
                  removed = True
                  break
            if not removed:
               print( f'No breakpoint on {uname}.' )

   def _cmd_bo( self, cmd, rest, ctx, env ):
      """bo [*]    show break-on list (auto-breaks during rd)
      bo name+  add to break-on list
      bo- name  remove from break-on list
      bo- *     clear break-on list
      bo+       restore default break-on list"""

      if cmd == 'bo+':
         self.break_on = set( _DEFAULT_BREAK_ON )
         print( f'Break-on restored: {", ".join( sorted( self.break_on ) )}' )
      elif cmd == 'bo-':
         if rest == '':
            print( 'Usage: bo- <name> or bo- *' )
         elif rest == '*':
            self.break_on.clear()
            print( 'Break-on list cleared.' )
         else:
            names = [ n.upper() for n in rest.split() ]
            for n in names:
               self.break_on.discard( n )
            if self.break_on:
               print( f'Break-on: {", ".join( sorted( self.break_on ) )}' )
            else:
               print( 'Break-on list cleared.' )
      elif rest == '' or rest == '*':
         # bo or bo *
         if self.break_on:
            print( f'Break-on: {", ".join( sorted( self.break_on ) )}' )
         else:
            print( 'Break-on list is empty.' )
      else:
         # bo name+
         names = [ n.upper() for n in rest.split() ]
         for n in names:
            self.break_on.add( n )
         print( f'Break-on: {", ".join( sorted( self.break_on ) )}' )

   def _cmd_bt( self, cmd, rest, ctx, env ):
      """bt  show backtrace (call stack)"""
      K = self._current_K
      if K is None:
         print( 'No backtrace available.' )
         return
      from pythonslisp.Evaluator import ArgFrame
      frames = []
      for frame in reversed( K ):
         if isinstance( frame, ArgFrame ):
            cf = getattr( frame, 'call_form', None )
            if cf is not None:
               frames.append( cf )
      if not frames:
         print( 'No call frames recorded.' )
         return
      for i, cf in enumerate( frames ):
         print( f'  {i}: {prettyPrintSExpr( cf )}' )

   def _cmd_c( self, cmd, rest, ctx, env ):
      """c  continue execution"""
      pass
   _cmd_c._doc_only = True

   def _cmd_e( self, cmd, rest, ctx, env ):
      """e expr  evaluate expr in current environment"""
      if rest:
         self.safe_eval( ctx, env, rest )

   def _cmd_h( self, cmd, rest, ctx, env ):
      """h [cmd]  show help for all commands or a specific command"""
      if rest:
         base   = rest.lstrip( ':' ).rstrip( '-+!' )
         method = getattr( self, f'_cmd_{base}', None )
         if method is not None and method.__doc__:
            entries = _parse_help_entries( method.__doc__ )
            if entries:
               _print_help_entries( entries )
            else:
               print( method.__doc__.strip() )
         else:
            print( f'No help for "{rest}".' )
         return

      # h alone - show all commands
      entries = []
      seen    = set()
      for attr in sorted( dir( self ) ):
         if not attr.startswith( '_cmd_' ):
            continue
         method = getattr( self, attr )
         if not callable( method ) or method.__doc__ is None:
            continue
         func_id = id( method.__func__ )
         if func_id in seen:
            continue
         seen.add( func_id )
         entries.extend( _parse_help_entries( method.__doc__ ) )
      _print_help_entries( entries )

   def _cmd_i( self, cmd, rest, ctx, env ):
      """i expr  evaluate and interactively inspect the result"""
      if rest:
         self.safe_inspect( ctx, env, rest )

   def _cmd_n( self, cmd, rest, ctx, env ):
      """n  step over the next expression"""
      pass
   _cmd_n._doc_only = True

   def _cmd_o( self, cmd, rest, ctx, env ):
      """o  step out (continue until current function returns)"""
      pass
   _cmd_o._doc_only = True

   def _cmd_q( self, cmd, rest, ctx, env ):
      """q / quit  exit the debugger"""
      pass
   _cmd_q._doc_only = True
   _cmd_quit = _cmd_q

   def _cmd_r( self, cmd, rest, ctx, env ):
      """:r n      invoke restart by number
      :r name  invoke restart by name"""
      restarts = self._collect_restarts( ctx )
      if not restarts:
         print( 'No restarts available.' )
         return
      if rest == '':
         print( 'Restarts:' )
         for i, name in enumerate( restarts ):
            print( f'  {i}: [{name}]' )
         return
      if rest.isdigit():
         idx = int( rest )
         if 0 <= idx < len( restarts ):
            raise RestartInvoked( restarts[idx], [] )
         print( f'Restart index {idx} out of range (0-{len(restarts)-1}).' )
      else:
         name = rest.upper()
         if name in restarts:
            raise RestartInvoked( name, [] )
         print( f'No restart named {name}.' )

   def _cmd_rd( self, cmd, rest, ctx, env ):
      """rd expr  run expr with debugging (debug> only)
      rd       re-run / restart the current rd expression"""
      if ctx._debugging and self._last_rd_expr is not None:
         raise _RestartRd()
      print( 'No active rd session.' )

   def _cmd_s( self, cmd, rest, ctx, env ):
      """s  step into the next expression"""
      pass
   _cmd_s._doc_only = True

   def _cmd_v( self, cmd, rest, ctx, env ):
      """v [n]     show local variables (n limits scope depth)
      v name+  show specific named variables"""
      if rest == '':
         _print_scoped_locals( env )
      elif rest.isdigit():
         _print_scoped_locals( env, int( rest ) )
      else:
         _print_named_vars( env, rest.split() )

   def _cmd_w( self, cmd, rest, ctx, env ):
      """w [*]      show watch list
      w expr+    add watches (variables or expressions)
      w- expr+   remove from watch list
      w- *       clear watch list"""

      if cmd == 'w-':
         if rest == '':
            print( 'Usage: w- <expr> or w- *' )
         elif rest == '*':
            self.watch_list = []
            print( 'Watch list cleared.' )
         else:
            for canonical in self._parse_watch_args( rest, ctx ):
               if canonical in self.watch_list:
                  self.watch_list.remove( canonical )
               else:
                  print( f'{canonical} not in watch list.' )
            if self.watch_list:
               print( f'Watching: {", ".join( self.watch_list )}' )
            else:
               print( 'Watch list cleared.' )
         return

      # cmd == 'w'
      if rest == '' or rest == '*':
         if self.watch_list:
            print( f'Watching: {", ".join( self.watch_list )}' )
         else:
            print( 'Watch list is empty.' )
      else:
         for canonical in self._parse_watch_args( rest, ctx ):
            if canonical not in self.watch_list:
               self.watch_list.append( canonical )
         print( f'Watching: {", ".join( self.watch_list )}' )

   @staticmethod
   def _parse_watch_args( source, ctx ):
      """Parse source as a sequence of expressions and return canonical forms."""
      try:
         ast = ctx.parse( source )
         return [ prettyPrintSExpr( form ) for form in ast[1:] ]
      except Exception:
         return [ source.upper() ]

   # ── Command dispatch ─────────────────────────────────────────────────

   def _dispatch( self, line, ctx, env ):
      """Route line to a _cmd_X handler via getattr.
      Returns True if handled.  May raise _RestartRd or RestartInvoked."""
      word   = _cmd_word( line )
      rest   = _cmd_rest( line )
      base   = word.lstrip( ':' ).rstrip( '-+!' )
      method = getattr( self, f'_cmd_{base}', None )
      if method is not None and not getattr( method, '_doc_only', False ):
         method( word, rest, ctx, env )
         return True
      return False

   # ── Internal helpers ─────────────────────────────────────────────────

   def _prune_stale_inner( self, env ):
      """Remove inner breakpoints whose function has been redefined."""
      if env is None or not self._inner_targets:
         return
      stale = []
      for node_id, ( display, _cond, _node, fn_name, fn_obj ) in self._inner_targets.items():
         try:
            current_fn = env.lookup( fn_name )
         except KeyError:
            current_fn = None
         if current_fn is not fn_obj:
            stale.append( ( node_id, display ) )
      for node_id, display in stale:
         del self._inner_targets[node_id]
         self._disabled.discard( display )
         print( f'  (stale breakpoint {display} removed)' )

   @staticmethod
   def _collect_restarts( ctx ):
      """Collect available restarts from the restart stack."""
      from pythonslisp.Evaluator import RestartCaseBodyFrame
      restarts = []
      seen     = set()
      for K in reversed( ctx._restart_stack ):
         for frame in reversed( K ):
            if isinstance( frame, RestartCaseBodyFrame ):
               for rname, params, body in frame.clauses:
                  if rname.name not in seen:
                     seen.add( rname.name )
                     restarts.append( rname.name )
      return restarts

   def _set_inner_breakpoint( self, spec, cond, env ):
      """Resolve and set an inner breakpoint from a spec like F:EVAL:2."""
      parts = spec.split( ':' )
      if len( parts ) == 2:
         fn_name, call_name = parts
         index = 1
      elif len( parts ) == 3:
         fn_name, call_name = parts[0], parts[1]
         try:
            index = int( parts[2] )
         except ValueError:
            print( f'Invalid index: {parts[2]}' )
            return
         if index < 1:
            print( f'Index must be >= 1.' )
            return
      else:
         print( f'Invalid breakpoint spec: {spec}' )
         return

      # Look up the function
      if env is None:
         print( 'Cannot resolve inner breakpoint without an environment.' )
         return
      try:
         fn = env.lookup( fn_name )
      except KeyError:
         print( f'{fn_name} is not defined.' )
         return
      if not isinstance( fn, LFunction ):
         print( f'{fn_name} is not a user-defined function.' )
         return

      # Walk the body to find the Nth call
      target = _find_nth_call( fn.bodyAST, call_name, index )
      if target is None:
         print( f'No call to {call_name} at index {index} in {fn_name}.' )
         return

      display = spec
      self._inner_targets[id( target )] = ( display, cond, target, fn_name, fn )
      self._disabled.discard( display )
      if cond is None:
         print( f'Breakpoint set on {display}.' )
      else:
         print( f'Breakpoint set on {display} :when {cond}' )

   def print_watch( self, env, ctx ):
      """Print current watch list values.  Entries may be variable names
      or full expressions."""
      if not self.watch_list:
         return
      saved_hook    = ctx.step_hook
      ctx.step_hook = None
      try:
         print( '  [watch]' )
         for entry in self.watch_list:
            try:
               ast    = ctx.parse( entry )
               result = L_NIL
               for form in ast[1:]:
                  result = ctx.lEval( env, form )
               print( f'    {entry} = {prettyPrintSExpr( result )}' )
            except Exception as ex:
               print( f'    {entry} = %%% {ex}' )
      finally:
         ctx.step_hook = saved_hook

   # ── Eval helpers ─────────────────────────────────────────────────────

   @staticmethod
   def safe_eval( ctx, env, source ):
      """Evaluate source in env, printing result.  Suppresses step hook."""
      saved_hook    = ctx.step_hook
      ctx.step_hook = None
      try:
         ast    = ctx.parse( source )
         result = L_NIL
         for form in ast[1:]:
            form   = ctx.expand( env, form )
            ctx.analyze( env, form )
            result = ctx.lEval( env, form )
         print( f'==> {prettyPrintSExpr( result )}' )
      except Exception as ex:
         print( f'%%% {ex}' )
      finally:
         ctx.step_hook = saved_hook

   @staticmethod
   def safe_inspect( ctx, env, source ):
      """Evaluate source, then interactively inspect.  Suppresses step hook."""
      from pythonslisp.extensions.debug import _run_inspect
      saved_hook    = ctx.step_hook
      ctx.step_hook = None
      try:
         ast    = ctx.parse( source )
         result = L_NIL
         for form in ast[1:]:
            form   = ctx.expand( env, form )
            ctx.analyze( env, form )
            result = ctx.lEval( env, form )
         _run_inspect( result, ctx.outStrm )
      except Exception as ex:
         print( f'%%% {ex}' )
      finally:
         ctx.step_hook = saved_hook

   @staticmethod
   def debug_eval( ctx, env, source ):
      """Evaluate source with breakpoints active (no step hook suppression)."""
      try:
         ast    = ctx.parse( source )
         result = L_NIL
         for form in ast[1:]:
            form   = ctx.expand( env, form )
            ctx.analyze( env, form )
            result = ctx.lEval( env, form )
         print( f'==> {prettyPrintSExpr( result )}' )
      except _RestartRd:
         raise
      except Exception as ex:
         print( f'%%% {ex}' )

   # ── Debugger REPL ────────────────────────────────────────────────────

   def run_debugger_repl( self, ctx, env ):
      """Run the debug> REPL.  Returns NIL."""
      print( '\n*** Debugger ***' )
      if self.breakpoints:
         print( 'Breakpoints:' )
         for name, cond in sorted( self.breakpoints.items() ):
            if cond is None:
               print( f'  {name}' )
            else:
               print( f'  {name}  :when {cond}' )
      if self.watch_list:
         print( f'Watching: {", ".join( self.watch_list )}' )
      if self.break_on:
         print( f'Break-on: {", ".join( sorted( self.break_on ) )}' )
      print()

      self._swap_history_in()
      try:
         while True:
            try:
               line = self.input_fn( 'debug> ' ).strip()
            except EOFError:
               print()
               break
            except KeyboardInterrupt:
               print()
               continue

            if line == '':
               continue
            elif line in ( 'q', 'quit' ):
               break
            elif _cmd_word( line ) == 'rd':
               rest = _cmd_rest( line )
               if not rest:
                  expr = self._last_rd_expr
                  if expr is None:
                     print( 'No previous rd expression.' )
                     continue
               else:
                  expr = rest
                  self._last_rd_expr = expr
               from pythonslisp.Evaluator import set_stack_traces
               ctx._debugging = True
               set_stack_traces( True )
               try:
                  while True:
                     try:
                        self.debug_eval( ctx, env, expr )
                        break
                     except _RestartRd:
                        print( f'Restarting: {expr}' )
               finally:
                  ctx._debugging = False
                  ctx.step_hook  = None
                  set_stack_traces( False )
                  self._current_K = None
            elif self._dispatch( line, ctx, env ):
               pass
            else:
               print( 'Unknown command.  Type h for help.' )
      finally:
         self._swap_history_out()

      return L_NIL


# ── StepHook ────────────────────────────────────────────────────────────

class StepHook:
   """Interactive step debugger hook called by the CEK loop."""

   def __init__( self, ctx ):
      self._ctx              = ctx
      self._skip_until_depth = None
      self._step_over_first  = False

   def on_expr( self, C, E, K ):
      """Called by CEK loop.  Returns 'step', 'continue', or 'abort'."""
      depth = len( K )
      dbg   = self._ctx.debugger

      # Step-over / step-out: skip if deeper than target
      if self._skip_until_depth is not None:
         if depth > self._skip_until_depth:
            return 'step'
         self._skip_until_depth = None

      # Auto step-over from break's 'n' command
      if self._step_over_first:
         self._step_over_first  = False
         self._skip_until_depth = depth
         indent = '  ' * min( depth, 20 )
         print( f'{indent}{prettyPrintSExpr( C )}' )
         dbg.print_watch( E, self._ctx )
         return 'step'

      # Display current expression
      indent = '  ' * min( depth, 20 )
      print( f'{indent}{prettyPrintSExpr( C )}' )
      dbg.print_watch( E, self._ctx )

      # Interactive prompt
      dbg._current_K = K
      dbg._swap_history_in()
      try:
         while True:
            try:
               line = dbg.input_fn( 'step> ' ).strip()
            except ( EOFError, KeyboardInterrupt ):
               print()
               return 'abort'

            if line in ( 's', '' ):
               return 'step'
            elif line == 'n':
               self._skip_until_depth = depth
               return 'step'
            elif line == 'o':
               if depth == 0:
                  print( 'Already at top level.' )
               else:
                  self._skip_until_depth = depth - 1
                  return 'step'
            elif line == 'c':
               return 'continue'
            elif line == 'abort':
               return 'abort'
            elif dbg._dispatch( line, self._ctx, E ):
               pass
            else:
               print( 'Unknown command.  Type h for help.' )
      finally:
         dbg._swap_history_out()


# ── BreakpointHook ──────────────────────────────────────────────────────

class BreakpointHook:
   """Checks breakpoints in the CEK loop before each list expression."""

   def __init__( self, debugger: Debugger ):
      self._debugger = debugger

   def check( self, C, E, K, ctx ):
      """Check if expression C triggers a breakpoint.  Returns True if
      breakpoint fired, 'abort' to abort, False otherwise."""
      dbg = self._debugger
      has_breakpoints = dbg.breakpoints or dbg._inner_targets
      has_break_on    = ctx._debugging and dbg.break_on
      if not has_breakpoints and not has_break_on:
         return False

      if not isinstance( C, list ) or len( C ) == 0:
         return False

      # Check inner targets first (by identity)
      cid = id( C )
      if cid in dbg._inner_targets:
         display_name, cond_src, _node, _fn_name, _fn_obj = dbg._inner_targets[cid]
         if display_name in dbg._disabled:
            return False
         bp_name = display_name
      else:
         # Check function-entry breakpoints and break-on list
         head = C[0]
         if not isinstance( head, LSymbol ):
            return False
         name = head.name
         if name in dbg.breakpoints and name not in dbg._disabled:
            cond_src = dbg.breakpoints[name]
            bp_name  = name
         elif has_break_on and name in dbg.break_on:
            cond_src = None
            bp_name  = f'{name} [break-on]'
         else:
            return False

      # Evaluate condition if present
      if cond_src is not None:
         saved_hook    = ctx.step_hook
         ctx.step_hook = None
         try:
            ast    = ctx.parse( cond_src )
            result = L_NIL
            for form in ast[1:]:
               form   = ctx.expand( E, form )
               ctx.analyze( E, form )
               result = ctx.lEval( E, form )
            if result is L_NIL or result is False:
               return False
         except Exception:
            return False
         finally:
            ctx.step_hook = saved_hook

      # Breakpoint fires
      depth  = len( K )
      indent = '  ' * min( depth, 20 )
      print( f'\n*** Breakpoint: {bp_name} ***' )
      print( f'{indent}{prettyPrintSExpr( C )}' )
      dbg.print_watch( E, ctx )

      # Interactive prompt
      dbg._current_K = K
      dbg._swap_history_in()
      try:
         while True:
            try:
               line = dbg.input_fn( 'break> ' ).strip()
            except ( EOFError, KeyboardInterrupt ):
               print()
               return 'abort'

            if line in ( 's', '' ):
               ctx.step_hook = StepHook( ctx )
               return True
            elif line == 'n':
               hook = StepHook( ctx )
               hook._skip_until_depth = depth
               ctx.step_hook = hook
               return True
            elif line == 'o':
               if depth == 0:
                  print( 'Already at top level.' )
               else:
                  hook = StepHook( ctx )
                  hook._skip_until_depth = depth - 1
                  ctx.step_hook = hook
                  return True
            elif line == 'c':
               return True
            elif line == 'abort':
               return 'abort'
            elif dbg._dispatch( line, ctx, E ):
               pass
            else:
               print( 'Unknown command.  Type h for help.' )
      finally:
         dbg._swap_history_out()

"""System primitives: time, process information, and the help system."""
from __future__ import annotations
import datetime
import os
import sys
import time
from pathlib import Path
from typing import Any

from pythonslisp.Environment import Environment, ModuleEnvironment
from pythonslisp.AST import ( LSymbol, LCallable, LPrimitive, LSpecialOperator,
                               LFunction, LMacro, prettyPrint, prettyPrintSExpr,
                               got_str, L_T, L_NIL )
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimePrimError, LRuntimeUsageError
from pythonslisp.Utils import columnize
from pythonslisp.extensions import LambdaListMode, primitive
from pythonslisp.Highlighter import render_markdown


# Display order for built-in extension categories in (help) output.
# Extensions not listed here (user extensions, modules) appear after
# these in alphabetical order.  Titles come from each extension's
# LISP_DOCUMENTATION_TITLE constant, falling back to stem.title().
_BUILTIN_EXT_ORDER = [
    'control', 'meta', 'math', 'sequences', 'strings',
    'types', 'io', 'pathnames', 'system', 'modules', 'values', 'conditions', 'debug',
    'predicates',
]


def _ext_title( stem: str ) -> str:
   """Resolve the display title for an extension category."""
   mod_name = f'pythonslisp.extensions.{stem}'
   mod = sys.modules.get( mod_name )
   if mod and hasattr( mod, 'LISP_DOCUMENTATION_TITLE' ):
      return mod.LISP_DOCUMENTATION_TITLE
   return stem.title()


# ── Time ─────────────────────────────────────────────────────────────────

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

@primitive( 'load', '(filespec)' )
def LP_load( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Loads and evaluates a file.  For .py files, loads as a Python extension.
For all other files, reads and evaluates as Lisp source.  Returns T."""
   filespec = args[0]
   if not isinstance( filespec, str ):
      raise LRuntimeUsageError( LP_load, f'Invalid argument 1. STRING expected{got_str(filespec)}.' )
   if filespec.endswith( '.py' ):
      ctx.loadExt( filespec )
   else:
      try:
         with open( filespec, 'r', encoding='utf-8' ) as f:
            content = f.read()
      except FileNotFoundError:
         raise LRuntimePrimError( LP_load, f'File not found "{filespec}".' )
      ast = ctx.parse( content )
      for form in ast[1:]:
         form = ctx.expand( env, form )
         ctx.analyze( env, form )
         ctx.lEval( env, form )
   return L_T

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


# ── Help system ───────────────────────────────────────────────────────────

HELP_DIR = Path(__file__).parent.parent / 'help'


def is_struct_descriptor( obj ) -> bool:
   return isinstance(obj, dict) and obj.get(LSymbol('STRUCT-TYPE')) == LSymbol('%STRUCT-DESCRIPTOR%')

def print_struct_help( descriptor, outStrm ) -> None:
   name    = descriptor.get(LSymbol('NAME'), LSymbol('?'))
   docstr  = descriptor.get(LSymbol('DOCSTRING'), L_NIL)
   fields  = descriptor.get(LSymbol('FIELDS'), [])
   nameStr = name.name if isinstance(name, LSymbol) else str(name)
   nameLow = nameStr.lower()
   field_names = [ f[0].name.lower()
                   for f in fields
                   if isinstance(f, list) and f and isinstance(f[0], LSymbol) ]
   print( f'STRUCT  {nameStr}', file=outStrm )
   if isinstance(docstr, str) and docstr:
      print( file=outStrm )
      print( f'   {docstr}', file=outStrm )
   print( file=outStrm )
   print( 'Fields:', file=outStrm )
   if isinstance(fields, list) and fields:
      for fspec in fields:
         if isinstance(fspec, list) and fspec and isinstance(fspec[0], LSymbol):
            fname    = fspec[0].name
            fdefault = prettyPrintSExpr(fspec[1]) if len(fspec) > 1 else 'NIL'
            print( f'   {fname:<20} default: {fdefault}', file=outStrm )
   else:
      print( '   (none)', file=outStrm )
   print( file=outStrm )
   key_args = ' '.join(field_names)
   print( f'Constructor: (make-{nameLow} &key {key_args})', file=outStrm )
   print( f'Predicate:   ({nameLow}-p obj)', file=outStrm )
   if field_names:
      accessors = '  '.join(f'{nameLow}-{n}' for n in field_names)
      print( f'Accessors:   {accessors}', file=outStrm )
   print( f'Copier:      (copy-{nameLow} inst)', file=outStrm )

def printHelpListings( outStrm, env, find: str | None = None ) -> None:
   from collections import defaultdict

   # categories: stem -> list of (symbol_name, type_tag)
   # type_tag: 'special' | 'prim' | 'fn' | 'mac'
   # '' key holds REPL-defined callables with no source file.
   categories: dict[str, list] = defaultdict( list )
   variablesList: list = []
   typesList:     list = []

   findUpper = find.upper() if find is not None else None

   if HELP_DIR.exists():
      raw_groups: dict[str, set] = defaultdict( set )
      for f in list( HELP_DIR.glob( '**/*.txt' ) ) + list( HELP_DIR.glob( '**/*.md' ) ):
         rel   = f.relative_to( HELP_DIR )
         group = rel.parts[0] if len( rel.parts ) > 1 else ''
         raw_groups[group].add( f.stem.upper() )
      if findUpper is not None:
         topic_groups = { g: sorted( s for s in stems if findUpper in s )
                          for g, stems in raw_groups.items() }
         topic_groups = { g: v for g, v in topic_groups.items() if v }
      else:
         topic_groups = { g: sorted( stems ) for g, stems in raw_groups.items() }
   else:
      topic_groups = {}

   outFile  = outStrm or sys.stdout
   useColor = hasattr( outFile, 'isatty' ) and outFile.isatty()

   BOLD_WHITE = '\033[1;97m' if useColor else ''
   CYAN       = '\033[96m'   if useColor else ''
   GREEN      = '\033[92m'   if useColor else ''
   MAGENTA    = '\033[95m'   if useColor else ''
   YELLOW     = '\033[93m'   if useColor else ''
   RED        = '\033[91m'   if useColor else ''
   BLUE       = '\033[94m'   if useColor else ''
   RESET      = '\033[0m'    if useColor else ''

   # Map type tag to display color (computed after useColor is known)
   TYPE_COLOR = {
      'special': YELLOW,   # special operators - args not pre-evaluated
      'prim':    CYAN,     # built-in primitives
      'fn':      GREEN,    # user-defined functions
      'mac':     MAGENTA,  # macros
   }

   def hdr( text: str ) -> None:
      ul = '=' * len( text )
      print( f'{BOLD_WHITE}{text}{RESET}', file=outStrm )
      print( f'{BOLD_WHITE}{ul}{RESET}',   file=outStrm )

   def subhdr( text: str ) -> None:
      ul = '-' * len( text )
      print( f'{BOLD_WHITE}{text}{RESET}', file=outStrm )
      print( f'{BOLD_WHITE}{ul}{RESET}',   file=outStrm )

   def _tag( obj ) -> str | None:
      if isinstance( obj, LSpecialOperator ):
         return 'special'
      if isinstance( obj, LPrimitive ):
         return 'prim'
      if isinstance( obj, LFunction ):
         return 'fn'
      if isinstance( obj, LMacro ):
         return 'mac'
      return None

   def _cat_add( key: str, sym: str, tag: str ) -> None:
      categories[key].append( (sym, tag) )

   def _render_category( items: list ) -> None:
      """Print one sorted, color-coded columnize block for a category's items."""
      items_sorted = sorted( items )
      names  = [ s   for s, _ in items_sorted ]
      colors = [ TYPE_COLOR.get( t, '' ) or None for _, t in items_sorted ]
      columnize( names, 78, file=outStrm, itemColors=colors )

   globalEnv = env.getGlobalEnv()

   # Bin global-level symbols into their source category
   for symbolStr in globalEnv.localSymbols():
      if symbolStr.startswith( '%' ) or symbolStr.endswith( '-INTERNAL' ):
         continue
      if findUpper is not None and findUpper not in symbolStr:
         continue
      obj = env.lookupGlobal( symbolStr )
      tag = _tag( obj )
      if tag == 'special' or tag == 'prim':
         mod = getattr( obj.pythonFn, '__module__', '' ) or ''
         ext = mod.split( '.' )[-1]
         _cat_add( ext, symbolStr, tag )
      elif tag == 'fn':
         _cat_add( obj.source_file or '', symbolStr, tag )
      elif tag == 'mac':
         _cat_add( obj.source_file or '', symbolStr, tag )
      elif is_struct_descriptor( obj ):
         typesList.append( symbolStr )
      elif not isinstance( obj, ModuleEnvironment ):
         variablesList.append( symbolStr )

   # Collect callables from loaded module environments
   for symbolStr in globalEnv.localSymbols():
      obj = globalEnv._bindings.get( symbolStr )
      if not isinstance( obj, ModuleEnvironment ):
         continue
      modName = obj.name
      for modSym in obj.localSymbols():
         if modSym.startswith( '%' ) or modSym.endswith( '-INTERNAL' ):
            continue
         if findUpper is not None and findUpper not in modSym:
            continue
         modObj = obj._bindings.get( modSym )
         tag    = _tag( modObj )
         if tag:
            _cat_add( modName, modSym, tag )

   # --- Display ---

   hdr( 'Predefined Symbols' )
   columnize( sorted( variablesList ), 78, file=outStrm )
   print( file=outStrm )

   # Build ordered list of named categories (all keys except '')
   named_keys = { k for k in categories if k }
   seen:       set[str]            = set()
   ordered:    list[tuple[str,str]] = []
   for ext in _BUILTIN_EXT_ORDER:
      if ext in named_keys:
         ordered.append( (ext, _ext_title( ext )) )
         seen.add( ext )
   for k in sorted( named_keys - seen, key=_ext_title ):
      ordered.append( (k, _ext_title( k )) )

   for key, label in ordered:
      hdr( label )
      _render_category( categories[key] )
      print( file=outStrm )

   # REPL-defined callables (no source file) in generic sections at the end
   repl = categories.get( '', [] )
   repl_fns  = [ (s, t) for s, t in repl if t == 'fn' ]
   repl_macs = [ (s, t) for s, t in repl if t == 'mac' ]
   if repl_fns:
      hdr( 'Functions' )
      _render_category( repl_fns )
      print( file=outStrm )
   if repl_macs:
      hdr( 'Macros' )
      _render_category( repl_macs )
      print( file=outStrm )

   hdr( 'Types' )
   columnize( sorted( typesList ), 78, file=outStrm, itemColor=RED or None )
   print( file=outStrm )

   hdr( 'TOPICS' )
   for group in sorted( topic_groups ):
      label = 'Tutorials' if group == 'tutorial' else group.title() if group else 'General'
      items = [f'"{s}"' for s in topic_groups[group]]
      print( file=outStrm )
      subhdr( label )
      columnize( items, 78, file=outStrm, itemColor=BLUE or None )
   print( file=outStrm )
   print( "Type '(help callable)' for available documentation on a callable.", file=outStrm )
   print( "Type '(help \"topic\")' for available documentation on the named topic.", file=outStrm )
   print( "Type '(apropos \"substring\")' to search all names by substring.", file=outStrm )
   if useColor:
      print( f'{YELLOW}special operator  {CYAN}primitive  {GREEN}function  {MAGENTA}macro  {RED}type  {BLUE}topic{RESET}', file=outStrm )


@primitive( 'help', '(&optional target &key (substring nil))',
            mode=LambdaListMode.FULL_BINDING )
def LP_help( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Prints a set of tables for all the globally defined symbols and
topics currently available in Python's Lisp. Or prints the usage and
documentation for a specific callable (primitive, function or macro) or topic.

Type '(help callable)' for available documentation on a callable.
Type '(help "topic")' for available documentation on the named topic.
Type '(help "substring" :substring t)' to search all names by substring."""
   target    = env.lookup( 'TARGET' )
   substring = env.lookup( 'SUBSTRING' )

   if substring is not L_NIL:
      if not isinstance( target, str ):
         raise LRuntimeUsageError( LP_help, f':substring t requires a string target{got_str(target)}.' )
      _apropos_search( target, env, ctx.outStrm )
      return L_T

   if isinstance( target, list ) and not target:   # NIL - no target given
      printHelpListings( ctx.outStrm, env )
      return L_T

   pos_arg = target

   # Positional arg: show specific callable or topic
   if isinstance(pos_arg, str):
      topicName = pos_arg.upper()
      topicMd  = next(HELP_DIR.glob(f'**/{topicName}.md'),  None)
      topicTxt = next(HELP_DIR.glob(f'**/{topicName}.txt'), None)
      if topicMd is not None:
         outFile   = ctx.outStrm or sys.stdout
         use_color = ( hasattr(outFile, 'isatty')
                       and outFile.isatty()
                       and os.environ.get('NO_COLOR') is None
                       and os.environ.get('TERM') != 'dumb' )
         content  = topicMd.read_text( encoding='utf-8' )
         rendered = render_markdown( content, use_color=use_color )
         print( rendered, file=ctx.outStrm )
      elif topicTxt is not None:
         print( topicTxt.read_text( encoding='utf-8' ), file=ctx.outStrm )
      else:
         print( f'Unknown topic: "{topicName}"', file=ctx.outStrm )
      return L_T

   # If passed a symbol, resolve it to its global value
   if isinstance(pos_arg, LSymbol):
      try:
         pos_arg = env.lookupGlobalSym(pos_arg)
      except KeyError:
         raise LRuntimePrimError( LP_help, f'Unbound variable: {pos_arg.name}.')

   if is_struct_descriptor(pos_arg):
      print_struct_help( pos_arg, ctx.outStrm )
      return L_T

   if not isinstance(pos_arg, LCallable):
      raise LRuntimeUsageError( LP_help, 'First argument expected to be a callable or struct type.' )
   callableObj = pos_arg

   outStrm  = ctx.outStrm
   outFile  = outStrm or sys.stdout
   useColor = hasattr(outFile, 'isatty') and outFile.isatty()
   RESET   = '\033[0m'  if useColor else ''
   if not useColor:
      usageColor = ''
   elif isinstance( callableObj, LSpecialOperator ):
      usageColor = '\033[93m'   # yellow  - special operator
   elif isinstance( callableObj, LPrimitive ):
      usageColor = '\033[96m'   # cyan    - built-in primitive
   elif isinstance( callableObj, LFunction ):
      usageColor = '\033[92m'   # green   - user-defined function
   elif isinstance( callableObj, LMacro ):
      usageColor = '\033[95m'   # magenta - macro
   else:
      usageColor = ''

   args_label = 'args: unevaluated' if isinstance(callableObj, (LMacro, LSpecialOperator)) else 'args: pre-evaluated'
   print( f'{callableObj.typeLabel()}  |  {args_label}', file=outStrm )
   print( file=outStrm )
   print( f'   {usageColor}Usage: {callableObj.callForm()}{RESET}', file=outStrm )
   print( file=outStrm )
   if callableObj.docString != '':
      valueStr = prettyPrint( callableObj.docString )
      print( valueStr, file=outStrm )

   return L_T

@primitive( 'define-help-topic', '(name-string text-string &key (category ""))',
            mode=LambdaListMode.FULL_BINDING )
def LP_define_help_topic( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Defines a new help topic by writing a text file to the help directory.
The topic is immediately available via (help \"name\").  Returns the topic name
as a symbol.  An existing topic with the same name is overwritten.
:category names a subdirectory within the help directory (created if needed)."""
   name     = env.lookup( 'NAME-STRING' )
   text     = env.lookup( 'TEXT-STRING' )
   category = env.lookup( 'CATEGORY' )
   if not isinstance( name, str ):
      raise LRuntimeUsageError( LP_define_help_topic, f'Invalid argument 1. STRING expected{got_str(name)}.' )
   if not isinstance( text, str ):
      raise LRuntimeUsageError( LP_define_help_topic, f'Invalid argument 2. STRING expected{got_str(text)}.' )
   if not isinstance( category, str ):
      raise LRuntimeUsageError( LP_define_help_topic, f'Invalid keyword :category. STRING expected{got_str(category)}.' )
   target_dir = HELP_DIR / category if category else HELP_DIR
   target_dir.mkdir( parents=True, exist_ok=True )
   topicFile  = target_dir / f'{name.upper()}.txt'
   topicFile.write_text( text, encoding='utf-8' )
   return LSymbol( name )

@primitive( 'undefine-help-topic', '(name-string)' )
def LP_undefine_help_topic( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Removes a help topic by deleting its file from the help directory.
Returns T if the topic existed and was removed, NIL if the topic was not found."""
   name = args[0]
   if not isinstance( name, str ):
      raise LRuntimeUsageError( LP_undefine_help_topic, f'Invalid argument 1. STRING expected{got_str(name)}.' )
   topicFile = next(HELP_DIR.glob(f'**/{name.upper()}.txt'), None)
   if topicFile is not None:
      topicFile.unlink()
      return L_T
   return L_NIL


# ── Apropos ──────────────────────────────────────────────────────────────

def _apropos_search( pattern: str, env: Environment, outStrm ) -> None:
   """Core apropos logic: find and display all symbols matching pattern."""
   pattern = pattern.upper()
   globalEnv = env.getGlobalEnv()
   matches: list[tuple[str,str]] = []

   for sym in globalEnv.localSymbols():
      if sym.startswith( '%' ) or sym.endswith( '-INTERNAL' ):
         continue
      if pattern not in sym:
         continue
      obj = globalEnv._bindings.get( sym )
      if isinstance( obj, LSpecialOperator ):
         tag = 'special operator'
      elif isinstance( obj, LPrimitive ):
         tag = 'primitive'
      elif isinstance( obj, LFunction ):
         tag = 'function'
      elif isinstance( obj, LMacro ):
         tag = 'macro'
      elif isinstance( obj, ModuleEnvironment ):
         tag = 'module'
      else:
         tag = 'variable'
      matches.append( (sym, tag) )

   for sym, tag in sorted( matches ):
      print( f'{sym}  ({tag})', file=outStrm )


@primitive( 'apropos', '(substring)' )
def LP_apropos( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Lists all symbols whose names contain SUBSTRING.
Each match is printed with its type (function, macro, primitive, variable, etc.).
Returns NIL."""
   pattern = args[0]
   if not isinstance( pattern, str ):
      raise LRuntimeUsageError( LP_apropos, f'Invalid argument 1. STRING expected{got_str(pattern)}.' )
   _apropos_search( pattern, env, ctx.outStrm )
   return L_NIL


# ── Dribble ──────────────────────────────────────────────────────────────

@primitive( 'dribble', '(&optional pathname)' )
def LP_dribble( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """With a pathname, starts recording the session to that file.
With no arguments, stops recording and closes the file.
Returns the pathname on success, NIL if there was nothing to stop."""
   if args:
      pathname = args[0]
      if not isinstance( pathname, str ):
         raise LRuntimeUsageError( LP_dribble, f'Invalid argument 1. STRING expected{got_str(pathname)}.' )
      if ctx.start_dribble is None:
         raise LRuntimePrimError( LP_dribble, 'Dribble requires a Listener session.' )
      return ctx.start_dribble( pathname )
   else:
      if ctx.stop_dribble is None:
         raise LRuntimePrimError( LP_dribble, 'Dribble requires a Listener session.' )
      result = ctx.stop_dribble()
      return result if result is not None else L_NIL

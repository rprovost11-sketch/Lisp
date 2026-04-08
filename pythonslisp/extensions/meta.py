from __future__ import annotations
from typing import Any

from pythonslisp.Environment import Environment, ModuleEnvironment
from pythonslisp.AST import ( LSymbol, LMacro, LMultipleValues, got_str,
                               L_T, L_NIL, prettyPrintSExpr, lisp_type_name )
from pythonslisp.Context import Context
from pythonslisp.Exceptions import ( LRuntimeError, LRuntimeUsageError, LArgBindingError,
                                      Thrown, Signaled, ContinuationInvoked )
from pythonslisp.Parser import ParseError
from pythonslisp.Expander import Expander
from pythonslisp.extensions import LambdaListMode, primitive, macro
from pythonslisp.extensions.modules import resolve_module_path


# ── Core definition macros (registered from Python so they are available
#    before any .lisp extension file loads) ─────────────────────────────────

@macro( 'defun', '(fnName lambda-list &rest body)',
        '`(setf ,fnName (lambda (,@lambda-list) ,@body))' )
def _defun():
   """Define and return a new globally named function.  The first expr in the body can be an optional documentation string."""

@macro( 'alias', '(new old)',
        '`(setf ,new ,old)' )
def _alias():
   """Define an alias for an existing named object."""

@macro( 'setf', '(place value &rest more)', '''
(let ((single
       (cond
          ((symbolp place)
           `(setq ,place ,value))
          ((not place)
           (error "setf: lvalue cannot be NIL or ()"))
          ((listp place)
           (cond
              ((= (car place) 'at)
               (if (/= (length place) 3)
                   (error "setf: (at ...) place form expected 3 elements")
                   `(at-set ,(car (cdr place)) ,(car (cdr (cdr place))) ,value)))
              ((= (car place) ':)
               (let ((path (cdr place)))
                  (if (< (length path) 2)
                      (error "setf: (: ...) place form requires at least 2 path elements")
                      (if (= (length path) 2)
                          `(module-set! ,(car path) ',(cadr path) ,value)
                          `(module-set! (: ,@(butlast path)) ',(car (last path)) ,value)))))
              ((= (car place) 'slot-value)
               (if (/= (length place) 3)
                   (error "setf: (slot-value ...) requires exactly 2 arguments")
                   `(set-slot-value! ,(cadr place) ,(caddr place) ,value)))
              ((/= (length place) 2)
               (error "setf: struct accessor place must have exactly 1 instance argument"))
              (t
               `(set-accessor! ',(car place) ,(car (cdr place)) ,value))))
          (t
           (error "setf: unrecognized place form")))))
   (if more
       `(progn ,single (setf ,@more))
       single))''' )
def _setf():
   """Generalized assignment.
(setf sym val)               -> setq  (handles lambda auto-naming)
(setf (at key coll) val)     -> at-set  (mutates list or map in place)
(setf (accessor inst) val)   -> set-accessor!  (struct field via registry)
Multiple (place value) pairs expand to a progn of individual setfs."""


# ── Evaluation primitives ─────────────────────────────────────────────────

@primitive( 'lambda', '(lambda-list &rest body)', special=True )
def LP_lambda( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Creates and returns an unnamed lambda function.  When evaluating such a
function the body exprs are evaluated within a nested scope.  This primitive
captures the environment it is defined in to allow for closures.  The first body
expression can be a documentation string."""
   raise LRuntimeUsageError( LP_lambda, 'Handled by CEK machine.' )

@primitive( 'quote', '(sexpr)', special=True )
def LP_quote( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns expr without evaluating it."""
   return args[0]

@primitive( 'quasiquote', '(sexpr)', special=True )
def LP_quasiquote( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Similar to quote but allows unquote and unquote-splicing expressions within expr.
Quasiquotes may be nested; each level of unquote belongs to the nearest enclosing quasiquote."""
   raise LRuntimeUsageError( LP_quasiquote, 'Handled by CEK machine.' )

@primitive( 'unquote', '(sexpr)', special=True )
def LP_unquote( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Must occur within a quasiquote expr or it's an error."""
   raise LRuntimeUsageError( LP_unquote, 'UNQUOTE can only occur inside a QUASIQUOTE.')

@primitive( 'unquote-splicing', '(sexpr)', special=True )
def LP_unquote_splicing( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Must occur within a quasiquote expr or it's an error."""
   raise LRuntimeUsageError( LP_unquote_splicing, 'UNQUOTE-SPLICING can only occur inside a QUASIQUOTE.')

@primitive( 'funcall', '(callable &rest args)', mode=LambdaListMode.DOC_ONLY, min_args=1, special=True )
def LP_funcall( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Calls a function with the args listed."""
   raise LRuntimeUsageError( LP_funcall, 'Evaluation handled by main eval loop.' )

@primitive( 'apply', '(function &rest args)',
            mode=LambdaListMode.DOC_ONLY, min_args=2, special=True )
def LP_apply( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Applies function to the args.  The last argument must be a list whose
elements are appended to any preceding individual args.  Returns the result of
that function application.  function is any callable that is not a special form."""
   raise LRuntimeUsageError( LP_apply, 'Evaluation handled by main eval loop.' )

@primitive( 'eval', '(sexpr)' )
def LP_eval( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Evaluates expr in the current scope."""
   return ctx.lEval( env, args[0] )

@primitive( 'eval-for-display', '(sexpr)' )
def LP_eval_for_display( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Evaluates sexpr (a pre-parsed AST, typically from parse) and returns a
list of all result values suitable for display in a REPL.  A normal single-value
result returns a one-element list.  Multiple values return a list of all values.
An empty multiple-values object returns NIL.
Use as (eval-for-display (parse input)) in a REPL loop to correctly display
(values ...) forms without losing values to multiple-value stripping."""
   result = ctx.lEval( env, args[0] )
   if type(result) is LMultipleValues:
      return result.values if result.values else L_NIL
   return [result]

@primitive( 'raweval-for-display', '(string &optional stream)', min_args=1, max_args=2 )
def LP_raweval_for_display( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Parses string as Lisp, runs the full pipeline (macro-expand, analyze, eval),
and returns a list of all result values suitable for display in a REPL.  A normal
single-value result returns a one-element list.  Multiple values return a list of
all values.  An empty multiple-values object returns NIL.
If stream is supplied, ctx.outStrm is set to it for the duration of the call so
that all output (including from user-defined functions with captured environments)
goes to that stream.
Use as (raweval-for-display input-string) or (raweval-for-display input-string stream)."""
   source = args[0]
   if not isinstance( source, str ):
      raise LRuntimeUsageError( LP_raweval_for_display, 'Invalid argument 1. STRING expected.' )
   from io import IOBase
   _UNSET = object()   # sentinel distinct from None so we can detect "did we change ctx.outStrm?"
   stream_val = args[1] if len(args) > 1 else None
   if isinstance( stream_val, IOBase ) and stream_val.writable():
      old_outStrm = ctx.outStrm   # save old value - may be None if Python listener set it so
      ctx.outStrm = stream_val
   else:
      old_outStrm = _UNSET        # sentinel: we did NOT change ctx.outStrm
   result = L_NIL
   try:
      ast       = ctx.parse( source )   # [PROGN, form1, ...]
      top_forms = ast[1:]               # strip PROGN wrapper
      for form in top_forms:
         form   = ctx.expand( env, form )
         ctx.analyze( env, form )
         result = ctx.lEval( env, form )
   except LArgBindingError as e:
      # Convert to plain LRuntimeError so CEK's outer except-LArgBindingError
      # (in _do_apply) does not re-wrap this with "RAWEVAL-FOR-DISPLAY" as the
      # function name.  The error originated inside expand/eval, not in our own
      # argument binding.
      raise LRuntimeError( str(e) ) from e
   except LRuntimeError:
      raise
   except Signaled as e:
      _ct = prettyPrintSExpr( e.condition.get('CONDITION-TYPE', LSymbol('UNKNOWN')) )
      _cm = e.condition.get('MESSAGE', '')
      raise LRuntimeError( f'Unhandled condition {_ct}: {_cm}' if _cm else f'Unhandled condition {_ct}' ) from e
   except Thrown as e:
      raise LRuntimeError( f'throw: no catch for tag {prettyPrintSExpr(e.tag)}.' ) from e
   except ContinuationInvoked:
      raise LRuntimeError( 'Continuation invoked outside its dynamic extent.' )
   except Exception as e:
      raise LRuntimeError( str(e) ) from e
   finally:
      if old_outStrm is not _UNSET:
         ctx.outStrm = old_outStrm
   if type(result) is LMultipleValues:
      return result.values if result.values else L_NIL
   return [result]

@primitive( 'python', '(string)' )
def LP_python( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Executes some python code from Lisp."""
   thePythonCode = args[0]
   if not isinstance(thePythonCode, str):
      raise LRuntimeUsageError( LP_python, f'Invalid argument 1. STRING expected{got_str(thePythonCode)}.' )
   theReturnVal = eval( thePythonCode, globals(), locals() )
   return theReturnVal

@primitive( 'parse', '(string)' )
def LP_parse( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Parses the string as a Lisp sexpression and returns the resulting expression tree."""
   theExprStr = args[0]
   if not isinstance(theExprStr, str):
      raise LRuntimeUsageError( LP_parse, f'Invalid argument 1. STRING expected{got_str(theExprStr)}.' )
   return ctx.parse( theExprStr )


# ── Macro definition and expansion ───────────────────────────────────────

@primitive( 'defmacro', '(symbol lambda-list &rest body)', special=True )
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
         macroDef = env.lookupSym( head )
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
      macroDef = env.lookupSym( head )
   except KeyError:
      return form
   if not isinstance( macroDef, LMacro ):
      return form

   return Expander.expandMacroCall( ctx, env, macroDef, form[1:] )


# ── Struct/setf registry ──────────────────────────────────────────────────

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
   from pythonslisp.AST import LInstance
   if isinstance( instance, LInstance ):
      sname = field_key.name
      sd = instance._class.slots.get( sname )
      if sd and sd.allocation == 'CLASS':
         for c in instance._class.cpl:
            if sname in c.class_slots:
               c.class_slots[sname] = newval
               return newval
      instance._slots[sname] = newval
      return newval
   if not isinstance( instance, dict ):
      raise LRuntimeUsageError( LP_set_accessor, f'Invalid argument 2. STRUCT INSTANCE expected{got_str(instance)}.' )
   instance[ field_key ] = newval
   return newval


# ── Variable operations ───────────────────────────────────────────────────

@primitive( 'setq', '(symbol1 sexpr1 symbol2 sexpr2 ...)',
            mode=LambdaListMode.DOC_ONLY, special=True )
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
   env.getGlobalEnv().unbindSym( key )
   return L_NIL


# ── Reflection and introspection ──────────────────────────────────────────

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
      return LSymbol( f'G{counter}' )
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
      return LSymbol( f'{x.name}{counter}' )
   elif isinstance( x, int ) and not isinstance( x, bool ) and x >= 0:
      return LSymbol( f'G{x}' )
   raise LRuntimeUsageError( LP_gensym, 'Invalid argument 1. STRING, SYMBOL, or NON-NEGATIVE INTEGER expected.' )


# ── Extension loading and interpreter management ──────────────────────────

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
   return L_T

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
   return L_T

@primitive( 'interpreter-reboot', '()' )
def LP_interpreter_reboot( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Reboots the interpreter: clears all user-defined functions and variables,
reloads the standard library, and re-runs startup.lisp.  Equivalent to the
]reboot listener command.  Returns T."""
   ctx.reboot()
   return L_T


# ── Type Introspection ──────────────────────────────────────────────────

@primitive( 'type-of', '(sexpr)' )
def LP_typeof( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the type of its argument as a symbol (CL type-of conventions)."""
   arg = args[0]
   if isinstance( arg, ModuleEnvironment ):
      return LSymbol('MODULE')
   return LSymbol( lisp_type_name(arg) )

@primitive( 'make-symbol', '(string)' )
def LP_make_symbol( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Takes a string and returns a new symbol whose print string is that string."""
   arg = args[0]
   if not isinstance(arg, str):
      raise LRuntimeUsageError( LP_make_symbol, f'Invalid argument 1. STRING expected{got_str(arg)}.' )
   try:
      sym, _ = ctx.parseOne(arg)
   except ParseError:
      raise LRuntimeUsageError( LP_make_symbol, f'Invalid argument 1. Valid symbol name expected; "{arg}" is not valid.' )
   if not isinstance(sym, LSymbol):
      raise LRuntimeUsageError( LP_make_symbol, f'Invalid argument 1. Valid symbol name expected; "{arg}" is not valid.' )
   return sym

from __future__ import annotations
from typing import Any

# AST types - the building blocks of all Lisp values
from pythonslisp.AST import (
    LSymbol,          # a Lisp symbol
    LNil,             # the NIL type (also an empty list)
    LCallable,        # base class for all callable types
    LPrimitive,       # a built-in primitive function
    LFunction,        # a user-defined function (defun/lambda)
    LMacro,           # a user-defined macro (defmacro)
    LNUMBER,          # isinstance tuple: (int, float, Fraction)
    T_SYM,            # the interned T symbol
    L_NIL,            # the singleton NIL value
    prettyPrint,      # convert any Lisp value to a user-readable string
    prettyPrintSExpr, # convert any Lisp value to a programmer-readable string
    eql,              # eql comparison (same type + value for numbers)
    equal,            # structural equality
    equalp,           # structural equality, case-insensitive strings, numeric type coercion
)

# Context - carries per-evaluation state; first arg to every primitive.
# Commonly used attributes:
#   ctx.outStrm                    - current output stream; write here instead of print()
#   ctx.lEval(env, expr)           - evaluate an AST node in a given scope
#   ctx.lApply(ctx, env, fn, args) - call a Lisp callable with pre-evaluated args
#   ctx.parse(source)              - parse a Lisp string into an AST (wraps in progn)
#   ctx.parseFile(filename)        - parse a Lisp file into an AST (wraps in progn)
#   ctx.parseOne(source)           - parse one s-expression; returns (ast, chars_consumed)
#   ctx.expand(env, ast)           - macro-expand an AST
#   ctx.analyze(env, ast)          - statically analyze an AST for errors
#   ctx.loadExt(path, targetEnv)   - load a single extension file (.py or .lisp);
#                                     optional targetEnv (ModuleEnvironment) binds into a module
#   ctx.loadExtDir(path)           - load a directory of extensions
#
# Full parse/expand/analyze/eval pattern:
#   result = ctx.lEval(env, ctx.analyze(env, ctx.expand(env, ctx.parse(source))))
from pythonslisp.Context import Context

# Environment - the scope/binding chain; type for env in all primitive signatures
from pythonslisp.Environment import Environment

# Environment - Environment subclass with Lisp argument-binding semantics.
# Import this only if you need to construct a nested scope manually, e.g.:
#   inner_env = Environment(env, evalFn=ctx.lEval)
from pythonslisp.Environment import Environment, ModuleEnvironment

# Exceptions - raise these to signal errors back to the interpreter
from pythonslisp.Exceptions import (
    LRuntimeError,     # general runtime error
    LRuntimeUsageError, # usage error: wrong type/arity — includes PRIMITIVE USAGE hint
)

# primitive - decorator for defining Lisp primitives
# LambdaListMode - controls how the decorator interprets the lambda list
from pythonslisp.extensions import primitive, macro, LambdaListMode


# -----------------------------------------------------------------------
# Example 1: ARITY_ONLY (the most common case)
#
# The lambda list is used to compute min/max arg counts and for
# documentation, but your function receives args as a plain list and
# must unpack them manually.  Use this for most new primitives.
# -----------------------------------------------------------------------

@primitive( 'greet', '(name &optional (greeting "Hello"))' )
def LP_greet( ctx: Context, env: Environment, args: list[Any] ) -> Any:
    """Returns a greeting string.  NAME is required.  GREETING defaults to "Hello"."""
    name = args[0]
    greeting = args[1] if len(args) > 1 else "Hello"
    if not isinstance(name, str):
        raise LRuntimeUsageError( LP_greet, '1st argument NAME must be a string.' )
    return f'{greeting}, {prettyPrint(name)}!'

# -----------------------------------------------------------------------
# Example 2: FULL_BINDING
#
# The interpreter runs its full argument-binding machinery before calling
# your function.  Every parameter in the lambda list is bound as a local
# variable in env and must be retrieved with env.lookup().  Use this when
# you have &key parameters or other complex but syntactically valid lambda
# lists and you want the interpreter to do the unpacking for you.
# -----------------------------------------------------------------------

@primitive( 'repeat-string', '(string &key (count 2) (separator ""))',
            mode=LambdaListMode.FULL_BINDING )
def LP_repeat_string( ctx: Context, env: Environment, args: list[Any] ) -> Any:
    """Returns STRING repeated COUNT times with SEPARATOR between each copy.
:count (default 2) controls how many copies are produced.
:separator (default empty string) is inserted between copies."""
    string    = env.lookup('STRING')
    count     = env.lookup('COUNT')
    separator = env.lookup('SEPARATOR')
    if not isinstance(string, str):
        raise LRuntimeUsageError( LP_repeat_string, '1st argument STRING must be a string.' )
    if not isinstance(count, int) or count < 0:
        raise LRuntimeUsageError( LP_repeat_string, ':count must be a non-negative integer.' )
    if not isinstance(separator, str):
        raise LRuntimeUsageError( LP_repeat_string, ':separator must be a string.' )
    return separator.join([string] * count)

# -----------------------------------------------------------------------
# Example 3: macro registration
#
# Use @macro when you need a form whose arguments are NOT evaluated.
# The macro receives its arguments as raw AST, just like defmacro in Lisp.
# @macro takes: (lisp-name, lambda-list-string, body-expression-string)
# -----------------------------------------------------------------------

@macro( 'comment', '(&rest forms)', '(quote nil)' )
def _macro_comment():
    """Ignores all its arguments and returns NIL.  Useful as a block comment."""

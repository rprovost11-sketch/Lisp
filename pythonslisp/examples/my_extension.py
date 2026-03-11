from __future__ import annotations
from typing import Any

# AST types — the building blocks of all Lisp values
from pythonslisp.AST import (
    LSymbol,          # a Lisp symbol
    LNil,             # the NIL type (also an empty list)
    LCallable,        # base class for all callable types
    LPrimitive,       # a built-in primitive function
    LFunction,        # a user-defined function (defun/lambda)
    LMacro,           # a user-defined macro (defmacro)
    LNUMBER,          # isinstance tuple: (int, float, Fraction)
    L_T,              # the singleton T value
    L_NIL,            # the singleton NIL value
    prettyPrint,      # convert any Lisp value to a user-readable string
    prettyPrintSExpr, # convert any Lisp value to a programmer-readable string
    eql,              # eql comparison (same type + value for numbers)
    equal,            # structural equality
    equalp,           # structural equality, case-insensitive strings, numeric type coercion
)

# Context — carries per-evaluation state; first arg to every primitive.
# Commonly used attributes:
#   ctx.outStrm                    — current output stream; write here instead of print()
#   ctx.lEval(env, expr)           — evaluate an AST node in a given scope
#   ctx.lApply(ctx, env, fn, args) — call a Lisp callable with pre-evaluated args
#   ctx.parse(source)              — parse a Lisp string into an AST (wraps in progn)
#   ctx.parseFile(filename)        — parse a Lisp file into an AST (wraps in progn)
#   ctx.parseOne(source)           — parse one s-expression; returns (ast, chars_consumed)
#   ctx.expand(env, ast)           — macro-expand an AST
#   ctx.analyze(env, ast)          — statically analyze an AST for errors
#   ctx.loadExt(path)              — load a single extension file (.py or .lisp)
#   ctx.loadExtDir(path)           — load a directory of extensions
#
# Full parse/expand/analyze/eval pattern:
#   result = ctx.lEval(env, ctx.analyze(env, ctx.expand(env, ctx.parse(source))))
from pythonslisp.Context import Context

# EnvironmentBase — the scope/binding chain; type for env in all primitive signatures
from pythonslisp.ltk.EnvironmentBase import EnvironmentBase

# Environment — EnvironmentBase subclass with Lisp argument-binding semantics.
# Import this only if you need to construct a nested scope manually, e.g.:
#   inner_env = Environment(env, evalFn=ctx.lEval)
from pythonslisp.Environment import Environment, ModuleEnvironment

# Exceptions — raise these to signal errors back to the interpreter
from pythonslisp.Exceptions import (
    LRuntimeError,     # general runtime error
    LRuntimePrimError, # runtime error attributed to a specific primitive (preferred)
)

# LambdaListMode — controls how the decorator interprets the lambda list
from pythonslisp.extensions import LambdaListMode


def register(primitive) -> None:

    # -----------------------------------------------------------------------
    # Example 1: ARITY_ONLY (the most common case)
    #
    # The lambda list is used to compute min/max arg counts and for
    # documentation, but your function receives args as a plain list and
    # must unpack them manually.  Use this for most new primitives.
    # -----------------------------------------------------------------------

    @primitive( 'greet', '(name &optional (greeting "Hello"))' )
    def LP_greet( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
        """Returns a greeting string.  NAME is required.  GREETING defaults to "Hello"."""
        name = args[0]
        greeting = args[1] if len(args) > 1 else "Hello"
        if not isinstance(name, str):
            raise LRuntimePrimError( LP_greet, '1st argument NAME must be a string.' )
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
    def LP_repeat_string( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
        """Returns STRING repeated COUNT times with SEPARATOR between each copy.
:count (default 2) controls how many copies are produced.
:separator (default empty string) is inserted between copies."""
        string    = env.lookup('STRING')
        count     = env.lookup('COUNT')
        separator = env.lookup('SEPARATOR')
        if not isinstance(string, str):
            raise LRuntimePrimError( LP_repeat_string, '1st argument STRING must be a string.' )
        if not isinstance(count, int) or count < 0:
            raise LRuntimePrimError( LP_repeat_string, ':count must be a non-negative integer.' )
        if not isinstance(separator, str):
            raise LRuntimePrimError( LP_repeat_string, ':separator must be a string.' )
        return separator.join([string] * count)

    # -----------------------------------------------------------------------
    # Example 3: DOC_ONLY with a special form (preEvalArgs=False)
    #
    # preEvalArgs=False means arguments arrive unevaluated (raw AST).
    # Use this for forms that need to control their own evaluation -- macros
    # and control structures.  DOC_ONLY with explicit min_args/max_args is
    # required when the lambda list can't express the real arity (e.g. it
    # uses non-standard syntax as documentation shorthand).
    # -----------------------------------------------------------------------

    @primitive( 'comment', '(&rest forms)', preEvalArgs=False,
                mode=LambdaListMode.DOC_ONLY, min_args=0, max_args=None )
    def LP_comment( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
        """Ignores all its arguments and returns NIL.  Useful as a block comment.
Arguments are never evaluated."""
        return L_NIL

"""
IttyBittyLisp2 — A looping Lisp evaluator with tail-call optimization (TCO).

This is the design used by Python's Lisp before v0.38.  The key idea:
instead of recursing into tail positions, rebind expr/env and loop.

Compare with IttyBittyLisp.py, which uses a naive recursive evaluator
and overflows Python's call stack for deeply tail-recursive programs.

Run with: python pythonslisp/examples/IttyBittyLisp2.py
"""

# ---------------------------------------------------------------------------
# Environment: a linked chain of scopes
# ---------------------------------------------------------------------------

class Env:
    def __init__( self, parent=None, bindings=None ):
        self.vars   = dict(bindings or {})
        self.parent = parent

    def lookup( self, name ):
        scope = self
        while scope:
            if name in scope.vars:
                return scope.vars[name]
            scope = scope.parent
        raise NameError( f'Unbound variable: {name}' )

    def set( self, name, val ):
        scope = self
        while scope:
            if name in scope.vars:
                scope.vars[name] = val
                return val
            scope = scope.parent
        root = self
        while root.parent:
            root = root.parent
        root.vars[name] = val
        return val


# ---------------------------------------------------------------------------
# Function: a closure capturing its lexical environment
# ---------------------------------------------------------------------------

class Function:
    def __init__( self, params, body, env ):
        self.params = params   # list of parameter name strings
        self.body   = body     # list of body expressions; last is the tail
        self.env    = env      # lexical environment at definition time


# ---------------------------------------------------------------------------
# The looping evaluator with TCO
# ---------------------------------------------------------------------------

def lEval( expr, env ):
    while True:

        # --- Atoms ---

        if isinstance( expr, str ):        # symbol — variable lookup
            return env.lookup( expr )
        if not isinstance( expr, list ):   # number, bool, etc. — self-evaluate
            return expr
        if len( expr ) == 0:               # empty list — NIL
            return []

        head = expr[0]

        # --- Special forms ---
        # Tail positions use `continue` — no new Python frame is pushed.
        # Non-tail positions call lEval() recursively as usual.

        if head == 'if':
            cond = lEval( expr[1], env )     # condition: not tail, recurse
            expr = expr[2] if cond else expr[3]
            continue                          # tail branch: loop

        if head == 'progn':
            for sub in expr[1:-1]:
                lEval( sub, env )             # non-tail forms: recurse
            expr = expr[-1]
            continue                          # tail: last form

        if head == 'let':
            vardefs = expr[1]                 # list of [name, init-expr] pairs
            body    = expr[2:]
            new_env = Env( parent=env )
            for vardef in vardefs:
                new_env.vars[vardef[0]] = lEval( vardef[1], env )   # not tail
            env  = new_env
            for sub in body[:-1]:
                lEval( sub, env )
            expr = body[-1]
            continue                          # tail: last body form

        if head == 'setq':
            val = lEval( expr[2], env )
            env.set( expr[1], val )
            return val

        if head == 'lambda':
            return Function( expr[1], expr[2:], env )

        if head == 'quote':
            return expr[1]

        # --- Function call ---
        # All sub-expressions (function position + arguments) are non-tail.

        fn   = lEval( head, env )
        args = [lEval( a, env ) for a in expr[1:]]

        if callable( fn ):                   # Python primitive: return directly
            return fn( args )

        # User-defined function: TCO — rebind env and loop.
        # The new scope is opened on the *captured* (lexical) environment,
        # not the caller's environment.  This is what makes closures work.
        new_env = Env( parent=fn.env, bindings=dict( zip( fn.params, args ) ) )
        env  = new_env
        for sub in fn.body[:-1]:
            lEval( sub, env )                # non-tail body forms: recurse
        expr = fn.body[-1]
        continue                             # tail call: loop, no stack growth


# ---------------------------------------------------------------------------
# Primitives and global environment
# ---------------------------------------------------------------------------

global_env = Env( bindings={
    '+':     lambda args: args[0] + args[1],
    '-':     lambda args: args[0] - args[1],
    '*':     lambda args: args[0] * args[1],
    '=':     lambda args: 1 if args[0] == args[1] else 0,
    '<':     lambda args: 1 if args[0] <  args[1] else 0,
    'print': lambda args: (print( args[0] ), args[0])[1],
} )


# ---------------------------------------------------------------------------
# Helpers and demo
# ---------------------------------------------------------------------------

def run( expr, label=None ):
    result = lEval( expr, global_env )
    tag = label or repr( expr )
    print( f'>>> {tag}' )
    print( f'==> {result}' )
    print()


def main():
    # Basic arithmetic
    run( ['+', ['-', 10, 7], 2],
         '(+ (- 10 7) 2)' )

    # setq and variable lookup
    run( ['setq', 'x', ['*', 6, 7]],
         '(setq x (* 6 7))' )
    run( 'x', 'x' )

    # lambda creates a closure
    run( ['setq', 'square', ['lambda', ['n'], ['*', 'n', 'n']]],
         '(setq square (lambda (n) (* n n)))' )
    run( ['square', 5], '(square 5)' )

    # let creates a local scope
    run( ['let', [['a', 3], ['b', 4]],
          ['+', ['*', 'a', 'a'], ['*', 'b', 'b']]],
         '(let ((a 3) (b 4)) (+ (* a a) (* b b)))' )

    # Tail-recursive countdown.
    # The naive recursive evaluator in IttyBittyLisp.py would hit Python's
    # ~1000-frame stack limit and crash.  With TCO each tail call reuses
    # the same Python frame, so 100,000 iterations need only a handful of
    # stack frames.
    run( ['setq', 'countdown',
          ['lambda', ['n'],
           ['if', ['=', 'n', 0],
            0,
            ['countdown', ['-', 'n', 1]]]]],
         '(setq countdown (lambda (n) (if (= n 0) 0 (countdown (- n 1)))))' )

    run( ['countdown', 100000],
         '(countdown 100000)' )


if __name__ == '__main__':
    main()

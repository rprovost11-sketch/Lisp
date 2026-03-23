"""
IttyBittyLisp1b - A recursive Lisp evaluator with closures.

Extends IttyBittyLisp1a.py (Part 1a) with:
  - Env   : a linked chain of scopes for lexical binding
  - Function : a closure that captures its defining environment
  - let   : local variable binding
  - lambda: first-class functions (closures)
  - quote : suppress evaluation

This is a *recursive* evaluator -- every call in tail position pushes a new
Python stack frame.  It will overflow Python's ~1000-frame limit for deeply
recursive Lisp programs.  See IttyBittyLisp2.py for the looping version that
avoids this with tail-call optimization (TCO).

Run with: python pythonslisp/examples/IttyBittyLisp1b.py
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
        # Walk to the innermost scope that already owns the name.
        scope = self
        while scope:
            if name in scope.vars:
                scope.vars[name] = val
                return val
            scope = scope.parent
        # Name not found anywhere -- create it in the global (root) scope.
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
# The recursive evaluator
# ---------------------------------------------------------------------------

def lEval( expr, env ):
    if isinstance( expr, str ):        # symbol -- variable lookup
        return env.lookup( expr )
    elif not isinstance( expr, list ): # number, etc. -- self-evaluate
        return expr
    elif len( expr ) == 0:
        return []                      # NIL

    head = expr[0]

    if head == 'if':
        cond = lEval( expr[1], env )
        return lEval( expr[2] if cond else expr[3], env )

    elif head == 'progn':
        for sub in expr[1:-1]:
            lEval( sub, env )
        return lEval( expr[-1], env )

    elif head == 'let':
        vardefs = expr[1]              # list of [name, init-expr] pairs
        body    = expr[2:]
        new_env = Env( parent=env )
        for vardef in vardefs:
            new_env.vars[vardef[0]] = lEval( vardef[1], env )
        for sub in body[:-1]:
            lEval( sub, new_env )
        return lEval( body[-1], new_env )

    elif head == 'setq':
        val = lEval( expr[2], env )
        env.set( expr[1], val )
        return val

    elif head == 'lambda':
        return Function( expr[1], expr[2:], env )

    elif head == 'quote':
        return expr[1]

    # Function call: evaluate the head and all arguments.
    fn, *args = [ lEval( subexpr, env ) for subexpr in expr ]
    if callable( fn ):               # Python primitive
        return fn( args )

    # User-defined function: bind arguments in a new scope on the captured env.
    new_env = Env( parent=fn.env, bindings=dict( zip( fn.params, args ) ) )
    for sub in fn.body[:-1]:
        lEval( sub, new_env )
    return lEval( fn.body[-1], new_env )


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
    # Basic arithmetic (same as Part 1a)
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

    # Higher-order function: make-adder returns a closure
    run( ['setq', 'make-adder',
          ['lambda', ['n'],
           ['lambda', ['x'], ['+', 'n', 'x']]]],
         '(setq make-adder (lambda (n) (lambda (x) (+ n x))))' )
    run( ['setq', 'add5', ['make-adder', 5]],
         '(setq add5 (make-adder 5))' )
    run( ['add5', 3], '(add5 3)' )

    # let creates a local scope
    run( ['let', [['a', 3], ['b', 4]],
          ['+', ['*', 'a', 'a'], ['*', 'b', 'b']]],
         '(let ((a 3) (b 4)) (+ (* a a) (* b b)))' )

    # Recursive factorial.
    # NOTE: each call pushes a Python stack frame.  This works for moderate n
    # but will crash with RecursionError for very large n (no TCO).
    run( ['setq', 'factorial',
          ['lambda', ['n'],
           ['if', ['=', 'n', 0],
            1,
            ['*', 'n', ['factorial', ['-', 'n', 1]]]]]],
         '(setq factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))))' )
    run( ['factorial', 10], '(factorial 10)' )

    # quote
    run( ['quote', ['a', 'b', 'c']], "(quote (a b c))" )


if __name__ == '__main__':
    main()

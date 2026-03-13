"""
IttyBittyLisp3 — A CEK machine Lisp evaluator.

The CEK machine is named for its three-part state:
  C — Control:      the current expression to evaluate, OR a Val(value) to deliver
  E — Environment:  the current lexical scope
  K — Kontinuation: an explicit stack of continuation frames

Unlike the looping evaluator (IttyBittyLisp2), the CEK machine never calls
lEval recursively -- not even for non-tail sub-expressions.  Instead it pushes
a continuation frame onto K that resumes when the sub-expression's value arrives.
Non-tail depth is absorbed by K (a heap list), not the Python call stack.

Run with: python pythonslisp/examples/IttyBittyLisp3.py
"""

# ---------------------------------------------------------------------------
# Val: the value / code discriminator
# ---------------------------------------------------------------------------

class Val:
    """
    Wraps a computed value so the machine loop doesn't re-evaluate it as code.

    Without this, a result that happens to be a list -- e.g. [1, 2, 3] -- would
    be indistinguishable from a function call [fn, arg, arg] and get evaluated
    again.  Val is the machine's way of saying "this is data, not code".
    """
    def __init__( self, v ):
        self.v = v


def is_value( c ):
    """True when c is a completed value ready to be delivered to the K stack."""
    if isinstance( c, Val ):
        return True
    if isinstance( c, str ):                    # symbol — still needs lookup
        return False
    if isinstance( c, list ) and len( c ) > 0:  # non-empty list — code to eval
        return False
    return True                                  # number, empty list, etc.


# ---------------------------------------------------------------------------
# Environment and Function (same design as IttyBittyLisp2)
# ---------------------------------------------------------------------------

class Env:
    def __init__( self, parent=None, bindings=None ):
        self.vars   = dict( bindings or {} )
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


class Function:
    def __init__( self, params, body, env ):
        self.params = params   # list of parameter name strings
        self.body   = body     # list of body expressions; last is the tail
        self.env    = env      # lexical environment at definition time


# ---------------------------------------------------------------------------
# Continuation frame classes
#
# A frame is a suspended computation.  It was pushed onto K when the machine
# needed a sub-expression's value before it could continue.  When that value
# arrives, frame.step(value, K) is called.  It returns (C, E) -- the next
# control expression and environment for the machine loop.
#
# Frames that return an expression  -> return (ast_node, env)
# Frames that return a value        -> return (Val(v), env)
# ---------------------------------------------------------------------------

class IfFrame:
    """Waiting for the IF condition.  Picks the correct branch."""
    def __init__( self, then_expr, else_expr, env ):
        self.then_expr = then_expr
        self.else_expr = else_expr
        self.env       = env

    def step( self, value, K ):
        branch = self.then_expr if value else self.else_expr
        return branch, self.env          # expression — not Val-wrapped (tail position)


class PrognFrame:
    """Sequencing: discard the incoming value, then evaluate the next form."""
    def __init__( self, remaining, env ):
        self.remaining = remaining       # list of forms; the last IS the tail
        self.env       = env

    def step( self, value, K ):
        if len( self.remaining ) == 1:
            return self.remaining[0], self.env   # tail form — TCO
        nxt            = self.remaining[0]
        self.remaining = self.remaining[1:]
        K.append( self )
        return nxt, self.env


class LetFrame:
    """
    Collect LET init values one at a time (all evaluated in the outer env).
    Once all are collected, open the new scope and begin the body.
    """
    def __init__( self, current_name, pending, bound, body, outer_env ):
        self.current_name = current_name
        self.pending      = pending      # list of (name, init-expr) pairs still to eval
        self.bound        = bound        # dict of already-evaluated bindings
        self.body         = body
        self.outer_env    = outer_env

    def step( self, value, K ):
        self.bound[self.current_name] = value
        if self.pending:
            name, form        = self.pending[0]
            self.current_name = name
            self.pending      = self.pending[1:]
            K.append( self )
            return form, self.outer_env     # next init expr — in outer env
        new_env = Env( parent=self.outer_env, bindings=self.bound )
        return _begin_body( self.body, new_env, K )


class SetqFrame:
    """Receive the rvalue, bind it, return it."""
    def __init__( self, name, env ):
        self.name = name
        self.env  = env

    def step( self, value, K ):
        self.env.set( self.name, value )
        return Val( value ), self.env    # value — Val-wrapped so it isn't re-evaluated


class ArgFrame:
    """
    Collect the function value then each argument value for a call.

    Phase 1 (self.fn is None): waiting for the function value.
    Phase 2 (self.fn is set):  waiting for each argument value in sequence.
    When all values are in hand, dispatch through do_apply().
    """
    def __init__( self, fn, pending, done, env ):
        self.fn      = fn
        self.pending = pending   # unevaluated argument expressions still to reduce
        self.done    = done      # evaluated argument values collected so far
        self.env     = env

    def step( self, value, K ):
        if self.fn is None:
            # Phase 1: this value IS the function.
            self.fn = value
            if not self.pending:
                return do_apply( self.fn, self.done, self.env, K )
            nxt          = self.pending[0]
            self.pending = self.pending[1:]
            K.append( self )
            return nxt, self.env      # next argument expression

        # Phase 2: this value is an evaluated argument.
        self.done.append( value )
        if self.pending:
            nxt          = self.pending[0]
            self.pending = self.pending[1:]
            K.append( self )
            return nxt, self.env      # next argument expression

        return do_apply( self.fn, self.done, self.env, K )


# ---------------------------------------------------------------------------
# Apply helper
# ---------------------------------------------------------------------------

def do_apply( fn, args, env, K ):
    """
    Invoke fn with args.  Returns (C, E) for the machine loop.

    Primitives produce a Val-wrapped result — a value to deliver.
    User-defined Functions return the first body form as an expression
    to reduce — no Python stack frame, pure TCO.
    """
    if callable( fn ):
        return Val( fn( args ) ), env         # primitive: Val-wrap the result

    # User-defined function: open a new scope and begin the body.
    new_env = Env( parent=fn.env, bindings=dict( zip( fn.params, args ) ) )
    return _begin_body( fn.body, new_env, K )


def _begin_body( body, env, K ):
    """Set up evaluation of a body (list of expressions) in env."""
    if not body:
        return Val( [] ), env
    if len( body ) > 1:
        K.append( PrognFrame( list( body[1:] ), env ) )
    return body[0], env                       # first form — NOT Val-wrapped (it's code)


# ---------------------------------------------------------------------------
# The CEK machine loop
# ---------------------------------------------------------------------------

def lEval( expr, env ):
    C = expr
    E = env
    K = []   # continuation stack

    while True:

        # Case 1: C is a value — deliver it to the top continuation frame.
        if is_value( C ):
            v = C.v if isinstance( C, Val ) else C
            if not K:
                return v                      # K empty: computation is finished
            frame = K.pop()
            C, E  = frame.step( v, K )
            continue

        # Case 2: C is a symbol — look it up and Val-wrap the result.
        if isinstance( C, str ):
            C = Val( E.lookup( C ) )
            continue

        # Case 3: C is an empty list — NIL.
        if len( C ) == 0:
            C = Val( [] )
            continue

        # Case 4: C is a non-empty list — an expression to reduce.
        head = C[0]

        if head == 'if':
            then_ = C[2] if len( C ) > 2 else []
            else_ = C[3] if len( C ) > 3 else []
            K.append( IfFrame( then_, else_, E ) )
            C = C[1]                          # reduce the condition next
            continue

        if head == 'progn':
            if len( C ) <= 1:
                C = Val( [] )
                continue
            if len( C ) == 2:
                C = C[1]                      # single body form — tail position
                continue
            K.append( PrognFrame( list( C[2:] ), E ) )
            C = C[1]
            continue

        if head == 'let':
            vardefs = C[1]
            body    = list( C[2:] )
            if not vardefs:
                new_env = Env( parent=E )
                C, E    = _begin_body( body, new_env, K )
                continue
            pairs             = [(vd[0], vd[1]) for vd in vardefs]
            first_name, first = pairs[0]
            K.append( LetFrame( first_name, pairs[1:], {}, body, E ) )
            C = first                         # reduce the first init expr next
            continue

        if head == 'setq':
            K.append( SetqFrame( C[1], E ) )
            C = C[2]                          # reduce the rvalue next
            continue

        if head == 'lambda':
            C = Val( Function( C[1], list( C[2:] ), E ) )
            continue

        if head == 'quote':
            C = Val( C[1] )
            continue

        # Function call: push ArgFrame and reduce the function position first.
        K.append( ArgFrame( None, list( C[1:] ), [], E ) )
        C = C[0]
        continue


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
    # As in IttyBittyLisp2, tail calls do not grow the Python stack.
    # Unlike IttyBittyLisp2, even the condition (= n 0) and argument (- n 1)
    # evaluations push frames onto K instead of recursing into lEval --
    # the Python call depth is bounded by a small constant at all times.
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

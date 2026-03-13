# The Looping Evaluator

Python's Lisp previously used a **looping tree-walk evaluator** — the
`_lEval` function present before v0.38.  (See `EVALUATOR-DOC` for the
simpler recursive baseline it grew from.)

The looping design solves one concrete problem: Python has a ~1000-frame
call-stack limit.  A naive recursive evaluator overflows for any deeply
tail-recursive Lisp program.  The looping evaluator avoids this by
recognizing **tail positions** — places where the return value of a call
is directly the return value of the caller — and looping there instead of
recursing.

## The Problem with Naive Recursion

The recursive evaluator from `EVALUATOR-DOC` handles `if` like this:

```python
if head == 'if':
    cond = lEval( expr[1], env )
    return lEval( expr[2] if cond else expr[3], env )   # recursive call
```

A tail-recursive Lisp countdown:

```lisp
(setq countdown (lambda (n) (if (= n 0) 0 (countdown (- n 1)))))
(countdown 100000)
```

generates 100,000 nested Python calls and crashes with `RecursionError`.
The recursive call to the tail branch of `if` is the culprit — each call
waits for the next one to return, so the stack grows one frame per
iteration.

## The Fix: Loop Instead of Recurse at Tail Positions

The looping evaluator wraps everything in `while True:`.  Instead of
recursing at the tail branch of `if`, it rebinds `expr` and `continue`s:

```python
def lEval( expr, env ):
    while True:
        ...
        if head == 'if':
            cond = lEval( expr[1], env )   # condition: NOT tail — recurse
            expr = expr[2] if cond else expr[3]
            continue                        # tail branch: loop, no new frame
```

`continue` jumps back to the top of the `while True:` loop.  No new Python
stack frame is created.  The same pattern applies to every other tail
position: the last form of a `progn`, the last form of a `let` body, and
— most importantly — any user-defined function call in tail position.

## Two Kinds of Sub-expression

Every sub-expression falls into one of two categories:

| Position | Action | Stack grows? |
|---|---|---|
| Tail position | `expr = sub; continue` | No |
| Non-tail position | `lEval( sub, env )` | Yes |

The condition in `if`, all arguments to a function call, and all but the
last form in a `progn` or `let` body are **non-tail** — they recurse
normally.  Only the final result-producing expression gets the loop.

## Closures

User-defined functions created by `lambda` are **closures** — they capture
the environment at the point of definition.  When called, a new scope is
opened on top of the *captured* environment (not the caller's), so free
variables resolve lexically.

```python
class Function:
    def __init__( self, params, body, env ):
        self.params = params   # parameter names
        self.body   = body     # body expressions; last is the tail
        self.env    = env      # lexical environment at definition time
```

Calling a user-defined function in the evaluator:

```python
# Bind arguments in a new scope on the *captured* environment.
new_env = Env( parent=fn.env, bindings=dict( zip( fn.params, args ) ) )
env  = new_env
for sub in fn.body[:-1]:
    lEval( sub, env )        # non-tail body forms: recurse normally
expr = fn.body[-1]
continue                     # tail call: loop — no stack growth
```

Tail-calling a function does not grow the Python call stack, regardless of
how deeply the Lisp recursion goes.

## The Complete Evaluator

```python
def lEval( expr, env ):
    while True:

        if isinstance( expr, str ):        # symbol — variable lookup
            return env.lookup( expr )
        if not isinstance( expr, list ):   # number, etc. — self-evaluate
            return expr
        if len( expr ) == 0:
            return []                      # NIL

        head = expr[0]

        if head == 'if':
            cond = lEval( expr[1], env )
            expr = expr[2] if cond else expr[3]
            continue

        if head == 'progn':
            for sub in expr[1:-1]:
                lEval( sub, env )
            expr = expr[-1]
            continue

        if head == 'let':
            vardefs = expr[1]
            body    = expr[2:]
            new_env = Env( parent=env )
            for vardef in vardefs:
                new_env.vars[vardef[0]] = lEval( vardef[1], env )
            env  = new_env
            for sub in body[:-1]:
                lEval( sub, env )
            expr = body[-1]
            continue

        if head == 'setq':
            val = lEval( expr[2], env )
            env.set( expr[1], val )
            return val

        if head == 'lambda':
            return Function( expr[1], expr[2:], env )

        if head == 'quote':
            return expr[1]

        # Function call: all sub-expressions are non-tail.
        fn   = lEval( head, env )
        args = [lEval( a, env ) for a in expr[1:]]

        if callable( fn ):               # Python primitive
            return fn( args )

        # User-defined function: TCO.
        new_env = Env( parent=fn.env, bindings=dict( zip( fn.params, args ) ) )
        env  = new_env
        for sub in fn.body[:-1]:
            lEval( sub, env )
        expr = fn.body[-1]
        continue
```

## Running the Example

The complete working code is in `pythonslisp/examples/IttyBittyLisp2.py`.
It demonstrates a tail-recursive countdown from 100,000 — which crashes
the recursive evaluator from `EVALUATOR-DOC` but completes here in one
flat Python stack frame.

```
python pythonslisp/examples/IttyBittyLisp2.py
```

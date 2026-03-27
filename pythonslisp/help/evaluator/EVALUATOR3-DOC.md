# The CEK Machine Evaluator

Python's Lisp uses a **CEK machine** as its evaluator since v0.38.
(See `EVALUATOR-DOC` for the simplest recursive baseline, and
`EVALUATOR2-DOC` for the looping evaluator it replaced.)

## What Is a CEK Machine?

A CEK machine is a formal model of computation introduced by Matthias
Felleisen and Dan Friedman in 1987 as a rigorous operational semantics for
call-by-value languages.  The name stands for its three-component state:

- **C - Control**: the current expression to evaluate, *or* a computed value
  ready to be delivered to the next waiting computation
- **E - Environment**: the current lexical scope - a mapping from variable names
  to values
- **K - Kontinuation**: an explicit stack of suspended computation frames, each
  representing "what to do with the next value that arrives"

The machine runs as a pure loop.  Each iteration is one reduction step.
The evaluator itself never calls itself recursively - not even for
sub-expressions in non-tail positions.  Instead of recursing, it pushes a
**continuation frame** onto K that will resume the suspended computation
when a value arrives, then loops.

This is the fundamental contrast between the three evaluator generations:

| Situation | Recursive (EVALUATOR-DOC) | Looping (EVALUATOR2-DOC) | CEK (this doc) |
|---|---|---|---|
| Tail position | recurse | `expr = sub; continue` | set C and loop |
| Non-tail position | recurse | recurse | push frame; set C and loop |

The CEK machine eliminates the Python call-stack growth that non-tail
positions caused in both earlier designs.  Sub-expression depth is absorbed
by K - a heap-allocated Python list - rather than the Python call stack.

The deeper point: the CEK machine makes the **call stack a first-class data
structure**.  The K list at any moment is exactly the information that was
previously spread across implicit Python stack frames - made visible,
inspectable, and copyable.  This is what enables `call/cc`: capturing the
current continuation means copying K.

## The Value Problem

Before the machine loop can be described, one subtlety must be addressed:
in a Lisp that represents code as lists, both expressions and values can be
Python lists.  A computed result `[1, 2, 3]` is visually identical to a
function call expression `[fn, arg, arg]`.  The machine must tell them apart.

The solution is a `Val` wrapper:

```python
class Val:
    def __init__( self, v ):
        self.v = v
```

Any computed value is wrapped in `Val` before being placed in C.  A predicate
`is_value(C)` treats `Val` instances - and self-evaluating atoms like numbers
and empty lists - as values ready for delivery.  Non-empty unwrapped lists
are always treated as code to evaluate:

```python
def is_value( c ):
    if isinstance( c, Val ):
        return True
    if isinstance( c, str ):                    # symbol - needs lookup
        return False
    if isinstance( c, list ) and len( c ) > 0:  # non-empty list - code
        return False
    return True                                  # number, empty list, etc.
```

Every path in the machine that produces a value wraps it in `Val`.  Every
path that produces an expression to evaluate leaves it unwrapped.  This
discipline is what enables TCO: a user-function body returned without `Val`
is treated as code in the next iteration, not as a value to deliver.

The looping evaluator in EVALUATOR2-DOC did not need this distinction because
it always held code in `expr` and values in local variables.  In the CEK
machine, C holds both - the wrapper is the only way to tell which.

## The Machine Loop

The main loop has four cases for C:

```python
def lEval( expr, env ):
    C = expr
    E = env
    K = []   # continuation stack

    while True:

        # Case 1: C is a value - deliver it to the top continuation frame.
        if is_value( C ):
            v = C.v if isinstance( C, Val ) else C
            if not K:
                return v                     # K empty: computation finished
            frame = K.pop()
            C, E  = frame.step( v, K )
            continue

        # Case 2: C is a symbol - look it up, Val-wrap the result.
        if isinstance( C, str ):
            C = Val( E.lookup( C ) )
            continue

        # Case 3: C is an empty list - NIL.
        if len( C ) == 0:
            C = Val( [] )
            continue

        # Case 4: C is a non-empty list - an expression to reduce.
        head = C[0]
        ...
```

When K is empty and a value arrives in Case 1, the entire computation is
finished and the value is returned.  Otherwise, the top frame is popped and
handed the value; it returns the next `(C, E)` pair for the loop.

Notice that Case 1 both consumes values **and** resumes the next computation.
There is no separate "return" path - every value flows through the same
delivery mechanism.  When K empties, delivery becomes the final return.

## Step-by-Step: Evaluating `(let ((x 2)) (+ x 1))`

The best way to understand the machine is to watch it run.  The table below
shows the state at the start of each loop iteration.  E is abbreviated;
`{x:2}->g` means a scope with `x=2` whose parent is the global environment.

| Step | C | K |
|---|---|---|
| 1 | `(let ((x 2)) (+ x 1))` | `[]` |
| 2 | `2` | `[LetFrame(current=x)]` |
| 3 | `(+ x 1)` | `[]` |
| 4 | `'+'` | `[ArgFrame(fn=?, pending=[x,1])]` |
| 5 | `Val(fn_+)` | `[ArgFrame(fn=?, pending=[x,1])]` |
| 6 | `'x'` | `[ArgFrame(fn=fn_+, pending=[1])]` |
| 7 | `Val(2)` | `[ArgFrame(fn=fn_+, pending=[1])]` |
| 8 | `1` | `[ArgFrame(fn=fn_+, done=[2])]` |
| 9 | `Val(3)` | `[]` |

**Step 1** - Case 4, `head = 'let'`.  The machine extracts the vardef pairs,
pushes `LetFrame(current='x', pending=[], bound={}, body=[(+ x 1)])`, and
sets C = `2` (the init expression for `x`).

**Step 2** - Case 1, `2` is a value.  Pop `LetFrame`, call `LetFrame.step(2)`.
The frame stores `bound['x'] = 2`, sees no more pending bindings, opens a new
scope `{x:2}->global`, and calls `_begin_body`.  The body has one form so no
`PrognFrame` is pushed; it returns `(['+', 'x', 1], new_env)`.  C and E are
updated.  K is now empty - `let` consumed no permanent K space.

**Step 3** - Case 4, function call `(+ x 1)`.  Push
`ArgFrame(fn=None, pending=['x', 1], done=[])`, set C = `'+'`.

**Step 4** - Case 2, `'+'` is a symbol.  Look it up in E, get `fn_+`,
wrap: C = `Val(fn_+)`.

**Step 5** - Case 1, `Val(fn_+)` is a value.  Pop `ArgFrame`, call
`ArgFrame.step(fn_+)`.  **Phase 1**: the frame records `self.fn = fn_+`,
takes `'x'` from pending (leaving `[1]`), pushes itself back onto K,
returns `('x', E)`.

**Step 6** - Case 2, symbol `'x'`.  Look up in `{x:2}->global`, get `2`,
wrap: C = `Val(2)`.

**Step 7** - Case 1, `Val(2)`.  Pop `ArgFrame`, call `ArgFrame.step(2)`.
**Phase 2**: append `2` to `done = [2]`, take `1` from pending (leaving `[]`),
push itself back, return `(1, E)`.

**Step 8** - Case 1, `1` is a self-evaluating number.  Pop `ArgFrame`,
call `ArgFrame.step(1)`.  Phase 2: append `1`, `done = [2, 1]`, pending
empty - call `do_apply(fn_+, [2, 1])`.  `fn_+` is a Python callable, so
it returns `Val(3)`.

**Step 9** - Case 1, `Val(3)`.  K is empty - return `3`.

## K Is the Explicit Call Stack

The K list at any moment is exactly the information that a recursive evaluator
would have spread across implicit Python stack frames.

Consider how the recursive evaluator from EVALUATOR-DOC would handle `(+ x 1)`:

```
Python frame 1: evaluating '+' - "I need the function value for this call"
Python frame 2: evaluating 'x' - "I need argument 0 for this call"
Python frame 3: evaluating 1   - "I need argument 1 for this call"
```

In the CEK machine, that same information lives in a single `ArgFrame` object,
mutated across three iterations:

| Loop iteration | ArgFrame state |
|---|---|
| Evaluating `'+'` | `fn=None, pending=['x', 1], done=[]` |
| Evaluating `'x'` | `fn=fn_+, pending=[1], done=[]` |
| Evaluating `1` | `fn=fn_+, pending=[], done=[2]` |

Each iteration that would have pushed a Python stack frame instead updates the
ArgFrame in place and loops.  The frame is not popped until all its work is
done and `do_apply` is called.

For nested calls like `(f (g (h 1)))`, three separate `ArgFrame` instances
accumulate on K - one per call - just as three levels of Python recursion
would accumulate in the other evaluators.  The difference is that K is a
heap list: it can grow to any depth without a `RecursionError`.

**Tail calls** are the special case where K does not grow.  When a
user-defined function's body is set as C and `_begin_body` returns, the
`ArgFrame` that triggered the call has already been popped and discarded.
Nothing new is pushed.  K stays the same size.

## Continuation Frames in Depth

### IfFrame

The simplest frame.  Pushed when `(if cond then else)` is seen; receives
the condition value and picks a branch:

```python
class IfFrame:
    def __init__( self, then_expr, else_expr, env ):
        self.then_expr = then_expr
        self.else_expr = else_expr
        self.env       = env

    def step( self, value, K ):
        branch = self.then_expr if value else self.else_expr
        return branch, self.env   # expression - NOT Val-wrapped
```

The chosen branch is returned unwrapped.  Neither branch pushes a frame -
both are in tail position.

### PrognFrame

Sequences a list of forms, discarding all results except the last.  The
last form is delivered in tail position by *not* pushing the frame back:

```python
class PrognFrame:
    def __init__( self, remaining, env ):
        self.remaining = remaining
        self.env       = env

    def step( self, value, K ):
        if len( self.remaining ) == 1:
            return self.remaining[0], self.env   # tail - no re-push
        nxt            = self.remaining[0]
        self.remaining = self.remaining[1:]
        K.append( self )                          # more forms remain
        return nxt, self.env
```

For `(progn a b c)`, the machine pushes `PrognFrame([b, c])` and evaluates
`a`.  The frame receives `a`'s value (discards it), shrinks to `[c]`, pushes
itself, evaluates `b`.  The frame receives `b`'s value, sees `remaining = [c]`
has length 1, returns `c` without re-pushing - `c` is the tail.

### LetFrame

The `let` form binds multiple variables, but all init expressions must be
evaluated in the **outer** environment before any binding takes effect.
`LetFrame` enforces this by holding a reference to `outer_env` and using it
for every init evaluation, only opening the new scope once all values are
collected:

```python
class LetFrame:
    def step( self, value, K ):
        self.bound[self.current_name] = value
        if self.pending:
            name, form        = self.pending[0]
            self.current_name = name
            self.pending      = self.pending[1:]
            K.append( self )
            return form, self.outer_env      # always the outer env
        new_env = Env( parent=self.outer_env, bindings=self.bound )
        return _begin_body( self.body, new_env, K )
```

This is exactly what distinguishes `let` from `let*`: a `LetStarFrame` would
bind each name immediately and pass the growing inner environment to the next
init expression instead of `outer_env`.

### ArgFrame

The most complex frame.  A single `ArgFrame` manages all phases of one
function call - evaluating the function, evaluating each argument in order,
and finally dispatching:

```python
class ArgFrame:
    def step( self, value, K ):
        if self.fn is None:
            # Phase 1: received the function.
            self.fn = value
            if not self.pending:
                return do_apply( self.fn, self.done, self.env, K )
            nxt          = self.pending[0]
            self.pending = self.pending[1:]
            K.append( self )
            return nxt, self.env

        # Phase 2: received an argument value.
        self.done.append( value )
        if self.pending:
            nxt          = self.pending[0]
            self.pending = self.pending[1:]
            K.append( self )
            return nxt, self.env

        return do_apply( self.fn, self.done, self.env, K )
```

The frame is pushed once per call and re-pushed up to `1 + len(args)` times
before finally calling `do_apply`.  Each re-push corresponds to one sub-
expression that would have been a recursive `lEval` call in the earlier
evaluators.

## Function Calls and TCO

When all argument values are in hand, `ArgFrame` calls `do_apply`:

```python
def do_apply( fn, args, env, K ):
    if callable( fn ):
        return Val( fn( args ) ), env    # primitive: Val-wrap the result

    # User-defined function: open a new scope and begin the body.
    new_env = Env( parent=fn.env, bindings=dict( zip( fn.params, args ) ) )
    return _begin_body( fn.body, new_env, K )

def _begin_body( body, env, K ):
    if not body:
        return Val( [] ), env
    if len( body ) > 1:
        K.append( PrognFrame( list( body[1:] ), env ) )
    return body[0], env              # first body form - NOT Val-wrapped
```

The critical line is the last one.  The first body form is returned
**without** a `Val` wrapper.  On the next loop iteration `is_value` is
False, so the machine evaluates it as code.  No new Python stack frame was
created anywhere in this chain - `do_apply` and `_begin_body` both return
immediately to the main loop.

For a **tail call** - a function call in the final position of a body:

1. `ArgFrame` was the only frame pushed for this call.
2. It delivers args to `do_apply`, then is discarded.
3. `do_apply` returns an unwrapped expression.
4. The main loop evaluates that expression with K unchanged (possibly empty).

K does not grow.  Python call stack depth stays constant.

For a **non-tail call** - a function call whose result feeds into further
computation (e.g. an argument to an outer call):

1. The outer `ArgFrame` is on K, waiting for this sub-result.
2. `do_apply` sets C to the callee's body; K still has the outer frame on it.
3. K has grown by whatever frames the callee's body evaluation requires.

K depth grows with nesting depth, just as the Python call stack would in the
earlier evaluators.  The difference is that K lives on the heap - there is
no `RecursionError` limit.

## The Complete Evaluator

```python
def lEval( expr, env ):
    C = expr
    E = env
    K = []

    while True:

        if is_value( C ):
            v = C.v if isinstance( C, Val ) else C
            if not K:
                return v
            frame = K.pop()
            C, E  = frame.step( v, K )
            continue

        if isinstance( C, str ):
            C = Val( E.lookup( C ) )
            continue

        if len( C ) == 0:
            C = Val( [] )
            continue

        head = C[0]

        if head == 'if':
            then_ = C[2] if len( C ) > 2 else []
            else_ = C[3] if len( C ) > 3 else []
            K.append( IfFrame( then_, else_, E ) )
            C = C[1]
            continue

        if head == 'progn':
            if len( C ) <= 1:
                C = Val( [] )
                continue
            if len( C ) == 2:
                C = C[1]
                continue
            K.append( PrognFrame( list( C[2:] ), E ) )
            C = C[1]
            continue

        if head == 'let':
            vardefs = C[1]
            body    = list( C[2:] )
            if not vardefs:
                C, E = _begin_body( body, Env( parent=E ), K )
                continue
            pairs             = [(vd[0], vd[1]) for vd in vardefs]
            first_name, first = pairs[0]
            K.append( LetFrame( first_name, pairs[1:], {}, body, E ) )
            C = first
            continue

        if head == 'setq':
            K.append( SetqFrame( C[1], E ) )
            C = C[2]
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
```

Frame class definitions (`IfFrame`, `PrognFrame`, `LetFrame`, `SetqFrame`,
`ArgFrame`) and the supporting `Env`, `Function`, `Val`, `is_value`,
`do_apply`, and `_begin_body` are shown in full in the example file.

## Running the Example

The complete working code is in `pythonslisp/examples/IttyBittyLisp3.py`.
It runs the same countdown-from-100,000 as `IttyBittyLisp2.py`.  The
difference is invisible in the output but real in the machinery: in
IttyBittyLisp2 evaluating `(= n 0)` and `(- n 1)` each recurse into
`lEval`; here those evaluations push `ArgFrame` instances onto K and loop.
At no point during the countdown does the Python call stack grow beyond a
constant handful of frames.

```
python pythonslisp/examples/IttyBittyLisp3.py
```

The real interpreter's `Evaluator.py` extends this design with tracing, macros,
multiple values, continuations, and the full lambda-list argument binding.
Because K is an explicit Python list, capturing the entire continuation
at any point - the basis of `call/cc` - requires nothing more than
copying K.

## Challenges

- **Implement a macro expander.** A macro is a function that transforms code
  before it is evaluated.  Before the function-call path, check whether the
  head of an expression names a macro; if so, call it with the *unevaluated*
  arguments and loop with the result as the new `expr`.  This is where `code
  is data` becomes essential -- the macro receives and returns plain lists,
  and `lEval` never knows the difference.  Start with `(defmacro when (cond . body) \`(if ,cond (progn ,@body)))`.

- **Implement `call/cc`.** `(call/cc (lambda (k) ...))` captures the current
  continuation -- the entire K stack -- and passes it as an argument.
  Calling `k` with a value discards the current K and reinstates the
  captured one.  Start with escape continuations (calling `k` only from
  within the dynamic extent of the `call/cc`): capturing is `list(K)` and
  reinstating is `K[:] = captured`.  For **full re-invocable continuations**,
  each mutable frame must also be copied so that re-invocation cannot corrupt
  the saved state; add a `copy()` method to every frame class (mutable frames
  return a new instance; immutable frames return `self`), then capture with
  `[f.copy() for f in K]`.  Python's Lisp uses this full approach.

- **Implement `dynamic-wind`.** `(dynamic-wind before thunk after)` guarantees
  that `after` runs whenever control leaves the thunk -- whether by normal
  return, exception, or continuation jump.  It requires tracking "winders"
  alongside K and running the appropriate before/after thunks as
  continuations cross their boundaries.  This is a significant challenge
  but it reveals exactly why continuations and `dynamic-wind` interact the
  way they do.

- **Add a tracing mode.** Before evaluating any function call, check a flag
  and if set print the function name and arguments.  On return, print the
  result with indentation proportional to call depth.  Because the CEK
  machine's K already encodes the call depth implicitly -- it is `len(K)`
  -- you do not need a separate counter.

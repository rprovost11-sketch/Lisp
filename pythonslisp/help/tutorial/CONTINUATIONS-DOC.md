# Continuations

*Quick reference: `(help "control-transfer")` - Full documentation: this file.*


## What Is a Continuation?

At any point during a computation, the **continuation** is "everything that
still needs to happen to produce the final result."  Normally continuations
are implicit - the runtime just keeps running.  `call/cc` makes the current
continuation into a first-class value you can store, call, and call again.

```lisp
(+ 1 (call/cc (lambda (k)
                (+ 10 (k 42)))))
;==> 43
```

When `k` is called with `42`, execution jumps immediately back to the
`call/cc` site as if `call/cc` had returned `42`.  The `(+ 10 ...)` is
abandoned.  The outer `(+ 1 ...)` completes normally, giving `43`.


## Syntax

```lisp
(call/cc (lambda (k) body...))
```

`k` receives the current continuation.  Calling `(k value)` delivers `value`
to the `call/cc` site.  If the lambda returns normally without calling `k`,
`call/cc` returns that value instead.

**Important**: `return`, `block`, `setq`, and other special forms cannot be
used as parameter names - they are always interpreted as special operators.
Use names like `exit`, `abort`, or `k` for continuation parameters.


---

## Use Case 1: Escape (Aborting a Computation)

The simplest use: call `k` to abandon the rest of the computation early and
return a result directly.

```lisp
; Find the first even element, or NIL
(defun find-even (lst)
  (call/cc (lambda (exit)
    (dolist (x lst)
      (when (evenp x) (exit x)))
    nil)))

(find-even '(1 3 5 4 7))   ;==> 4
(find-even '(1 3 5))        ;==> NIL
```

```lisp
; Abort a recursive tree walk on the first match
(defun find-in-tree (tree target)
  (call/cc (lambda (exit)
    (defun tree-walk (node)
      (cond
        ((null node) nil)
        ((equal node target) (exit t))
        ((listp node) (tree-walk (car node)) (tree-walk (cdr node)))))
    (tree-walk tree)
    nil)))

(find-in-tree '(1 (2 (3 4) 5)) 4)   ;==> T
(find-in-tree '(1 (2 (3 4) 5)) 9)   ;==> NIL
```

For simple early-exit patterns, `catch`/`throw` is often clearer.  `call/cc`
is preferred when the continuation itself needs to be stored or passed around.


---

## Use Case 2: Re-Invocable Continuations

A stored continuation may be called **multiple times**.  Each call restores
the saved computation state.

```lisp
; Continuation used as a loop back-edge
(let ((n 0) (k nil))
  (call/cc (lambda (c) (setf k c)))  ; save state here
  (setf n (+ n 1))
  (if (< n 4)
      (k nil)    ; jump back to the call/cc point
      n))
;==> 4
```

Trace of execution:

| Invocation    | n before | n after | action              |
|---------------|----------|---------|---------------------|
| 1st (call/cc) | 0        | 1       | `(k nil)` - loop    |
| 2nd (k nil)   | 1        | 2       | `(k nil)` - loop    |
| 3rd (k nil)   | 2        | 3       | `(k nil)` - loop    |
| 4th (k nil)   | 3        | 4       | condition false - return 4 |

The value passed to `k` is delivered as the result of the `call/cc`
expression.  Here `nil` is passed but discarded; what matters is the
side-effect on `n`.


---

## Use Case 3: Checkpointing

A checkpoint saves the computation state at a particular moment so it can be
replayed from that exact point, any number of times.

```lisp
; Replay from the checkpoint until the log has 5 entries
(let ((resume nil) (log '()))
  (call/cc (lambda (k) (setf resume k)))   ; checkpoint here
  (setf log (append log (list (length log))))
  (when (< (length log) 5)
    (resume nil))                           ; jump back to checkpoint
  log)
;==> (0 1 2 3 4)
```

Each time `(resume nil)` is called, execution restarts from the `call/cc`
line.  The `log` list grows because `setf` updates the binding in the
captured environment, which is shared across all re-entries.

**Practical use:** checkpointing is useful for retry loops, backtracking
search, and simulation rewind - any situation where you want to re-run a
block of code from a fixed starting state while retaining accumulated changes.


---

## Use Case 4: Generators

A **generator** is a function that produces a sequence of values on demand,
pausing between each one.  Full continuations make this straightforward.

```lisp
(let ((gen-k nil) (caller-k nil))

  (defun yield (val)
    "Pause the generator; deliver val to the caller."
    (call/cc (lambda (k)
      (setf gen-k k)       ; save where to resume the generator
      (caller-k val))))    ; jump back to the caller with val

  (defun gen-body ()
    "The generator sequence - add more yield calls to extend it."
    (yield 10)
    (yield 20)
    (yield 30)
    (caller-k 'done))      ; signal exhaustion

  (defun gen-next ()
    "Advance the generator by one step."
    (call/cc (lambda (k)
      (setf caller-k k)              ; save where to return to
      (if gen-k
          (gen-k nil)                ; resume generator
          (gen-body))))))            ; first call: start generator

(gen-next)   ;==> 10
(gen-next)   ;==> 20
(gen-next)   ;==> 30
(gen-next)   ;==> DONE
```

**How it works:**

1. `gen-next` captures its own continuation as `caller-k`, then either starts
   or resumes the generator.
2. `yield` captures the generator's continuation as `gen-k`, then jumps to
   `caller-k` delivering the value.
3. The next call to `gen-next` updates `caller-k` and calls `gen-k`, resuming
   the generator exactly where it yielded.

The pattern generalises: wrap the shared state, `yield`, `gen-body`, and
`gen-next` in a closure and you have a reusable generator factory.


---

## Use Case 5: Coroutines

**Coroutines** are independent computations that take turns running.  A
minimal scheduler uses a queue of continuations; `sched-yield` suspends the
current task by enqueuing its continuation, then runs the next one.

```lisp
(let ((queue '()) (log '()))

  (defun sched-enqueue (k) (setf queue (append queue (list k))))
  (defun sched-dequeue () (let ((f (car queue))) (setf queue (cdr queue)) f))

  (defun sched-yield ()
    "Suspend this task; resume the next one in the queue."
    (call/cc (lambda (k)
      (sched-enqueue k)
      ((sched-dequeue) nil))))

  (defun task-a (n)
    (when (<= n 3)
      (setf log (append log (list (ustring "A" n))))
      (sched-yield)
      (task-a (+ n 1))))

  (defun task-b (n)
    (when (<= n 3)
      (setf log (append log (list (ustring "B" n))))
      (sched-yield)
      (task-b (+ n 1))))

  (sched-enqueue (lambda (_) (task-a 1)))
  (sched-enqueue (lambda (_) (task-b 1)))
  ((sched-dequeue) nil)   ; start the scheduler
  log)
;==> ("A1" "B1" "A2" "B2" "A3" "B3")
```

**How it works:**

1. Two tasks are wrapped in lambdas and placed in the queue.
2. The scheduler starts by dequeuing and calling the first task.
3. Each task runs until it calls `sched-yield`, which enqueues its own
   continuation and immediately runs the next task from the queue.
4. Tasks alternate until both are exhausted.

This is cooperative multitasking: tasks decide when to yield.  The scheduler
imposes no preemption.


---

## Use Case 6: Non-Determinism with `amb`

`amb` (ambiguous choice) is the classic non-determinism operator.
`(amb x y z)` speculatively chooses one value; if the computation later
calls `(*fail*)`, it backtracks and tries the next choice.

```lisp
; Set up the backtracking machinery
(setf *fail* nil)

(defun amb (&rest choices)
  (call/cc (lambda (k)
    (let ((old-fail *fail*)
          (try-next nil))
      (setf try-next (lambda (cs)
        (if (null cs)
            (progn (setf *fail* old-fail) (*fail*))   ; all choices exhausted
            (progn
              (setf *fail* (lambda () (try-next (cdr cs))))  ; backtrack point
              (k (car cs))))))                               ; try this choice
      (try-next choices)))))

(defun assert! (condition)
  "Backtrack if condition is false."
  (when (not condition) (*fail*)))
```

Once defined, `amb` and `assert!` let you write search as if choices were
free variables:

```lisp
; Find an even number greater than 3 from a list
(let ((x (amb 1 2 3 4 5 6)))
  (assert! (evenp x))
  (assert! (> x 3))
  x)
;==> 4
```

```lisp
; Find integers a, b such that a^2 + b^2 = 25
(let ((a (amb 3 4 5))
      (b (amb 3 4 5)))
  (assert! (= (+ (* a a) (* b b)) 25))
  (list a b))
;==> (3 4)
```

**How it works:** each `amb` call captures the current continuation `k`.
`*fail*` is set to a closure that tries the next choice by calling `k` with
it.  When `assert!` calls `(*fail*)`, execution backtracks to the most recent
`amb` choice point and tries the next option.  If all choices are exhausted,
the previous `*fail*` is restored and backtracking continues further up.


---

## Use Case 7: dynamic-wind - Guaranteed Cleanup

`(dynamic-wind before thunk after)` guarantees that `before` and `after` run
on **every entry and exit** of the thunk, including non-local exits via
`throw`, `return-from`, errors, and - crucially - continuation invocations.

```lisp
; after always runs, even when the thunk throws
(let ((log '()))
  (catch :done
    (dynamic-wind
      (lambda () (setf log (append log '("before"))))
      (lambda () (throw :done nil))
      (lambda () (setf log (append log '("after"))))))
  log)
;==> ("before" "after")
```

**With continuations** - `dynamic-wind` tracks every boundary crossing:

```lisp
; before and after run on each re-entry and re-exit via continuation
(let ((log '()) (resume nil) (n 0))
  (dynamic-wind
    (lambda () (setf log (append log '("in"))))
    (lambda ()
      (call/cc (lambda (k) (setf resume k)))
      (setf n (+ n 1)))
    (lambda () (setf log (append log '("out")))))
  (when (< n 3) (resume nil))
  log)
;==> ("in" "out" "in" "out" "in" "out")
```

Each call to `resume` re-enters the dynamic-wind scope, triggering `before`
again.  Each exit - whether via normal return or continuation jump - triggers
`after`.

**Nested `dynamic-wind`** - afters run innermost first, matching the structure
of the dynamic extent:

```lisp
(let ((log '()))
  (dynamic-wind
    (lambda () (setf log (append log '("outer-before"))))
    (lambda ()
      (dynamic-wind
        (lambda () (setf log (append log '("inner-before"))))
        (lambda () (setf log (append log '("thunk"))))
        (lambda () (setf log (append log '("inner-after"))))))
    (lambda () (setf log (append log '("outer-after")))))
  log)
;==> ("outer-before" "inner-before" "thunk" "inner-after" "outer-after")
```

**Typical uses:** resource cleanup, lock acquire/release, transaction
brackets, instrumentation, and any situation where setup and teardown must
be symmetric even under non-local control flow.


---

## call/cc vs catch/throw

| | `call/cc` | `catch`/`throw` |
|---|---|---|
| Continuation is a value | Yes - storable, callable | No - implicit |
| Re-invocable | Yes | No |
| Tag required | No | Yes |
| Crosses function calls | Yes | Yes |
| Typical use | Generators, coroutines, backtracking | Simple cross-function abort |

For **simple early exit** prefer `catch`/`throw` - the intent is clearer.
Reach for `call/cc` when you need to *store* or *resume* a computation.


---

## Quick Reference

| Expression | Effect |
|---|---|
| `(call/cc (lambda (k) body))` | Capture continuation; run body with k bound to it |
| `(k value)` | Deliver value to the call/cc site; resume saved computation |
| `(setf saved k)` | Store continuation for later use |
| `(saved value)` | Re-instate saved computation, delivering value |
| `(dynamic-wind before thunk after)` | Run before; run thunk; run after - even on non-local exit or re-entry |

See also: `(help "control-transfer-doc")` for `block`/`return-from`,
`catch`/`throw`, and `handler-case`.

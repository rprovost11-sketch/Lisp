# Closures and Lexical Scope

A closure is a function that captures the lexical environment in which it
was defined.  Any free variable referenced in the function body is looked
up in the environment that existed when the function was created, not when
it is called.  This makes closures the primary tool for encapsulation,
data hiding, and stateful objects in Python's Lisp.

---

## Lambda — the Core of Closures

```lisp
(lambda (lambda-list) body...)
```

Every `lambda` captures a reference to the environment at the point of its
creation.  `defun` is just a macro for `(setq name (lambda ...))` — named
functions are closures too.

```lisp
; A lambda that closes over a free variable
(setf base 10)
(setf adder (lambda (x) (+ x base)))
(adder 5)     ;==> 15

(setf base 100)
(adder 5)     ;==> 105   -- base looked up dynamically in captured env
```

When created inside a `let` or function body the closure captures the
local binding, not a global one:

```lisp
(setf make-adder
  (lambda (n)
    (lambda (x) (+ x n))))

(setf add5  (make-adder 5))
(setf add10 (make-adder 10))

(add5  3)   ;==> 8
(add10 3)   ;==> 13
```

Each call to `make-adder` produces a new closure that captures a separate
binding of `n`.

---

## Lexical Scope Rules

Variables are resolved in the **lexical** (static) scope — the structure
of the source code, not the call stack.  An inner form can see all names
introduced by enclosing `let`, `let*`, `lambda`, `defun`, or `defmacro`
forms.

```lisp
(setf x "global")

(let ((x "outer"))
  (let ((x "inner"))
    x)          ;==> "inner"
  x)            ;==> "outer"

x               ;==> "global"
```

A nested function sees the bindings of its defining scope:

```lisp
(defun make-counter (start step)
  (let ((n start))
    (lambda ()
      (let ((current n))
        (setf n (+ n step))
        current))))

(setf c (make-counter 0 2))
(c)   ;==> 0
(c)   ;==> 2
(c)   ;==> 4
```

`n` lives in the `let` binding created by `make-counter`.  Each call to
the returned closure reads and updates that same `n`.

---

## Closures over Mutable State

Because `setf` mutates the binding a closure captured, closures can
accumulate state across calls.

```lisp
; A simple counter object
(defun make-counter ()
  (let ((count 0))
    (lambda ()
      (setf count (+ count 1))
      count)))

(setf c1 (make-counter))
(setf c2 (make-counter))

(c1)   ;==> 1
(c1)   ;==> 2
(c2)   ;==> 1   -- independent state
(c1)   ;==> 3
```

### Multiple closures sharing state

Return several closures from one factory and they all share the same
captured binding:

```lisp
(defun make-account (initial-balance)
  (let ((balance initial-balance))
    (let ((deposit  (lambda (amt) (setf balance (+ balance amt)) balance))
          (withdraw (lambda (amt) (setf balance (- balance amt)) balance))
          (get-bal  (lambda ()    balance)))
      (list deposit withdraw get-bal))))

(setf acct (make-account 100))
(setf dep  (at 0 acct))
(setf wdw  (at 1 acct))
(setf bal  (at 2 acct))

(dep 50)    ;==> 150
(wdw 30)    ;==> 120
(bal)       ;==> 120
```

---

## Closures as Callbacks

Pass a closure where a function is expected:

```lisp
(setf threshold 3)
(setf big-items
  (remove-if-not
    (lambda (x) (> x threshold))
    '(1 5 2 8 3 7)))
;==> (5 8 7)
```

The lambda closes over `threshold` from the surrounding scope.

---

## Recursive Closures with a Forward Reference

A closure cannot refer to itself by name in a simple `let` because the
name is not yet bound when the lambda is created.  Use a forward reference:

```lisp
(let ((go nil))
  (setf go
    (lambda (lst acc)
      (if (null lst)
          (reverse acc)
          (go (cdr lst) (cons (* (car lst) 2) acc)))))
  (go '(1 2 3 4 5) nil))
;==> (2 4 6 8 10)
```

`go` is first bound to NIL, then rebound to the lambda.  The lambda
captures the binding location of `go`, so when it calls `go` the updated
value (the lambda itself) is found.

---

## Closures and funcall / apply

Closures are first-class values.  Store them, pass them, return them, and
call them with `funcall` or `apply`:

```lisp
(setf ops (list (lambda (x) (* x x))
                (lambda (x) (+ x 10))
                (lambda (x) (- x 1))))

(mapcar (lambda (f) (funcall f 5)) ops)
;==> (25 15 4)
```

---

## Quick Reference

| Concept | How it works |
|---|---|
| Closure created | `(lambda ...)` at the point of evaluation |
| Free variable | Looked up in the captured lexical env |
| Shared state | Multiple closures over the same `let` binding |
| Mutation | `setf` updates the captured binding |
| Call a closure | `(funcall fn arg...)` or `(fn arg...)` |
| Recursive closure | Bind name to NIL first, then `setf` to the lambda |

See also: SCOPING for `let`/`let*` scoping rules.

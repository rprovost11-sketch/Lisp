# Scoping

*Quick reference: `(help "scoping")` — Full documentation: this file.*


Python's Lisp uses **lexical scope**: a variable is resolved by looking
outward through the statically nested forms in which it appears, not
through the runtime call stack.  This document covers how bindings are
created and how scope interacts with closures and global state.

---

## Global Bindings — setf / setq

`setf` (a macro) and `setq` (a primitive) bind or rebind a name in the
**global** environment when used at the top level:

```lisp
(setf x 42)    ; bind X globally
(setf y "hi")  ; bind Y globally

x   ;==> 42
```

Inside a function or `let`, `setf` with a bare symbol mutates the nearest
enclosing binding for that name (which may be local or global):

```lisp
(setf n 0)

(defun bump ()
  (setf n (+ n 1)))   ; mutates the global N

(bump)
n   ;==> 1
```

There is no `defvar` — use `setf` at the top level to define globals.

---

## Local Bindings — let

```lisp
(let ((var1 init1)
      (var2 init2)
      ...)
  body...)
```

All init expressions are evaluated **before** any binding takes effect,
using the enclosing scope.  Then a new scope is created with all bindings
in place, and `body` is evaluated within it.

```lisp
(setf x 10)

(let ((x 1)
      (y (+ x 5)))   ; x here is still the outer x = 10
  (list x y))
;==> (1 15)
```

Bindings are local; the outer binding is unchanged:

```lisp
x   ;==> 10
```

---

## Sequential Bindings — let*

`let*` evaluates and binds each variable in order.  Later bindings can
reference earlier ones:

```lisp
(let* ((x 1)
       (y (+ x 1))    ; x is already 1 here
       (z (+ y 1)))   ; y is already 2 here
  (list x y z))
;==> (1 2 3)
```

Use `let*` when later initializers depend on earlier ones.  Use `let` when
they are independent (clearer intent and avoids order surprises).

---

## Function Parameters — Local by Default

Function parameters are always local to the function body:

```lisp
(setf n 100)

(defun add (n m)   ; this n is local, shadows the global
  (+ n m))

(add 3 4)   ;==> 7
n           ;==> 100  -- global unchanged
```

---

## Nested Scopes

Inner bindings shadow outer ones with the same name:

```lisp
(setf x "global")

(let ((x "outer"))
  (let ((x "inner"))
    x)      ;==> "inner"
  x)        ;==> "outer"

x           ;==> "global"
```

Shadowing is purely lexical.  A function defined inside a `let` sees its
own parameter as the innermost binding:

```lisp
(let ((x 10))
  (defun double-x ()
    (* x 2)))          ; captures the let's x

(setf x 999)           ; rebinds global x
(double-x)             ;==> 20  -- closure still holds let's x binding
```

Wait — `defun` at top level places the function in the global environment,
but the lambda body captures the `let`'s `x`.  See CLOSURES for details
on captured environments.

---

## Mutation Inside a Scope

`setf` with a bare symbol name mutates the variable in the scope that
defines it, not always the global:

```lisp
(let ((count 0))
  (dotimes (i 5)
    (setf count (+ count 1)))
  count)
;==> 5
```

Multiple closures over the same `let` binding share state:

```lisp
(let ((n 0))
  (setf inc! (lambda () (setf n (+ n 1))))
  (setf get  (lambda () n)))

(inc!)  (inc!)  (inc!)
(get)   ;==> 3
```

---

## block — Named Lexical Block

`block` introduces a named scope that can be exited early with
`return-from`:

```lisp
(block my-block
  (setf x 1)
  (when (= x 1)
    (return-from my-block "early"))
  "never reached")
;==> "early"
```

`let` does not give the scope a name; if you need early exit wrap with
`block`:

```lisp
(block nil
  (dolist (x '(1 2 3 4 5))
    (when (= x 3)
      (return x))))
;==> 3
```

See CONTROL-TRANSFER for the full non-local exit API.

---

## progn — Sequential Evaluation

`progn` evaluates forms in order and returns the last value.  It does not
create a new scope — bindings established inside are visible afterward:

```lisp
(progn
  (setf a 1)
  (setf b 2)
  (+ a b))
;==> 3
a   ;==> 1   (global, because progn doesn't scope)
```

Use `let` when you want the bindings to be local.

---

## Scope Summary

| Form | Creates new scope? | Bindings visible after? |
|---|---|---|
| `let` | Yes | No — bindings end with body |
| `let*` | Yes | No |
| `lambda` / `defun` | Yes (per call) | No — local to each invocation |
| `block` | No (reuses current) | Yes |
| `progn` | No | Yes (mutates current) |
| `setf` (top level) | N/A | Yes — global |

---

## Quick Reference

| Expression | Meaning |
|---|---|
| `(setf name val)` | Bind or rebind NAME (global or nearest scope) |
| `(let ((v init)...) body)` | Parallel local bindings |
| `(let* ((v init)...) body)` | Sequential local bindings |
| `(defun name (args) body)` | Named function; params are local |
| `(lambda (args) body)` | Anonymous function; captures surrounding scope |
| `(block name body)` | Named block; supports return-from |
| `(progn body...)` | Sequence; no new scope |

See also: CLOSURES for captured environments; CONTROL-TRANSFER for block/return-from.

---

*See `(help "scoping")` for the condensed quick reference.*

# Control Transfer

Python's Lisp provides several mechanisms for non-local exit — returning
from a point other than the normal end of a computation.  Each mechanism
has a different scope and use case.

| Mechanism | Dynamic? | Who catches | Typical use |
|---|---|---|---|
| `return` / `return-from` | Lexical | Enclosing `block` | Early loop/function exit |
| `catch` / `throw` | Dynamic | Nearest `catch` with matching tag | Cross-function exit |
| `call/cc` | Escape only | The calling context | Abort a computation |
| `handler-case` / `signal` | Dynamic | Nearest matching handler | Structured error handling |

---

## return and return-from — Exiting a Named Block

`block` establishes a named lexical scope.  `return-from` exits it
immediately, returning a value.  `return` is shorthand for
`(return-from nil ...)`.

```lisp
(block my-block
  (dotimes (i 10)
    (when (= i 5)
      (return-from my-block i)))
  "not reached")
;==> 5
```

### return — exit from block nil

```lisp
; while, dotimes, dolist, and for all wrap their body in (block nil ...)
; so (return value) exits any of them

(dotimes (i 100)
  (when (= i 7)
    (return i)))
;==> 7

(dolist (x '(1 2 3 4 5))
  (when (evenp x)
    (return x)))
;==> 2
```

### return-from — exit a named block

```lisp
(defun search-matrix (matrix target)
  (block found
    (dolist (row matrix)
      (dolist (cell row)
        (when (= cell target)
          (return-from found t))))
    nil))

(search-matrix '((1 2 3) (4 5 6) (7 8 9)) 5)
;==> T
(search-matrix '((1 2 3) (4 5 6)) 99)
;==> NIL
```

### Explicit block wrapping

```lisp
(block nil
  (setf i 0)
  (while t
    (setf i (+ i 1))
    (when (= i 3)
      (return i))))
;==> 3
```

---

## catch and throw — Dynamic Exit

`catch` establishes a dynamic catch point tagged by a value.  `throw`
unwinds the stack to the nearest `catch` whose tag is `eql` to the throw's
tag.  Unlike `block`/`return-from`, `catch`/`throw` cross function call
boundaries.

```lisp
(catch 'done
  (dotimes (i 10)
    (when (= i 5)
      (throw 'done i))))
;==> 5
```

The tag is evaluated; any value works as a tag:

```lisp
(setf my-tag 'escape)
(catch my-tag
  (throw 'escape "bye"))
;==> "bye"
```

### Throwing across function calls

```lisp
(defun deep-worker ()
  (throw 'abort "deep abort"))

(catch 'abort
  (setf x 1)
  (deep-worker)      ; throw unwinds through here
  (setf x 2))       ; never reached
;==> "deep abort"
```

### Nested catch — inner tag matches first

```lisp
(catch 'outer
  (catch 'inner
    (throw 'inner "inner wins"))
  "outer continues")
;==> "outer continues"   ; outer catch body continues normally
```

If no matching `catch` exists the throw is unhandled and an error is raised.

---

## call/cc — Escape Continuations

`call/cc` (call with current continuation) captures the current
continuation — what would happen next if this expression returned — and
passes it to `procedure`.  Calling the continuation immediately returns
its argument to the `call/cc` call site.

Python's Lisp supports **escape continuations only**: the continuation
may only be called during the dynamic extent of the `call/cc` call (i.e.,
before `call/cc` itself has returned normally).

```lisp
(call/cc (lambda (k)
  (* 2 (+ 1 (k 42)))))   ; k is called — call/cc returns 42
;==> 42   ; the (* 2 ...) is never completed
```

### abort a search

```lisp
(defun find-even (lst)
  (call/cc (lambda (return)
    (dolist (x lst)
      (when (evenp x)
        (return x)))
    nil)))

(find-even '(1 3 5 4 7))   ;==> 4
(find-even '(1 3 5))        ;==> NIL
```

### Differences from catch/throw

`call/cc` is more general in concept but limited to escape use here.
`catch`/`throw` is simpler and preferred for most cross-function exit
patterns.  Use `call/cc` when you need to capture the continuation
explicitly or when adapting algorithms from continuation-passing style.

---

## Comparison of Mechanisms

### Early exit from a loop

```lisp
; return — simplest, works in loop macros
(dolist (x '(1 2 3 4 5))
  (when (= x 3) (return x)))

; throw — works when you're inside helper functions too
(catch 'found
  (dolist (x '(1 2 3 4 5))
    (when (= x 3) (throw 'found x))))

; call/cc — captures the return as a value
(call/cc (lambda (k)
  (dolist (x '(1 2 3 4 5))
    (when (= x 3) (k x)))))
```

### Aborting a recursive computation

```lisp
; catch/throw is natural for this
(defun product (lst)
  (catch 'zero
    (let ((acc 1))
      (dolist (n lst acc)
        (when (= n 0)
          (throw 'zero 0))
        (setf acc (* acc n))))))

(product '(1 2 3 4 5))   ;==> 120
(product '(1 2 0 4 5))   ;==> 0
```

---

## handler-case and signal — Condition-Based Transfer

`signal` and `handler-case` are the structured alternative to `throw`/
`catch`.  They add typed conditions, allowing callers to distinguish
different kinds of exceptional situations.

```lisp
(handler-case
  (signal 'not-found "item missing")
  (not-found (e)
    (ustring "Recovered: " (condition-message e))))
;==> "Recovered: item missing"
```

See CONDITIONS for the complete condition system API.

---

## Quick Reference

| Expression | Meaning |
|---|---|
| `(block name body...)` | Named lexical block |
| `(return-from name val)` | Exit named block, returning val |
| `(return val)` | Exit `(block nil ...)` |
| `(catch tag body...)` | Dynamic catch point |
| `(throw tag val)` | Exit to matching catch |
| `(call/cc (lambda (k) body))` | Escape continuation |
| `(k val)` | Invoke continuation — returns val to call/cc site |
| `(signal 'type "msg")` | Signal a typed condition |
| `(handler-case form (type (e) body))` | Catch by condition type |

See also: LOOPING for `return` in loop macros; CONDITIONS for the full condition API.

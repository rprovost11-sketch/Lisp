# Errors

*Quick reference: `(help "errors")` — Full documentation: this file.*


Python's Lisp has two ways to report exceptional situations: the `error`
primitive for unrecoverable failures, and the condition system (`signal`,
`handler-case`) for structured recoverable conditions.  This document
focuses on `error`; see CONDITIONS for the full condition system.

---

## The error Primitive

```lisp
(error message)
```

Signals an unrecoverable error with the given message string and
immediately terminates the current computation.  At the top level the
interpreter prints the message and the REPL continues; inside code the
error propagates until something catches it.

```lisp
(error "something went wrong")
; %%% something went wrong

(defun positive-sqrt (n)
  (when (< n 0)
    (error (ustring "positive-sqrt: argument must be non-negative, got " n)))
  (sqrt (float n)))

(positive-sqrt -4)
; %%% positive-sqrt: argument must be non-negative, got -4
```

---

## Building Error Messages

Use `ustring` to build descriptive messages from mixed values:

```lisp
(defun expect-integer (x label)
  (unless (integerp x)
    (error (ustring label " must be an integer, got: " (type-of x)))))

(expect-integer 3.14 "count")
; %%% count must be an integer, got: FLOAT
```

Use `writef`-style formatting for positional placeholders:

```lisp
(defun bounded (x lo hi)
  (unless (and (>= x lo) (<= x hi))
    (error (ustring "value " x " is out of range [" lo ", " hi "]"))))
```

---

## Catching Errors with handler-case

`error` raises a condition of type `ERROR`.  Catch it with `handler-case`
using `error` or `t` as the clause type:

```lisp
(handler-case (error "oops")
  (error (e)
    (ustring "caught: " (condition-message e))))
;==> "caught: oops"

; t catches any condition including error
(handler-case (error "anything")
  (t (e) "recovered"))
;==> "recovered"
```

### Providing a default value

```lisp
(defun safe-divide (a b)
  (handler-case (/ a b)
    (error () 0)))

(safe-divide 10 2)   ;==> 5
(safe-divide 10 0)   ;==> 0
```

### Logging without stopping

```lisp
(defun try-parse (s)
  (handler-case (integer s)
    (error (e)
      (uwriteLn! (ustring "Warning: could not parse \"" s "\": "
                          (condition-message e)))
      nil)))

(try-parse "42")    ;==> 42
(try-parse "abc")   ; prints warning, returns NIL
```

---

## ignore-errors

`ignore-errors` suppresses any error or condition and returns NIL when one
occurs.  Use it when failure is truly acceptable:

```lisp
(ignore-errors (error "swallowed"))   ;==> NIL
(ignore-errors (* 3 4))               ;==> 12

; Multiple body forms
(ignore-errors
  (setf n (integer s))
  (* n 2))
```

Prefer `handler-case` when you need to log the error or distinguish types.

---

## When to Use error vs signal

| Situation | Use |
|---|---|
| Unrecoverable programming error | `error` |
| Invalid argument | `error` |
| Domain violation with no reasonable recovery | `error` |
| Expected failure caller may want to handle | `signal` (user-defined type) |
| Resource not found, optional operation | `signal` or return NIL |

`error` is simpler and appropriate for most internal guards.  `signal`
with typed conditions lets callers distinguish different failure modes
without parsing error strings.  See CONDITIONS for the full signal/handler-case API.

---

## Unhandled Errors

If no `handler-case` catches an error it propagates to the top level where
the interpreter reports it:

```lisp
; Top-level unhandled error
(/ 1 0)
; %%% integer division or modulo by zero
```

Inside a nested call the error unwinds the stack, skipping all intermediate
`let` and function return points, until a `handler-case` or the top level
is reached.

---

## Practical Patterns

### Validate preconditions at the top of a function

```lisp
(defun safe-at (n lst)
  (unless (integerp n)
    (error (ustring "safe-at: index must be integer, got " (type-of n))))
  (unless (listp lst)
    (error "safe-at: second argument must be a list"))
  (unless (and (>= n 0) (< n (length lst)))
    (error (ustring "safe-at: index " n " out of range [0, " (length lst) ")")))
  (at n lst))
```

### Return NIL instead of erroring on optional lookup

```lisp
(defun find-key (key alist)
  (ignore-errors (at key alist)))
```

### Convert an error to a typed condition for callers

```lisp
(defun load-config (path)
  (handler-case (readall (open path))
    (error (e)
      (signal 'config-error
              (ustring "Cannot load config from " path ": "
                       (condition-message e))))))
```

---

## Quick Reference

| Expression | Meaning |
|---|---|
| `(error "msg")` | Signal an unrecoverable error |
| `(ustring ...)` | Build an error message from values |
| `(handler-case form (error (e) body))` | Catch errors |
| `(handler-case form (t (e) body))` | Catch errors and conditions |
| `(ignore-errors body...)` | Suppress all errors, return NIL |
| `(condition-message e)` | Extract message from caught condition |
| `(condition-type e)` | Extract type from caught condition |

See also: CONDITIONS for the full condition/signal/handler-case API.

---

*See `(help "errors")` for the condensed quick reference.*

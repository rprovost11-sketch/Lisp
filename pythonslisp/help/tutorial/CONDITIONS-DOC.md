# The Condition System

*Quick reference: `(help "conditions")` - Full documentation: this file.*


The condition system provides a structured way to signal and handle
exceptional situations.  A condition is a typed object carrying a message.
Code that detects a problem signals a condition; code that knows how to
recover from the problem handles it.  This separates the detection of a
problem from the decision about what to do about it.

---

## Condition Objects

A condition object is created with `make-condition`.  It carries two pieces
of information: a type symbol that identifies the kind of condition, and an
optional message string describing the specific occurrence.

```lisp
(make-condition 'file-error "config.txt not found")
(make-condition 'type-mismatch "expected integer, got string")
(make-condition 'my-error)   ; message defaults to ""
```

Three accessors inspect a condition object:

```lisp
(conditionp obj)        ; T if obj is a condition, NIL otherwise
(condition-type  cond)  ; returns the type symbol
(condition-message cond) ; returns the message string
```

```lisp
(setf c (make-condition 'file-error "not found"))
(conditionp c)           ;==> T
(condition-type c)       ;==> FILE-ERROR
(condition-message c)    ;==> "not found"
```

---

## Signaling a Condition

`signal` raises a condition so that a handler can catch it.  If no handler
catches it, the interpreter reports an unhandled condition error.

```lisp
; Signal by type and message - most common form
(signal 'file-error "config.txt not found")

; Signal by type alone - message is empty
(signal 'overflow)

; Signal a pre-built condition object
(setf c (make-condition 'type-mismatch "expected integer"))
(signal c)
```

---

## Handling Conditions with handler-case

`handler-case` evaluates a protected form and establishes a set of condition
handlers.  If the form completes without signaling, its value is returned and
no handler runs.  If a condition is signaled, the first clause whose type
matches the condition type is executed and its value becomes the result of the
entire `handler-case`.

```lisp
(handler-case protected-form
  (type-1 (var) handler-body...)
  (type-2 (var) handler-body...)
  ...)
```

- `protected-form` - any expression; evaluated with handlers in place
- `type-N` - a symbol naming the condition type to match
- `(var)` - a one-element list binding the condition object in the handler body; use `()` if you do not need the object
- `handler-body` - one or more forms; the value of the last is returned

### Basic usage

```lisp
(handler-case
  (signal 'file-error "log.txt not found")
  (file-error (e)
    (ustring "Caught: " (condition-message e))))
;==> "Caught: log.txt not found"

; Normal execution - handler never runs
(handler-case (* 6 7)
  (file-error (e) 'should-not-run))
;==> 42
```

### Omitting the condition variable

When the handler body does not need the condition object, use an empty
variable list:

```lisp
(handler-case (signal 'oops "ignored")
  (oops () 'recovered))
;==> RECOVERED
```

### Catch-all handlers: T and ERROR

Use `T` or `ERROR` as the clause type to catch any condition, including those
raised by the built-in `error` primitive.

```lisp
; T catches any condition type
(handler-case (signal 'unknown-type "mystery")
  (t (e) (ustring "caught: " (condition-message e))))
;==> "caught: mystery"

; ERROR also catches any condition (including error primitive calls)
(handler-case (error "something went wrong")
  (error (e) (condition-message e)))
;==> "something went wrong"
```

When `error` is caught, the condition object has type `ERROR` and its message
is the string passed to `error`:

```lisp
(handler-case (error "bad input")
  (error (e)
    (list (condition-type e) (condition-message e))))
;==> (ERROR "bad input")
```

### First matching clause wins

Clauses are tried in order.  Put more specific types before catch-alls:

```lisp
(handler-case (signal 'file-error "not found")
  (file-error (e) 'specific-handler)   ; matched first
  (t          (e) 'catch-all))
;==> SPECIFIC-HANDLER
```

---

## Unhandled Conditions

If a condition is signaled and no matching handler is found, the condition
propagates.  At the top level the interpreter reports an unhandled condition
error and displays the condition object.

```lisp
(handler-case (signal 'type-x "unhandled")
  (type-y (e) 'wrong))
; %%% Unhandled condition: ...
```

A condition propagates out of an inner `handler-case` if none of its clauses
match; an enclosing `handler-case` then gets the opportunity to handle it.

---

## Nested handler-case

`handler-case` forms can be nested freely.  The innermost matching handler
wins.

```lisp
; Inner handler catches first
(handler-case
  (handler-case (signal 'inner-err "inner")
    (inner-err (e) 'inner-caught))
  (t (e) 'outer-caught))
;==> INNER-CAUGHT

; Unmatched condition bubbles out to the enclosing handler
(handler-case
  (handler-case (signal 'unmatched "bubbles up")
    (other-type (e) 'wrong))
  (t (e) 'outer-caught))
;==> OUTER-CAUGHT
```

---

## ignore-errors

`ignore-errors` is a convenience macro that suppresses any condition or
error, returning NIL if one occurs and the value of the last body form
otherwise.  It is equivalent to a `handler-case` with a `T` catch-all.

```lisp
(ignore-errors (error "swallowed"))          ;==> NIL
(ignore-errors (signal 'bad-thing "gone"))   ;==> NIL
(ignore-errors (* 3 4))                      ;==> 12

; Multiple body forms - value of the last one
(ignore-errors
  (setf x (parse-integer input))
  (* x 2))
```

Use `ignore-errors` when a failure is truly acceptable and you have a
sensible default or simply want to continue.  Prefer `handler-case` when you
want to log the error, recover specifically, or distinguish condition types.

---

## Practical Patterns

### Providing a default value on failure

```lisp
(defun safe-divide (a b)
  (handler-case (/ a b)
    (error () 0)))

(safe-divide 10 2)   ;==> 5
(safe-divide 10 0)   ;==> 0
```

### Re-signaling with added context

```lisp
(defun load-config (path)
  (handler-case (load-file-contents path)
    (file-error (e)
      (signal 'config-error
              (ustring "Could not load config from " path
                       ": " (condition-message e))))))
```

### Logging errors without stopping execution

```lisp
(defun try-process (item)
  (handler-case
    (process item)
    (error (e)
      (format t "Warning: ~a failed: ~a~%"
              item (condition-message e))
      nil)))
```

### Distinguishing condition types

```lisp
(handler-case (fetch-data url)
  (network-error  (e) (ustring "Network problem: " (condition-message e)))
  (timeout-error  (e) "Request timed out, try again")
  (parse-error    (e) (ustring "Bad response: " (condition-message e)))
  (t              (e) (ustring "Unexpected error: " (condition-message e))))
```

---

## Restarts

Restarts let code that detects a problem offer named recovery strategies.
A handler higher up the call stack can then choose which strategy to use -
without the detecting code needing to know what the right recovery is.

This is the key difference from `handler-case`: with `handler-case`, the
handler runs *after* the stack is unwound - the signaling code is gone.
With restarts, the handler runs *before* unwinding, and can direct control
to a recovery point established by the signaling code.

### restart-case

`restart-case` evaluates a body form with named restarts established.  If a
restart is invoked, the matching clause runs and its value becomes the result
of the `restart-case`.

```lisp
(restart-case body-form
  (restart-name-1 (params...) body...)
  (restart-name-2 (params...) body...)
  ...)
```

If no restart is invoked, the body's value is returned normally:

```lisp
(restart-case (* 6 7)
  (use-value (v) v))
;==> 42
```

### invoke-restart

`invoke-restart` transfers control to a matching restart.  Arguments after
the name are passed to the restart's parameter list:

```lisp
(restart-case (invoke-restart 'use-value 42)
  (use-value (v) v))
;==> 42
```

### handler-bind

`handler-bind` is like `handler-case` but runs the handler *without
unwinding* the stack.  The handler executes in the dynamic context of the
signaler, which means restarts established at the signal point are still
active.

```lisp
(handler-bind ((type handler-fn) ...)
  body...)
```

If the handler returns normally (without invoking a restart), the condition
continues to propagate.

### The Restart Pattern

The typical pattern combines all three: `restart-case` establishes recovery
options around risky code, and `handler-bind` higher up decides which
recovery to use.

```lisp
(defun safe-parse-int (str)
  (restart-case
    (let ((n (read-from-string str)))
      (if (integerp n) n
        (signal 'parse-error (ustring "not an integer: " str))))
    (use-value (v) v)
    (use-zero  ()  0)))

;; Caller decides the recovery strategy:
(handler-bind ((parse-error
                (lambda (c) (invoke-restart 'use-zero))))
  (safe-parse-int "abc"))
;==> 0

(handler-bind ((parse-error
                (lambda (c) (invoke-restart 'use-value -1))))
  (safe-parse-int "abc"))
;==> -1
```

The power is that `safe-parse-int` does not need to know what the right
default is.  Different callers can make different choices.

### find-restart and compute-restarts

```lisp
;; Check if a restart is available before invoking it
(restart-case
  (if (find-restart 'fix) 'available 'not-available)
  (fix () 'fixed))
;==> AVAILABLE

;; List all active restarts
(restart-case (length (compute-restarts))
  (a () nil) (b () nil))
;==> 2

;; Get the name of a restart object
(restart-name (find-restart 'fix))
;==> FIX
```

### Debugger Integration

When a breakpoint fires during `rd` execution, available restarts are
listed automatically.  Use `:r` to invoke them:

```
*** Breakpoint: VALIDATE ***
  (VALIDATE X)
Restarts:
  0: [USE-VALUE]
  1: [SKIP]

debug> :r 0
```

---

## Quick Reference

| Expression | Meaning |
|---|---|
| `(make-condition 'type "msg")` | Create a condition object |
| `(conditionp obj)` | T if obj is a condition |
| `(condition-type cond)` | Type symbol of condition |
| `(condition-message cond)` | Message string of condition |
| `(signal 'type "msg")` | Signal a condition by type and message |
| `(signal cond)` | Signal a pre-built condition object |
| `(handler-case form (type (var) body...))` | Catch condition (unwinds stack) |
| `(handler-case form (t (var) body...))` | Catch-all: any condition or error |
| `(handler-bind ((type fn) ...) body...)` | Bind handlers without unwinding |
| `(ignore-errors body...)` | Suppress all errors, return NIL on failure |
| `(restart-case form (name (params) body...))` | Establish named restarts |
| `(invoke-restart 'name args...)` | Transfer to matching restart |
| `(find-restart 'name)` | Restart object if active, NIL otherwise |
| `(compute-restarts)` | List all active restart objects |
| `(restart-name restart)` | Name symbol of a restart object |

---

*See `(help "conditions")` for the condensed quick reference.*

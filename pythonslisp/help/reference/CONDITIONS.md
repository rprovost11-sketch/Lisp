# Conditions Quick Reference

## Conditions

| Expression | Meaning |
|---|---|
| `(make-condition 'type "msg")` | Create a condition object |
| `(conditionp obj)` | T if obj is a condition |
| `(condition-type cond)` | Type symbol of the condition |
| `(condition-message cond)` | Message string of the condition |
| `(signal 'type "msg")` | Signal a condition by type and message |
| `(signal cond)` | Signal a pre-built condition object |

## Handlers

| Expression | Meaning |
|---|---|
| `(handler-case form (type (var) body...))` | Catch condition by type (unwinds stack) |
| `(handler-case form (t (var) body...))` | Catch-all: any condition or error |
| `(handler-bind ((type fn) ...) body...)` | Bind handlers without unwinding |
| `(ignore-errors body...)` | Suppress all errors; return NIL on failure |

## Restarts

| Expression | Meaning |
|---|---|
| `(restart-case form (name (params) body...))` | Establish named restarts around form |
| `(invoke-restart 'name args...)` | Transfer control to matching restart |
| `(find-restart 'name)` | Return restart object if active, NIL otherwise |
| `(compute-restarts)` | Return list of all active restart objects |
| `(restart-name restart)` | Name symbol of a restart object |

## Pattern

```lisp
;; Handler-case: catch and recover (stack is unwound)
(handler-case (risky-operation)
  (specific-error (e) (handle e))
  (t              (e) (fallback e)))

;; Handler-bind + restart-case: recovery without unwinding
(handler-bind ((error (lambda (c) (invoke-restart 'use-value 0))))
  (restart-case (risky-operation)
    (use-value (v) v)
    (skip      ()  nil)))
```

See `(help "conditions-doc")` for full documentation and examples.

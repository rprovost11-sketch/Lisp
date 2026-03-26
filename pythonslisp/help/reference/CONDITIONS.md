# Conditions Quick Reference

| Expression | Meaning |
|---|---|
| `(make-condition 'type "msg")` | Create a condition object |
| `(conditionp obj)` | T if obj is a condition |
| `(condition-type cond)` | Type symbol of the condition |
| `(condition-message cond)` | Message string of the condition |
| `(signal 'type "msg")` | Signal a condition by type and message |
| `(signal 'type)` | Signal with empty message |
| `(signal cond)` | Signal a pre-built condition object |
| `(handler-case form (type (var) body...))` | Catch condition by type |
| `(handler-case form (t (var) body...))` | Catch-all: any condition or error |
| `(handler-case form (error (var) body...))` | Catch-all: same as T |
| `(ignore-errors body...)` | Suppress all errors; return NIL on failure |

## Pattern

```lisp
(handler-case (risky-operation)
  (specific-error (e) (handle e))
  (t              (e) (fallback e)))
```

See `(help "conditions-doc")` for full documentation and examples.

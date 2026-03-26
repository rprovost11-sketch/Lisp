# Errors Quick Reference

| Expression | Meaning |
|---|---|
| `(error "msg")` | Signal an unrecoverable error |
| `(error (ustring "bad: " val))` | Error with computed message |
| `(handler-case form (error (e) body))` | Catch errors |
| `(handler-case form (t (e) body))` | Catch errors and conditions |
| `(ignore-errors body...)` | Suppress all errors; return NIL on failure |
| `(condition-message e)` | Extract message string from caught condition |
| `(condition-type e)` | Extract type symbol from caught condition |

## When to use error vs signal

| Situation | Use |
|---|---|
| Invalid argument, programming error | `(error "msg")` |
| Caller may want to distinguish failure types | `(signal 'my-type "msg")` |
| Failure is acceptable, continue silently | `(ignore-errors ...)` |

See `(help "errors-doc")` for full documentation and examples.

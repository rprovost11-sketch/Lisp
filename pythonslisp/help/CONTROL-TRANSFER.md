# Control Transfer Quick Reference

| Expression | Meaning |
|---|---|
| `(block name body...)` | Named lexical block |
| `(return-from name val)` | Exit named block, returning val |
| `(return val)` | Exit `(block nil ...)` — works inside loop macros |
| `(catch tag body...)` | Dynamic catch point (crosses function boundaries) |
| `(throw tag val)` | Exit to nearest `catch` with matching tag |
| `(call/cc (lambda (k) body))` | Capture escape continuation as k |
| `(k val)` | Invoke continuation — call/cc immediately returns val |
| `(signal 'type "msg")` | Signal a typed condition |
| `(handler-case form (type (e) body))` | Catch by condition type |

## Mechanism Comparison

| | Scope | Crosses fns? | Tagged? | Use for |
|---|---|---|---|---|
| `block`/`return-from` | Lexical | No | Named | Early loop/fn exit |
| `catch`/`throw` | Dynamic | Yes | Any value | Cross-function abort |
| `call/cc` | Escape only | Yes | N/A | Abort a computation |
| `handler-case`/`signal` | Dynamic | Yes | Typed | Structured errors |

See `(help "control-transfer-doc")` for full documentation and examples.

# Control Transfer Quick Reference

| Expression | Meaning |
|---|---|
| `(block name body...)` | Named lexical block |
| `(return-from name val)` | Exit named block, returning val |
| `(return val)` | Exit `(block nil ...)` - works inside loop macros |
| `(catch tag body...)` | Dynamic catch point (crosses function boundaries) |
| `(throw tag val)` | Exit to nearest `catch` with matching tag |
| `(call/cc (lambda (k) body))` | Capture continuation as k; run body |
| `(k val)` | Deliver val to call/cc site; resume saved state |
| `(dynamic-wind before thunk after)` | Run before; run thunk; run after on any exit or re-entry |
| `(signal 'type "msg")` | Signal a typed condition |
| `(handler-case form (type (e) body))` | Catch by condition type |

## Mechanism Comparison

| | Scope | Crosses fns? | Tagged? | Use for |
|---|---|---|---|---|
| `block`/`return-from` | Lexical | No | Named | Early loop/fn exit |
| `catch`/`throw` | Dynamic | Yes | Any value | Cross-function abort |
| `call/cc` | First-class | Yes | N/A | Generators, coroutines, complex control |
| `dynamic-wind` | Dynamic | Yes | N/A | Guaranteed cleanup around non-local exits |
| `handler-case`/`signal` | Dynamic | Yes | Typed | Structured errors |

See `(help "control-transfer-doc")` for full documentation and examples.
See `(help "continuations-doc")` for the full `call/cc` and `dynamic-wind` guide.

# Scoping Quick Reference

| Expression | Meaning |
|---|---|
| `(setf name val)` | Bind/rebind NAME (global at top level; nearest scope inside) |
| `(let ((v init)...) body)` | Parallel local bindings; inits use outer scope |
| `(let* ((v init)...) body)` | Sequential local bindings; each init sees prior ones |
| `(defun name (args) body)` | Named function; params are local per call |
| `(lambda (args) body)` | Anonymous function; captures surrounding scope |
| `(block name body)` | Named block; supports `return-from` for early exit |
| `(progn body...)` | Sequential eval; no new scope |

## Scope Rules

| Form | New scope? | Bindings escape? |
|---|---|---|
| `let` / `let*` | Yes | No - end with body |
| `lambda` / `defun` | Yes (per call) | No - local per invocation |
| `block` | No | Yes (same scope) |
| `progn` | No | Yes (mutates current) |
| top-level `setf` | N/A | Yes - global |

Inner `setf` mutates the **nearest enclosing binding**, which may be local or global.

See `(help "scoping-doc")` for full documentation and examples.

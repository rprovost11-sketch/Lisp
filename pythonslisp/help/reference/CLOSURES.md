# Closures Quick Reference

| Concept | How it works |
|---|---|
| Create a closure | `(lambda ...)` captures the lexical env at that point |
| Free variable | Looked up in the captured env, not the call site |
| Named function | `defun` is `(setq name (lambda ...))` - also a closure |
| Shared state | Multiple closures over the same `let` binding share it |
| Mutation | `(setf name val)` updates the captured binding |
| Call a closure | `(funcall fn arg...)` or `((lambda ...) arg...)` |
| Recursive closure | Bind name to NIL first, then `setf` to the lambda |

## Lexical Scope Rule

A name resolves in the **statically enclosing** form where it was bound,
not the call stack.  Inner bindings shadow outer ones with the same name.

```lisp
(let ((n 0))
  (setf inc! (lambda () (setf n (+ n 1)) n)))

(inc!)   ;==> 1
(inc!)   ;==> 2
```

See `(help "closures-doc")` for full documentation and examples.

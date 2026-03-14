# Macros Quick Reference

| Expression | Meaning |
|---|---|
| `(defmacro name (args) body)` | Define macro NAME |
| `` `form `` | Quasiquote — quoted data with substitutions |
| `,expr` | Unquote — insert value of expr |
| `,@expr` | Unquote-splice — insert list elements inline |
| `(macroexpand '(form))` | Fully expand all macros in form |
| `(macroexpand-1 '(form))` | Expand exactly one macro level |
| `(gensym)` | Fresh unique symbol G*n* |
| `(gensym "prefix")` | Fresh symbol with given prefix |
| `(macrop x)` | T if x is a macro object |

## Key Rules

- Macro arguments arrive **unevaluated**; the body returns code to be evaluated.
- Use `gensym` to avoid variable capture when introducing local bindings.
- Use `macroexpand` to debug: inspect the generated code before running it.
- `defun` and `defmacro` are themselves macros.

See `(help "macros-doc")` for full documentation and examples.

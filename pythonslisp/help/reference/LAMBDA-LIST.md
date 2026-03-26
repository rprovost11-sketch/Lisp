# Lambda List Quick Reference

## Formal Grammar

```
lambda-list -> ( {var}*
                 [ &optional {var | (var [initForm [pvar]])}* ]
                 [ &rest var ]
                 [ &key {var | ({var | (keyword var)} [initForm [pvar]])}*
                        [&allow-other-keys] ]
                 [ &aux {var | (var [initForm])}* ] )
```

Macro lambda lists additionally support:
- `&body` as an alias for `&rest`
- Nested list patterns (destructuring) in positional and `&body` positions

## Section Ordering

Positional → `&optional` → `&rest` / `&body` → `&key` → `&allow-other-keys` → `&aux`

## Parameter Spec Forms

| Form | Section | Meaning |
|---|---|---|
| `var` | positional | Required; bound by position |
| `var` | `&optional` | Optional; default NIL |
| `(var default)` | `&optional` or `&key` | Optional with default |
| `(var default svar)` | `&optional` or `&key` | As above; `svar` = T if supplied |
| `var` | `&rest` / `&body` | Collects remaining positional args as list |
| `var` | `&key` | Caller passes `:VAR val` |
| `((kw var) default)` | `&key` | Caller uses `:kw`; binds as `var` |
| `&allow-other-keys` | `&key` section | Suppress unknown-keyword error |
| `(var init)` | `&aux` | Local binding; not an argument |
| `(pattern ...)` | macro positional | Destructure nested list argument |

## Notes

- Duplicate parameter names across all sections are an error.
- Defaults are evaluated at call time, not definition time.
- First occurrence wins when a keyword argument is repeated.
- `&optional` and `&key` together in one lambda list is legal but confusing.
- Empty sections (`(&optional)`) are legal and equivalent to omitting the section.

See `(help "lambda-list-doc")` for full documentation and examples.

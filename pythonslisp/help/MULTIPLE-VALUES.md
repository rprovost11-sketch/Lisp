# Multiple Values Quick Reference

| Expression | Meaning |
|---|---|
| `(values v1 v2 ...)` | Return multiple values |
| `(values)` | Return zero values |
| `(multiple-value-bind (v1 v2) form body...)` | Bind all values from form |
| `(multiple-value-list form)` | Collect values into a list |
| `(values-list lst)` | Spread list as multiple values |
| `(nth-value n form)` | Select the Nth value (0-indexed); NIL if out of range |

## Rounding functions return two values

| Expression | Values returned |
|---|---|
| `(floor n d)` | Quotient (floor), remainder |
| `(ceiling n d)` | Quotient (ceiling), remainder |
| `(round n d)` | Quotient (nearest-even), remainder |
| `(truncate n d)` | Quotient (toward zero), remainder |

```lisp
(multiple-value-bind (q r) (floor 17 5)
  (list q r))   ;==> (3 2)

(nth-value 1 (floor 17 5))   ;==> 2   ; just the remainder
```

In scalar context (let init, if condition, function arg) only the **primary** value is used.

See `(help "multiple-values-doc")` for full documentation and examples.

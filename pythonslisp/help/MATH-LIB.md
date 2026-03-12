# Math Library Quick Reference

## Arithmetic

| Expression | Meaning |
|---|---|
| `(+ a b ...)` | Sum (variadic) |
| `(- a b)` / `(- a)` | Subtract / negate |
| `(* a b ...)` | Product (variadic) |
| `(/ a b)` | Exact division → fraction |
| `(// a b)` | Integer (floor) division |
| `(mod a b)` | Modulo (sign follows divisor) |
| `(expt b e)` | b raised to power e |
| `(sqrt x)` | Square root (float) |
| `(isqrt n)` | Integer square root (floor) |
| `(abs x)` | Absolute value |
| `(signum x)` | -1, 0, or 1 |
| `(gcd a b ...)` | Greatest common divisor |
| `(lcm a b ...)` | Least common multiple |

## Rounding — return two values: quotient + remainder

| Expression | Meaning |
|---|---|
| `(floor n d)` | Floor division |
| `(ceiling n d)` | Ceiling division |
| `(round n d)` | Round to nearest even |
| `(truncate n d)` | Truncate toward zero |

## Logarithms and Exponentials

| Expression | Meaning |
|---|---|
| `(log x)` | Natural log (base e) |
| `(log x base)` | Log in given base |
| `(exp x)` | e^x |
| `(ln x)` | Alias for `(log x)` |

## Trigonometry (radians)

`(sin x)` `(cos x)` `(tan x)` `(asin x)` `(acos x)` `(atan x)` `(atan y x)`

## Min / Max / Random

| Expression | Meaning |
|---|---|
| `(min a b ...)` / `(max a b ...)` | Minimum / maximum |
| `(random n)` | Random number in [0, n) |
| `(average a b ...)` | Arithmetic mean |
| `(incf place)` / `(decf place)` | Increment / decrement by 1 |

## Constants and Predicates

`PI`  `E`  `(zerop x)`  `(plusp x)`  `(minusp x)`  `(evenp n)`  `(oddp n)`

## Comparison

`(= a b ...)` `(/= a b ...)` `(< a b ...)` `(<= ...)` `(> ...)` `(>= ...)`

See `(help "math-lib-doc")` for full documentation and examples.

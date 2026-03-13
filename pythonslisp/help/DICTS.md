# Dicts Quick Reference

## Creating

| Expression | Meaning |
|---|---|
| `(make-dict)` | Empty dict |
| `(make-dict (key val) ...)` | Dict with initial key-value pairs |

Keys are **not evaluated** — use a symbol or string literal directly.
Symbol keys are interned as uppercase strings. Values **are** evaluated.

```lisp
(make-dict (name "Alice") (age 30))   ; symbol keys -> "NAME", "AGE"
(make-dict ("x" 1) ("y" 2))          ; string keys -> "x", "y"
```

## Access and Mutation

| Expression | Meaning |
|---|---|
| `(at key dict)` | Value for key; error if missing |
| `(at-set key dict val)` | Set key to val; returns val (mutates) |
| `(setf (at key dict) val)` | Same via setf syntax |
| `(update! dict1 dict2)` | Merge dict2 into dict1 (mutates dict1); returns dict1 |

## Membership

| Expression | Meaning |
|---|---|
| `(hasKey? key dict)` | T if key exists |
| `(hasValue? val dict)` | T if val exists anywhere in dict |
| `(dictp x)` | T if x is a dict |

## Printing

Dicts print as `(DICT ("key1" val1) ("key2" val2) ...)`.

See `(help "dicts-doc")` for full documentation and examples.

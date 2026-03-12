# Types Quick Reference

## Type Hierarchy (selected)

`T` → `ATOM` → `NUMBER` → `INTEGER` / `FLOAT` / `RATIO`
`T` → `ATOM` → `STRING` / `SYMBOL` / `FUNCTION` / `MACRO` / `STREAM` / `MODULE` / `DICT` / `<struct>`
`T` → `LIST` → `NULL` (NIL) / `CONS` (non-empty list)

## Querying Types

| Expression | Meaning |
|---|---|
| `(type-of x)` | Type name as a symbol |
| `(typep x 'type)` | T if x is of type |
| `(typecase x (t1 body) ...)` | Dispatch on type; last clause may be `t` |
| `(etypecase x (t1 body) ...)` | Like typecase; error if no clause matches |

## Compound type-specifiers for typep

| Specifier | Matches |
|---|---|
| `(or t1 t2 ...)` | Union |
| `(and t1 t2 ...)` | Intersection |
| `(not t)` | Complement |
| `(member v1 v2 ...)` | Explicit value set |
| `(satisfies fn)` | fn returns true |
| `(integer lo hi)` | Integer in range; `*` = unbounded; `(n)` = exclusive |

## Type Predicates

`(numberp x)` `(integerp x)` `(floatp x)` `(rationalp x)` `(stringp x)`
`(symbolp x)` `(listp x)` `(consp x)` `(atom x)` `(dictp x)`
`(functionp x)` `(macrop x)` `(streamp x)` `(modulep x)`
`(file-stream-p x)` `(string-stream-p x)`

## Equality

| Expression | Meaning |
|---|---|
| `(eq a b)` | Object identity; symbols by name |
| `(eql a b)` | Like eq; numbers same type+value |
| `(equal a b)` | Structural; lists recursively, strings by content |
| `(equalp a b)` | Like equal; case-insensitive strings, type-loose numbers |

## Type Conversion

`(float n)` `(integer n)` `(rational n)` `(string v...)` `(ustring v...)` `(make-symbol s)`

See `(help "types-doc")` for full documentation and examples.

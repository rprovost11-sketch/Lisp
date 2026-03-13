# Quoting Quick Reference

## The Four Quote Forms

| Syntax | Expands to | Meaning |
|---|---|---|
| `'expr`    | `(quote expr)`     | Return expr unevaluated |
| `` `expr `` | `(backquote expr)` | Template -- mostly unevaluated |
| `,expr`    | `(comma expr)`     | Evaluate expr inside a backquote |
| `,@expr`   | `(comma-at expr)`  | Splice a list into a backquote |

## Quote -- Suppress Evaluation

```lisp
'foo          ;==> FOO        (symbol, not a variable lookup)
'(1 2 3)      ;==> (1 2 3)   (list literal)
'(a b c)      ;==> (A B C)   (list of symbols)
```

## Backquote -- Templates

```lisp
(setf x 42)
`(the answer is ,x)      ;==> (THE ANSWER IS 42)
`(a ,(+ 1 2) c)          ;==> (A 3 C)
```

## Unquote-Splicing -- Insert a List

```lisp
(setf items '(2 3 4))
`(1 ,@items 5)           ;==> (1 2 3 4 5)
```

Backquote is the primary tool for writing macros.

See `(help "quoting-doc")` for full documentation and examples.

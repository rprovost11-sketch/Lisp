# Quoting Quick Reference

## The Four Quote Forms

| Syntax | Expands to | Meaning |
|---|---|---|
| `'expr`    | `(quote expr)`     | Return expr unevaluated |
| `` `expr `` | `(quasiquote expr)` | Template -- mostly unevaluated |
| `,expr`    | `(unquote expr)`     | Evaluate expr inside a quasiquote |
| `,@expr`   | `(unquote-splicing expr)`  | Splice a list into a quasiquote |

## Quote -- Suppress Evaluation

```lisp
'foo          ;==> FOO        (symbol, not a variable lookup)
'(1 2 3)      ;==> (1 2 3)   (list literal)
'(a b c)      ;==> (A B C)   (list of symbols)
```

## Quasiquote -- Templates

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

Quasiquote is the primary tool for writing macros.

See `(help "quoting-doc")` for full documentation and examples.

# Quoting and Backquote

*Quick reference: `(help "quoting")` -- Full documentation: this file.*


In Lisp, code and data have the same structure -- both are s-expressions.
Quoting is the mechanism that tells the evaluator to treat an expression
as data rather than as code to run.

---

## quote -- Returning Data Unevaluated

```lisp
(quote expr)
'expr          ; shorthand
```

Without quoting, every symbol is looked up as a variable and every list
is treated as a function call.  `quote` suppresses that:

```lisp
foo            ; evaluates foo as a variable
'foo           ;==> FOO   (the symbol itself)

(+ 1 2)        ;==> 3     (function call)
'(+ 1 2)       ;==> (+ 1 2)   (a three-element list)
```

A quoted list is a literal -- its elements are not evaluated:

```lisp
'(a b c)       ;==> (A B C)   (three symbols)
'(1 "x" nil)   ;==> (1 "x" NIL)
```

Quote is shallow -- it suppresses evaluation of the top-level expression
only.  The elements of a quoted list are data, but they are not themselves
quoted in a nested sense.  This rarely matters in practice because you
would not normally quote a list that contains sub-expressions you want
to evaluate.

---

## Backquote -- Quasi-quotation

Backquote (`` ` ``) is like `quote` with holes.  Most of the expression
is returned unevaluated, but positions marked with `,` are evaluated and
their results spliced in.

```lisp
`expr          ; like 'expr, but , and ,@ are active inside
```

### Unquote -- `,`

`,expr` inside a backquote evaluates `expr` and inserts the result:

```lisp
(setf name "Alice")
(setf age  30)

`(the person ,name is ,age years old)
;==> (THE PERSON "Alice" IS 30 YEARS OLD)
```

Positions without `,` are left as symbols or literals:

```lisp
`(x is ,x)     ; x the symbol, then x the variable
; if x = 7:
;==> (X IS 7)
```

### Unquote-Splicing -- `,@`

`,@expr` evaluates `expr` (which must produce a list) and splices its
elements directly into the surrounding list -- no extra nesting:

```lisp
(setf middle '(b c d))

`(a ,middle e)     ;==> (A (B C D) E)   -- , inserts the list as one element
`(a ,@middle e)    ;==> (A B C D E)     -- ,@ splices the elements in
```

The difference: `,` inserts one value; `,@` inserts many.

---

## Practical Uses

### Building lists with computed parts

```lisp
(setf fields '(name age city))
`(select ,@fields from users)
;==> (SELECT NAME AGE CITY FROM USERS)
```

### Writing macros

Backquote is the standard tool for constructing the code a macro returns.
The macro body is a template; `,` and `,@` fill in the parts supplied by
the caller:

```lisp
(defmacro swap! (a b)
  `(let ((tmp ,a))
     (setf ,a ,b)
     (setf ,b tmp)))
```

When `(swap! x y)` is called, the macro returns:

```lisp
(let ((tmp x))
  (setf x y)
  (setf y tmp))
```

`a` and `b` are the unevaluated argument forms `x` and `y`; `,a` and `,b`
splice them into the template.

### Generating function calls at runtime

```lisp
(setf op '+)
(setf args '(3 4))

(eval `(,op ,@args))    ;==> 7
```

---

## Nesting Backquotes

Backquotes can be nested, which is occasionally needed when a macro
generates another macro.  Each `,` peels off one level of quoting.
This is an advanced use case; in practice a single level handles the
vast majority of macro-writing tasks.

---

## Summary

| Form | Evaluates? | Use |
|---|---|---|
| `'x`      | Nothing inside | Data literals, symbol constants |
| `` `x ``  | Only `,` and `,@` positions | Macro templates, code generation |
| `` `,x `` | x is evaluated | Inject a computed value into a template |
| `` `,@x `` | x is evaluated (must be list) | Splice a computed list into a template |

The key intuition: `'` says "this is data"; `` ` `` says "this is mostly
data, except where I tell you otherwise."

See also: MACROS for `defmacro` and macro expansion.

---

*See `(help "quoting")` for the condensed quick reference.*

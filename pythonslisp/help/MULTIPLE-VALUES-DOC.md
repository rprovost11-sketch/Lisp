# Multiple Values

*Quick reference: `(help "multiple-values")` - Full documentation: this file.*


Python's Lisp supports returning more than one value from a function, as
in Common Lisp.  Multiple values are distinct from returning a list: the
caller decides whether it wants all values, just the primary value, or
specific individual values.

---

## Returning Multiple Values

Use `values` to return more than one value:

```lisp
(defun min-max (lst)
  "Return the minimum and maximum of a list."
  (values (reduce 'min lst)
          (reduce 'max lst)))

(min-max '(3 1 4 1 5 9))
;==> 1
;==> 9        ; two lines of output in the REPL
```

```lisp
; Zero values
(values)        ;==> (no output in REPL)

; One value - same as returning the value directly
(values 42)     ;==> 42
```

---

## Receiving Multiple Values

### multiple-value-bind

The primary tool for capturing multiple return values:

```lisp
(multiple-value-bind (var1 var2 ...) values-form
  body...)
```

```lisp
(multiple-value-bind (lo hi) (min-max '(3 1 4 1 5 9))
  (ustring "range: " lo " to " hi))
;==> "range: 1 to 9"
```

Extra variables beyond the number of values are bound to NIL.  Extra values
beyond the number of variables are discarded:

```lisp
(multiple-value-bind (a b c) (values 10 20)
  (list a b c))
;==> (10 20 NIL)

(multiple-value-bind (a) (values 1 2 3)
  a)
;==> 1    ; 2 and 3 are discarded
```

### Rounding functions

`floor`, `ceiling`, `round`, and `truncate` all return two values:
the quotient and the remainder.  This is one of the most common uses of
`multiple-value-bind`:

```lisp
(multiple-value-bind (q r) (floor 17 5)
  (list q r))
;==> (3 2)

(multiple-value-bind (q r) (round 7 2)
  (list q r))
;==> (4 -1)

; If you only need the quotient, just use it directly
(floor 17 5)   ;==> 3   (primary value in scalar context)
```

---

## Working with Multiple-Value Lists

### multiple-value-list

Collects all values into a plain list:

```lisp
(multiple-value-list (values 1 2 3))   ;==> (1 2 3)
(multiple-value-list (floor 17 5))     ;==> (3 2)
(multiple-value-list 42)               ;==> (42)
```

### values-list

The inverse: spread a list into multiple values.

```lisp
(values-list '(10 20 30))
;==> 10
;==> 20
;==> 30
```

---

## Selecting a Specific Value

### nth-value

Returns the Nth value (zero-indexed) from a multiple-values form:

```lisp
(nth-value 0 (values 10 20 30))   ;==> 10
(nth-value 1 (values 10 20 30))   ;==> 20
(nth-value 2 (values 10 20 30))   ;==> 30
(nth-value 1 (floor 17 5))        ;==> 2   (the remainder)
(nth-value 5 (values 1 2))        ;==> NIL  (out of range)
```

---

## Primary Value in Scalar Context

When a multiple-values expression appears in a position that expects a
single value - such as a `let` initializer, the condition of an `if`, or
an argument to a function - only the **primary** (first) value is used.
Remaining values are silently discarded.

```lisp
; Only the quotient is used here
(let ((q (floor 17 5)))
  q)
;==> 3

; Only T or NIL matters
(if (values nil 99) 'yes 'no)
;==> NO    ; first value is NIL
```

---

## Defining Your Own Multiple-Value Functions

```lisp
(defun divide-and-check (n d)
  "Return quotient, remainder, and whether it divides evenly."
  (multiple-value-bind (q r) (floor n d)
    (values q r (= r 0))))

(multiple-value-bind (q r exact) (divide-and-check 10 5)
  (list q r exact))
;==> (2 0 T)

(multiple-value-bind (q r exact) (divide-and-check 10 3)
  (list q r exact))
;==> (3 1 NIL)
```

---

## Quick Reference

| Expression | Meaning |
|---|---|
| `(values v1 v2 ...)` | Return multiple values |
| `(values)` | Return zero values |
| `(multiple-value-bind (v1 v2) form body...)` | Bind all values from form |
| `(multiple-value-list form)` | Collect values into a list |
| `(values-list lst)` | Spread list as multiple values |
| `(nth-value n form)` | Select the Nth value (0-indexed) |
| `(floor n d)` | Quotient + remainder (2 values) |
| `(ceiling n d)` | Ceiling quotient + remainder |
| `(round n d)` | Round quotient + remainder |
| `(truncate n d)` | Truncate quotient + remainder |

See also: MATH-LIB for floor/ceiling/round/truncate details.

---

*See `(help "multiple-values")` for the condensed quick reference.*

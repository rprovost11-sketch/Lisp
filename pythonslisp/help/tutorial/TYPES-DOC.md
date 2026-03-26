# Types

*Quick reference: `(help "types")` - Full documentation: this file.*


Python's Lisp is dynamically typed: every value carries its type at
runtime.  This document describes the type system, how to query and test
types, and the full type hierarchy.

---

## Type Hierarchy

```
T  (the universal supertype - everything is of type T)
├── NULL       (NIL, the empty list)
├── CONS       (non-empty list)
├── ATOM       (everything that is not a non-empty list)
│   ├── NUMBER / REAL
│   │   ├── INTEGER    (Python int)
│   │   ├── FLOAT      (Python float)
│   │   └── RATIO      (Python Fraction, e.g. 1/3)
│   │   └── RATIONAL   (INTEGER or RATIO)
│   ├── STRING
│   ├── SYMBOL
│   ├── BOOLEAN    (T or NIL)
│   ├── FUNCTION   (lambda / defun)
│   ├── MACRO      (defmacro)
│   ├── PRIMITIVE  (built-in C-level function)
│   ├── CONTINUATION (call/cc continuation)
│   ├── STREAM
│   │   ├── FILE-STREAM   (open)
│   │   └── STRING-STREAM (make-string-output-stream etc.)
│   ├── MODULE
│   ├── DICT
│   └── <struct-type-name>   (any defstruct type)
```

Note: `LIST` = `NULL` ∪ `CONS`.  `ATOM` = anything that is not `CONS`.

---

## type-of

Returns the type of a value as a symbol:

```lisp
(type-of 42)          ;==> INTEGER
(type-of 3.14)        ;==> FLOAT
(type-of 1/3)         ;==> RATIO
(type-of "hello")     ;==> STRING
(type-of 'foo)        ;==> SYMBOL
(type-of '())         ;==> NULL
(type-of '(1 2))      ;==> CONS
(type-of t)           ;==> SYMBOL   (T is a symbol)
(type-of nil)         ;==> NULL

(type-of (lambda (x) x))  ;==> FUNCTION
(type-of 'car)             ;==> SYMBOL  (symbol, not the primitive)
(type-of car)              ;==> PRIMITIVE

(defstruct point (x 0) (y 0))
(type-of (make-point))   ;==> POINT   (the struct type name)

(type-of (open "f.txt"))               ;==> FILE-STREAM
(type-of (make-string-output-stream))  ;==> STRING-STREAM
```

---

## typep - Test Against a Type Specifier

```lisp
(typep obj type-specifier)
```

Returns T if `obj` belongs to the type, NIL otherwise.

### Atomic type specifiers

```lisp
(typep 42    'integer)    ;==> T
(typep 3.14  'float)      ;==> T
(typep 1/3   'ratio)      ;==> T
(typep 42    'number)     ;==> T
(typep "hi"  'string)     ;==> T
(typep 'foo  'symbol)     ;==> T
(typep '()   'null)       ;==> T
(typep '(1)  'cons)       ;==> T
(typep '(1)  'list)       ;==> T
(typep '()   'list)       ;==> T
(typep 42    'atom)       ;==> T
(typep '(1)  'atom)       ;==> NIL  (cons cells are not atoms)
(typep 42    't)           ;==> T    (everything is of type T)
(typep 42    'nil)         ;==> NIL  (nothing is of type NIL)

; Struct types
(defstruct point (x 0) (y 0))
(typep (make-point) 'point)   ;==> T
```

### Compound type specifiers

```lisp
; OR - union
(typep 42  '(or integer string))    ;==> T
(typep "h" '(or integer string))    ;==> T
(typep 3.0 '(or integer string))    ;==> NIL

; AND - intersection
(typep 5 '(and integer (integer 0 10)))  ;==> T  (int in [0,10])
(typep 15 '(and integer (integer 0 10))) ;==> NIL

; NOT
(typep "x" '(not number))   ;==> T
(typep 42  '(not number))   ;==> NIL

; MEMBER - explicit set of values
(typep 'b '(member a b c))   ;==> T
(typep 'd '(member a b c))   ;==> NIL

; SATISFIES - arbitrary predicate
(typep 4 '(satisfies evenp))   ;==> T
(typep 3 '(satisfies evenp))   ;==> NIL

; Numeric ranges - inclusive bound, or (n) for exclusive
(typep 5 '(integer 1 10))         ;==> T    ; 1 <= 5 <= 10
(typep 0 '(integer 1 10))         ;==> NIL
(typep 5 '(integer (0) *))        ;==> T    ; strictly > 0
(typep 5 '(real * (5)))           ;==> NIL  ; strictly < 5
```

---

## typecase and etypecase

Dispatch on type without a chain of `typep` calls:

```lisp
(defun describe (x)
  (typecase x
    (integer  (ustring x " is an integer"))
    (float    (ustring x " is a float"))
    (string   (ustring "\"" x "\" is a string"))
    (cons     (ustring "a list of length " (length x)))
    (t        "something else")))

(describe 42)       ;==> "42 is an integer"
(describe "hi")     ;==> "\"hi\" is a string"
(describe '(1 2))   ;==> "a list of length 2"
(describe t)        ;==> "something else"
```

`etypecase` is like `typecase` but raises an error when no clause matches,
instead of returning NIL:

```lisp
(etypecase 3.14
  (integer "int")
  (string  "str"))
; %%% etypecase: no matching clause for value: 3.14
```

---

## Type Predicate Functions

Each common type has a dedicated predicate:

```lisp
(numberp   42)    ;==> T
(integerp  42)    ;==> T
(floatp  3.14)    ;==> T
(rationalp 1/3)   ;==> T
(stringp "hi")    ;==> T
(symbolp 'foo)    ;==> T
(listp '(1 2))    ;==> T
(listp '())       ;==> T    (NIL is a list)
(consp '(1 2))    ;==> T
(consp '())       ;==> NIL  (NIL is not a cons)
(atom 42)         ;==> T
(atom '(1))       ;==> NIL
(dictp (map (a 1))) ;==> T
(modulep mymod)     ;==> T
(streamp f)         ;==> T
(file-stream-p f)   ;==> T
(string-stream-p s) ;==> T
(functionp (lambda (x) x))  ;==> T
(macrop 'when)               ;==> NIL  (when is looked up, not quoted right)
(functionp car)              ;==> NIL  (car is a primitive)
```

For struct types, `defstruct` generates a predicate automatically:

```lisp
(defstruct point (x 0) (y 0))
(point-p (make-point))   ;==> T
(point-p 42)             ;==> NIL
```

---

## Equality Predicates

Python's Lisp has three equality predicates:

| Predicate | Meaning |
|---|---|
| `(eq a b)` | Same object identity; symbols with same name are always `eq` |
| `(eql a b)` | Like `eq` but numbers of same type and value are eql |
| `(equal a b)` | Structural equality; recursively compares lists and strings |
| `(equalp a b)` | Like `equal` but case-insensitive strings and type-loose numbers |

```lisp
(eq 'foo 'foo)           ;==> T
(eql 1 1)                ;==> T
(eql 1 1.0)              ;==> NIL   (different types)
(equal '(1 2) '(1 2))   ;==> T
(equal '(1 2) '(1 3))   ;==> NIL
(equalp "ABC" "abc")     ;==> T
(equalp 1 1.0)           ;==> T
```

`=` uses Python `==` semantics and works across numeric types:

```lisp
(= 1 1.0)   ;==> T    (numeric equality, not eql)
```

---

## Type Conversion

```lisp
(float 3)          ;==> 3.0
(integer 3.7)      ;==> 3          ; truncates
(rational 0.5)     ;==> 1/2
(string 42)        ;==> "42"       ; programmer form
(ustring 42)       ;==> "42"       ; user form
(make-symbol "FOO")  ;==> FOO      ; string → symbol
```

---

## Quick Reference

| Expression | Meaning |
|---|---|
| `(type-of x)` | Type name as symbol |
| `(typep x 'type)` | T if x is of type |
| `(typep x '(or t1 t2))` | Union type test |
| `(typep x '(and t1 t2))` | Intersection type test |
| `(typep x '(not t))` | Negation |
| `(typep x '(member v...))` | Membership test |
| `(typep x '(satisfies fn))` | Custom predicate |
| `(typep x '(integer lo hi))` | Numeric range |
| `(typecase x (t1 body) ...)` | Dispatch on type |
| `(etypecase x (t1 body) ...)` | Dispatch; error if no match |
| `(numberp x)` `(stringp x)` etc. | Type predicates |
| `(eq a b)` `(eql a b)` `(equal a b)` | Equality |

---

*See `(help "types")` for the condensed quick reference.*

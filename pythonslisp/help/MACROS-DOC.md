# Macros

*Quick reference: `(help "macros")` — Full documentation: this file.*


Macros are code transformers.  A macro takes unevaluated s-expressions as
arguments and returns a new s-expression which is then evaluated in their
place.  This lets you extend the language with new control structures,
binding forms, and domain-specific syntax.

---

## Defining a Macro

```lisp
(defmacro name (lambda-list) body...)
```

The body returns the expansion — the code that will be evaluated.  Use
quasiquote to construct that code conveniently.

```lisp
; A simple swap macro
(defmacro swap! (a b)
  `(let ((tmp ,a))
     (setf ,a ,b)
     (setf ,b tmp)))

(setf x 1)
(setf y 2)
(swap! x y)
x   ;==> 2
y   ;==> 1
```

`defmacro` is itself a macro — it expands to a `setq` that binds a
`LMacro` object in the global environment.

---

## Quasiquote, Unquote, and Splicing

These three reader macros are the primary tools for writing macro bodies.

| Syntax | Name | Meaning |
|---|---|---|
| `` `form `` | quasiquote | Return form as data, with selected parts substituted |
| `,expr` | unquote | Evaluate expr and insert its value here |
| `,@expr` | unquote-splice | Evaluate expr (must be a list) and splice its elements here |

```lisp
; Quasiquote without any unquotes — same as quote
`(+ 1 2)         ;==> (+ 1 2)

; Unquote inserts a value
(setf n 42)
`(the answer is ,n)   ;==> (THE ANSWER IS 42)

; Unquote-splice inserts a list's contents
(setf args '(1 2 3))
`(+ ,@args)      ;==> (+ 1 2 3)
```

Quasiquotes may be nested.  Each unquote belongs to the nearest enclosing
quasiquote.

---

## Inspecting Macro Expansions

### macroexpand

Returns the fully expanded form — expands all macros recursively until the
result is not a macro call.

```lisp
(macroexpand '(when (> x 0) (print x)))
;==> (IF (> X 0) (PROGN (PRINT X)) NIL)
```

### macroexpand-1

Performs exactly one level of expansion.

```lisp
(macroexpand-1 '(when (> x 0) (print x)))
;==> (IF (> X 0) (PROGN (PRINT X)) NIL)
```

These are indispensable for debugging macros: if a macro misbehaves, expand
it and inspect the generated code.

---

## Generating Unique Symbols with gensym

When a macro introduces a local binding, using a plain name risks clashing
with variables in the caller's code.  `gensym` generates a fresh symbol
guaranteed not to appear anywhere else.

```lisp
(gensym)       ;==> G1  (counter increments each call)
(gensym)       ;==> G2
(gensym "TMP") ;==> TMP3
(gensym 100)   ;==> G100  (counter set to 100)
```

Using `gensym` in macros prevents **variable capture**:

```lisp
; Unsafe — if caller has a variable named RESULT it will be shadowed
(defmacro with-result-bad (expr)
  `(let ((result ,expr))
     (* result result)))

; Safe — uses a fresh uninternable symbol
(defmacro with-result (expr)
  (let ((var (gensym "R")))
    `(let ((,var ,expr))
       (* ,var ,var))))

(setf result 99)   ; won't interfere with the safe macro
(with-result (+ 3 4))   ;==> 49
result                  ;==> 99  -- unchanged
```

---

## Built-in Macros as Patterns

The standard library is full of macros.  Reading their expansions shows
typical patterns.

### when and unless

```lisp
(defmacro when (condition &rest body)
  `(if ,condition (progn ,@body) nil))

(defmacro unless (condition &rest body)
  `(when (not ,condition) ,@body))
```

### and and or

```lisp
; (and) -- base case
; (and form) -- single arg, just evaluate it
; (and form rest...) -- short-circuit: if form is nil, return nil
(defmacro and (&rest forms)
  (cond ((null forms) t)
        ((null (cdr forms)) (car forms))
        (t `(if ,(car forms) (and ,@(cdr forms)) nil))))

; (or) -- returns nil
; (or form rest...) -- bind to temp, return if truthy, else try rest
(defmacro or (&rest forms)
  (cond ((null forms) nil)
        ((null (cdr forms)) (car forms))
        (t (let ((var (gensym "OR")))
             `(let ((,var ,(car forms)))
                (if ,var ,var (or ,@(cdr forms))))))))
```

`or` uses `gensym` to avoid evaluating the first form twice and to prevent
capture.

### defun itself is a macro

```lisp
(defmacro defun (fnName lambda-list &rest body)
  `(setq ,fnName (lambda ,lambda-list ,@body)))
```

---

## Macros vs Functions

| | Macro | Function |
|---|---|---|
| Arguments | Unevaluated | Pre-evaluated |
| Return value | Code (then evaluated) | The value itself |
| Can introduce new syntax | Yes | No |
| Can conditionally evaluate args | Yes | No |
| Debuggable with macroexpand | Yes | N/A |

Use a function when you just need to compute a value.  Use a macro when
you need to control how or whether arguments are evaluated, or when you
want to synthesize code.

---

## Practical Patterns

### Wrapping code with setup and teardown

```lisp
(defmacro with-logging (tag &rest body)
  `(progn
     (uwrite-line (ustring "ENTER " ,tag))
     (let ((result (progn ,@body)))
       (uwrite-line (ustring "EXIT " ,tag))
       result)))

(with-logging "compute"
  (* 6 7))
; prints: ENTER compute
; prints: EXIT compute
;==> 42
```

### Building a dispatch table from keyword arguments

```lisp
(defmacro dispatch-on (key &rest cases)
  `(case ,key
     ,@cases))

(dispatch-on 'b
  (a 1) (b 2) (c 3))   ;==> 2
```

### Assert macro with descriptive error

```lisp
(defmacro assert! (condition message)
  `(when (not ,condition)
     (error (ustring "Assertion failed: " ,message))))

(assert! (> x 0) "x must be positive")
```

---

## Quick Reference

| Expression | Meaning |
|---|---|
| `(defmacro name (args) body)` | Define macro NAME |
| `` `form `` | Quasiquote — data with substitutions |
| `,expr` | Unquote — insert value of expr |
| `,@expr` | Unquote-splice — insert list elements |
| `(macroexpand '(form))` | Fully expand a macro call |
| `(macroexpand-1 '(form))` | Expand one level |
| `(gensym)` | Fresh unique symbol G*n* |
| `(gensym "prefix")` | Fresh symbol with given prefix |
| `(macrop x)` | T if x is a macro |

---

*See `(help "macros")` for the condensed quick reference.*

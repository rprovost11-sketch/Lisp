# Lambda Lists

A lambda list is the parameter specification that appears in a `lambda`,
`defun`, or `defmacro` form.  It describes what arguments the callable
accepts, how they are bound to names, and what default values are used
when arguments are omitted.

For the compact formal grammar see `(help "LAMBDA-LIST-REF")`.

---

## Overview of Sections

A lambda list is a flat (or for macros, nested) list of parameter
specifications divided into sections by keyword markers.  Each section is
optional and the ordering is fixed:

```
(positional... [&optional opt...] [&rest rest] [&key key... [&allow-other-keys]] [&aux aux...])
```

All markers begin with `&`.  The known markers are:

| Marker | Section | Purpose |
|---|---|---|
| *(none)* | Positional | Required arguments, bound in order |
| `&optional` | Optional | Arguments that may be omitted |
| `&rest` | Rest | Collects remaining positional args into a list |
| `&key` | Keyword | Named arguments passed as `:name value` pairs |
| `&allow-other-keys` | Key modifier | Suppress error on unknown keywords |
| `&aux` | Aux | Local bindings computed at call time; not arguments |
| `&body` | Body (macros only) | Alias for `&rest` in macro lambda lists |

---

## Functions and Lambdas

### Positional parameters

The simplest lambda list is a list of plain symbols.  Each symbol is bound
to the corresponding argument by position.

```lisp
(defun add (a b) (+ a b))
(add 3 4)   ;==> 7

(lambda (x y z) (list x y z))
```

Calling with the wrong number of arguments is an error.

### &optional parameters

Optional parameters follow `&optional`.  Each spec is either a bare symbol
(default NIL) or `(name default)` or `(name default supplied-p)`.

- `default` is evaluated at call time if the argument is omitted.
- `supplied-p` is bound to T if the caller supplied the argument, NIL otherwise.

```lisp
(defun greet (name &optional (greeting "Hello"))
  (ustring greeting ", " name "!"))

(greet "Alice")            ;==> "Hello, Alice!"
(greet "Bob" "Hi")         ;==> "Hi, Bob!"
```

```lisp
; supplied-p lets you distinguish "not given" from "given as NIL"
(defun maybe-double (n &optional (factor 2 factor-p))
  (if factor-p
      (* n factor)
      n))

(maybe-double 5)      ;==> 5     ; factor not supplied
(maybe-double 5 nil)  ;==> NIL   ; factor supplied as NIL
(maybe-double 5 3)    ;==> 15
```

### &rest parameter

`&rest` collects all remaining positional arguments (after positional and
optional params are bound) into a single list.  Only one `&rest` variable
is allowed.

```lisp
(defun my-list (&rest items) items)
(my-list 1 2 3)    ;==> (1 2 3)
(my-list)          ;==> NIL

(defun sum (&rest nums)
  (reduce '+ nums))
(sum 1 2 3 4 5)    ;==> 15
```

`&rest` and `&key` may be combined.  The rest list will contain the
keyword-value pairs:

```lisp
(defun show (&rest args &key verbose)
  (when verbose (uwriteLn! "verbose mode"))
  args)
(show :verbose t :x 1)  ;==> (:VERBOSE T :X 1)
```

**Note:** `&rest` followed by `&key` in the same lambda list has a known
limitation in this implementation — see LAMBDA-LIST-REF for details.
Prefer `DOC_ONLY` with manual keyword scanning in that case (relevant to
Python extension authors only).

### &key parameters

Keyword arguments are passed as `:name value` pairs in any order.  Each
spec is a symbol, `(name default)`, `(name default supplied-p)`, or
`((keyword name) default)` to use a different external keyword name.

```lisp
(defun make-rect (&key (width 1) (height 1))
  (list width height))

(make-rect)                   ;==> (1 1)
(make-rect :width 5)          ;==> (5 1)
(make-rect :height 3 :width 5) ;==> (5 3)  -- order doesn't matter
```

When a caller supplies a keyword argument more than once, the **first
occurrence wins** (CL 3.4.1.4.1 semantics):

```lisp
(defun show-x (&key x) x)
(show-x :x 1 :x 2)   ;==> 1
```

Passing an unknown keyword is an error unless `&allow-other-keys` is
present or the caller passes `:allow-other-keys t`.

#### Keyword renaming

Use `((keyword name) default)` to accept `:from` externally but bind it
as `source` internally:

```lisp
(defun copy-range (&key ((from source) 0) ((to dest) 100))
  (list source dest))

(copy-range :from 10 :to 50)   ;==> (10 50)
```

### &allow-other-keys

Suppresses the unknown-keyword error.  Any extra keyword arguments are
silently ignored (unless you also have `&rest` to capture them).

```lisp
(defun lenient (&key x &allow-other-keys) x)
(lenient :x 5 :y 99 :z 0)   ;==> 5
```

### &aux bindings

`&aux` introduces local variables that are computed at call time but are
not arguments.  The caller never provides them.

```lisp
(defun circle-area (r &aux (pi 3.14159) (r2 (* r r)))
  (* pi r2))

(circle-area 5)   ;==> 78.53975
```

`&aux` is mostly a style convenience.  The equivalent with `let*` is
equally idiomatic.

---

## Macro Lambda Lists

Macro lambda lists extend function lambda lists in two ways:

1. **`&body`** is an alias for `&rest`.  Use it to signal that the
   collected arguments are body forms.

2. **Destructuring** — positional parameters (and `&body`/`&rest`
   patterns) may be nested lists that match against the structure of the
   corresponding argument.

### &body

```lisp
(defmacro my-progn (&body forms)
  `(progn ,@forms))

; Equivalent: (defmacro my-progn (&rest forms) ...)
```

### Destructuring patterns

A nested list in the positional section is matched against the
corresponding argument, binding each element of the pattern to the
corresponding element of the value:

```lisp
(defmacro swap! ((a b))   ; expects one arg that is a 2-element list
  `(let ((tmp ,a))
     (setf ,a ,b)
     (setf ,b tmp)))

(swap! (x y))   ; the pair (x y) matches pattern (a b)
```

Destructuring can be nested to arbitrary depth:

```lisp
(defmacro first-two (((a b) &rest _))
  `(list ,a ,b))

(first-two ((10 20 30)))   ;==> (10 20)
```

The standard library uses destructuring in loop macro control specs:

```lisp
(defmacro dotimes ((var count &optional (result nil)) &body body)
  ...)
```

Here `(var count &optional (result nil))` is a destructuring pattern
applied to the first argument of `dotimes`.

---

## Lambda Lists for Primitives

Built-in primitives (written in Python) use one of three modes:

| Mode | Meaning |
|---|---|
| `ARITY_ONLY` | Only argument count is checked; args pre-evaluated |
| `FULL_BINDING` | Full CL lambda list parsed; params bound by name in env |
| `DOC_ONLY` | Lambda list is documentation only; primitive handles all binding |

This is relevant only when **writing Python extensions**.  From the Lisp
programmer's perspective, primitives behave like functions: positional
args work normally, and keyword args (`FULL_BINDING`) work as documented.
See EXTENSIONS for more on writing primitives.

---

## Rules and Restrictions

1. **Ordering is fixed**: positional → `&optional` → `&rest` → `&key` →
   `&allow-other-keys` → `&aux`.  Omitting a section is fine; reordering
   is an error.

2. **No duplicate parameter names** across all sections — the validator
   catches this before binding.

3. **Defaults are evaluated at call time**, not at definition time.  A
   default of `(list 1 2)` produces a fresh list every call.

4. **`&optional` and `&key` should not be mixed** in the same lambda list
   (legal but confusing — the `&optional` params consume positional args
   that keyword callers might intend as keyword values).

5. **Empty sections are legal**: `(&optional)` is the same as omitting
   `&optional` entirely.

---

## Practical Patterns

### Accepting either positional or keyword arguments

```lisp
; Most CL-style: keyword args with sensible defaults
(defun connect (&key (host "localhost") (port 80) (timeout 30))
  (list host port timeout))

(connect)                     ;==> ("localhost" 80 30)
(connect :port 443)           ;==> ("localhost" 443 30)
(connect :host "example.com" :port 8080)  ;==> ("example.com" 8080 30)
```

### Optional trailing arguments

```lisp
(defun read-line-or-default (stream &optional (eof-error t) eof-value)
  ...)
```

### Variadic function with required first arg

```lisp
(defun ustring-join (sep &rest items)
  (if (null items)
      ""
      (reduce (lambda (a b) (ustring a sep b)) items)))

(ustring-join ", " "a" "b" "c")   ;==> "a, b, c"
```

### Macro with structured control form

```lisp
(defmacro for-range ((var start end &optional (step 1)) &body body)
  `(let ((,var ,start))
     (while (< ,var ,end)
       ,@body
       (setf ,var (+ ,var ,step)))))

(for-range (i 0 10 2)
  (print i))
; prints: 0 2 4 6 8
```

---

## Quick Reference

| Spec form | Section | Meaning |
|---|---|---|
| `var` | Positional | Required; bound by position |
| `var` | `&optional` | Optional; default NIL |
| `(var default)` | `&optional` or `&key` | Optional; given default |
| `(var default sp)` | `&optional` or `&key` | As above; `sp` = T if supplied |
| `var` | `&rest` / `&body` | Collects remaining args as list |
| `:name` | `&key` | Caller passes `:name val` |
| `((kw var) default)` | `&key` | Caller uses `:kw`, binds as `var` |
| `&allow-other-keys` | `&key` section | Ignore unknown keywords |
| `(var init)` | `&aux` | Local binding; not an argument |
| `(pattern)` | Macro positional | Destructure nested list |

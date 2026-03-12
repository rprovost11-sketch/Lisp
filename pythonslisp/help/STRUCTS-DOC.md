# Structs

`defstruct` defines a named record type and generates a complete set of
functions for creating, inspecting, and copying instances.  Each struct type
is independent: the predicate and accessors for one type know nothing about
any other type.

---

## Defining a Struct Type

```lisp
(defstruct typename field-spec...)
```

`typename` is a symbol naming the new type.  Each `field-spec` is either a
bare symbol (field with default NIL) or a `(name default)` pair.  An
optional docstring may appear as the first element after the typename.

```lisp
; Bare symbol fields — all default to NIL
(defstruct point x y)

; Fields with explicit defaults
(defstruct point (x 0) (y 0))

; Mixed
(defstruct person name (age 0) (active t))

; With a docstring
(defstruct color "An RGB color triple, components 0-255."
  (r 0) (g 0) (b 0))
```

`defstruct` returns the typename symbol and binds it globally to a struct
descriptor (see Introspection below).

### What defstruct generates

For `(defstruct point (x 0) (y 0))` the following are created:

| Name | Kind | Purpose |
|---|---|---|
| `make-point` | function | Constructor — creates an instance |
| `point-p` | function | Predicate — T if argument is a POINT |
| `point-x` | function | Accessor — reads field X |
| `point-y` | function | Accessor — reads field Y |
| `copy-point` | function | Copier — makes a shallow copy |
| `point` | variable | Struct descriptor object |

---

## Creating Instances

The constructor accepts keyword arguments for any combination of fields.
Omitted fields receive their default values.  Defaults are expressions
evaluated fresh at construction time, not at definition time.

```lisp
; All defaults
(make-point)             ;==> (DICT ("STRUCT-TYPE" POINT) ("X" 0) ("Y" 0))

; Some fields supplied
(make-point :x 3 :y 4)  ;==> (DICT ("STRUCT-TYPE" POINT) ("X" 3) ("Y" 4))

; Only one field — y gets its default
(make-point :x 10)       ;==> (DICT ("STRUCT-TYPE" POINT) ("X" 10) ("Y" 0))
```

Passing an unknown keyword is an error:

```lisp
(make-point :z 99)
; %%% Unexpected keyword found Z.
```

---

## Reading Fields

Each field has a named accessor function:

```lisp
(defstruct person name (age 0))
(setf bob (make-person :name "Bob" :age 30))

(person-name bob)   ;==> "Bob"
(person-age  bob)   ;==> 30
```

---

## Writing Fields

Use `setf` with the accessor as the place form:

```lisp
(setf (person-age bob) 31)
(person-age bob)   ;==> 31

(setf (point-x p) 100)
(setf (point-y p) 200)
```

Setting a field on a non-struct instance is an error:

```lisp
(setf (point-x 42) 5)
; %%% Argument 2 must be a struct instance.
```

---

## The Predicate

Each struct type has a predicate that returns T only for instances of that
exact type.  Different struct types are entirely independent.

```lisp
(defstruct point (x 0) (y 0))
(defstruct person name age)

(setf p (make-point :x 1 :y 2))
(setf b (make-person :name "Bob" :age 30))

(point-p  p)   ;==> T
(point-p  b)   ;==> NIL   -- b is a person, not a point
(person-p b)   ;==> T
(person-p p)   ;==> NIL
(point-p  42)  ;==> NIL   -- not a struct at all
```

---

## Copying

`copy-<typename>` returns a new instance with the same field values.  The
copy is shallow — if a field holds a list or another struct, both the
original and the copy reference the same object.

```lisp
(setf p1 (make-point :x 3 :y 4))
(setf p2 (copy-point p1))

(setf (point-x p1) 0)   ; modify original
(point-x p1)             ;==> 0
(point-x p2)             ;==> 3   -- copy is unaffected
```

---

## Type Introspection

### type-of

Returns the struct type name as a symbol:

```lisp
(type-of p)    ;==> POINT
(type-of bob)  ;==> PERSON
```

### typep

`typep` accepts struct type names as type specifiers:

```lisp
(typep p   'point)   ;==> T
(typep bob 'point)   ;==> NIL
(typep bob 'person)  ;==> T
```

Compound type specifiers work too:

```lisp
(typep p (list 'or 'point 'person))   ;==> T
(typep 42 (list 'satisfies 'point-p)) ;==> NIL
```

### typecase and etypecase

Dispatch on struct type using `typecase`.  `etypecase` additionally signals
an error if no clause matches.

```lisp
(defun describe-shape (s)
  (typecase s
    (point  (ustring "point at " (point-x s) "," (point-y s)))
    (circle (ustring "circle r=" (circle-radius s)))
    (t      "unknown shape")))
```

---

## Field Defaults Are Evaluated at Construction Time

Default expressions in the field specs are stored unevaluated and re-evaluated
each time the constructor is called without that argument.  This means a
default like `(list 1 2)` produces a fresh list each time.

```lisp
(defstruct sized (size (* 2 3)))

; Default (* 2 3) is evaluated at construction, not at defstruct time
(sized-size (make-sized))   ;==> 6
```

---

## The Struct Descriptor

`defstruct` binds the typename to a struct descriptor object in the global
environment.  The descriptor carries the type name, docstring, and field
specifications.  Use `(help typename)` to view it formatted:

```lisp
(defstruct color "An RGB color triple." (r 0) (g 0) (b 0))
(help color)
```

```
STRUCT  COLOR

   An RGB color triple.

Fields:
   R                    default: 0
   G                    default: 0
   B                    default: 0

Constructor: (make-color &key r g b)
Predicate:   (color-p obj)
Accessors:   color-r  color-g  color-b
Copier:      (copy-color inst)
```

---

## Practical Patterns

### A linked list of nodes

```lisp
(defstruct node value (next nil))

(defun make-linked-list (&rest items)
  (let ((head nil))
    (dolist (item (reverse items))
      (setf head (make-node :value item :next head)))
    head))

(defun linked-list->list (node)
  (if (null node)
      nil
      (cons (node-value node)
            (linked-list->list (node-next node)))))

(setf lst (make-linked-list 1 2 3))
(linked-list->list lst)   ;==> (1 2 3)
```

### Updating a field and returning the struct

```lisp
(defun birthday! (person)
  "Increment person's age in place and return them."
  (setf (person-age person) (+ (person-age person) 1))
  person)
```

### A dispatch function using typecase

```lisp
(defstruct circle   (radius 1))
(defstruct rectangle (width 1) (height 1))

(defun area (shape)
  (typecase shape
    (circle    (* 3.14159 (circle-radius shape) (circle-radius shape)))
    (rectangle (* (rectangle-width shape) (rectangle-height shape)))
    (t (error "area: unknown shape type"))))

(area (make-circle :radius 5))        ;==> 78.53975
(area (make-rectangle :width 4 :height 6))  ;==> 24
```

### Using copy to produce modified versions

```lisp
(defun with-x (pt new-x)
  "Return a copy of pt with x replaced."
  (let ((p (copy-point pt)))
    (setf (point-x p) new-x)
    p))

(setf origin (make-point :x 0 :y 0))
(setf moved  (with-x origin 5))
(point-x origin)   ;==> 0
(point-x moved)    ;==> 5
```

---

## Quick Reference

| Expression | Meaning |
|---|---|
| `(defstruct name field...)` | Define struct type NAME |
| `(defstruct name "doc" field...)` | With docstring |
| `(defstruct name (field default)...)` | Fields with defaults |
| `(make-name :field val...)` | Create an instance |
| `(name-field inst)` | Read a field |
| `(setf (name-field inst) val)` | Write a field |
| `(name-p obj)` | T if obj is an instance of NAME |
| `(copy-name inst)` | Shallow copy of instance |
| `(type-of inst)` | Returns type name symbol |
| `(typep obj 'name)` | T if obj is an instance of NAME |
| `(help name)` | Show struct descriptor |

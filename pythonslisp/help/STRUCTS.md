# Structs Quick Reference

For `(defstruct point (x 0) (y 0))` the following are generated:

| Generated name | Kind | Purpose |
|---|---|---|
| `make-point` | function | Constructor |
| `point-p` | function | Type predicate |
| `point-x` / `point-y` | function | Field accessors (read) |
| `copy-point` | function | Shallow copy |
| `point` | variable | Struct descriptor object |

## Core Operations

| Expression | Meaning |
|---|---|
| `(defstruct name field...)` | Define struct type NAME |
| `(defstruct name "doc" field...)` | With docstring |
| `(defstruct name (field default)...)` | Fields with defaults |
| `(defstruct (name (:include parent)) field...)` | Inherit fields from PARENT |
| `(make-name :field val...)` | Create an instance |
| `(name-field inst)` | Read a field |
| `(setf (name-field inst) val)` | Write a field |
| `(name-p obj)` | T if obj is an instance of NAME or any descendant |
| `(copy-name inst)` | Shallow copy of instance |
| `(type-of inst)` | Returns the struct type name symbol |
| `(typep obj 'name)` | T if obj is an instance of NAME or any descendant |
| `(help name)` | Display struct descriptor |

## Notes

- Defaults are evaluated fresh at construction time, not at `defstruct` time.
- `copy-name` is shallow — nested structs/lists are shared.
- `:include` inherits all parent fields and their defaults; parent accessors work unchanged on child instances.
- Parent predicates return T for child instances; child predicates return NIL for parent instances.
- Inheritance chains of any depth are supported.

See `(help "structs-doc")` for full documentation and examples.

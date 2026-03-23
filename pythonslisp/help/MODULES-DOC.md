# The Module System

*Quick reference: `(help "modules")` - Full documentation: this file.*


Modules are named namespaces that isolate definitions from the global
environment.  A module holds any number of symbol bindings - functions,
macros, variables - that are accessed through the module rather than looked
up globally.  Packages are modules that contain other modules, forming a
hierarchy navigated with colon notation.

---

## Creating Modules

### make-module

`make-module` creates an empty module and binds it in the global environment.

```lisp
(make-module 'mylib)   ;==> #<MODULE MYLIB>
(modulep mylib)        ;==> T
(module-name mylib)    ;==> "MYLIB"
(module-symbols mylib) ;==> NIL
```

You can then populate the module using `module-set!` or colon-setf:

```lisp
(module-set! mylib 'pi 3.14159)
(setf mylib:greeting "Hello")
(module-symbols mylib)   ;==> (GREETING PI)
```

### load-module

`load-module` evaluates a `.lisp` file inside a new module and binds it in
the global environment.  All definitions in the file become symbols of the
module; nothing leaks into the global environment.

```lisp
; geometry.lisp defines: area, perimeter, PI
(load-module "geometry.lisp")     ;==> #<MODULE GEOMETRY>

(modulep geometry)                ;==> T
(module-symbols geometry)         ;==> (AREA PERIMETER PI)
(boundp 'area)                    ;==> NIL   -- not in global env
```

By default the module name is derived from the filename stem, uppercased.
Supply `:name` to choose a different name:

```lisp
(load-module "geometry.lisp" :name 'geo)
;==> #<MODULE GEO>
```

---

## Colon Notation

The parser expands `module:symbol` to the list `(: module symbol)` before
evaluation.  The `:` primitive then navigates the module hierarchy and
returns the named value.

```lisp
geometry:pi          ;==> 3.14159
(: geometry pi)      ;==> 3.14159   -- equivalent long form
```

These two forms are identical - the shorthand is purely a parser convenience.

**Important:** a symbol whose name *begins* with a colon is a keyword, not
colon notation.  `:key` stays as the keyword symbol `:KEY` and is never
expanded.

```lisp
':key    ;==> :KEY        -- keyword, not navigation
'geo:pi  ;==> (: GEO PI)  -- navigation
```

### Calling module functions

Place the colon expression in the function position of a call:

```lisp
((: geometry area) 5 3)    ;==> 15
(geometry:area 5 3)        ;==> 15   -- shorthand form
```

### Reading module variables

```lisp
geometry:pi     ;==> 3.14159
(: geo area)    ;==> #<FUNCTION AREA ...>
```

---

## Setting Module Bindings

Use `setf` with colon notation to add or update a binding inside a module.
The parser expands `(setf module:x val)` to `(module-set! module 'x val)`.

```lisp
(setf geometry:pi 3.14159265)   ; update an existing binding
(setf geometry:tau (* 2 geometry:pi))  ; add a new binding
geometry:tau   ;==> 6.2831853
```

`module-set!` is the underlying primitive and may be called directly:

```lisp
(module-set! geometry 'author "R. Provost")
geometry:author   ;==> "R. Provost"
```

---

## Packages - Nested Modules

Modules can be nested to form a package hierarchy.  Use a multi-segment
colon path to navigate several levels at once:

```lisp
mylib:utils:square    ;==> navigates MYLIB -> UTILS -> SQUARE
(: mylib utils square 6)   ;==> calls square with argument 6
```

`load-module` with a multi-segment `:name` creates the entire hierarchy
automatically.  Intermediate packages are created as needed:

```lisp
(load-module "math-utils.lisp" :name 'mylib:math)
;==> #<MODULE MATH>

(modulep mylib)        ;==> T   -- package created automatically
(modulep mylib:math)   ;==> T

(mylib:math:square 7)  ;==> 49
```

Nest as deeply as required:

```lisp
(load-module "tools.lisp" :name 'mylib:extra:tools)
;==> #<MODULE TOOLS>

((: mylib extra tools some-fn) arg)
```

---

## Aliasing Module Symbols into Scope

Copy a module binding into a local name so it can be called without the
module prefix:

```lisp
(load-module "geometry.lisp")

(setf area geometry:area)
(setf perim geometry:perimeter)

(area 5 3)    ;==> 15
(perim 5 3)   ;==> 16
```

This copies the function object at the time of the `setf`.  Subsequent
changes to `geometry:area` do not affect the local alias.

---

## Loading Python Extensions into Modules

`load-extension` with `:name` registers Python primitives directly into a
named module instead of the global environment.  This keeps the global
namespace clean when loading third-party extension files.

```lisp
(load-extension "my-prims.py" :name 'mylib)

(modulep mylib)            ;==> T
(module-symbols mylib)     ;==> (PRIM-A PRIM-B ...)
(mylib:prim-a arg)         ;==> ...
```

The module is created if it does not already exist.  If it exists and is
already a module, the new primitives are added to it.

---

## Inspecting Modules

```lisp
(modulep obj)          ; T if obj is a module, NIL otherwise
(module-name mod)      ; returns the module name as a string
(module-symbols mod)   ; returns a sorted list of symbols in the module
(type-of mod)          ; ==> MODULE
```

Example - explore a loaded module:

```lisp
(load-module "geometry.lisp")
(module-name geometry)     ;==> "GEOMETRY"
(module-symbols geometry)  ;==> (AREA PERIMETER PI)
(type-of geometry)         ;==> MODULE
```

---

## Quick Reference

| Expression | Meaning |
|---|---|
| `(make-module 'name)` | Create an empty module bound as NAME |
| `(load-module "f.lisp")` | Load file into module; name from filename |
| `(load-module "f.lisp" :name 'mod)` | Load file into module named MOD |
| `(load-module "f.lisp" :name 'pkg:mod)` | Load into PKG:MOD, creating PKG |
| `mod:sym` | Read symbol SYM from module MOD |
| `(: mod sym)` | Same, long form |
| `(setf mod:sym val)` | Set SYM in MOD to VAL |
| `(mod:fn arg...)` | Call function FN in MOD |
| `(load-extension "f.py" :name 'mod)` | Register Python primitives into MOD |
| `(modulep x)` | T if X is a module |
| `(module-name mod)` | Name of module as a string |
| `(module-symbols mod)` | Sorted list of symbols in module |

---

*See `(help "modules")` for the condensed quick reference.*

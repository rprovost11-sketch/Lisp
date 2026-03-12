# Writing and Loading Extensions

Extensions add new primitives and Lisp-level definitions to the interpreter
without modifying the interpreter source.  An extension can be a `.py` file
that defines Python-backed primitives, a `.lisp` file that defines functions
and macros in Lisp, or a directory containing both.

---

## Python Extensions

A Python extension defines new primitives using the `@primitive` decorator.
Each decorated function is registered automatically when the file is loaded.
For a full list to give you access to everything in the interpreter see the
examples file my_extension.py.

### Minimal imports

```python
from __future__ import annotations
from typing import Any
from pythonslisp.ltk.EnvironmentBase import EnvironmentBase
from pythonslisp.AST import L_NIL, L_T, prettyPrint, prettyPrintSExpr
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimeError, LRuntimePrimError
from pythonslisp.extensions import primitive, LambdaListMode
```

Add any other imports your primitives need (e.g. `import os`, `import math`).

### The @primitive decorator

```python
@primitive( 'my-func', '(x y)' )
def LP_my_func( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
    """Documentation string shown by (help my-func)."""
    x, y = args
    return x + y
```

The first argument to `@primitive` is the name the primitive will have inside
the Lisp interpreter.  The second argument is a lambda-list string that
specifies the parameter list.  Every primitive function must return a value.

The Python function's docstring becomes the documentation text shown by
`(help my-func)`.

### Lambda-list modes

The `mode` keyword argument to primitive controls how the lambda-list string
is interpreted.  The following modes are supported:

**`LambdaListMode.ARITY_ONLY` (default)** the lambda list is used to
auto-compute min/max argument counts, and it's used for documentation.  Your
primitive receives `args` as a plain list and must unpack it manually.  Use this
mode for most new primitives.

```python
@primitive( 'greet', '(name &optional (greeting "Hello"))' )
def LP_greet( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
    """Returns a greeting string."""
    name     = args[0]
    greeting = args[1] if len(args) > 1 else "Hello"
    return f'{greeting}, {prettyPrint(name)}!'
```

**`LambdaListMode.FULL_BINDING`** the interpreter runs its full
argument-binding machinery before your function is called.  Every parameter
in the lambda list is bound as a local variable in `env` and retrieved with
`env.lookup()`.  Use this when you have `&key` parameters or other complex
lambda lists and you want the interpreter to do the unpacking.

FULL_BINDING should be used sparingly as the built-in argument binding machinery
is relatively slow.  Prefer unpacking arguments manually to maintain the
performance of the interpreter.

```python
@primitive( 'repeat-string', '(string &key (count 2) (separator ""))',
            mode=LambdaListMode.FULL_BINDING )
def LP_repeat_string( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
    """Returns STRING repeated COUNT times, joined by SEPARATOR."""
    string    = env.lookup('STRING')
    count     = env.lookup('COUNT')
    separator = env.lookup('SEPARATOR')
    return separator.join([string] * count)
```

**`LambdaListMode.DOC_ONLY`** treats the lambda list as documentation only.
You must supply explicit `min_args` and `max_args` keyword arguments and parse
`args` manually.  Use this when the lambda list uses non-standard shorthand
syntax that the parser cannot handle, or when you need a completely custom
arity check.

```python
@primitive( 'comment', '(&rest forms)', preEvalArgs=False,
            mode=LambdaListMode.DOC_ONLY, min_args=0, max_args=None )
def LP_comment( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
    """Ignores all its arguments and returns NIL."""
    return L_NIL
```

### Special forms (preEvalArgs=False)

By default arguments are evaluated before your function is called.  Use the
@primitive argument `preEvalArgs=False` to receive the raw unevaluated AST
arguments instead.  Use this for forms that must control their own evaluation:
primarily macros and control structures.

```python
@primitive( 'my-if', '(test then else)', preEvalArgs=False )
def LP_my_if( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
    """Evaluates THEN if TEST is true, ELSE otherwise."""
    test, then, else_ = args
    return ctx.lEval( env, then ) if ctx.lEval( env, test ) else ctx.lEval( env, else_ )
```

### Raising errors

Use `LRuntimePrimError` to raise an error attributed to your primitive:

```python
raise LRuntimePrimError( LP_my_func, 'argument must be a string.' )
```

Use `LRuntimeError` for errors not attributed to a specific primitive.

### Full annotated example

A fully annotated example covering all three `LambdaListMode` cases is
available at `pythonslisp/examples/my_extension.py`.  It includes the
complete set of imports and can be used as a starting point.

---

## Lisp Extensions

A Lisp extension is a plain `.lisp` file containing any valid Lisp
expressions: `defun`, `defmacro`, `setf`, or any other form.  When loaded,
the file is parsed and evaluated in order, exactly as if you had typed the
expressions in the REPL.

```lisp
;;; my-utils.lisp

(defun square (x)
  "Returns X squared."
  (* x x))

(defun cube (x)
  "Returns X cubed."
  (* x x x))
```

All definitions made in a Lisp extension are visible in the global
environment (or in a named module, if one is specified).

---

## How the Interpreter Loads Extensions

### Startup sequence

Each time the interpreter is initialized (by calling `reboot()`) it loads
extensions in the following order:

1. **Built-in Python extensions** all `.py` files in `pythonslisp/extensions/`
   are loaded alphabetically and their primitives registered.
2. **Built-in Lisp extensions** all `.lisp` files in `pythonslisp/extensions/`
   are loaded alphabetically.
3. **Caller-supplied extension directory** if `reboot(ext_dir=...)` is given,
   all `.py` files in that directory are loaded alphabetically, then all
   `.lisp` files.
4. **`startup.lisp`** the system startup script is loaded.
5. **`~/.pythonslisp_rc`** if this file exists in the user's home directory
   it is loaded as user customizations.

### Runtime loading from Python

Use `Interpreter._loadExtFile(path)` to load a single extension file at
any time after `reboot()`.  Use `Interpreter._loadExtDir(path)` to load a
directory.  Both are private methods; the public way to do this from within
Lisp is with the primitives described below.

### Runtime loading from Lisp

Four primitives are available for loading extensions from inside a running
Lisp session.

---

## Lisp Callables for Loading Extensions

### load-extension

```lisp
(load-extension filespec... &key name)
```

Loads one or more extension files.  Each `filespec` must be a string
containing a file path.  `.py` files are imported and their `@primitive`
decorators registered.  `.lisp` files are parsed and evaluated.

`:name` optionally specifies a module or package path (a symbol, or a
`pkg:submod` notation) into which the new primitives are bound instead of
the global environment.

```lisp
(load-extension "my-utils.lisp")
(load-extension "my-prims.py" "extra.lisp")
(load-extension "db-prims.py" :name db)
```

Returns `T` if all files loaded successfully.

### load-extension-dirs

```lisp
(load-extension-dirs dirspec...)
```

Loads one or more directories of extensions.  Within each directory all
`.py` files are loaded alphabetically first, then all `.lisp` files
alphabetically.  Each `dirspec` must be a string containing a directory path.

```lisp
(load-extension-dirs "~/my-extensions/")
```

Returns `T` if all directories loaded successfully.

### load-module

```lisp
(load-module filespec &key name)
```

Loads a `.lisp` file into a new named module and binds it in the global
environment.  All definitions in the file become symbols inside the module
rather than in the global environment.  Access them with the colon accessor:
`(module:symbol)`.

`filespec` is a string path to the `.lisp` file.  `:name` optionally
specifies the module name as a symbol or string; otherwise the name is
derived from the filename (stem uppercased).

```lisp
(load-module "geometry.lisp")           ; module name: GEOMETRY
(load-module "geometry.lisp" :name geo) ; module name: GEO
(geometry:area shape)
```

Returns the new module object.

### make-module

```lisp
(make-module name)
```

Creates and returns a new empty module with the given name.  `name` may be
a symbol or a string.  The module is bound in the global environment.  You
can then load primitives into it with `load-extension :name`:

```lisp
(make-module 'mylib)
(load-extension "my-prims.py" :name mylib)
(mylib:my-func 1 2)
```

Returns the new module object.

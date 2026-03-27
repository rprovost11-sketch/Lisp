# Modifying the Interpreter

Python's Lisp is designed to be easy to extend and modify.  The code
prioritizes readability and organization over performance, and the cleanest
path to adding new capabilities is through the extension system rather than
editing the interpreter core.

---

## Writing Extensions

The recommended way to add new primitives or Lisp-level definitions is to
write an extension file and load it into the interpreter.  Extensions require
no changes to the interpreter source.

For a full guide covering Python extensions with `@primitive`, Lisp
extensions, the startup loading sequence, and the Lisp callables for loading
extensions at runtime, evaluate:

```lisp
(help "EXTENSIONS")
```

A fully annotated example Python extension is available at
`pythonslisp/examples/my_extension.py`.  It covers all three `LambdaListMode`
cases and includes the complete set of imports needed to get started.

---

## Exceptions in the Evaluator

When writing primitives or modifying the interpreter core, the exception
hierarchy is important to understand.

During s-expression evaluation, catch low-level Python exceptions as close
to their source as possible and re-raise them as higher-level Lisp exceptions
- `LRuntimeError` or `LRuntimePrimError` - with information useful to the
Lisp programmer.  This ensures that error messages make sense in Lisp terms
rather than exposing Python internals.

Do **not** wrap a call to `cek_eval()` (or anything that calls
`cek_eval()`) in a broad `try` block.  Doing so intercepts exceptions that are
meant to propagate upward - condition signals, `throw`/`catch` tags,
`return-from` tokens, and continuation invocations all travel as exceptions
through the call stack.  Catching them prematurely will break control flow in
ways that are difficult to diagnose.  The listener's `repl` has the
authoritative top-level handler; let exceptions reach it.

The exception types are in `pythonslisp/Exceptions.py`:

- `LRuntimeError` - general runtime error; use when the error is not
  attributable to a specific primitive
- `LRuntimePrimError` - runtime error attributed to a named primitive;
  preferred because it produces cleaner error messages
- `LArgBindingError` - argument binding failure; raised by the environment
  during lambda-list processing
- `LAnalysisError` - static analysis error; raised by the analyzer before
  evaluation begins
- `Signaled` - a Lisp condition signal; carries a condition object
- `Thrown` - a `throw` with a tag and value; caught by `catch`
- `ReturnFrom` - a `return-from`; caught by `block`
- `ContinuationInvoked` - a full re-invocable continuation; raised by `call/cc`

---

## Private vs Public API

Methods and attributes whose names begin with `_` are private implementation
details.  They are not part of the public interface and may change without
notice between versions.

The public interface for embedding the interpreter in Python code is
described in `(help "API")` (or in the API section of `README.md`).  The
primary classes you will interact with are `pythonslisp.Interpreter`,
`pythonslisp.AST`, and `pythonslisp.Parser`.

Note that the public interface for `pythonslisp.Parser.Lexer` is found in
its base class `pythonslisp.ltk.ParserBase.LexerBase`; `Lexer` itself only
implements private methods required by the base class.

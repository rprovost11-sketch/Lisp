# Debugging

Python's Lisp provides several tools for understanding and diagnosing program
behavior: function call tracing, error stack traces, and the break debugger.

---

## Function Call Tracing

`(trace fn...)` enables tracing for one or more named functions.  Each call
to a traced function prints an entry line showing the function name and
arguments, and an exit line showing the return value.  Calls are indented
to reflect nesting depth.

`(untrace fn...)` removes the named functions from the trace list.  With no
arguments, `(untrace)` removes all named-function tracing.

`(trace)` with no arguments returns the list of currently traced functions
without changing anything.

Both `trace` and `untrace` return the updated list of traced function names.

**Note:** Only user-defined functions (created with `defun` or `lambda`) are
traced.  Primitive functions are never traced.

### Example

    >>> (defun fact (n)
    ...   (if (= n 0) 1 (* n (fact (- n 1)))))
    ==> FACT
    >>> (trace fact)
    ==> (FACT)
    >>> (fact 3)
     0: (FACT 3)
     1:   (FACT 2)
     2:     (FACT 1)
     3:       (FACT 0)
     3:       FACT returned 1
     2:     FACT returned 1
     1:   FACT returned 2
     0: FACT returned 6
    ==> 6
    >>> (untrace fact)
    ==> ()

Each output line is prefixed with the call depth.  Entry lines show the
function call form; exit lines show the return value.

---

## Error Stack Traces

`]traces on|off` enables or disables call-stack traces on errors.  With no
argument, it reports the current state.

When stack traces are on, a runtime error prints a call stack showing each
active function call at the time of the error — one frame per line, with the
source line and a caret indicating the position of the call.

**Performance note:** Stack-trace mode makes the evaluator roughly 20–30%
slower.  It is off by default.  Turn it on when diagnosing errors in
file-loaded code, and off again for normal use.

---

## The Break Debugger

`(break)` drops execution into a nested debug REPL at the point where it is
called.  All local variables in scope are displayed.  The nested prompt is
`brk>>> `.

    (defun divide (a b)
      (break)
      (/ a b))

When `(break)` is reached, output similar to the following appears:

    *** Break ***
    Locals:
      A = 10
      B = 0

      ]continue [expr]  resume execution (returning expr or NIL)
      ]abort            abort to top level

    brk>>>

You can evaluate any Lisp expression at the `brk>>> ` prompt to inspect or
modify program state.  Multi-line expressions and the super-bracket are
supported.

`(break "message")` displays *message* alongside the break notification,
which is useful for distinguishing multiple breakpoints.

### Break Commands

| Command | Effect |
|---|---|
| `]continue` | Resume execution, returning NIL to the call site |
| `]continue expr` | Resume execution, returning the value of *expr* |
| `]abort` | Abort execution and return to the top-level `>>> ` prompt |

### Example Session

    brk>>> (+ a b)
    ==> 10
    brk>>> ]continue 42
    ==> 42

The value passed to `]continue` becomes the return value of the `(break)`
call in the surrounding code.

---

## Quick Reference

| Tool | What it does |
|---|---|
| `(trace fn...)` | Trace specific functions by name |
| `(untrace fn...)` | Remove named functions from trace list |
| `(untrace)` | Clear all named-function tracing |
| `(trace)` | Return list of currently traced functions |
| `]traces on\|off` | Enable/disable call-stack traces on errors (slower) |
| `(break)` | Drop into a nested debug REPL at the call site |
| `(break "msg")` | Same, with an identifying message |

# Debugging

*Quick reference: `(help "debugging")` - Full documentation: this file.*

Python's Lisp provides a comprehensive debugging toolkit: an interactive
debugger with breakpoints, watches, and stepping; function call tracing;
trace output helpers; object inspection; and error stack traces.
For performance profiling tools, see `(help "profiling-doc")`.

All debugging commands at the `debug>` prompt share a unified
interface.  Type `h` at any debug prompt for a full command reference,
or `h cmd` for help on a specific command.

| Command | Effect |
|---|---|
| `s` | Step into the next expression |
| `n` | Step over the next expression |
| `o` | Step out (continue until current function returns) |
| `c` | Continue execution |
| `abort` | Abort execution to the top level |
| `bt` | Show backtrace (call stack) |
| `e expr` | Evaluate *expr* in the current environment |
| `i expr` | Evaluate *expr* and interactively inspect the result |
| `v [n]` | Show local variables (optional *n* limits scope depth); filters out callables |
| `v name+` | Show specific named variables (like `trace-vars`) |
| `w [*]` | Show the watch list |
| `w expr+` | Add watches - variables or expressions (printed after each step) |
| `w- expr\|#` | Remove from the watch list (by expression or number) |
| `w- *` | Clear the watch list |
| `b [*]` | List all breakpoints |
| `b name` | Break on function entry |
| `b name (cond)` | Conditional function-entry breakpoint |
| `b fn:call[:n]` | Break at the *n*th call to *call* inside *fn*'s body |
| `b! name\|#` | Toggle enable/disable (by name or number) |
| `b- name\|#` | Remove breakpoint (by name or number) |
| `b- *` | Clear all breakpoints |
| `bo` | Show the break-on list |
| `bo name+` | Add to break-on list |
| `bo- name` | Remove from break-on list |
| `bo- *` | Clear break-on list |
| `bo+` | Restore default break-on list |
| `:r n` | Invoke restart by number (from the restart list) |
| `:r name` | Invoke restart by name |
| `h [cmd]` | Show help for all commands or a specific command |

---

## Debugger

`]debug` opens an interactive debugger REPL where you can set up
breakpoints and watches before running code.  Use `rd` to run expressions
with breakpoints active.

    >>> ]debug

    *** Debugger ***
    Type h for command help.

    debug> b fact
    Breakpoint set on FACT.
    debug> w n
    Watching: N
    debug> rd (fact 5)

    *** Breakpoint: FACT ***
      (FACT 5)
      [watch]
        N = 5
    debug> c
    ...
    ==> 120
    debug> b- *
    All breakpoints cleared.
    debug> q

### Debugger commands

| Command | Effect |
|---|---|
| `rd expr` | Run *expr* with breakpoints and watches active |
| `rd` | Re-run the last *rd* expression from the beginning |
| `e expr` | Evaluate *expr* (breakpoints suppressed) |
| `i expr` | Evaluate *expr* and inspect the result |
| `b`, `b-` | Breakpoint commands (same as break/step prompts) |
| `w`, `w-` | Watch commands (same as break/step prompts) |
| `bo`, `bo-`, `bo+` | Break-on list management |
| `:r n\|name` | Invoke a restart by number or name |
| `q` / `quit` | Exit the debugger |
| `h [cmd]` | Show help for all commands or a specific command |

### Restarting

Typing `rd` alone at the `debug>` prompt
restarts the most recent expression from the beginning.  This aborts
the current execution and re-evaluates the expression with all breakpoints
and watches intact.

    debug> rd (process-data big-list)
    *** Breakpoint: VALIDATE ***
    debug> rd
    Restarting: (PROCESS-DATA BIG-LIST)
    *** Breakpoint: VALIDATE ***
    debug>

---

## Stepping

At a breakpoint, `s`, `n`, and `o` enter stepping mode.  The stepper
pauses before each list expression is evaluated, showing what is about
to be evaluated indented by nesting depth.

    *** Breakpoint: FACT ***
      (FACT 5)
    debug> s
        (IF (= N 0) 1 (* N (FACT (- N 1))))
    debug> n
        (* N (FACT (- N 1)))
    debug> c
    ==> 120

- `s` stepped into the function body, revealing the `if` expression
- `n` stepped over the `if` (evaluated it without pausing inside), then paused at the next expression
- `c` continued to completion

This lets you run at full speed to a breakpoint, then step through the
interesting part.

### Step out

`o` continues execution until the current function returns, then pauses:

    debug> o
        (PROCESS-ITEMS REMAINING)
    debug>

This is useful when you have stepped too deep into a call chain and want
to return to the caller.

### Backtrace

`bt` shows the current call stack at the `debug>` prompt:

    debug> bt
      0: (FACT 3)
      1: (FACT 4)
      2: (FACT 5)

Frame 0 is the most recent call.  Stack traces are automatically enabled
during `rd` execution.

### Inspecting state while stepping

Use `e` to evaluate expressions, `i` to inspect a value, and `v` to view
locals at any pause point:

    debug> v
    --- scope 0 ---
    N:   5
    debug> e (+ n 1)
    ==> 6
    debug> v n
    N:   5

The `v` command filters out bindings whose values are functions, primitives,
or macros to reduce clutter.  Use `v name` to look up specific variables
(these are shown even if callable).

The stepper only pauses on non-empty list expressions (function calls and
special forms).  Atomic expressions like numbers and symbol lookups are
evaluated silently.

---

## Watch List

The `w` command manages a watch list.  After every step (`s` or `n`),
the watched values are printed automatically before the next prompt.

Watches can be simple variable names or full expressions:

    debug> w n
    Watching: N
    debug> w (- n 1)
    Watching: N, (- N 1)
    ...
      (FACT (- N 1))
      [watch]
        N = 3
        (- N 1) = 2
    debug>

### Managing the watch list

    w                    ; show the watch list  (w * is the same)
    w x y                ; add X, Y to the watch list
    w (length lst)       ; watch an expression
    w x (- n 1) total    ; mix variables and expressions
    w- x                 ; remove X from the watch list
    w- 2                 ; remove watch #2 (from numbered listing)
    w- *                 ; clear the watch list

Watches are available at the `debug>` prompt.
They carry over when transitioning between prompts.

---

## Runtime Breakpoints

Breakpoints pause execution without needing to edit source code.  Set them
from the `debug>` prompt.

### Function-entry breakpoints

Break whenever a named function is about to be called:

    debug> b foo
    Breakpoint set on FOO.

### Inner breakpoints

Break at a specific call site inside a function's body using
`b fn:callable` or `b fn:callable:n`:

    (defun process (items)
      (mapcar #'validate items)
      (mapcar #'transform items))

    debug> b process:mapcar:2
    Breakpoint set on PROCESS:MAPCAR:2.

This breaks just before the second `(mapcar ...)` call in `process`.
If the index is omitted, it defaults to 1 (the first call):

    debug> b process:mapcar
    Breakpoint set on PROCESS:MAPCAR.

Inner breakpoints work by capturing a reference to the exact AST node in
the function's body, so they fire only for that specific call site - not
for calls to the same function from other locations.

### Conditional breakpoints

Any breakpoint can have a condition.  A condition expression in parentheses
is evaluated in the calling environment.  The breakpoint fires only when
the condition is non-NIL:

    debug> b foo (> x 5)
    Breakpoint set on FOO :when (> x 5)

    debug> b process:mapcar:2 (> (length items) 10)
    Breakpoint set on PROCESS:MAPCAR:2 :when (> (length items) 10)

### Managing breakpoints

    b              ; list all breakpoints  (b * is the same)
    b! foo         ; toggle enable/disable (disabled breakpoints don't fire)
    b! 2           ; toggle breakpoint #2 (from numbered listing)
    b- foo         ; remove breakpoint on FOO
    b- 1           ; remove breakpoint #1 (from numbered listing)
    b- *           ; clear all breakpoints

The `b` listing shows enabled breakpoints in bold and disabled breakpoints
in dim text with a `[disabled]` tag.

### When a breakpoint fires

Execution pauses and shows the expression about to be evaluated.  You get
a `debug>` prompt with the full command set - `s` to step into, `n` to step
over, `o` to step out, `c` to continue, etc.

    *** Breakpoint: FOO ***
      (FOO 3 4)
    debug>

Breakpoints persist across evaluations until explicitly removed.  They
survive `c` (continue) and fire again on the next matching call.

---

## Break-On List

The break-on list is a set of function names that automatically trigger
breakpoints during `rd` execution.  By default it includes `ERROR`,
`WARN`, `SIGNAL`, and `THROW` - so the debugger pauses whenever your code
hits an error-related call.

    debug> bo
    Break-on: ERROR, SIGNAL, THROW, WARN

The break-on list is shown in the `b`/`b *` listing with a `[break-on]` tag.

### Managing the break-on list

    bo             ; show the break-on list
    bo parse       ; add PARSE to the list
    bo- warn       ; remove WARN from the list
    bo- *          ; clear the entire list
    bo+            ; restore the default list

The break-on list persists across debugger sessions until the interpreter
is rebooted.  Break-on entries only fire during `rd` execution - they
do not affect normal evaluation.

---

## Restarts in the Debugger

When a breakpoint fires, available restarts from the current execution
context are displayed automatically:

    *** Breakpoint: VALIDATE ***
      (VALIDATE X)
    Restarts:
      0: [USE-VALUE]
      1: [SKIP]

    debug>

Use the `:r` command to invoke a restart by number or name:

    debug> :r 0
    debug> :r use-value

Restarts are established by `restart-case` in the program's code.  See
`(help "conditions-doc")` for full documentation on the restart system.

---

## Function Call Tracing

`(trace fn...)` enables tracing for one or more named functions.  Each call
to a traced function prints an entry line showing the function name and
arguments, and an exit line showing the return value.  Calls are indented
to reflect nesting depth.

`(untrace fn...)` removes the named functions from the trace list.  With no
arguments, `(untrace)` removes all tracing.

`(trace)` with no arguments returns the list of currently traced functions.

### Example

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

Trace output goes to `*trace-output*`.

---

## Trace Output Helpers

These tools print diagnostic output to `*trace-output*` without stopping
execution.  All accept an optional string label as the first argument.

### trace-vars

Print named variables and their values:

    (trace-vars x y)           ; X:   42\nY:   10
    (trace-vars "checkpoint" x y)  ; with label header

### trace-locals

Print local variable bindings grouped by scope:

    (trace-locals)             ; all scopes
    (trace-locals 2)           ; innermost 2 scopes
    (trace-locals "tag" 1)     ; with label, 1 scope

### trace-eval

Print expressions and their evaluated results:

    (trace-eval (+ x 1) (* y 2))  ; (+ X 1):   43\n(* Y 2):   20

---

## Inspection Tools

### describe

`(describe obj)` prints a structured description of any object - its type,
attributes, and documentation (for callables):

    >>> (describe car)
    CAR is a PRIMITIVE
      Usage: (CAR list)
      Arity: 1
      Documentation:
        Returns the first item in a list.

### inspect

`(inspect obj)` interactively navigates a structured object.  For lists,
dicts, and structs, elements are numbered for drill-down:

    >>> (inspect '(1 (2 3) 4))
    CONS, 3 elements
      0: 1
      1: (2 3)
      2: 4
    inspect> 1
    CONS, 2 elements
      0: 2
      1: 3
    inspect> ]up
    ...
    inspect> ]quit

### apropos

`(apropos "substring")` lists all symbols whose names contain the substring:

    >>> (apropos "MAP")
    MAPCAR  (primitive)
    ...

---

## Error Stack Traces

`]traces on|off` enables or disables call-stack traces on errors.

When enabled, a runtime error prints a call stack showing each active
function call at the time of the error.

**Performance note:** Stack-trace mode makes the evaluator roughly 20-30%
slower.  It is off by default.


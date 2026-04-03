# The Listener

The Listener is a smart Read-Eval-Print Loop (REPL) that reads expressions,
evaluates them, and prints results.  Each expression is entered at the `>>> `
prompt.  Continuation lines are shown with the `... ` prompt.

---

## Auto-Submission

The Listener tracks parenthesis depth as you type.  When the depth returns
to zero - meaning all open parentheses have been closed - the expression is
submitted automatically without requiring a blank line.

A blank line at the `... ` continuation prompt also submits the accumulated
expression immediately.

---

## Super-Bracket

Typing `]` as the last character of a line closes all outstanding open
parentheses and submits the expression.  For example:

    >>> (defun factorial (n)
    ...   (if (= n 0) 1
    ...     (* n (factorial (- n 1]

The `]` appends the three closing parentheses needed and evaluates.

The super-bracket is ignored when the parentheses are already balanced or
when the `]` appears inside a string literal.

---

## History Recall

Previously entered expressions are saved in a history file
(`~/.lisp_history`).  Use the **Up** and **Down** arrow keys to navigate
through history at the `>>> ` or `... ` prompt.  History persists across
sessions.

---

## Ctrl-R: Incremental History Search

Press **Ctrl-R** to enter reverse incremental search mode.  Type any
substring to search backwards through history — the matching entry is shown
as you type.  Press **Ctrl-R** again to find the next older match.
**Enter** accepts the match and returns it to the prompt for editing or
immediate submission.  **Escape** or **Ctrl-C** cancels and restores your
current input.

---

## Auto-Indent

When entering a multi-line expression, each continuation line is
automatically pre-filled with 3 spaces of indentation per open parenthesis
depth.  The prefilled indent can be overridden by editing the line before
pressing Enter.

---

## Last-Result Variables

The three most recent top-level results are always available as `%`, `%%`,
and `%%%`.  `%` holds the result of the last expression evaluated; `%%` the
one before that; `%%%` the one before that.

    >>> (+ 1 2)
    ==> 3
    >>> (* 4 5)
    ==> 20
    >>> (+ % %%)
    ==> 23

These variables are updated after every successful evaluation.  They are
not updated when an error occurs.

---

## Interrupting Evaluation

Press **Ctrl-C** at any time to:

- Interrupt a running evaluation and return to the `>>> ` prompt, or
- Cancel a partially typed multi-line expression and start over.

---

## Listener Commands

Listener commands begin with `]` and control the session rather than
evaluating Lisp.  Type `]help` for a full list.  Key commands include:

| Command | Purpose |
|---|---|
| `]help [cmd]` | List all commands, or show help for a specific command |
| `]log <file>` | Begin recording a session log |
| `]close` | Close the current session log |
| `]continue <file>` | Restore a log and continue appending to it |
| `]readlog <file>` | Replay a session log |
| `]readsrc <file>` | Load and evaluate a Lisp source file |
| `]reboot` | Reboot the interpreter to a clean state |
| `]trace` | Toggle global function call tracing on/off |
| `]traces on\|off` | Enable/disable call-stack traces on errors |
| `]test [file]` | Run a test log file, or the full test suite |
| `]exit` / `]quit` | Exit the Listener |

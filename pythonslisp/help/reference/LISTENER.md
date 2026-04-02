# The Listener

The Listener is the interactive Read-Eval-Print Loop (REPL) that reads
expressions, evaluates them, and prints results.  Each expression is entered
at the `>>> ` prompt.  Continuation lines are shown with the `... ` prompt.

---

## Auto-Submission

The Listener tracks parenthesis depth as you type.  When the depth returns
to zero — meaning all open parentheses have been closed — the expression is
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

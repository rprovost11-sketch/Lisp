# Debugging - Quick Reference

*Full documentation: `(help "debugging-doc")`*

| Tool | What it does |
|---|---|
| `]debug` | Open debugger REPL for breakpoints, watches, and stepping |
| `(trace fn...)` | Trace specific functions by name |
| `(untrace fn...)` | Remove functions from trace list |
| `(trace-vars ...)` | Print variable names and values |
| `(trace-locals ...)` | Print local bindings by scope |
| `(trace-eval ...)` | Print expressions and results |
| `(describe obj)` | Print structured object description |
| `(inspect obj)` | Interactively navigate a structure |
| `(apropos "str")` | Search symbols by substring |
| `]traces on\|off` | Toggle call-stack traces on errors |

## Debugger commands (at break>, step>, debug> prompts)

| Command | Effect |
|---|---|
| `s` | Step into |
| `n` | Step over |
| `o` | Step out |
| `c` | Continue |
| `abort` | Abort to top level |
| `bt` | Backtrace |
| `e expr` | Evaluate in current environment |
| `v [n]` | Show local variables |
| `w expr+` | Add watches |
| `w- expr\|#` | Remove watch (by expression or number) |
| `b name` | Set breakpoint |
| `b name (cond)` | Conditional breakpoint |
| `b! name\|#` | Toggle breakpoint (by name or number) |
| `b- name\|#` | Remove breakpoint (by name or number) |
| `rd expr` | Run with breakpoints (debug> only) |
| `h [cmd]` | Help |

# Profiling & Timing - Quick Reference

*Full documentation: `(help "profiling-doc")`*

## Timing

| Form | Effect |
|---|---|
| `(time expr)` | Evaluate and print elapsed time + GC activity |
| `(benchmark n expr)` | Run `expr` n times, print avg/min/max stats |

## Function Profiling

| Form | Effect |
|---|---|
| `(profile 'fn ...)` | Enable profiling for named functions |
| `(profile)` | List currently profiled function names |
| `(unprofile 'fn ...)` | Remove profiling from named functions |
| `(unprofile)` | Remove all profiling |
| `(profile-report)` | Print timing report (calls, total, avg, %) |
| `(profile-reset)` | Zero counters without removing instrumentation |

## Typical workflow

```lisp
(profile 'fib 'helper)   ; instrument
(my-code)                 ; run
(profile-report)          ; analyze
(unprofile)               ; restore
```

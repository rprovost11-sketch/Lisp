# Profiling & Timing

*Quick reference: `(help "profiling")` - Full documentation: this file.*

Python's Lisp provides three profiling tools for measuring and optimizing
the performance of your code: `time` for quick one-off measurements,
`benchmark` for statistical analysis over repeated runs, and a
function-level profiler for understanding where time is spent across
multiple functions.

---

## time

`(time expr)` evaluates an expression, prints elapsed wall-clock time
and GC activity to `*trace-output*`, and returns the result.

```lisp
>>> (time (fib 20))
; Evaluation took 0.356695 seconds of real time.
; 50 GC collections during evaluation (gen0=50, gen1=0, gen2=0).
==> 6765
```

The GC line is only printed when collections actually occurred.  The
three generations (gen0, gen1, gen2) correspond to Python's generational
garbage collector - gen0 collections are the most frequent and cheapest.

`time` preserves multiple return values:

```lisp
>>> (multiple-value-list (time (values 1 2 3)))
; Evaluation took 0.000004 seconds of real time.
==> (1 2 3)
```

`time` composes transparently inside other expressions:

```lisp
>>> (+ 10 (time (* 3 4)))
; Evaluation took 0.000002 seconds of real time.
==> 22
```

---

## benchmark

`(benchmark n expr)` evaluates `expr` exactly `n` times and prints
timing statistics: total time, per-iteration average, minimum, and
maximum.  Returns the result of the last evaluation.

```lisp
>>> (benchmark 10000 (fib 15))
; 10,000 iterations in 3.412 seconds.
; Per iteration: avg 341.2 us, min 298.1 us, max 1,204.3 us.
==> 610
```

Units are chosen automatically: seconds for long times, milliseconds for
moderate, microseconds for fast operations.

Use `benchmark` to:

- **Compare algorithms.** Run the same workload with two implementations
  and compare the averages.
- **Detect variance.** A large gap between min and max suggests GC pauses
  or other interference - run more iterations to get a stable average.
- **Validate optimizations.** Measure before and after a change to confirm
  the improvement is real.

```lisp
; Compare two approaches
>>> (benchmark 1000 (remove-if-not #'evenp (iota 100)))
; 1,000 iterations in 0.987654 seconds.
; Per iteration: avg 987.7 us, min 945.0 us, max 1,123.0 us.
==> ...

>>> (benchmark 1000 (mapcar #'(lambda (x) (when (evenp x) x)) (iota 100)))
; 1,000 iterations in 1.234567 seconds.
; Per iteration: avg 1.2346 ms, min 1.1890 ms, max 1.4560 ms.
==> ...
```

---

## Function Profiling

The profiler wraps individual functions with timing instrumentation.
Only profiled functions pay any overhead - the evaluator itself is
unmodified, so non-profiled code runs at full speed.

### Basic workflow

```lisp
; 1. Define some functions
(defun fib (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
(defun fib-sum (a b) (+ (fib a) (fib b)))

; 2. Enable profiling (pass quoted symbols)
(profile 'fib 'fib-sum)

; 3. Run your code
(fib-sum 10 12)

; 4. View results
(profile-report)
; Function     Calls     Total (s)    Avg (ms)       %
; --------  --------  ------------  ----------  ------
; FIB            642      0.133578      0.2081   88.0%
; FIB-SUM          1      0.018190     18.1899   12.0%
; --------  --------  ------------  ----------  ------
; Total          643      0.151768

; 5. Clean up
(unprofile)
```

### Commands

| Form | Effect |
|---|---|
| `(profile 'fn ...)` | Enable profiling for named functions |
| `(profile)` | List currently profiled function names |
| `(unprofile 'fn ...)` | Remove profiling from named functions |
| `(unprofile)` | Remove all profiling |
| `(profile-report)` | Print timing report |
| `(profile-reset)` | Zero all counters without removing instrumentation |

### Reading the report

The report is sorted by total time (most expensive first):

- **Calls** - number of times the function was invoked
- **Total (s)** - cumulative wall-clock time in seconds
- **Avg (ms)** - average time per call in milliseconds
- **%** - percentage of total profiled time

### Iterating with profile-reset

Use `profile-reset` to zero counters between experiments without
removing the instrumentation:

```lisp
(profile 'fib)
(fib 10)
(profile-report)    ; see results for fib(10)

(profile-reset)     ; zero counters
(fib 20)
(profile-report)    ; see results for fib(20) only
(unprofile)
```

### How it works

When you profile a function, the profiler saves the original and rebinds
the name to a wrapper that records timing around each call.  This means:

- **Recursive calls are counted.** `(fib 10)` produces 177 calls because
  each recursive call goes through the wrapper.
- **Only global bindings are affected.** Closures that captured the
  original function before profiling will not be measured.
- **Profiled functions lose tail-call optimization with each other**,
  since the wrapper interposes a call boundary.  This is expected -
  profiling inherently measures what would otherwise be optimized away.
- **Multiple values are reduced to the primary value** for profiled
  functions, since the wrapper dispatches through `cek_apply`.

### Tips

- Profile the smallest set of functions you suspect.  Start with the
  top-level entry point, read the report, then add callees as needed.
- Use `time` first to get a rough picture, then switch to `profile` when
  you need per-function breakdowns.
- After profiling, always `(unprofile)` to restore full performance and
  TCO behavior.

---

## Choosing the right tool

| Goal | Tool |
|---|---|
| "How long does this take?" | `(time expr)` |
| "How fast is this per iteration?" | `(benchmark n expr)` |
| "Where is the time going?" | `(profile ...)` + `(profile-report)` |

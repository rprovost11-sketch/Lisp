# Looping and Iteration

Python's Lisp provides four looping macros for imperative iteration, a set
of higher-order functions for functional-style iteration over sequences, and
full tail-call optimization (TCO) so that tail-recursive functions can iterate
over arbitrarily large inputs without growing the call stack.

---

## while

```lisp
(while condition body...)
```

Evaluates `body` repeatedly as long as `condition` is non-NIL.  Returns the
value of the last body form from the final iteration, or NIL if the loop
never runs.

```lisp
(setf n 1)
(while (<= n 5)
  (print n)
  (setf n (+ n 1)))
; prints 1 2 3 4 5
```

`while` has no built-in early-exit mechanism.  Wrap it in a `block` and use
`return` if you need to break out early:

```lisp
(block nil
  (setf i 0)
  (while t
    (if (= i 3) (return i))
    (setf i (+ i 1))))
;==> 3
```

---

## dotimes

```lisp
(dotimes (var count [result]) body...)
```

Executes `body` exactly `count` times.  `var` is bound to 0 on the first
iteration and incremented by 1 each time, up to `count - 1`.  After the
loop, `var` equals `count` when `result` is evaluated; `result` defaults to
NIL.  Use `(return value)` for early exit.

```lisp
(dotimes (i 5)
  (print i))
; prints 0 1 2 3 4

; Accumulate into a list; return it as the loop result
(let ((acc nil))
  (dotimes (i 5 (reverse acc))
    (push! acc (* i i))))
;==> (0 1 4 9 16)

; Early exit
(dotimes (i 100)
  (if (= i 7) (return i)))
;==> 7
```

---

## dolist

```lisp
(dolist (var list-expr [result]) body...)
```

Iterates over each element of `list-expr`, binding `var` to each element in
turn.  After the loop, `var` is NIL when `result` is evaluated; `result`
defaults to NIL.  Use `(return value)` for early exit.

```lisp
(dolist (x '(apple banana cherry))
  (print x))
; prints APPLE BANANA CHERRY

; Sum a list
(let ((total 0))
  (dolist (n '(1 2 3 4 5) total)
    (setf total (+ total n))))
;==> 15

; Find the first even number
(dolist (n '(1 3 5 4 7))
  (if (evenp n) (return n)))
;==> 4
```

---

## for

```lisp
(for (var initForm) condition nextForm body...)
```

A general-purpose C-style loop.  `var` is bound to `initForm` before the
first iteration.  Before each iteration `condition` is tested; the loop
continues while it is non-NIL.  After each iteration `var` is rebound to
the value of `nextForm`.  Use `(return value)` for early exit.

```lisp
; Count up
(for (i 0) (< i 5) (+ i 1)
  (print i))
; prints 0 1 2 3 4

; Count down, accumulate
(let ((acc nil))
  (for (i 10) (> i 0) (- i 2)
    (push! acc i))
  (reverse acc))
;==> (10 8 6 4 2)

; Read a file line by line until EOF
(let ((results nil))
  (with-open-file (f "data.txt")
    (for (line (read-line f nil nil)) line (read-line f nil nil)
      (push! results line)))
  (reverse results))
```

---

## Higher-Order Iteration

When the goal is to produce a new sequence or reduce a sequence to a value,
the functional forms are more expressive than the imperative macros above.

### mapcar

Applies a function to each element of a list and returns a new list of the
results.  Accepts multiple lists; the function receives one element from
each list per call.

```lisp
(mapcar 'oddp '(1 2 3 4 5))
;==> (T NIL T NIL T)

(mapcar '+ '(1 2 3) '(10 20 30))
;==> (11 22 33)

(mapcar (lambda (x) (* x x)) '(1 2 3 4 5))
;==> (1 4 9 16 25)
```

### mapc

Like `mapcar` but intended for side effects.  Returns the original first
list rather than a list of results.

```lisp
(mapc 'print '(1 2 3))
; prints 1 2 3
;==> (1 2 3)
```

### reduce

Folds a list to a single value by applying a two-argument function
cumulatively from left to right.

```lisp
(reduce '+ '(1 2 3 4 5))   ;==> 15
(reduce '* '(1 2 3 4 5))   ;==> 120
(reduce 'max '(3 1 4 1 5 9 2 6))  ;==> 9
```

### remove-if / remove-if-not

Filter a sequence by a predicate.

```lisp
(remove-if 'oddp '(1 2 3 4 5 6))      ;==> (2 4 6)
(remove-if-not 'oddp '(1 2 3 4 5 6))  ;==> (1 3 5)
```

### every / some

Test whether a predicate holds for all or any elements of a sequence.

```lisp
(every 'oddp '(1 3 5))   ;==> T
(every 'oddp '(1 2 5))   ;==> NIL
(some  'evenp '(1 3 4))  ;==> T
```

---

## Tail-Recursive Iteration with TCO

The interpreter applies tail-call optimization (TCO) to all tail-position
calls.  A tail-recursive function therefore iterates in constant stack space
— it will not overflow the call stack regardless of how many iterations it
performs.

The key rule: the recursive call must be in **tail position** — the last
thing the function does before returning.  Wrapping the call in `+`, `cons`,
or any other form takes it out of tail position.

### Accumulator pattern

Use an accumulator parameter to carry the result forward rather than building
it on the way back up:

```lisp
; NOT tail-recursive -- cons wraps the recursive call
(defun my-map-bad (fn lst)
  (if (null lst)
      nil
      (cons (funcall fn (car lst))
            (my-map-bad fn (cdr lst)))))

; Tail-recursive -- accumulate in reverse, then reverse at the end
(defun my-map (fn lst &optional (acc nil))
  (if (null lst)
      (reverse acc)
      (my-map fn (cdr lst) (cons (funcall fn (car lst)) acc))))

(my-map (lambda (x) (* x x)) '(1 2 3 4 5))
;==> (1 4 9 16 25)
```

### State parameter pattern

Pass all mutable state as parameters; each iteration is a new call with
updated values:

```lisp
; Sum a list of numbers iteratively via TCO
(defun sum-list (lst &optional (acc 0))
  (if (null lst)
      acc
      (sum-list (cdr lst) (+ acc (car lst)))))

(sum-list '(1 2 3 4 5))   ;==> 15

; Count elements satisfying a predicate
(defun count-if-tco (pred lst &optional (n 0))
  (cond
    ((null lst) n)
    ((funcall pred (car lst)) (count-if-tco pred (cdr lst) (+ n 1)))
    (t (count-if-tco pred (cdr lst) n))))

(count-if-tco 'evenp '(1 2 3 4 5 6))  ;==> 3
```

### Named-let style (using labels or a local defun)

Common Lisp does not have named `let`, but the same pattern is available
by binding a local helper with `let` and `setf`:

```lisp
(defun flatten (lst)
  "Flatten an arbitrarily nested list into a flat list."
  (let ((go nil))
    (setf go (lambda (node acc)
               (cond
                 ((null node) acc)
                 ((listp (car node))
                  (go (cdr node) (go (car node) acc)))
                 (t
                  (go (cdr node) (cons (car node) acc))))))
    (reverse (go lst nil))))

(flatten '(1 (2 3) (4 (5 6)) 7))
;==> (1 2 3 4 5 6 7)
```

### When to use TCO vs the loop macros

Use `dolist` / `dotimes` / `for` / `while` when:
- Iterating a fixed number of times or over a known list
- Side effects are the primary goal
- Readability matters more than generality

Use tail recursion when:
- The termination condition is complex or data-driven
- You are threading multiple pieces of state through the loop
- You want a value computed across the entire iteration
- The pattern naturally mirrors a mathematical definition

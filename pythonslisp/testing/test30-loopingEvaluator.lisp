; ============================================================
; test25-tco.lisp
; Tail-call optimisation (TCO) tests.
; All "deep" calls use n=50000, well above the Python recursion
; limit of 10,000, so any test that does not benefit from TCO
; will raise a RecursionError.
; ============================================================

; --- 1. Self-tail-recursion via IF ---

>>> (defun tco-count (n)
...    (if (= n 0)
...       'done
...       (tco-count (- n 1))))
...

==> (FUNCTION TCO-COUNT (N) ... )

>>> ;;; 50000 tail calls -- would overflow without TCO
... (tco-count 50000)
...

==> DONE

>>> (defun tco-sum (n acc)
...    (if (= n 0)
...       acc
...       (tco-sum (- n 1) (+ acc n))))
...

==> (FUNCTION TCO-SUM (N ACC) ... )

>>> ;;; accumulator-style sum of 1..50000
... (tco-sum 50000 0)
...

==> 1250025000

; --- 2. Tail call in LET body ---

>>> (defun tco-let (n)
...    (let ((m (- n 1)))
...       (if (= m 0) 'done (tco-let m))))
...

==> (FUNCTION TCO-LET (N) ... )

>>> (tco-let 50000)
...

==> DONE

; --- 3. Tail call in LET* body ---

>>> (defun tco-letstar (n)
...    (let* ((m (- n 1)))
...       (if (= m 0) 'done (tco-letstar m))))
...

==> (FUNCTION TCO-LETSTAR (N) ... )

>>> (tco-letstar 50000)
...

==> DONE

; --- 4. Tail call in PROGN last form ---

>>> (defun tco-progn (n)
...    (progn
...       (if (= n 0) 'done (tco-progn (- n 1)))))
...

==> (FUNCTION TCO-PROGN (N) ... )

>>> (tco-progn 50000)
...

==> DONE

; --- 5. Tail call via WHEN (macro expands to IF) ---

>>> (defun tco-when (n)
...    (when (> n 0) (tco-when (- n 1))))
...

==> (FUNCTION TCO-WHEN (N) ... )

>>> ;;; when body is in if-tail position via macro expansion
... (tco-when 50000)
...

==> NIL

; --- 6. Tail call via UNLESS (macro expands to IF) ---

>>> (defun tco-unless (n)
...    (unless (= n 0) (tco-unless (- n 1))))
...

==> (FUNCTION TCO-UNLESS (N) ... )

>>> (tco-unless 50000)
...

==> NIL

; --- 7. Mutual tail recursion ---

>>> (defun tco-even (n) (if (= n 0) t   (tco-odd  (- n 1))))
...

==> (FUNCTION TCO-EVEN (N) ... )

>>> (defun tco-odd  (n) (if (= n 0) nil (tco-even (- n 1))))
...

==> (FUNCTION TCO-ODD (N) ... )

>>> ;;; even number of steps -> T
... (tco-even 50000)
...

==> T

>>> ;;; odd number of steps -> T  (odd 1 -> even 0 -> T)
... (tco-odd 50001)
...

==> T

; --- 8. Return-type variety ---

>>> (defun tco-str (n s)
...    (if (= n 0) s (tco-str (- n 1) s)))
...

==> (FUNCTION TCO-STR (N S) ... )

>>> (tco-str 50000 "hello")
...

==> "hello"

>>> (defun tco-lst (n)
...    (if (= n 0) '(1 2 3) (tco-lst (- n 1))))
...

==> (FUNCTION TCO-LST (N) ... )

>>> (tco-lst 50000)
...

==> (1 2 3)

>>> (defun tco-nil (n)
...    (if (= n 0) nil (tco-nil (- n 1))))
...

==> (FUNCTION TCO-NIL (N) ... )

>>> (tco-nil 50000)
...

==> NIL

; --- 9. Closure that tail-calls itself through a global binding ---

>>> (setf tco-closure
...    (let ((x 42))
...       (lambda (n)
...          (if (= n 0) x (tco-closure (- n 1))))))
...

==> (FUNCTION TCO-CLOSURE (N) ... )

>>> (tco-closure 50000)
...

==> 42

; --- 10. &rest parameter in tail-called function ---

>>> (defun tco-rest (n &rest r)
...    (if (= n 0) (length r) (tco-rest (- n 1) 1)))
...

==> (FUNCTION TCO-REST (N &REST R) ... )

>>> ;;; each recursive call passes one rest arg; final sees (length '(1)) = 1
... (tco-rest 50000)
...

==> 1

; --- 11. Non-tail recursion still correct (sanity check) ---

>>> (defun fact (n)
...    (if (= n 0) 1 (* n (fact (- n 1)))))
...

==> (FUNCTION FACT (N) ... )

>>> (fact 10)
...

==> 3628800

>>> (fact 0)
...

==> 1

; --- 12. Error propagates correctly from deep in a TCO chain ---

>>> (defun tco-err (n)
...    (if (= n 0) (// 1 0) (tco-err (- n 1))))
...

==> (FUNCTION TCO-ERR (N) ... )

>>> ;;; division by zero at the bottom of 50000 tail calls
... (tco-err 50000)

%%% ERROR '//': division by zero
%%% USAGE: (// &rest numbers)
==>

; --- 13. Arity error in tail position preserves function name ---

>>> (defun tco-arity (n)
...    (if (= n 0) (tco-arity) (tco-arity (- n 1))))
...

==> (FUNCTION TCO-ARITY (N) ... )

>>> ;;; zero-arg call in tail position -> arg-binding error with correct name
... (tco-arity 3)

%%% Error binding arguments in call to function "TCO-ARITY".
%%% Too few positional arguments.
==>

; --- 14. Unbound variable in tail position propagates correctly ---

>>> (defun tco-unbound (n)
...    (if (= n 0) no-such-var (tco-unbound (- n 1))))
...

==> (FUNCTION TCO-UNBOUND (N) ... )

>>> ;;; unbound symbol error at the end of the chain
... (tco-unbound 5)

%%% Unbound Variable: NO-SUCH-VAR.
==>

; test27-callcc.lisp tests for call/cc (escape continuations)

; Basic escape: continuation invoked immediately
>>> (call/cc (lambda (k) (k 42)))
==> 42

; Lambda returns normally without invoking the continuation
>>> (call/cc (lambda (k) 99))
==> 99

; Continuation invoked inside nested arithmetic outer + still uses escaped value
>>> (+ 1 (call/cc (lambda (k) (+ 10 (k 42)))))
==> 43

; type-of a continuation is CONTINUATION
>>> (call/cc (lambda (k) (type-of k)))
==> CONTINUATION

; functionp is NIL for continuations
>>> (call/cc (lambda (k) (functionp k)))
==> NIL

; macrop is NIL for continuations
>>> (call/cc (lambda (k) (macrop k)))
==> NIL

>>> (call/cc (lambda (k) (k 99)))
==> 99

; Inner call/cc escape stays within the inner scope; outer computation proceeds normally
>>> (call/cc (lambda (k1) (+ 100 (call/cc (lambda (k2) (k2 5))))))
==> 105

; Inner call/cc escapes to the outer call/cc, bypassing the (+ 100 ...) entirely
>>> (+ 1000 (call/cc (lambda (k1) (+ 100 (call/cc (lambda (k2) (k1 5)))))))
==> 1005

; Early exit from dolist loop using call/cc
>>> (defun find-first (pred lst)
...    (call/cc (lambda (exit)
...                (dolist (x lst)
...                   (if (pred x) (exit x) nil))
...                nil)))
==> (FUNCTION FIND-FIRST (PRED LST) ... )

>>> (find-first evenp '(1 3 5 4 7))
==> 4

>>> (find-first evenp '(1 3 5 7))
==> NIL

; Continuation object pretty-prints as #<CONTINUATION>
>>> (string (call/cc (lambda (k) k)))
==> "#<CONTINUATION>"

; Stale continuation: invoking after call/cc has already returned is an error
>>> (let ((saved nil))
...    (call/cc (lambda (k) (setq saved k)))
...    (saved 42))
%%% Continuation invoked outside its dynamic extent.

; call/cc requires exactly 1 argument
>>> (call/cc)
%%% ERROR 'CALL/CC': 1 argument expected.
%%% USAGE: (CALL/CC procedure)

; call/cc argument must be a callable
>>> (call/cc 42)
%%% ERROR 'CALL/CC': Argument must be a callable.
%%% USAGE: (CALL/CC procedure)

; call/cc argument may not be a special form pass quote (unquoted) to get the primitive
>>> (call/cc quote)
%%% ERROR 'CALL/CC': Argument may not be a special form.
%%% USAGE: (CALL/CC procedure)

; Continuation requires exactly 1 argument
>>> (call/cc (lambda (k) (k 1 2)))
%%% Continuation expects exactly 1 argument, got 2.

; --- eql ---

>>> (eql 'foo 'foo)
==> T

>>> (eql 'foo 'bar)
==> NIL

>>> (eql 1 1)
==> T

>>> (eql 1 1.0)
==> NIL

>>> (eql 1/3 1/3)
==> T

>>> (eql nil nil)
==> T

>>> (eql t t)
==> T

>>> (eql "hello" "hello")
==> NIL

; --- catch and throw ---

; throw immediately returns value to catch; remaining body skipped
>>> (catch 'done (throw 'done 42) 99)
==> 42

; normal completion no throw; returns last body form
>>> (catch 'done 1 2 3)
==> 3

; empty body returns NIL
>>> (catch 'tag)
==> NIL

; throw crosses function-call boundary (dynamic extent)
>>> (defun thrower () (throw 'escape 99))
==> (FUNCTION THROWER () ... )

>>> (catch 'escape (thrower) 0)
==> 99

; nested catches: inner tag matches caught by inner; outer returns body after
>>> (catch 'outer
...    (catch 'inner
...       (throw 'inner 1))
...    2)
==> 2

; nested catches: outer tag propagates past inner, caught by outer
>>> (catch 'outer
...    (catch 'inner
...       (throw 'outer 99))
...    42)
==> 99

; tag is evaluated at runtime
>>> (let ((tag 'my-tag)) (catch tag (throw tag 77)))
==> 77

; numeric tag uses eql (same type, same value)
>>> (catch 1 (throw 1 "hit") "miss")
==> "hit"

; eql distinguishes numeric types mismatched tag is an unmatched throw
>>> (catch 1 (throw 1.0 "hit") "miss")
%%% throw: no catch for tag 1.0.

; throw with no matching catch error
>>> (throw 'nowhere 42)
%%% throw: no catch for tag NOWHERE.

; throw requires exactly 2 args
>>> (throw 'foo)
%%% ERROR 'THROW': 2 arguments expected.
%%% USAGE: (THROW tag result)

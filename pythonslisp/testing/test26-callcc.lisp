; ============================================================
; test26-callcc.lisp
; Tests for call/cc (call-with-current-continuation).
; call/cc captures the current escape continuation and passes
; it to a user-supplied function.  Invoking the continuation
; immediately returns its argument as the value of the
; enclosing call/cc expression, bypassing any computation
; that would otherwise follow.
; ============================================================

; --- basic: callable returns normally, k unused ---

>>> ;;; call/cc where the callable ignores k returns normally
... (call/cc (lambda (k) 42))
...

==> 42

>>> ;;; call/cc can return any value type (symbol)
... (call/cc (lambda (k) 'hello))
...

==> HELLO

>>> ;;; call/cc returning nil
... (call/cc (lambda (k) nil))
...

==> NIL

; --- k called immediately: bypasses the rest of the body ---

>>> ;;; calling k short-circuits the rest of the lambda body
... (call/cc (lambda (k) (k 99) 0))
...

==> 99

>>> ;;; k called with a symbol
... (call/cc (lambda (k) (k 'escaped) 'not-reached))
...

==> ESCAPED

>>> ;;; k called with nil
... (call/cc (lambda (k) (k nil) "not reached"))
...

==> NIL

; --- k bypasses outer computation ---

>>> ;;; k skips the surrounding addition
... (+ 1 (call/cc (lambda (k) (k 10) 99)))
...

==> 11

>>> ;;; k skips multiple levels of surrounding computation
... (* 2 (+ 1 (call/cc (lambda (k) (k 5) 0))))
...

==> 12

; --- non-local exit from a loop ---

>>> ;;; find-first: returns first matching element or nil
... ;;; uses while loop (dolist not available in this Lisp)
... (defun find-first (pred lst)
...    (call/cc
...       (lambda (return)
...          (let ((remaining lst))
...             (while remaining
...                (if (funcall pred (car remaining))
...                   (return (car remaining)))
...                (setf remaining (cdr remaining))))
...          nil)))
...

==> (FUNCTION FIND-FIRST (PRED LST) ... )

>>> ;;; finds 4 (first even) and exits early
... (find-first 'evenp '(1 3 5 4 7))
...

==> 4

>>> ;;; returns nil when no element matches
... (find-first 'evenp '(1 3 5 7))
...

==> NIL

>>> ;;; works on empty list
... (find-first 'evenp '())
...

==> NIL

>>> ;;; finds first element matching a lambda predicate
... (find-first (lambda (x) (= x "b")) '("a" "b" "c"))
...

==> "b"

; --- exception-like non-local exit ---

>>> ;;; safe-divide: returns symbol on division by zero
... (defun safe-divide (a b)
...    (call/cc
...       (lambda (escape)
...          (if (= b 0)
...             (escape 'division-by-zero)
...             (/ a b)))))
...

==> (FUNCTION SAFE-DIVIDE (A B) ... )

>>> ;;; normal division (Python / always returns float for int/int)
... (safe-divide 10 2)
...

==> 5.0

>>> ;;; division by zero escapes with symbol
... (safe-divide 10 0)
...

==> DIVISION-BY-ZERO

; --- continuationp predicate ---

>>> ;;; continuationp returns t for a continuation
... (call/cc (lambda (k) (continuationp k)))
...

==> T

>>> ;;; continuationp returns nil for a non-continuation
... (continuationp 42)
...

==> NIL

>>> ;;; continuationp returns nil for a function
... (continuationp (lambda (x) x))
...

==> NIL

; --- type-of ---

>>> ;;; type-of a continuation is CONTINUATION
... (call/cc (lambda (k) (type-of k)))
...

==> CONTINUATION

; --- escape continuation becomes inactive after normal return ---

>>> ;;; capture the continuation in a global, then call/cc returns normally
... (setf cc26 nil)
...

==> NIL

>>> (call/cc (lambda (k) (setf cc26 k) 42))
...

==> 42

>>> ;;; calling the continuation after normal return is an error
... (funcall cc26 99)

%%% Escape continuation invoked outside its dynamic extent.
==>

; --- call/cc errors ---

>>> ;;; call/cc with no arguments
... (call/cc)

%%% ERROR 'CALL/CC': 1 argument expected.
%%% USAGE: (CALL/CC <callable>)
==>

>>> ;;; call/cc with too many arguments
... (call/cc (lambda (k) k) 2)

%%% ERROR 'CALL/CC': 1 argument expected.
%%% USAGE: (CALL/CC <callable>)
==>

>>> ;;; call/cc argument must be callable
... (call/cc 42)

%%% ERROR 'CALL/CC': Argument must be a callable.
%%% USAGE: (CALL/CC <callable>)
==>

; --- continuation argument errors ---

>>> ;;; continuation called with no arguments is an error
... (call/cc (lambda (k) (k)))

%%% Continuation expects exactly 1 argument.
==>

>>> ;;; continuation called with two arguments is an error
... (call/cc (lambda (k) (k 1 2)))

%%% Continuation expects exactly 1 argument.
==>

; --- call/cc via funcall and apply ---

>>> ;;; call/cc works when k is invoked via funcall
... (call/cc (lambda (k) (funcall k 77)))
...

==> 77

>>> ;;; call/cc works when k is invoked via apply
... (call/cc (lambda (k) (apply k '(88))))
...

==> 88

; --- nested call/cc ---

>>> ;;; inner k does not affect outer call/cc
... (call/cc
...    (lambda (outer)
...       (call/cc
...          (lambda (inner)
...             (inner 1)))
...       2))
...

==> 2

>>> ;;; outer k escapes both levels
... (call/cc
...    (lambda (outer)
...       (call/cc
...          (lambda (inner)
...             (outer 99)))
...       'not-reached))
...

==> 99

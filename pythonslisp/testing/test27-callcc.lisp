; test27-callcc.lisp — tests for call/cc (escape continuations)

; Basic escape: continuation invoked immediately
>>> (call/cc (lambda (k) (k 42)))
==> 42

; Lambda returns normally without invoking the continuation
>>> (call/cc (lambda (k) 99))
==> 99

; Continuation invoked inside nested arithmetic — outer + still uses escaped value
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

; call-with-current-continuation is an alias for call/cc
>>> (call-with-current-continuation (lambda (k) (k 99)))
==> 99

; Inner call/cc escape stays within the inner scope; outer computation proceeds normally
>>> (call/cc (lambda (k1) (+ 100 (call/cc (lambda (k2) (k2 5))))))
==> 105

; Inner call/cc escapes to the outer call/cc, bypassing the (+ 100 ...) entirely
>>> (+ 1000 (call/cc (lambda (k1) (+ 100 (call/cc (lambda (k2) (k1 5)))))))
==> 1005

; Early exit from foreach loop using call/cc
>>> (defun find-first (pred lst)
...    (call/cc (lambda (return)
...                (foreach x lst
...                   (if (pred x) (return x) nil))
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
%%% USAGE: (CALL/CC <procedure>)

; call/cc argument must be a callable
>>> (call/cc 42)
%%% ERROR 'CALL/CC': Argument must be a callable.
%%% USAGE: (CALL/CC <procedure>)

; call/cc argument may not be a special form — pass quote (unquoted) to get the primitive
>>> (call/cc quote)
%%% ERROR 'CALL/CC': Argument may not be a special form.
%%% USAGE: (CALL/CC <procedure>)

; Continuation requires exactly 1 argument
>>> (call/cc (lambda (k) (k 1 2)))
%%% Continuation expects exactly 1 argument, got 2.

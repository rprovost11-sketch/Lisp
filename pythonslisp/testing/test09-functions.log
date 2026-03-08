>>> ((lambda (x) (* 2 x)) 16)
...

==> 32

>>> ;;; Test Closures
... (defun adder (x)
...    (lambda (y) (+ x y)))
... 

==> (FUNCTION ADDER (X) ...)

>>> (setf add5 (adder 5))
... 

==> (FUNCTION ADD5 (Y) ...)

>>> (add5 8)
... 

==> 13

>>> (defun int-seq ()
...    (let ((i 0))
...       (lambda ()
...          (incf i))))
... 

==> (FUNCTION INT-SEQ () ...)

>>> (defun main ()
...    (let ((next-int (int-seq)))
...       (writeln! (next-int))
...       (writeln! (next-int))
...       (writeln! (next-int))))
... 

==> (FUNCTION MAIN () ...)

>>> (main)
... 
1
2
3

==> 3

>>> (defun make-counter ()
...    (let ((n 0))
...       (lambda () (incf n))))
... 

==> (FUNCTION MAKE-COUNTER () ...)

>>> (setf c1 (make-counter))
... 

==> (FUNCTION C1 () ...)

>>> (setf c2 (make-counter))
... 

==> (FUNCTION C2 () ...)

>>> (c1)
... 

==> 1

>>> (c1)
... 

==> 2

>>> (c1)
... 

==> 3

>>> (c2)
... 

==> 1

>>> (c2)
... 

==> 2

>>> (defun make-account (balance)
...    (let ((bal balance))
...       (list (lambda (amt) (setf bal (- bal amt)) bal)
...             (lambda () bal))))
... 

==> (FUNCTION MAKE-ACCOUNT (BALANCE) ...)

>>> (setf acct (make-account 100))
... 

==> ((FUNCTION  (AMT) ...) (FUNCTION  () ...))

>>> (setf withdraw (at 0 acct))
... 

==> (FUNCTION WITHDRAW (AMT) ...)

>>> (setf get-bal (at 1 acct))
... 

==> (FUNCTION GET-BAL () ...)

>>> (get-bal)
... 

==> 100

>>> (withdraw 30)
... 

==> 70

>>> (get-bal)
... 

==> 70

>>> (withdraw 20)
... 

==> 50

>>> (get-bal)
... 

==> 50

>>> (setf multiplier (let ((factor 3)) (lambda (x) (* factor x))))
... 

==> (FUNCTION MULTIPLIER (X) ...)

>>> (multiplier 5)
... 

==> 15

>>> (multiplier 7)
... 

==> 21

>>> (defun make-adder (x) (lambda (y) (lambda (z) (+ x y z))))
... 

==> (FUNCTION MAKE-ADDER (X) ...)

>>> (setf add1-2 ((make-adder 1) 2))
... 

==> (FUNCTION ADD1-2 (Z) ...)

>>> (add1-2 10)
... 

==> 13

>>> (add1-2 20)
... 

==> 23


>>> ;;; Error: first element of a list must be callable
... (1 2 3)

%%% Badly formed list expression '1'.  The first element should evaluate to a callable.
==>

>>> ;;; Error: eval requires exactly one argument
... (eval)

%%% ERROR 'EVAL': 1 argument expected.
%%% PRIMITIVE USAGE: (EVAL sexpr)
==>


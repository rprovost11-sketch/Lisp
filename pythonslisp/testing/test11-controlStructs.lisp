>>> (setf val1 3)
...

==> 3

>>> (cond
...    ((= val1 1)
...                 "one")
...    ((= val1 2)
...                 "two")
...    ((= val1 3)
...                 "three"))
...

==> "three"

>>> (case val1
...    (1
...        "one")
...    (2
...        "two")
...    (3
...        "three")
...    )
...

==> "three"

>>> (defun countdown1 (num)
...    (while (> num 0)
...       (let ()
...          (writeLn! num)
...          (setf num (- num 1))
...          )
...       )
...    )
...

==> (FUNCTION COUNTDOWN1 (NUM) ... )

>>> (countdown1 5)
...
5
4
3
2
1

==> 0

>>> (defun countdown2 (num)
...    (while (> num 0)
...       (let ()
...          (writeLn! num)
...          (setf num (- num 1))
...          )
...    )
... )
...

==> (FUNCTION COUNTDOWN2 (NUM) ... )

>>> (countdown2 3)
3
2
1

==> 0

>>> (foreach item '(0 1 2 3 4 5)
...    (writeLn! item))
...
0
1
2
3
4
5

==> 5

>>> (doTimes (i 3) (writeLn! i))
...
0
1
2

==> 2

>>> (let ((sum 0)) (doTimes (i 5) (setf sum (+ sum i))) sum)
...

==> 10

>>> (progn 1 2 3)
...

==> 3

>>> (let ((n 5)) (progn (setf n (* n n)) n))
...

==> 25

>>> (let* ((a 2) (b (* a 3))) b)
...

==> 6

>>> (let* ((x 10) (y (+ x 5))) y)
...

==> 15

>>> (eval '(+ 1 2))
...

==> 3

>>> (eval '(list 1 2 3))
...

==> (1 2 3)

>>> (parse "(+ 1 2)")
...

==> (PROGN (+ 1 2))

>>> (parse "42")
...

==> (PROGN 42)


>>> ;;; Error: cond requires at least one clause
... (cond)

%%% ERROR 'COND': 1 or more argument expected.
%%% USAGE: (COND (<cond1> <body1>) (<cond2> <body2>) ...)
==>

>>> ;;; Error: let requires at least 2 arguments
... (let)

%%% ERROR 'LET': 2 or more arguments expected.
%%% USAGE: (LET ( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <sexpr1> <sexpr2> ...))
==>


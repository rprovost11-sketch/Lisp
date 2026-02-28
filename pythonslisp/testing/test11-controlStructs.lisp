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

>>> (dolist (item '(0 1 2 3 4 5))
...    (writeLn! item))
...
0
1
2
3
4
5

==> NIL

; --- dolist with result form ---

; result form evaluated after loop; var = NIL
>>> (let ((total 0)) (dolist (x '(1 2 3) total) (setf total (+ total x))))
...

==> 6

; result form can be any expr (evaluated with var = NIL)
>>> (dolist (x '(a b c) 'done) (write! x))
...
ABC
==> DONE

>>> (dotimes (i 3) (writeLn! i))
...
0
1
2

==> NIL

>>> (let ((sum 0)) (dotimes (i 5) (setf sum (+ sum i))) sum)
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

%%% ERROR 'COND': 1 or more arguments expected.
%%% USAGE: (COND (cond1 body1) (cond2 body2) ...)
==>

>>> ;;; Error: let requires at least 2 arguments
... (let)

%%% ERROR 'LET': 2 or more arguments expected.
%%% USAGE: (LET ( (var1 sexpr1) (var2 sexpr2) ...) &rest body)
==>

; --- block and return-from ---

; block returns value of last body form
>>> (block myblock 1 2 3)
==> 3

; block with no body returns NIL
>>> (block myblock)
==> NIL

; return-from exits the block immediately, skipping remaining forms
>>> (block myblock
...    (return-from myblock 42)
...    99)
==> 42

; return-from with no value returns NIL
>>> (block myblock
...    (return-from myblock)
...    99)
==> NIL

; return-from from inside a nested expression
>>> (+ 1 (block escape
...         (+ 10 (return-from escape 5))))
==> 6

; nested blocks with same name innermost is exited
>>> (block foo
...    (block foo
...       (return-from foo 1))
...    2)
==> 2

; return-from to outer block bypasses the inner block entirely
>>> (block outer
...    (block inner
...       (return-from outer 99))
...    42)
==> 99

; block used as early-exit in a search loop
>>> (defun find-even (lst)
...    (block found
...       (dolist (x lst)
...          (if (evenp x) (return-from found x)))
...       nil))
==> (FUNCTION FIND-EVEN (LST) ... )

>>> (find-even '(1 3 5 4 7))
==> 4

>>> (find-even '(1 3 5 7))
==> NIL

; --- block nil and return ---

; (block nil ...) acts like a named block
>>> (block nil 1 2 3)
==> 3

; (return value) exits (block nil ...)
>>> (block nil
...    (return 42)
...    99)
==> 42

; (return) with no value returns NIL
>>> (block nil
...    (return)
...    99)
==> NIL

; dotimes with early (return ...)
>>> (let ((sum 0))
...    (dotimes (i 10)
...       (if (= i 5) (return sum))
...       (setf sum (+ sum i)))
...    sum)
...

==> 10

; dotimes with result form
>>> (let ((total 0)) (dotimes (i 5 total) (setf total (+ total i))))
...

==> 10

; dotimes result form - var = count when evaluated
>>> (dotimes (i 5 i) nil)
...

==> 5

; dolist with early (return ...)
>>> (dolist (x '(1 2 3 4 5))
...    (if (= x 3) (return x)))
...

==> 3

; Error: stale return-from (no matching block active)
>>> (return-from nowhere 42)
%%% return-from: no block named NOWHERE is currently active.

; Error: stale return (no block nil active)
>>> (return 42)
%%% return-from: no block named NIL is currently active.

; Error: block name must be a symbol (not an integer)
>>> (block 42 1 2 3)
%%% block: name must be a symbol.

; Error: return-from name must be a symbol
>>> (return-from 42 99)
%%% return-from: name must be a symbol.

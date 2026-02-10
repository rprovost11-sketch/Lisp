>>> ((lambda (x) (* 2 x)) 16)
...

==> 32

>>> ;;; Test Closures
... (defun adder (x)
...    (lambda (y) (+ x y)))
... 

==> (Function ADDER (X) ... )

>>> (setf add5 (adder 5))
... 

==> (Function ADD5 (Y) ... )

>>> (add5 8)
... 

==> 13

>>> (defun int-seq ()
...    (let ((i 0))
...       (lambda ()
...          (incf i))))
... 

==> (Function INT-SEQ NIL ... )

>>> (defun main ()
...    (let ((next-int (int-seq)))
...       (writeln! (next-int))
...       (writeln! (next-int))
...       (writeln! (next-int))))
... 

==> (Function MAIN NIL ... )

>>> (main)
... 
1
2
3

==> 3

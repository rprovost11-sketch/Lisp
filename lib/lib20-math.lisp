>>> (defun signum (num)
...    (if (zerop num)
...       num
...       (/ num (abs num))))
...
>>> (defun sqrt (num)
...    (expt num 1/2))
...
>>> (defun isqrt (num)
...    (int (sqrt num)))
...
>>> (defun abs (num)
...    (if (minusp num)
...       (- num)
...       num))
...
>>> (defun exp (num)
...    (expt e num))
...
>>> (defun ln (num)
...    (log num e))
...
>>> (defun tan (num)
...    (/ (sin num) (cos num)))
...
>>> (defun evenp (intVal)
...          (= (mod intVal 2) 0))
...
>>> (defun oddp (intVal)
...          (= (mod intVal 2) 1))
...
>>> (setf isEven? evenp)
...
>>> (setf isOdd? oddp)
...
>>> (defun minusp (numVal)
...    (< numVal 0))
...
>>> (defun plusp (numVal)
...    (> numVal 1))
...
>>> (defun zerop (numVal)
...    (= (int numVal) 0))
...
>>> (setf isPositive? plusp)
...
>>> (setf isNegative? minusp)
...
>>> (setf isZero? zerop)
...
>>> (defun apply (aFn aList)
...          (if (null aList)
...              '( )
...              (cons (aFn (first aList))
...                    (apply aFn (rest aList)))))
...
>>> ; Compute the factorial
... ;
... ; (fact n)
... (defun fact (n)
...          (if (= n 0)
...              1
...              (* n (fact (- n 1)))))
...
>>> ; Compute the Fibonacci number
... ;
... ; (fib n)
... (defun fib (n)
...          (if (<= n 2)
...              1
...              (+ (fib (- n 1))
...                 (fib (- n 2)))))
...
>>> (defun d (expr)
...          (cond ((isNumber? expr)
...                       0)
...                ((isSymbol? expr)
...                       1)
...                ((isList? expr)
...                       (case (first expr)
...                             ('+        (list '+   (d (at expr 1)) (d (at expr 2))))
...                             ('*        (list '+   (list '* (at expr 2) (d (at expr 1))) (list '* (at expr 1) (d (at expr 2)))))
...                             ('sin      (list 'cos (at expr 1)))
...                             ('cos      (list 'neg (list 'sin (at expr 1))))
...                             ('pow      (list '*   (list '* (at expr 2) (list 'pow (at expr 1) (- (at expr 2) 1))) (d (at expr 1))))
...                             ))))
...
>>> (defmacro incf (var &optional (delta 1))
...    `(setf ,var (+ ,var ,delta)))
...
>>> ;(defmacro decf (var &optional (delta 1))
... ;   `(setf ,var (- ,var ,delta)))
... ;

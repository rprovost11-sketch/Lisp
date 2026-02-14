;;; Increase the recursion limit to 10,000
(let ( (newLimit 10000) )
   (if (< (recursion-Limit) newLimit)
      (if (recursion-Limit newLimit)
         (writef "- Recursion limit increased to {0:,d}\n" (list newLimit))
         (write "- Failed to increase recursion limit."))
      (uwriteln! "- No need to adjust recursion limit.")))

; Compute the factorial
;
; (fact n)
(defun fact (n)
         (if (= n 0)
             1
             (* n (fact (- n 1)))))

; Compute the Fibonacci number
;
; (fib n)
(defun fib (n)
         (if (<= n 2)
             1
             (+ (fib (- n 1))
                (fib (- n 2)))))

(defun d (expr)
         (cond ((isNumber? expr)
                      0)
               ((isSymbol? expr)
                      1)
               ((isList? expr)
                      (case (first expr)
                            ('+        (list '+   (d (at expr 1)) (d (at expr 2))))
                            ('*        (list '+   (list '* (at expr 2) (d (at expr 1))) (list '* (at expr 1) (d (at expr 2)))))
                            ('sin      (list 'cos (at expr 1)))
                            ('cos      (list 'neg (list 'sin (at expr 1))))
                            ('pow      (list '*   (list '* (at expr 2) (list 'pow (at expr 1) (- (at expr 2) 1))) (d (at expr 1))))
                            ))))



;;; Increase the recursion limit to 10,000
(let ( (newLimit 10000) )
   (if (< (recursion-Limit) newLimit)
      (if (recursion-Limit newLimit)
         (writef "- Recursion limit increased to {0:,d}\n" (list newLimit))
         (writeln! "- Failed to increase recursion limit."))
      (uwriteln! "- No need to adjust recursion limit.")))

; Compute the factorial - recursion test
;
; (fact n)
(defun fact (n)
         (if (= n 0)
             1
             (* n (fact (- n 1)))))

; Compute the Fibonacci number - recursion test
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
                            ('+        (list '+   (d (at 1 expr)) (d (at 2 expr))))
                            ('*        (list '+   (list '* (at 2 expr) (d (at 1 expr))) (list '* (at 1 expr) (d (at 2 expr)))))
                            ('sin      (list 'cos (at 1 expr)))
                            ('cos      (list '-   (list 'sin (at 1 expr))))
                            ('expt     (list '*   (list '* (at 2 expr) (list 'pow (at 1 expr) (- (at 2 expr) 1))) (d (at 1 expr))))
                            ))))



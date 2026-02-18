;;; Increase the recursion limit to 10,000
(let ( (newLimit 10000) )
   (if (< (recursion-Limit) newLimit)
      (if (recursion-Limit newLimit)
         (writef "- Recursion limit increased to {0:,d}\n" (list newLimit))
         (writeln! "- Failed to increase recursion limit."))
      (uwriteln! "- No need to adjust recursion limit.")))

;;; Inform the user of Python Lisp's online help system
(uwriteln! "- For LOHS (lisp online help system) type \"(help)\" to begin." )

;;; Some function definitions for testing purposes
(defun fact (n)
         "Recursively compute the factorial of an arbitrary integer n.  For general testing of the interpreter."
         (if (= n 0)
             1
             (* n (fact (- n 1)))))

(defun fib (n)
         "Recursively compute and return the nth fibonacci number."
         (if (<= n 2)
             1
             (+ (fib (- n 1))
                (fib (- n 2)))))

(defun d (expr)
         "Compute the first derivative of an expression."
         (cond ((isNumber? expr)
                      0)
               ((isSymbol? expr)
                      1)
               ((isList? expr)
                      (case (first expr)
                            ('+        (list '+   (d (at 1 expr)) (d (at 2 expr))))
                            ('*        (list '+   (list '* (at 2 expr) (d (at 1 expr))) (list '* (at 1 expr) (d (at 2 expr)))))
                            ('sin      (list '* (list 'cos (at 1 expr)) (d (at 1 expr))))
                            ('cos      (list '* (list '-   (list 'sin (at 1 expr))) (d (at 1 expr))))
                            ('expt     (list '*   (list '* (at 2 expr) (list 'expt (at 1 expr) (- (at 2 expr) 1))) (d (at 1 expr))))
                            ))))

(defun run-me ()
   (let ( (ct 0) )
      (dotimes (i 1000000)
         (incf ct))))

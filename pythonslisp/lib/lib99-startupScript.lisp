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

(defun fact-tco (n &optional (accum 1))
   "TCO-safe recursive factorial."
   (if (= n 0)
      accum
      (fact-tco (- n 1) (* n accum))))

(defun fib (n)
   "Recursively compute and return the nth fibonacci number."
   (if (<= n 2)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(defun fib-tco (n &optional (a 0) (b 1))
   "TCO-safe recursive fibonacci."
   (if (= n 0)
      a
      (fib-tco (- n 1) b (+ a b))))

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

(defun testrd (filename)
   (let ( (st (open filename))
          (line "") )
      (setf line (readln! st))
      (while (/= line "")
         (write! line)
         (setf line (readln! st)))))

(defmacro with-open-file (spec &rest body)
   "Opens a file, binds it to var, evaluates body forms, then closes the file.
spec is (var filespec &rest open-options) where open-options are keyword args
passed directly to open.  Default direction is :input.
Returns the value of the last body form.
Note: file is closed normally; on error the file may remain open (no unwind-protect)."
   (let ((var      (car spec))
         (filespec (car (cdr spec)))
         (options  (cdr (cdr spec))))
      `(let ((,var (open ,filespec ,@options)))
          (let ((_wof_result_ (progn ,@body)))
             (when ,var (close ,var))
             _wof_result_))))

(defmacro with-output-to-string (var-spec &rest body)
   "Creates a string output stream, evaluates body forms with the first element
of var-spec bound to the stream, then returns the accumulated string content."
   (let ((var (car var-spec)))
      `(let ((,var (make-string-output-stream)))
          ,@body
          (get-output-stream-string ,var))))

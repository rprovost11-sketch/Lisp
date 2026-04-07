;;; Debug extension - macros

(defmacro time (form)
  "Evaluates form, prints elapsed real time to *trace-output*, returns the result.
Preserves multiple values."
  (let ((start (gensym))
        (results (gensym))
        (elapsed (gensym)))
    `(let ((,start (get-internal-real-time)))
       (let ((,results (multiple-value-list ,form)))
         (let ((,elapsed (- (get-internal-real-time) ,start)))
           (%time-report ,elapsed)
           (values-list ,results))))))

(defmacro trace-vars (&rest args)
  "Print the names and current values of the specified variables to
*trace-output*, one per line.  An optional leading string labels the output.
  (trace-vars x y z)
  (trace-vars \"checkpoint\" x y z)"
  (if (stringp (car args))
    `(%trace-vars ,(car args) ',(cdr args) (list ,@(cdr args)))
    `(%trace-vars nil ',args (list ,@args))))

(defmacro trace-eval (&rest exprs)
  "Print each expression and its evaluated result to *trace-output*.
  (trace-eval (+ x 1) (* y 2))"
  `(%trace-eval ',exprs (list ,@exprs)))

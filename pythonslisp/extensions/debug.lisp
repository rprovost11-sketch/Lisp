;;; Debug extension - macros

(defmacro time (form)
  "Evaluates form, prints elapsed real time and GC activity to *trace-output*.
Returns the result.  Preserves multiple values."
  (let ((gc-before (gensym))
        (start (gensym))
        (results (gensym))
        (elapsed (gensym)))
    `(let ((,gc-before (%time-setup))
           (,start (get-internal-real-time)))
       (let ((,results (multiple-value-list ,form)))
         (let ((,elapsed (- (get-internal-real-time) ,start)))
           (%time-report ,elapsed ,gc-before)
           (values-list ,results))))))

(defmacro benchmark (n form)
  "Evaluates FORM N times and prints timing statistics (total, avg, min, max)
to *trace-output*.  Returns the result of the last evaluation.
  (benchmark 10000 (fibonacci 20))"
  (let ((i (gensym))
        (start (gensym))
        (elapsed (gensym))
        (total (gensym))
        (min-t (gensym))
        (max-t (gensym))
        (result (gensym)))
    `(let ((,total 0) (,min-t 999999999999) (,max-t 0) (,result nil))
       (dotimes (,i ,n)
         (let ((,start (get-internal-real-time)))
           (setf ,result ,form)
           (let ((,elapsed (- (get-internal-real-time) ,start)))
             (setf ,total (+ ,total ,elapsed))
             (when (< ,elapsed ,min-t) (setf ,min-t ,elapsed))
             (when (> ,elapsed ,max-t) (setf ,max-t ,elapsed)))))
       (%benchmark-report ,n ,total ,min-t ,max-t)
       ,result)))

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

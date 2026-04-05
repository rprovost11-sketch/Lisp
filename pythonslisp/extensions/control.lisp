;;; Control flow macros

(defmacro when (condition &rest body)
   "Executes body if condition is truthy (non-nil)."
   `(if ,condition (progn ,@body)))

(defmacro unless (condition &rest body)
   "Executes body if condition is falsy (nil)."
   `(when (not ,condition) ,@body))

(defmacro unwind-protect (protected &rest cleanup)
   "Evaluate protected; run cleanup forms on any exit (normal or non-local).
cleanup always runs, even if protected exits via throw, return-from, or error."
   `(dynamic-wind
      (lambda () nil)
      (lambda () ,protected)
      (lambda () ,@(if cleanup cleanup '(nil)))))

;;; Iteration macros

(defmacro while (cond &rest body)
   "Loops while cond is truthy, executing body each iteration.
Returns the last body value from the last iteration, or NIL if the loop never runs."
   (if (not body)
       (error "while: at least one body expression is required")
       (let ((fn  (gensym "WHILE"))
             (res (gensym "RESULT")))
          `(let ((,fn nil) (,res nil))
               (setq ,fn (lambda ()
                            (if ,cond
                                (progn (setq ,res (progn ,@body)) (,fn))
                                ,res)))
               (,fn)))))

(defmacro dotimes ((var count &optional (result nil)) &body body)
   "Executes body count times with the loop variable bound to 0, 1, ..., count-1.
Returns result (default NIL) after the loop; var = count when result is evaluated.
Supports (return value) for early exit."
   (if (not (symbolp var))
       (error "dotimes: loop variable must be a symbol")
       (let ((fn  (gensym "DOTIMES"))
             (cnt (gensym "COUNT")))
          `(block nil
               (let ((,cnt ,count) (,fn nil) (,var 0))
                   (if (not (integerp ,cnt))
                       (error "dotimes: count must be an integer")
                       (progn
                          (setq ,fn (lambda ()
                                       (if (< ,var ,cnt)
                                           (progn ,@body
                                                  (setq ,var (+ ,var 1))
                                                  (,fn))
                                           nil)))
                          (,fn)))
                   ,result)))))

(defmacro dolist ((var list-expr &optional (result nil)) &body body)
   "Iterates over each element of a list, binding the control variable to each element.
Returns result (default NIL) after the loop; var = NIL when result is evaluated.
Supports (return value) for early exit."
   (if (not (symbolp var))
       (error "dolist: control spec variable must be a symbol")
       (let ((fn  (gensym "DOLIST"))
             (rem (gensym "REM")))
          `(block nil
               (let ((,rem ,list-expr) (,fn nil) (,var nil))
                   (if (not (listp ,rem))
                       (error "dolist: list must evaluate to a list")
                       (progn
                          (setq ,fn (lambda ()
                                       (if ,rem
                                           (progn (setq ,var (car ,rem))
                                                  (setq ,rem (cdr ,rem))
                                                  ,@body
                                                  (,fn))
                                           nil)))
                          (,fn)))
                   (setq ,var nil)
                   ,result)))))

(defmacro for ((var init) cond nextForm &body body)
   "General-purpose for loop.
Syntax: (for (variable initForm) cond nextForm body+)

  variable -- loop variable, bound to initForm before the first iteration.
  initForm -- evaluated once to produce the initial value of variable.
  cond     -- evaluated before each iteration; the body runs while non-NIL.
  nextForm -- evaluated at the end of each iteration; its value replaces
              variable for the next iteration.  nextForm may reference variable.
  body     -- one or more forms evaluated each iteration.

Returns NIL when the loop ends normally.  Supports (return value) for early
exit.  variable is local to the loop and does not affect any outer binding.

Examples:
  Count from 0 to 4:
    (for (i 0) (< i 5) (+ i 1) (process i))
  Read a file line by line until EOF:
    (for (line (read-line f nil nil)) line (read-line f nil nil)
      (process-line line))"
   (cond
      ((not (symbolp var))
       (error "for: loop variable must be a symbol"))
      ((not body)
       (error "for: at least one body expression is required"))
      (t
       (let ((fn (gensym "FOR")))
          `(block nil
               (let ((,var ,init) (,fn nil))
                   (setq ,fn (lambda ()
                                (if ,cond
                                    (progn ,@body
                                           (setq ,var ,nextForm)
                                           (,fn))
                                    nil)))
                   (,fn)))))))

;;; Type dispatch macros

(defmacro typecase (keyform &rest clauses)
   "Evaluate keyform once and execute the body of the first clause whose
type specifier matches.  Each clause is (type-spec form*).  Use T or
OTHERWISE as a catch-all.  Returns NIL if no clause matches."
   (let ((var (gensym "TC")))
      `(let ((,var ,keyform))
         (cond ,@(mapcar (lambda (clause)
                           (let ((spec (first clause))
                                 (body (rest  clause)))
                              (cond ((eq spec 'otherwise) `(t ,@body))
                                    ((eq spec 't)         `(t ,@body))
                                    (t                    `((typep ,var ',spec) ,@body)))))
                         clauses)))))

(defmacro etypecase (keyform &rest clauses)
   "Like typecase but signals an error if no clause matches."
   (let ((var (gensym "TC")))
      `(let ((,var ,keyform))
         (cond ,@(mapcar (lambda (clause)
                           (let ((spec (first clause))
                                 (body (rest  clause)))
                              (cond ((eq spec 'otherwise) `(t ,@body))
                                    ((eq spec 't)         `(t ,@body))
                                    (t                    `((typep ,var ',spec) ,@body)))))
                         clauses)
               (t (error (ustring "etypecase: no matching clause for value: " ,var)))))))

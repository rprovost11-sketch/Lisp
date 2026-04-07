;;; Condition system convenience macros

(defmacro ignore-errors (&rest body)
   "Evaluates body forms.  If any error or condition is signaled, returns NIL.
Returns the value of the last body form if no error occurs."
   `(handler-case (progn ,@body)
      (t (_ign_e_) nil)))

(defmacro assert (test-form)
   "Signals an error if TEST-FORM evaluates to NIL.
The error message includes the text of the failing expression."
   (let ((msg (ustring "Assertion failed: " test-form)))
      `(when (not ,test-form)
          (error ,msg))))

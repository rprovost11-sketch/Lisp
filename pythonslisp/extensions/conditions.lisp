;;; Condition system convenience macros

(defmacro ignore-errors (&rest body)
   "Evaluates body forms.  If any error or condition is signaled, returns NIL.
Returns the value of the last body form if no error occurs."
   `(handler-case (progn ,@body)
      (t (_ign_e_) nil)))

;;; String accessors

(defmacro char (str idx)
   "Returns the character (as a single-character string) at position idx in str."
   `(at ,idx ,str))

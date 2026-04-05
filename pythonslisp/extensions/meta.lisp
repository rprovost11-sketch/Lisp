;;; Standard CL global variables
(setq *gensym-counter* 0)

(defmacro defparameter (name val)
   "Defines a global variable and always sets its value."
   `(setf ,name ,val))

(defmacro defvar (name &optional val)
   "Defines a global variable, setting it to val only if it is not already bound."
   `(progn (unless (boundp ',name) (setf ,name ,val)) nil))


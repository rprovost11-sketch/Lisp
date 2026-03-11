; module-fixture.lisp — loaded by test31-modules.log via load-module.
; Defines a small library for use in module system tests.

(setf version "1.0")

(defun greet (name)
   "Returns a greeting string."
   (ustring "Hello, " name "!"))

(defun square (n)
   "Returns n squared."
   (* n n))

(defun add (a b)
   "Returns the sum of a and b."
   (+ a b))

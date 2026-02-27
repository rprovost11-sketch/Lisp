(defun signum (num)
   "Returns 0 if the argument is zero, 1 if the argument is positive, or -1 if the argument is negative."
   (cond ((zerop num)   0)
         ((> num 0)     1)
         (1            -1)))

(defun sqrt (num)
   "Returns the square root of the argument."
   (expt num 1/2))

(defun isqrt (num)
   "Returns the integer square root of the argument."
   (integer (sqrt num)))

(defun abs (num)
   "Returns the absolute value of the argument."
   (if (>= num 0)
      num
      (- num)))

(defun exp (num)
   "Returns e raised to a number."
   (expt e num))

(defun ln (num)
   "Returns the natural logarithm of a number."
   (log num e))

(defun tan (num)
   "Returns the tangent of a number is radians."
   (/ (sin num) (cos num)))

(defun evenp (intVal)
   "Returns t if the argument is even otherwise nil."
         (= (mod intVal 2) 0))

(defun oddp (intVal)
   "Returns t if the argument is odd otherwise nil."
         (= (mod intVal 2) 1))

(alias isEven? evenp)

(alias isOdd? oddp)

(defun minusp (numVal)
   "Returns t if the argument is negative otherwise nil."
   (< numVal 0))

(defun plusp (numVal)
   "Returns t if the argument is positive otherwise nil."
   (> numVal 0))

(defun zerop (numVal)
   "Returns t if the number is very close to zero (within 1e-15) otherwise nil."
   (< (abs numVal) 1e-15))

(alias isPositive? plusp)

(alias isNegative? minusp)

(alias isZero? zerop)

(defmacro incf (var &optional (delta 1))
   "Increase the variable argument's value by 1 or delta."
   `(setf ,var (+ ,var ,delta)))

(defmacro decf (var &optional (delta 1))
   "Decrease the variable argument's value by 1 or delta."
   `(setf ,var (- ,var ,delta)))

(defun average (&rest values)
   "Compute and return the average of a list of number values."
   (let ( (accum  0) )
      (dolist (value values)
         (incf accum value))
      (/ accum (length values))))

(defmacro truncate (x)
   "Truncates a number toward zero, returning an integer. Equivalent to (integer x)."
   `(integer ,x))



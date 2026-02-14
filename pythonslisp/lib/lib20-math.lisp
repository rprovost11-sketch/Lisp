(defun signum (num)
   (if (zerop num)
      0
      (/ num (abs num))))

(defun sqrt (num)
   (expt num 1/2))

(defun isqrt (num)
   (integer (sqrt num)))

(defun abs (num)
   (if (>= num 0)
      num
      (- num)))

(defun exp (num)
   (expt e num))

(defun ln (num)
   (log num e))

(defun tan (num)
   (/ (sin num) (cos num)))

(defun evenp (intVal)
         (= (mod intVal 2) 0))

(defun oddp (intVal)
         (= (mod intVal 2) 1))

(alias isEven? evenp)

(alias isOdd? oddp)

(defun minusp (numVal)
   (< numVal 0))

(defun plusp (numVal)
   (> numVal 0))

(defun zerop (numVal)
   (< (abs numVal) 1e-15))

(alias isPositive? plusp)

(alias isNegative? minusp)

(alias isZero? zerop)

(defmacro incf (var &optional (delta 1))
   `(setf ,var (+ ,var ,delta)))

(defmacro decf (var &optional (delta 1))
   `(setf ,var (- ,var ,delta)))

; Compute the average of a list of values
(defun average (&rest values)
   (let ( (accum  0) )
      (foreach value values
         (incf accum value))
      (/ accum (list-length values))))



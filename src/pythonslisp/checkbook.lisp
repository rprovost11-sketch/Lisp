(defun make-register ()
   (setf __checkregister '()))

(defun register-add (date to for amount)
   (push! __checkregister (list date to for amount)))

(defun register-delete (transaction-num)
   (at-delete transaction-num __checkregister))

(defun register-list ()
   (let ( (running-balance 0.00)
          (trans-num       0   ) )
      (foreach register-entry __checkregister
          (incf running-balance (at 3 register-entry))
          (writef "{tnum:4d} {date:10s} {to:30s} ${amt:10.2f}  ${bal:10.2f}\n                {for:30s}\n"
                  (map ("tnum" trans-num)
                       ("date" (at 0 register-entry))
                       ("to"   (at 1 register-entry))
                       ("for"  (at 2 register-entry))
                       ("amt"  (at 3 register-entry))
                       ("bal"  running-balance)))
          (incf trans-num))
      running-balance))


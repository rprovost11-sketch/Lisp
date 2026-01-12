(defun make-register ()
   (setf __checkregister '()))

(defun register-add-entry (date to for chkNum amount)
   (let ( (the-new-entry   (list date to for chkNum amount)) )
      (push! __checkregister the-new-entry)
      the-new-entry))

(defun register-delete-entry (transaction-num)
   (at-delete (decf transaction-num) __checkregister))

(defun register-list ()
   (let ( (running-balance 0.00)
          (trans-num       1   ) )
      (writef "{0:4s}  {1:8s} {2:30s} {3:4s} {4:10s}   {5:10s}\n" '("TNum" "Date" "To/For" "CkNo" "Amount" "Balance"))
      (foreach register-entry __checkregister
          (incf running-balance (at 4 register-entry))
          (writef "{tnum:4d}  {date:8s} {to:30s} {ckno:4s} ${amt:10.2f}  ${bal:10.2f}\n               {for:30s}\n"
                  (map ("tnum" trans-num)
                       ("date" (at 0 register-entry))
                       ("to"   (at 1 register-entry))
                       ("for"  (at 2 register-entry))
                       ("ckno" (at 3 register-entry))
                       ("amt"  (at 4 register-entry))
                       ("bal"  running-balance)))
          (incf trans-num))
      running-balance))


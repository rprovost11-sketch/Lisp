(defstruct register-entry
   date
   to
   for
   chknum
   amount)

(defun make-register ()
   (setf __checkregister '()))

(defun register-add-entry (date to for chknum amount)
   (let ((entry (make-register-entry :date   date
                                     :to     to
                                     :for    for
                                     :chknum chknum
                                     :amount amount)))
      (push! __checkregister entry)
      entry))

(defun register-delete-entry (transaction-num)
   (at-delete (decf transaction-num) __checkregister))

(defun register-list ()
   (let ((running-balance 0.00)
         (trans-num 1))
      (writef "{0:4s}  {1:8s} {2:30s} {3:4s} {4:10s}   {5:10s}\n"
              '("TNum" "Date" "To/For" "CkNo" "Amount" "Balance"))
      (foreach entry __checkregister
         (incf running-balance (register-entry-amount entry))
         (writef "{tnum:4d}  {date:8s} {to:30s} {ckno:4s} ${amt:10.2f}  ${bal:10.2f}\n               {for:30s}\n"
                 (map ("tnum" trans-num)
                      ("date" (register-entry-date   entry))
                      ("to"   (register-entry-to     entry))
                      ("for"  (register-entry-for    entry))
                      ("ckno" (register-entry-chknum entry))
                      ("amt"  (register-entry-amount entry))
                      ("bal"  running-balance)))
         (incf trans-num))
      running-balance))

;;; List accessors

(defun nth (index lst)
   "Return the nth item of a list; 0 based."
   (at index lst))

(defmacro first (lst)
   "Return the first item of a list."
   `(car ,lst))

(defmacro second (lst)
   "Return the second item of a list."
   `(cadr ,lst))

(defmacro third (lst)
   "Return the third item in a list."
   `(caddr ,lst))

(defmacro fourth (lst)
   "Return the fourth item of a list."
   `(cadddr ,lst))

(defmacro fifth (lst)
   "Return the fifth item of a list."
   `(cadddr (cdr ,lst)))

(defmacro sixth (lst)
   "Return the sixth item of a list."
   `(cadddr (cddr ,lst)))

(defmacro seventh (lst)
   "Return the seventh item of a list."
   `(cadddr (cdddr ,lst)))

(defmacro eighth (lst)
   "Return the eighth item of a list."
   `(cadddr (cddddr ,lst)))

(defmacro ninth (lst)
   "Return the ninth item of a list."
   `(cadddr (cddddr (cdr ,lst))))

(defmacro tenth (lst)
   "Return the tenth item of a list."
   `(cadddr (cddddr (cddr ,lst))))

(alias rest cdr)

;;; c[ad]+r accessors

(defun caar (lst)
   "Return the caar of a list."
   (car (car lst)))

(defun cadr (lst)
   "Return the cadr of a list."
   (car (cdr lst)))

(defun cdar (lst)
   "Return the cdar of a list."
   (cdr (car lst)))

(defun cddr (lst)
   "Return the cddr of a list."
   (cdr (cdr lst)))

(defun caaar (lst)
   "Return the caaar of a list."
   (car (car (car lst))))

(defun caadr (lst)
   "Return the caadr of a list."
   (car (car (cdr lst))))

(defun cadar (lst)
   "Return the cadar of a list."
   (car (cdr (car lst))))

(defun caddr (lst)
   "Return the caddr of a list."
   (car (cdr (cdr lst))))

(defun cdaar (lst)
   "Return the cdaar of a list."
   (cdr (car (car lst))))

(defun cdadr (lst)
   "Return the cdadr of a list."
   (cdr (car (cdr lst))))

(defun cddar (lst)
   "Return the cddar of a list."
   (cdr (cdr (car lst))))

(defun cdddr (lst)
   "Return the cdddr of a list."
   (cdr (cdr (cdr lst))))

(defun caaaar (lst)
   "Return the caaaar of a list."
   (car (car (car (car lst)))))

(defun caaadr (lst)
   "Return the caadr of a list."
   (car (car (car (cdr lst)))))

(defun caadar (lst)
   "Return the caadar of a list."
   (car (car (cdr (car lst)))))

(defun caaddr (lst)
   "Return the caaddr of a list."
   (car (car (cdr (cdr lst)))))

(defun cadaar (lst)
   "Return the cadaar of a list."
   (car (cdr (car (car lst)))))

(defun cadadr (lst)
   "Return the cadadr of a list."
   (car (cdr (car (cdr lst)))))

(defun caddar (lst)
   "Return the caddar of a list."
   (car (cdr (cdr (car lst)))))

(defun cadddr (lst)
   "Return the cadddr of a list."
   (car (cdr (cdr (cdr lst)))))

(defun cdaaar (lst)
   "Return the cdaaar of a list."
   (cdr (car (car (car lst)))))

(defun cdaadr (lst)
   "Return the cdaadr of a list."
   (cdr (car (car (cdr lst)))))

(defun cdadar (lst)
   "Return the cdadar of a list."
   (cdr (car (cdr (car lst)))))

(defun cdaddr (lst)
   "Return the cdaddr of a list."
   (cdr (car (cdr (cdr lst)))))

(defun cddaar (lst)
   "Return cddaar of a list."
   (cdr (cdr (car (car lst)))))

(defun cddadr (lst)
   "Return the cddadr of a list."
   (cdr (cdr (car (cdr lst)))))

(defun cdddar (lst)
   "Return the cdddar of a list."
   (cdr (cdr (cdr (car lst)))))

(defun cddddr (lst)
   "Return the cddddr of a list."
   (cdr (cdr (cdr (cdr lst)))))

;;; List construction and copying

(defun list (&rest lst)
   "Return the arguments in a list maintaining order."
   lst)

(defun reverse (lst)
   "Return a copy of a list in reverse order."
   (reverse-aux (list) lst))

(defun reverse-aux (destLst srcLst)
   (if (null srcLst)
       destLst
       (reverse-aux (cons (first srcLst) destLst) (rest srcLst))))

(defun copy-list (lst)
   "Return a shallow copy of a list."
   (reverse (reverse lst)))

(defun copy-tree (expr)
   "Return a deep copy of a list."
   (cond ((null   expr)  '( ))
         ((isAtom?   expr)  expr)
         ((isString? expr)  expr)
         ((isList?   expr)  (cons (copy-tree (first expr))
                                  (copy-tree (rest  expr))))))

;;; List navigation

(defun last (lst &optional (n 1))
   "Returns the last n cons cells of lst."
   (let ((skip (- (length lst) n)))
      (nthcdr (if (> skip 0) skip 0) lst)))

(defun nthcdr (n lst)
   "Returns the result of calling cdr n times on lst."
   (if (= n 0)
       lst
       (nthcdr (- n 1) (cdr lst))))

(defun butlast (lst &optional (n 1))
   "Returns a copy of lst with the last n elements removed."
   (if (< (length lst) (+ n 1))
       nil
       (cons (first lst) (butlast (rest lst) n))))

;;; Higher-order sequence operations

(defun reduce (fn lst)
   "Cumulatively apply fn to lst elements left to right, reducing to a single value."
   (if (null lst)
       (error "reduce: list must not be empty")
       (let ((acc (first lst)))
          (dolist (item (rest lst))
             (setf acc (funcall fn acc item)))
          acc)))

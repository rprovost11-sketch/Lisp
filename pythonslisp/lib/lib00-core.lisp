(defmacro defun (fnName lambda-list &rest body)
   "Define and return a new globally named function.  The first expr in the body can be an optional documentation string."
   `(setf ,fnName (lambda (,@lambda-list) ,@body)))

(defmacro alias (new old)
   "Define an alias for an existing named object."
   `(setf ,new ,old))

; Prompt the user for input on the command line.
(defun read_prompt (promptStr)
   "Prompt the user for input and return the user input as a string."
            (write! promptStr)
            (readLn!)          )

(defun nth (index lst)
   "Return the nth item of a list; 0 based."
   (at index lst))

(defmacro first (lst)
   "Return the first item of a list."
   `(car ,lst))

(setf rest cdr)

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

(defun list (&rest lst)
   "Return the arguments in a list maintaining order."
   lst)

(defun remove (sym lst)
   "Remove a symbol from a list."
         (cond
            ((null lst)           nil)
            ((= sym (first lst))  (rest lst))
            (1                    (cons (first lst) (remove sym (rest lst))))))

(defun list-length (lst)
   "Compute the length of a list."
         (if (null lst)
             0
             (+ 1 (list-length (rest lst)))))

(defun reverse (lst)
   "Return a copy of a list in reverse order."
         (reverse-aux (list) lst))

(defun reverse-aux (destLst srcLst)
         (if (null srcLst)
             destLst
             (reverse-aux (cons (first srcLst) destLst) (rest srcLst))))

(defun copy (lst)
   "Return a shallow copy of a list."
         (reverse (reverse lst)))

(defun deepCopy (expr)
   "Return a deep copy of a list."
         (cond ((null   expr)  '( ))
                ((isAtom?   expr)  expr)
                ((isString? expr)  expr)
                ((isList?   expr)  (cons (deepcopy (first expr))
                                         (deepcopy (rest  expr)))) ))

(defun dig (aTree aPath)
   "Given a nested structure of lists and maps, this function will execute\na depth-first traversal down aPath - a list of map keys and list indicies.\nReturns the object in that location."
         (if (null aPath)
             aTree
             (dig (at aTree (first aPath)) (rest aPath)) ))

(defun tree-equal (expr1 expr2)
   "Compare two lists for deep equality."
         (cond ((or (atom expr1)
                    (null expr1))
                                      (= expr1 expr2))
                   ((and (listp expr1)
                         (listp expr2))
                                 (and (listp expr2)
                                      (and (equal? (first expr1) (first expr2))
                                           (equal? (rest expr1) (rest expr2)))))
                   (1
                                 nil)))

(let ( (symbolCounter 1) )
   (defun gensym (&optional (prefix "G"))
      "Generate a unique symbol with a prefix of 'G' or other value specified by\nthe user and affixing a counting-number."
      (let
         ((theSymbol (symbol prefix symbolCounter)))
         (incf symbolCounter)
         theSymbol)))

(defmacro when (condition &rest body)
   "Executes body if c is truthy (non-nil)."
   `(if ,condition (progn ,@body)))

(defmacro unless (condition &rest body)
   "Executes body if c is falsy (nil)."
   `(when (not ,condition) ,@body))

(defun mapcar (fn lst)
   "Apply fn to each element of lst and return the list of results."
   (let ((result nil))
      (foreach item lst
         (setf result (append result (list (fn item)))))
      result))


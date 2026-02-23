(defmacro defun (fnName lambda-list &rest body)
   "Define and return a new globally named function.  The first expr in the body can be an optional documentation string."
   `(setf ,fnName (lambda (,@lambda-list) ,@body)))

(defmacro alias (new old)
   "Define an alias for an existing named object."
   `(setf ,new ,old))

(defmacro setf (place value &rest more)
   "Generalized assignment.
(setf sym val)               -> setq  (handles lambda auto-naming)
(setf (at key coll) val)     -> at-set  (mutates list or map in place)
(setf (accessor inst) val)   -> set-accessor!  (struct field via registry)
Multiple (place value) pairs expand to a progn of individual setfs."
   (let ((single
          (cond
             ((symbolp place)
              `(setq ,place ,value))
             ((not place)
              (error "setf: lvalue cannot be NIL or ()"))
             ((listp place)
              (cond
                 ((= (car place) 'at)
                  (if (/= (length place) 3)
                      (error "setf: (at ...) place form expected 3 elements")
                      `(at-set ,(car (cdr place)) ,(car (cdr (cdr place))) ,value)))
                 ((/= (length place) 2)
                  (error "setf: struct accessor place must have exactly 1 instance argument"))
                 (t
                  `(set-accessor! ',(car place) ,(car (cdr place)) ,value))))
             (t
              (error "setf: unrecognized place form")))))
      (if more
          `(progn ,single (setf ,@more))
          single)))

; Prompt the user for input on the command line.
(defun read-prompt (promptStr)
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
            ((= sym (first lst))  (remove sym (rest lst)))
            (1                    (cons (first lst) (remove sym (rest lst))))))


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
                                         (copy-tree (rest  expr)))) ))

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
         (setq symbolCounter (+ symbolCounter 1))
         theSymbol)))

(defmacro when (condition &rest body)
   "Executes body if c is truthy (non-nil)."
   `(if ,condition (progn ,@body)))

(defmacro unless (condition &rest body)
   "Executes body if c is falsy (nil)."
   `(when (not ,condition) ,@body))

(defmacro while (cond &rest body)
   "Loops while cond is truthy, executing body each iteration.
Returns the last body value from the last iteration, or NIL if the loop never runs."
   (if (not body)
       (error "while: at least one body expression is required")
       (let ((fn  (gensym "WHILE"))
             (res (gensym "RESULT")))
          `(let ((,fn nil) (,res nil))
               (setq ,fn (lambda ()
                            (if ,cond
                                (progn (setq ,res (progn ,@body)) (,fn))
                                ,res)))
               (,fn)))))

(defmacro dotimes (control &rest body)
   "Executes body count times with the loop variable bound to 0, 1, ..., count-1.
Returns the last body value from the last iteration, or NIL if count <= 0."
   (cond
      ((not (listp control))
       (error "dotimes: first argument must be a (var count) list"))
      ((/= (length control) 2)
       (error "dotimes: first argument must have exactly 2 elements"))
      ((not (symbolp (car control)))
       (error "dotimes: loop variable must be a symbol"))
      ((not body)
       (error "dotimes: at least one body expression is required"))
      (t
       (let ((fn  (gensym "DOTIMES"))
             (cnt (gensym "COUNT"))
             (res (gensym "RESULT"))
             (var (car control))
             (count-expr (car (cdr control))))
          `(let ((,cnt ,count-expr) (,fn nil) (,res nil))
               (if (not (integerp ,cnt))
                   (error "dotimes: count must be an integer")
                   (progn
                      (setq ,fn (lambda (,var)
                                   (if (< ,var ,cnt)
                                       (progn (setq ,res (progn ,@body))
                                              (,fn (+ ,var 1)))
                                       ,res)))
                      (,fn 0))))))))

(defmacro foreach (var list-expr &rest body)
   "Iterates over each element of list-expr, binding var to the current element.
Returns the last body value from the last iteration, or NIL if the list is empty."
   (cond
      ((not (symbolp var))
       (error "foreach: first argument must be a symbol"))
      ((not body)
       (error "foreach: at least one body expression is required"))
      (t
       (let ((fn  (gensym "FOREACH"))
             (lst (gensym "LST"))
             (rem (gensym "REM"))
             (res (gensym "RESULT")))
          `(let ((,lst ,list-expr) (,fn nil) (,res nil))
               (if (not (listp ,lst))
                   (error "foreach: second argument must evaluate to a list")
                   (progn
                      (setq ,fn (lambda (,rem)
                                   (if ,rem
                                       (let ((,var (car ,rem)))
                                          (setq ,res (progn ,@body))
                                          (,fn (cdr ,rem)))
                                       ,res)))
                      (,fn ,lst))))))))

(defmacro dolist (control &rest body)
   "Iterates over each element of a list, binding the control variable to the current element.
Returns the last body value from the last iteration, or NIL if the list is empty or body is absent."
   (cond
      ((not (listp control))
       (error "dolist: first argument must be a (variable list) control spec"))
      ((/= (length control) 2)
       (error "dolist: control spec must have exactly 2 elements"))
      ((not (symbolp (car control)))
       (error "dolist: control spec variable must be a symbol"))
      (t
       (let ((fn   (gensym "DOLIST"))
             (lst  (gensym "LST"))
             (rem  (gensym "REM"))
             (res  (gensym "RESULT"))
             (var  (car control))
             (list-expr (car (cdr control))))
          (if body
              `(let ((,lst ,list-expr) (,fn nil) (,res nil))
                   (if (not (listp ,lst))
                       (error "dolist: list must evaluate to a list")
                       (progn
                          (setq ,fn (lambda (,rem)
                                       (if ,rem
                                           (let ((,var (car ,rem)))
                                              (setq ,res (progn ,@body))
                                              (,fn (cdr ,rem)))
                                           ,res)))
                          (,fn ,lst))))
              `(if (not (listp ,list-expr))
                   (error "dolist: list must evaluate to a list")
                   nil))))))

(defun mapcar (fn lst)
   "Apply fn to each element of lst and return the list of results."
   (let ((result nil))
      (dolist (item lst)
         (setf result (append result (list (fn item)))))
      result))

(defun last (lst)
   "Returns a list containing only the last element of lst."
   (cond ((null lst)       nil)
         ((null (cdr lst)) lst)
         (1                (last (cdr lst)))))

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

(defun member (item lst)
   "Returns the tail of lst starting at the first occurrence of item, or NIL."
   (cond ((null lst)            nil)
         ((= item (first lst))  lst)
         (1                     (member item (rest lst)))))

(defun assoc (key alist)
   "Returns the first pair in alist whose car equals key, or NIL."
   (cond ((null alist)                   nil)
         ((= key (first (first alist)))  (first alist))
         (1                              (assoc key (rest alist)))))

(defun every (fn lst)
   "Returns T if fn returns true for every element of lst, NIL otherwise."
   (cond ((null lst)              t)
         ((null (fn (first lst))) nil)
         (1                       (every fn (rest lst)))))

(defun some (fn lst)
   "Returns T if fn returns true for at least one element of lst, NIL otherwise."
   (cond ((null lst)         nil)
         ((fn (first lst))   t)
         (1                  (some fn (rest lst)))))

(defun reduce (fn lst)
   "Cumulatively apply fn to lst elements left to right, reducing to a single value."
   (if (null lst)
       (error "reduce: list must not be empty")
       (let ((acc (first lst)))
          (dolist (item (rest lst))
             (setf acc (fn acc item)))
          acc)))

(defun mapc (fn lst)
   "Apply fn to each element of lst for side effects. Returns lst."
   (dolist (item lst)
      (fn item))
   lst)

(alias call-with-current-continuation call/cc)


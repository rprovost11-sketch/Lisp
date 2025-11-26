>>> (defmacro defun (fnName argList &rest body)
...                    `(setf ,fnName (lambda (,@argList) ,@body)))
...
>>> ;(defmacro foreach (sym lst expr)
... ;         `(cond
... ;             ((= (length ',lst) 0)  '())
... ;             ((= (length ',lst) 1)   (setf ,sym (first ',lst))
... ;                                     (eval ,expr))
... ;             (t                      (setf ,sym (first ',lst))
... ;                                     (eval ,expr)
... ;                                     (foreach ,sym (rest ',lst) ,expr))
... ;                 ))
... ;
>>> ; Prompt the user for input on the command line.
... ;
... (defun read_prompt (promptStr)
...             (write! promptStr)
...             (readLn!)          )
...
>>> (setf first car)
...
>>> (setf rest cdr)
...
>>> (defun caar (lst)
...    (car (car lst)))
...
>>> (defun cadr (lst)
...    (car (cdr lst)))
...
>>> (defun cdar (lst)
...    (cdr (car lst)))
...
>>> (defun cddr (lst)
...    (cdr (cdr lst)))
...
>>> (defun caaar (lst)
...    (car (car (car lst))))
...
>>> (defun caadr (lst)
...    (car (car (cdr lst))))
...
>>> (defun cadar (lst)
...    (car (cdr (car lst))))
...
>>> (defun caddr (lst)
...    (car (cdr (cdr lst))))
...
>>> (defun cdaar (lst)
...    (cdr (car (car lst))))
...
>>> (defun cdadr (lst)
...    (cdr (car (cdr lst))))
...
>>> (defun cddar (lst)
...    (cdr (cdr (car lst))))
...
>>> (defun cdddr (lst)
...    (cdr (cdr (cdr lst))))
...
>>> (defun caaaar (lst)
...    (car (car (car (car lst)))))
...
>>> (defun caaadr (lst)
...    (car (car (car (cdr lst)))))
...
>>> (defun caadar (lst)
...    (car (car (cdr (car lst)))))
...
>>> (defun caaddr (lst)
...    (car (car (cdr (cdr lst)))))
...
>>> (defun cadaar (lst)
...    (car (cdr (car (car lst)))))
...
>>> (defun cadadr (lst)
...    (car (cdr (car (cdr lst)))))
...
>>> (defun caddar (lst)
...    (car (cdr (cdr (car lst)))))
...
>>> (defun cadddr (lst)
...    (car (cdr (cdr (cdr lst)))))
...
>>> (defun cdaaar (lst)
...    (cdr (car (car (car lst)))))
...
>>> (defun cdaadr (lst)
...    (cdr (car (car (cdr lst)))))
...
>>> (defun cdadar (lst)
...    (cdr (car (cdr (car lst)))))
...
>>> (defun cdaddr (lst)
...    (cdr (car (cdr (cdr lst)))))
...
>>> (defun cddaar (lst)
...    (cdr (cdr (car (car lst)))))
...
>>> (defun cddadr (lst)
...    (cdr (cdr (car (cdr lst)))))
...
>>> (defun cdddar (lst)
...    (cdr (cdr (cdr (car lst)))))
...
>>> (defun cddddr (lst)
...    (cdr (cdr (cdr (cdr lst)))))
...
>>> ; List - remove a symbol from a list - destructive
... ;
... ; (remove '<symbol> '<list>)
... (defun remove (sym lst)
...          (cond
...             ((null lst)           nil)
...             ((= sym (first lst))  (rest lst))
...             (1                    (cons (first lst) (remove sym (rest lst))))))
...
>>> ; List - compute the length of the list
... ;
... ; (list-length '<list>)
... (defun length (lst)
...          (if (null lst)
...              0
...              (+ 1 (length (rest lst)))))
...
>>> ; List - reverse the order of the top level elements
... ;
... ; (reverse '<list>)
... (defun reverse (lst)
...          (reverse-aux '() lst))
...
>>> (defun reverse-aux (destLst srcLst)
...          (if (null srcLst)
...              destLst
...              (reverse-aux (cons (first srcLst) destLst) (rest srcLst))))
...
>>> ; List - make a copy of the argument list
... ;
... ; (copy <list>)
... (defun copy (lst)
...          (reverse (reverse lst)))
...
>>> ; List - make a deepCopy fo the argument list
... ;
... ; (deepCopy <list>)
... (defun deepCopy (expr)
...          (cond ((null   expr)  '( ))
...                 ((isAtom?   expr)  expr)
...                 ((isString? expr)  expr)
...                 ((isList?   expr)  (cons (deepcopy (first expr))
...                                          (deepcopy (rest  expr)))) ))
...
>>> ; (dig aTree aPath)
... ;
... ; Given a nested structure of lists and maps, this function will execute
... ; a depth-first traversal down aPath - a list of map keys and list inidies.
... ; returns the object in that location.
... (defun dig (aTree aPath)
...          (if (null aPath)
...              aTree
...              (dig (at aTree (first aPath)) (rest aPath)) ))
...
>>> ; Compare two lists for deep equality - deep comparison
... ;
... ; (tree-equal '<expr-1> '<expr-2>)
... (defun tree-equal (expr1 expr2)
...          (cond ((or (atom expr1)
...                     (null expr1))
...                                       (= expr1 expr2))
...                    ((and (listp expr1)
...                          (listp expr2))
...                                  (and (listp expr2)
...                                       (and (equal? (first expr1) (first expr2))
...                                            (equal? (rest expr1) (rest expr2)))))
...                    (1
...                                  nil)))
...

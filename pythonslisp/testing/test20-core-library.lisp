>>> ;;; empty dict
... (dictp (make-dict))
...

==> T

>>> (caar (list (list 1 2) 3))
...

==> 1

>>> (cadr (list 1 2 3))
...

==> 2

>>> (cdar (list (list 1 2) 3))
...

==> (2)

>>> (cddr (list 1 2 3))
...

==> (3)

>>> (caaar (list (list (list 1)) 2))
...

==> 1

>>> (caddr (list 1 2 3 4))
...

==> 3

>>> (cdddr (list 1 2 3 4))
...

==> (4)

>>> (cadddr (list 1 2 3 4))
...

==> 4

>>> (cddddr (list 1 2 3 4 5))
...

==> (5)

>>> (second (list 10 20 30))
...

==> 20

>>> (third (list 10 20 30))
...

==> 30

>>> (fourth (list 10 20 30 40))
...

==> 40

>>> (fifth (list 10 20 30 40 50))
...

==> 50

>>> (sixth (list 10 20 30 40 50 60))
...

==> 60

>>> (seventh (list 10 20 30 40 50 60 70))
...

==> 70

>>> (eighth (list 10 20 30 40 50 60 70 80))
...

==> 80

>>> (ninth (list 10 20 30 40 50 60 70 80 90))
...

==> 90

>>> (tenth (list 10 20 30 40 50 60 70 80 90 100))
...

==> 100

>>> (nth 0 (list 10 20 30))
...

==> 10

>>> (nth 2 (list 10 20 30))
...

==> 30

>>> (copy-list (list 1 2 3))
...

==> (1 2 3)

>>> (mapcar (lambda (x) (* x x)) (list 1 2 3 4))
...

==> (1 4 9 16)

>>> (mapcar (lambda (x) (+ x 1)) (list 10 20 30))
...

==> (11 21 31)

>>> (when t 42)
...

==> 42

>>> (when nil 42)
...

==> NIL

>>> (unless nil 42)
...

==> 42

>>> (unless t 42)
...

==> NIL

>>> (length (list 1 2 3))
...

==> 3

>>> (length nil)
...

==> 0

>>> (reverse (list 1 2 3))
...

==> (3 2 1)

>>> (remove 'b '(a b c))
...

==> (A C)

>>> (remove 'a '(a b c))
...

==> (B C)


; --- endp ---

>>> (endp '())
...

==> T

>>> (endp '(1 2 3))
...

==> NIL

; --- last ---

>>> (last '(1 2 3))
...

==> (3)

>>> (last '(a))
...

==> (A)

>>> (last '())
...

==> NIL

; --- nthcdr ---

>>> (nthcdr 0 '(a b c d))
...

==> (A B C D)

>>> (nthcdr 2 '(a b c d))
...

==> (C D)

>>> (nthcdr 4 '(a b c d))
...

==> NIL

; --- butlast ---

>>> (butlast '(1 2 3 4))
...

==> (1 2 3)

>>> (butlast '(1 2 3 4) 2)
...

==> (1 2)

>>> (butlast '(1 2 3) 3)
...

==> NIL

; --- member ---

>>> (member 2 '(1 2 3 4))
...

==> (2 3 4)

>>> (member 'b '(a b c))
...

==> (B C)

>>> (member 9 '(1 2 3))
...

==> NIL

; --- assoc ---

>>> (assoc 'b '((a 1) (b 2) (c 3)))
...

==> (B 2)

>>> (assoc 'z '((a 1) (b 2)))
...

==> NIL

; --- every / some ---

>>> (every numberp '(1 2 3))
...

==> T

>>> (every numberp '(1 "a" 3))
...

==> NIL

>>> (every numberp '())
...

==> T

>>> (some numberp '(a 1 b))
...

==> T

>>> (some numberp '(a b c))
...

==> NIL

>>> (some numberp '())
...

==> NIL

; --- reduce ---

>>> (reduce + '(1 2 3 4))
...

==> 10

>>> (reduce * '(1 2 3 4))
...

==> 24

>>> (reduce + '(42))
...

==> 42

; --- mapc ---

>>> (setf mresult '())
...

==> NIL

>>> (mapc (lambda (x) (setf mresult (cons (* x x) mresult))) '(1 2 3))
...

==> (1 2 3)

>>> mresult
...

==> (9 4 1)

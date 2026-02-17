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

>>> (copy (list 1 2 3))
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

>>> (list-length (list 1 2 3))
...

==> 3

>>> (list-length nil)
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


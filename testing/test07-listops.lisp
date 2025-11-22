>>> (first '(a b c))
...

==> A

>>> (first '())
...

==> NIL

>>> (rest '(a b c))
...

==> (B C)

>>> (rest '())
...

==> NIL

>>> (cons "yes" '(a b c))
...

==> ("yes" A B C)

>>> (cons 3 '())
...

==> (3)

>>> (first '(fast computers are nice))
...

==> FAST

>>> (first '(a b c))
...

==> A

>>> (rest '(fast computers are nice))
...

==> (COMPUTERS ARE NICE)

>>> (rest '(a b c))
...

==> (B C)

>>> (rest '(c))
...

==> NIL

>>> (first '((a b) (c d)))
...

==> (A B)

>>> (first (rest '(a b c)))
...

==> B

>>> (first '(rest (a b c)))
...

==> REST

>>> (cons 'a '(b c))
...

==> (A B C)


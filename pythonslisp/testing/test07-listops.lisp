>>> (first '(a b c))
...

==> A

>>> (car '())
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

>>> (car (list 10 20 30))
...

==> 10

>>> (cdr (list 10 20 30))
...

==> (20 30)

>>> (setf mylist (list 10 20 30))
...

==> (10 20 30)

>>> (at-delete 1 mylist)
...

==> T

>>> mylist
...

==> (10 30)

>>> (setf mylist2 (list 10 30))
...

==> (10 30)

>>> (at-insert 1 mylist2 20)
...

==> 20

>>> mylist2
...

==> (10 20 30)

>>> (sorted (list 3 1 2))
...

==> (1 2 3)

>>> (sorted (list "b" "a" "c"))
...

==> ("a" "b" "c")


>>> ;;; Error: car/cdr require a list argument
... (car 1)

%%% ERROR 'CAR': 1st argument expected to be a list.
%%% USAGE: (CAR <list>)
==>

>>> (cdr "str")

%%% ERROR 'CDR': 1st argument expected to be a list.
%%% USAGE: (CDR <list>)
==>

>>> ;;; Error: cons requires exactly two arguments
... (cons 1)

%%% ERROR 'CONS': 2 arguments expected.
%%% USAGE: (CONS <obj> <list>)
==>

>>> ;;; Error: append requires at least two arguments
... (append '(1))

%%% ERROR 'APPEND': At least 2 arguments expected.
%%% USAGE: (APPEND <list1> <list2> ...)
==>

>>> ;;; Error: append requires list arguments
... (append 1 '(2 3))

%%% ERROR 'APPEND': Invalid argument.
%%% USAGE: (APPEND <list1> <list2> ...)
==>

>>> ;;; Error: at with out-of-range index
... (at 5 '(1 2 3))

%%% ERROR 'AT': Invalid argument key/index.
%%% USAGE: (AT <keyOrIndex> <mapListOrStr>)
==>

>>> ;;; Error: sorted requires a list argument
... (sorted 1)

%%% ERROR 'SORTED': Argument 1 expected to be a list.
%%% USAGE: (SORTED <list>)
==>


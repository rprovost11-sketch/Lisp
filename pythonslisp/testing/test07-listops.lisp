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

>>> (sort (list 3 1 2) <)
...

==> (1 2 3)

>>> (sort (list "b" "a" "c") string<)
...

==> ("a" "b" "c")


>>> ;;; Error: car/cdr require a list argument
... (car 1)

%%% ERROR 'CAR': 1st argument expected to be a list.
%%% PRIMITIVE USAGE: (CAR list)
==>

>>> (cdr "str")

%%% ERROR 'CDR': 1st argument expected to be a list.
%%% PRIMITIVE USAGE: (CDR list)
==>

>>> ;;; Error: cons requires exactly two arguments
... (cons 1)

%%% ERROR 'CONS': 2 arguments expected.
%%% PRIMITIVE USAGE: (CONS obj list)
==>

>>> ;;; append with no arguments returns NIL
... (append)
...

==> NIL

>>> ;;; append with one argument returns it unchanged
... (append '(1 2 3))
...

==> (1 2 3)

>>> ;;; Error: append requires list arguments
... (append 1 '(2 3))

%%% ERROR 'APPEND': Invalid argument.
%%% PRIMITIVE USAGE: (APPEND &rest lists)
==>

>>> ;;; Error: at with out-of-range index
... (at 5 '(1 2 3))

%%% ERROR 'AT': Invalid argument key/index.
%%% PRIMITIVE USAGE: (AT keyOrIndex dictListOrStr)
==>

>>> ;;; Error: sort requires a list argument (predicate now required)
... (sort 1 <)

%%% ERROR 'SORT': Argument 1 expected to be a list.
%%% PRIMITIVE USAGE: (SORT sequence predicate &key (key nil))
==>

; --- subseq ---

>>> (subseq '(a b c d e) 1 3)
==> (B C)

>>> (subseq '(a b c d e) 0 3)
==> (A B C)

>>> (subseq '(a b c d e) 2)
==> (C D E)

>>> (subseq '(a b c d e) 0)
==> (A B C D E)

>>> (subseq '(a b c d e) 5)
==> NIL

>>> (subseq '(a b c d e) 1 1)
==> NIL

>>> (subseq "hello" 1 3)
==> "el"

>>> (subseq "hello" 0)
==> "hello"

>>> (subseq "hello" 2)
==> "llo"

>>> (subseq "hello" 0 0)
==> ""

>>> (subseq '() 0)
==> NIL

; --- subseq errors ---

>>> ;;; not a list or string
... (subseq 42 0)

%%% ERROR 'SUBSEQ': 1st argument must be a list or string.
%%% PRIMITIVE USAGE: (SUBSEQ sequence start &optional end)
==>

>>> ;;; start not integer
... (subseq '(1 2 3) 1.0)

%%% ERROR 'SUBSEQ': 2nd argument must be an integer.
%%% PRIMITIVE USAGE: (SUBSEQ sequence start &optional end)
==>

>>> ;;; negative start
... (subseq '(1 2 3) -1)

%%% ERROR 'SUBSEQ': Start index must be non-negative.
%%% PRIMITIVE USAGE: (SUBSEQ sequence start &optional end)
==>

>>> ;;; start out of bounds
... (subseq '(1 2 3) 4)

%%% ERROR 'SUBSEQ': Start index out of bounds.
%%% PRIMITIVE USAGE: (SUBSEQ sequence start &optional end)
==>

>>> ;;; end < start
... (subseq '(1 2 3) 2 1)

%%% ERROR 'SUBSEQ': End index must be >= start index.
%%% PRIMITIVE USAGE: (SUBSEQ sequence start &optional end)
==>

>>> ;;; too few args
... (subseq '(1 2 3))

%%% ERROR 'SUBSEQ': 2 or 3 arguments expected.
%%% PRIMITIVE USAGE: (SUBSEQ sequence start &optional end)
==>

>>> ;;; too many args
... (subseq '(1 2 3) 0 2 99)

%%% ERROR 'SUBSEQ': 2 or 3 arguments expected.
%%% PRIMITIVE USAGE: (SUBSEQ sequence start &optional end)
==>

; --- remove-if / remove-if-not ---

>>> (remove-if oddp '(1 2 3 4 5))
==> (2 4)

>>> (remove-if evenp '(1 2 3 4 5))
==> (1 3 5)

>>> (remove-if oddp '())
==> NIL

>>> (remove-if (lambda (x) (> x 3)) '(1 2 3 4 5))
==> (1 2 3)

>>> (remove-if-not oddp '(1 2 3 4 5))
==> (1 3 5)

>>> (remove-if-not evenp '(1 2 3 4 5))
==> (2 4)

>>> (remove-if-not oddp '())
==> NIL

; --- find / find-if ---

>>> (find 3 '(1 2 3 4 5))
==> 3

>>> (find 9 '(1 2 3 4 5))
==> NIL

>>> (find 3 '())
==> NIL

>>> (find-if evenp '(1 2 3 4 5))
==> 2

>>> (find-if oddp '(2 4 6))
==> NIL

>>> (find-if (lambda (x) (> x 3)) '(1 2 3 4 5))
==> 4

; --- position / position-if ---

>>> (position 3 '(1 2 3 4 5))
==> 2

>>> (position 9 '(1 2 3 4 5))
==> NIL

>>> (position 3 '())
==> NIL

>>> (position-if evenp '(1 2 3 4 5))
==> 1

>>> (position-if oddp '(2 4 6))
==> NIL

>>> (position-if (lambda (x) (> x 3)) '(1 2 3 4 5))
==> 3

; --- count / count-if ---

>>> (count 2 '(1 2 3 2 4 2))
==> 3

>>> (count 9 '(1 2 3))
==> 0

>>> (count 2 '())
==> 0

>>> (count-if evenp '(1 2 3 4 5))
==> 2

>>> (count-if oddp '(2 4 6))
==> 0

; --- substitute / substitute-if ---

>>> (substitute 99 2 '(1 2 3 2 4))
==> (1 99 3 99 4)

>>> (substitute 99 9 '(1 2 3))
==> (1 2 3)

>>> (substitute 99 2 '())
==> NIL

>>> (substitute-if 99 evenp '(1 2 3 4 5))
==> (1 99 3 99 5)

>>> (substitute-if 99 oddp '(2 4 6))
==> (2 4 6)

; --- char ---

>>> (char "hello" 0)
==> "h"

>>> (char "hello" 4)
==> "o"

>>> (char "hello" 2)
==> "l"

>>> (list)

==> NIL

>>> (list 1)

==> (1)

>>> (list 1 2 3)

==> (1 2 3)

>>> ((lambda (a b) (+ a (* b 3))) 4 5)

==> 19

>>> ((lambda (a &optional (b 2)) (+ a (* b 3))) 4 5)

==> 19

>>> ((lambda (a &optional (b 2)) (+ a (* b 3))) 4)

==> 10

>>> ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)))

==> (2 NIL 3 NIL NIL)

>>> ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6)

==> (6 T 3 NIL NIL)

>>> ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6 3)

==> (6 T 3 T NIL)

>>> ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6 3 8)

==> (6 T 3 T (8))

>>> ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6 3 8 9 10 11)

==> (6 T 3 T (8 9 10 11))

>>> ((lambda (a b &key c d) (list a b c d)) 1 2)

==> (1 2 NIL NIL)

>>> ((lambda (a b &key c d) (list a b c d)) 1 2 :c 6)

==> (1 2 6 NIL)

>>> ((lambda (a b &key c d) (list a b c d)) 1 2 :d 8)

==> (1 2 NIL 8)

>>> ((lambda (a b &key c d) (list a b c d)) 1 2 :c 6 :d 8)

==> (1 2 6 8)

>>> ((lambda (a b &key c d) (list a b c d)) 1 2 :d 8 :c 6)

==> (1 2 6 8)

>>> ((lambda (a b &key c d) (list a b c d)) :a 1 :d 8 :c 6)

==> (:A 1 6 8)

>>> ((lambda (a b &key c d) (list a b c d)) :a :b :c :d)

==> (:A :B :D NIL)

>>> ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x)) 1)

==> (1 3 NIL 1 NIL)

>>> ((lambda (a &optional (b 3) &rest x &key c (d a)) ( list a b c d x)) 1 2)

==> (1 2 NIL 1 NIL)

>>> ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x)) :c 7)

==> (:C 7 NIL :C NIL)

>>> ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x)) 1 6 :c 7)

==> (1 6 7 1 (:C 7))

>>> ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x)) 1 6 :d 8)

==> (1 6 NIL 8 (:D 8))

>>> ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x)) 1 6 :d 8 :c 9 :d 10)

==> (1 6 9 10 (:D 8 :C 9 :D 10))

>>> ;;; Edge case: bare &optional symbol (no default, no svar) - not supplied
... ((lambda (&optional a) a))

==> NIL

>>> ;;; bare &optional symbol - supplied
... ((lambda (&optional a) a) 7)

==> 7

>>> ;;; &aux bare symbol - gets NIL default
... ((lambda (&aux a) a))

==> NIL

>>> ;;; &aux with initform
... ((lambda (&aux (a 42)) a))

==> 42

>>> ;;; &aux with computed initform using positional arg
... ((lambda (x &aux (a (* x 2))) (list x a)) 5)

==> (5 10)

>>> ;;; Error: too few positional arguments (lambda)
... ((lambda (a b) (+ a b)) 1)

%%% Error binding arguments in call to "(lambda ...)".
%%% Too few positional arguments.
==>

>>> ;;; Error: too many arguments, no &rest (lambda)
... ((lambda (a) a) 1 2 3)

%%% Error binding arguments in call to "(lambda ...)".
%%% Too many arguments.  Received 2.
==>

>>> ;;; Error: too many arguments after &optional exhausted (lambda)
... ((lambda (&optional a b) (list a b)) 1 2 3)

%%% Error binding arguments in call to "(lambda ...)".
%%% Too many arguments.  Received 3.
==>

>>> ;;; Error: unexpected keyword (lambda)
... ((lambda (&key a) a) :b 2)

%%% Error binding arguments in call to "(lambda ...)".
%%% Unexpected keyword found B.
==>

>>> ;;; Error: keyword not followed by value (lambda)
... ((lambda (&key a b) (list a b)) :a 1 :b)

%%% Error binding arguments in call to "(lambda ...)".
%%% Keyword B expected to be followed by a value.
==>

>>> ;;; Named function: error message shows function name
... (defun param-test-posit (a b) (+ a b))

==> (Function PARAM-TEST-POSIT (A B) ... )

>>> (param-test-posit 3 4)

==> 7

>>> (param-test-posit 1)

%%% Error binding arguments in call to function "PARAM-TEST-POSIT".
%%% Too few positional arguments.
==>

>>> (param-test-posit 1 2 3)

%%% Error binding arguments in call to function "PARAM-TEST-POSIT".
%%% Too many arguments.  Received 3.
==>


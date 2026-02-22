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

>>> ;;; Duplicate keyword: first occurrence wins (CL 3.4.1.4.1)
... ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x)) 1 6 :d 8 :c 9 :d 10)

==> (1 6 9 8 (:D 8 :C 9 :D 10))

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
%%% Too many arguments.  Received 3.
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

==> (FUNCTION PARAM-TEST-POSIT (A B) ... )

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


>>> ;;; Fix 1: bare (var) spec with no default in &key
... ((lambda (&key (a)) a))

==> NIL

>>> ((lambda (&key (a)) a) :a 7)

==> 7

>>> ;;; Fix 2: &aux is not swallowed as a key parameter name
... ((lambda (&key a &aux b) (list a b)) :a 5)

==> (5 NIL)

>>> ;;; &allow-other-keys: unknown keys accepted, known key bound
... ((lambda (&key a &allow-other-keys) a) :a 1 :b 99)

==> 1

>>> ;;; &allow-other-keys: no args supplied at all
... ((lambda (&key a &allow-other-keys) a))

==> NIL

>>> ;;; &allow-other-keys: only unknown keys supplied
... ((lambda (&key a &allow-other-keys) a) :z 42)

==> NIL

>>> ;;; &allow-other-keys: multiple unknown keys
... ((lambda (&key a &allow-other-keys) a) :a 1 :b 2 :c 3 :d 4)

==> 1

>>> ;;; &allow-other-keys: with svar — key supplied
... ((lambda (&key (a 0 ap) &allow-other-keys) (list a ap)) :a 7 :x 99)

==> (7 T)

>>> ;;; &allow-other-keys: with svar — key NOT supplied, only unknown keys
... ((lambda (&key (a 0 ap) &allow-other-keys) (list a ap)) :x 99)

==> (0 NIL)

>>> ;;; &allow-other-keys: with &rest — rest captures all key/val pairs
... ((lambda (&rest r &key a &allow-other-keys) (list a r)) :a 1 :b 2)

==> (1 (:A 1 :B 2))

>>> ;;; &allow-other-keys: with default initform
... ((lambda (&key (a 42) &allow-other-keys) a) :z 0)

==> 42

>>> ;;; &allow-other-keys: duplicate known key — first wins
... ((lambda (&key a &allow-other-keys) a) :a 1 :a 2 :b 3)

==> 1

>>> ;;; :allow-other-keys t in arg list permits unknown keys (CL 3.4.1.4.1)
... ((lambda (&key a) a) :a 1 :b 99 :allow-other-keys t)

==> 1

>>> ;;; :allow-other-keys t before any other keys
... ((lambda (&key a) a) :allow-other-keys t :a 5 :z 99)

==> 5

>>> ;;; :allow-other-keys nil does NOT permit unknown keys
... ((lambda (&key a) a) :a 1 :b 99 :allow-other-keys nil)

%%% Error binding arguments in call to "(lambda ...)".
%%% Unexpected keyword found B.
==>

>>> ;;; :allow-other-keys t with no declared keys — unknown keys silently ignored
... ((lambda (&key) nil) :x 1 :y 2 :allow-other-keys t)

==> NIL

>>> ;;; :allow-other-keys t without &allow-other-keys in lambda list
... ((lambda (&key a b) (list a b)) :a 1 :b 2 :c 3 :allow-other-keys t)

==> (1 2)

>>> ;;; :allow-other-keys t with svar — supplied key gets T, unsupplied gets NIL
... ((lambda (&key (a 0 ap) (b 0 bp)) (list a ap b bp)) :a 5 :z 99 :allow-other-keys t)

==> (5 T 0 NIL)

>>> ;;; Fix 1: &optional default (not the keyword) is used when next arg is a keyword
... ((lambda (&optional (x 99) &key y) (list x y)) :y 5)

==> (99 5)

>>> ;;; Fix 1: &optional with no &key — heuristic fires, :foo not consumed, extra-arg error
... ((lambda (&optional x) x) :foo)

%%% Error binding arguments in call to "(lambda ...)".
%%% Too many arguments.  Received 1.
==>

>>> ;;; Fix 1: non-NIL explicit default used when keyword heuristic fires
... ((lambda (&optional (x (+ 40 2)) &key y) (list x y)) :y 7)

==> (42 7)

>>> ;;; Fix 4: simple duplicate key — first wins
... ((lambda (&key a) a) :a 1 :a 2)

==> 1

>>> ;;; Fix 4: svar is still T when key appears twice; value is from first occurrence
... ((lambda (&key (a 0 a-p)) (list a a-p)) :a 10 :a 20)

==> (10 T)

>>> ;;; Fix 7: single-element (&aux (x)) binds x to NIL
... ((lambda (&aux (x)) x))

==> NIL

>>> ;;; Fix 7: (&aux (x) (y 7)) — single and two-element forms together
... ((lambda (&aux (x) (y 7)) (list x y)))

==> (NIL 7)

>>> ;;; NC6: (&optional) with no optional params is legal
... ((lambda (a &optional) a) 5)

==> 5

>>> ;;; NC6: (&key) with no key params is legal
... ((lambda (a &key) a) 10)

==> 10

>>> ;;; NC6: (&aux) with no aux vars is legal
... ((lambda (a &aux) a) 3)

==> 3

>>> ;;; NC6: (&key) with empty section — any keyword passed is an error
... ((lambda (&key) nil) :x 1)

%%% Error binding arguments in call to "(lambda ...)".
%%% Unexpected keyword found X.
==>

>>> ;;; NC5: duplicate positional parameter names
... ((lambda (a a) a) 1 2)

%%% Error binding arguments in call to "(lambda ...)".
%%% Duplicate parameter name A in positional parameters.
==>

>>> ;;; NC5: duplicate across positional and optional (both are plain symbols)
... ((lambda (a &optional a) a) 1)

%%% Error binding arguments in call to "(lambda ...)".
%%% Duplicate parameter name A in positional parameters.
==>

>>> ;;; NC5: duplicate positional and &rest name
... ((lambda (a &rest a) a) 1)

%%% Error binding arguments in call to "(lambda ...)".
%%% Duplicate parameter name A in &REST.
==>

>>> ;;; NC5: duplicate key variable and its own svar
... ((lambda (&key (a 0 a)) a) :a 5)

%%% Error binding arguments in call to "(lambda ...)".
%%% Duplicate parameter name A in supplied-p variable.
==>

>>> ;;; NC1: known keyword appearing out of order gives specific message
... ((lambda (&rest x &optional y) (list x y)) 1 2)

%%% Error binding arguments in call to "(lambda ...)".
%%% &OPTIONAL is misplaced in the lambda list.  Valid order: &optional, &rest, &key, &aux.
==>

>>> ;;; NC1: truly unknown &-keyword
... ((lambda (&body x) x) 1 2)

%%% Error binding arguments in call to "(lambda ...)".
%%% Unknown lambda list keyword: &BODY.
==>

>>> ;;; Bug 5: svar bound to NIL when key not supplied
... ((lambda (&key (a 0 ap)) (list a ap)) :a 10)

==> (10 T)

>>> ((lambda (&key (a 0 ap)) (list a ap)))

==> (0 NIL)

>>> ;;; Bug 5: multiple key params, some supplied, some not — svars correct
... ((lambda (&key (a 0 ap) (b 1 bp)) (list a ap b bp)) :b 99)

==> (0 NIL 99 T)

>>> ;;; NC1: key default can reference preceding key variable
... ((lambda (&key (a 10) (b a)) (list a b)))

==> (10 10)

>>> ;;; NC1: key default references preceding key — with supplied value
... ((lambda (&key (a 10) (b a)) (list a b)) :a 42)

==> (42 42)

>>> ;;; NC1: key default references preceding key — second key supplied overrides
... ((lambda (&key (a 10) (b a)) (list a b)) :a 42 :b 7)

==> (42 7)

>>> ;;; Bug 4: &-symbol rejected as &rest variable name
... ((lambda (&rest &foo) &foo) 1 2)

%%% Error binding arguments in call to "(lambda ...)".
%%% Symbol expected after &rest.
==>

>>> ;;; Bug 6: &-symbol rejected as variable name in &key list spec
... ((lambda (&key (&aux)) &aux))

%%% Error binding arguments in call to "(lambda ...)".
%%% Lambda list keyword &AUX cannot be used as a variable name in &KEY spec.
==>

>>> ;;; Bug 7: &-symbol rejected as variable name in &aux list spec
... ((lambda (&aux (&key 5)) &key))

%%% Error binding arguments in call to "(lambda ...)".
%%% Parameter spec following &AUX must be a list of (<variable> [<defaultvalue>]).
==>

>>> ;;; &-symbol rejected as svar in &optional spec
... ((lambda (&optional (a 0 &rest)) a))

%%% Error binding arguments in call to "(lambda ...)".
%%% Lambda list keyword &REST cannot be used as a supplied-p variable in &OPTIONAL spec.
==>

>>> ;;; &-symbol rejected as svar in &key spec
... ((lambda (&key (a 0 &aux)) a))

%%% Error binding arguments in call to "(lambda ...)".
%%% Lambda list keyword &AUX cannot be used as a supplied-p variable in &KEY spec.
==>

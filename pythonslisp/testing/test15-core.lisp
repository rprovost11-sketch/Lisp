; ============================================================
; test18-apply-funcall.lisp
; Tests for apply, funcall, eval, and python primitives.
; ============================================================

; --- funcall: basic usage ---

>>> ;;; funcall with a primitive
... (funcall + 1 2 3)
...

==> 6

>>> ;;; funcall with car
... (funcall car '(a b c))
...

==> A

>>> ;;; funcall with a lambda
... (funcall (lambda (x) (* x x)) 5)
...

==> 25

>>> ;;; funcall with a named function
... (funcall list 1 2 3)
...

==> (1 2 3)

; --- apply: basic usage ---

>>> ;;; apply with a list of args
... (apply + '(1 2 3))
...

==> 6

>>> ;;; apply with extra args prepended to list
... (apply + 1 2 '(3 4))
...

==> 10

>>> ;;; apply with empty list
... (apply + '())
...

==> 0

>>> ;;; apply with a named function
... (apply list '(a b c))
...

==> (A B C)

>>> ;;; apply with extra args and named function
... (apply cons 'a '((b c)))
...

==> (A B C)

; --- eval ---

>>> ;;; eval on a quoted expression
... (eval '(+ 10 20))
...

==> 30

; --- python ---

>>> ;;; python evaluates a python expression
... (python "3 + 4")
...

==> 7

>>> ;;; python with string expression
... (python "'hello' + ' world'")
...

==> "hello world"

; --- recursion-limit ---

>>> ;;; recursion-limit returns current limit
... (integerp (recursion-limit))
...

==> T

; --- funcall errors ---

>>> ;;; Error: funcall with no arguments
... (funcall)

%%% ERROR 'FUNCALL': At least 1 argument expected.
%%% PRIMITIVE USAGE: (FUNCALL callable &rest args)
==>

; --- apply errors ---

>>> ;;; Error: apply with no arguments
... (apply)

%%% ERROR 'APPLY': At least 2 arguments expected.
%%% PRIMITIVE USAGE: (APPLY function &rest args)
==>

>>> ;;; Error: apply last arg not a list
... (apply + 1)

%%% ERROR 'APPLY': Last argument expected to be a list.
%%% PRIMITIVE USAGE: (APPLY function &rest args)
==>

>>> ;;; Error: apply with a special form
... (apply 'if '(t 1 2))

%%% ERROR 'APPLY': First argument may not be a special form.
%%% PRIMITIVE USAGE: (APPLY function &rest args)
==>

; --- python errors ---

>>> ;;; Error: python with no arguments
... (python)

%%% ERROR 'PYTHON': 1 argument expected.
%%% PRIMITIVE USAGE: (PYTHON string)
==>

>>> ;;; Error: python with non-string
... (python 1)

%%% ERROR 'PYTHON': Argument expected to be a string.
%%% PRIMITIVE USAGE: (PYTHON string)
==>

; --- eval errors ---

>>> ;;; Error: eval with no arguments
... (eval)

%%% ERROR 'EVAL': 1 argument expected.
%%% PRIMITIVE USAGE: (EVAL sexpr)
==>

>>> ;;; Error: eval with too many arguments
... (eval 1 2)

%%% ERROR 'EVAL': 1 argument expected.
%%% PRIMITIVE USAGE: (EVAL sexpr)
==>

; --- recursion-limit errors ---

>>> ;;; Error: recursion-limit with non-integer
... (recursion-limit "a")

%%% ERROR 'RECURSION-LIMIT': Argument must be an integer.
%%% PRIMITIVE USAGE: (RECURSION-LIMIT &optional newLimit)
==>

>>> ;;; Error: recursion-limit with too many args
... (recursion-limit 1 2)

%%% ERROR 'RECURSION-LIMIT': 0 or 1 arguments expected.
%%% PRIMITIVE USAGE: (RECURSION-LIMIT &optional newLimit)
==>

; --- Control structure edge cases ---

>>> ;;; while with initially false condition returns NIL
... (while nil (+ 1 2))
...

==> NIL

>>> ;;; dolist on empty list returns NIL
... (dolist (x '()) (write! x))
...

==> NIL

>>> ;;; dotimes with 0 iterations returns NIL
... (dotimes (i 0) (write! i))
...

==> NIL

>>> ;;; progn with no arguments returns NIL
... (progn)
...

==> NIL

>>> ;;; cond with no matching clause returns NIL
... (cond (nil 1) (nil 2))
...

==> NIL

>>> ;;; case with no matching clause returns NIL
... (case 5 (1 "one") (2 "two"))
...

==> NIL

>>> ;;; case with matching clause
... (case 2 (1 10) (2 20) (3 30))
...

==> 20

>>> ;;; cond with first match
... (cond (t 1) (t 2))
...

==> 1

>>> ;;; cond skips false, matches second
... (cond (nil 1) (t 2))
...

==> 2

; --- List operation edge cases ---

>>> ;;; at with negative index (Python-style)
... (at -1 (list 10 20 30))
...

==> 30

>>> ;;; at on a string
... (at 0 "hello")
...

==> "h"

>>> ;;; at on string with negative index
... (at -1 "hello")
...

==> "o"

>>> ;;; at with symbol key on map
... (at 'a (make-dict (a 1) (b 2)))
...

==> 1

>>> ;;; sort on empty list
... (sort '() <)
...

==> NIL

>>> ;;; cons 3 onto empty list
... (cons 3 '())
...

==> (3)

>>> ;;; hasValue? on list (true)
... (hasValue? 2 '(1 2 3))
...

==> T

>>> ;;; hasValue? on list (false)
... (hasValue? 5 '(1 2 3))
...

==> NIL

>>> ;;; hasValue? on map (true)
... (hasValue? 2 (make-dict (a 1) (b 2)))
...

==> T

>>> ;;; hasValue? on map (false)
... (hasValue? 3 (make-dict (a 1) (b 2)))
...

==> NIL

>>> ;;; hasKey? on empty map
... (hasKey? 'a (make-dict))
...

==> NIL

>>> ;;; map with integer keys
... (at 1 (make-dict (1 "a") (2 "b")))
...

==> "a"

; --- push!/pop! edge cases ---

>>> (setf plist (list 1 2 3))
...

==> (1 2 3)

>>> ;;; push! mutates and returns list
... (push! plist 4)
...

==> (1 2 3 4)

>>> plist
...

==> (1 2 3 4)

>>> ;;; pop! removes and returns last element
... (pop! plist)
...

==> 4

>>> plist
...

==> (1 2 3)

; --- at-delete on map with symbol key ---

>>> (setf tmap (make-dict (a 10) (b 20) (c 30)))
...

==> (DICT
   ("A" 10)
   ("B" 20)
   ("C" 30)
)

>>> (at-delete 'a tmap)
...

==> T

>>> tmap
...

==> (DICT
   ("B" 20)
   ("C" 30)
)

; --- identity vs equality ---

>>> (setf lst1 (list 1 2))
...

==> (1 2)

>>> ;;; is? on same object returns T
... (is? lst1 lst1)
...

==> T

>>> ;;; is? on equal but different objects returns NIL
... (is? (list 1 2) (list 1 2))
...

==> NIL

; --- copy-list vs original ---

>>> (setf orig (list 1 2 3))
...

==> (1 2 3)

>>> (setf cop (copy-list orig))
...

==> (1 2 3)

>>> ;;; copy-list is not identity-equal
... (is? orig cop)
...

==> NIL

>>> ;;; copy-list is value-equal
... (= orig cop)
...

==> T

; --- copy-tree ---

>>> (copy-tree '(1 (2 3) 4))
...

==> (1 (2 3) 4)

; --- let edge cases ---

>>> ;;; let with bare symbol binds to NIL
... (let (x) x)
...

==> NIL

>>> ;;; let with single-element list binds to NIL
... (let ((x)) x)
...

==> NIL

>>> ;;; let with no body returns NIL
... (let ())
...

==> NIL

>>> ;;; let* with no body returns NIL
... (let* ())
...

==> NIL

; --- setf with multiple pairs ---

>>> (setf aa 10 bb 20)
...

==> 20

>>> aa
...

==> 10

>>> bb
...

==> 20

; --- integer conversion with base ---

>>> ;;; integer with base 16 (hex)
... (integer "ff" 16)
...

==> 255

>>> ;;; integer with base 2 (binary)
... (integer "10" 2)
...

==> 2

; --- string concatenation ---

>>> (string 1 2 3)
...

==> "123"

>>> (ustring 1 2 3)
...

==> "123"

; --- dolist accumulation ---

>>> (setf fsum 0)
...

==> 0

>>> (dolist (x '(1 2 3 4 5)) (setf fsum (+ fsum x)))
...

==> NIL

>>> fsum
...

==> 15

; --- dotimes accumulation ---

>>> (setf dsum 0)
...

==> 0

>>> (dotimes (i 5) (setf dsum (+ dsum i)))
...

==> NIL

>>> dsum
...

==> 10

; --- while with accumulation ---

>>> (let ((n 3) (acc 0))
...    (while (> n 0)
...       (setf acc (+ acc n))
...       (setf n (- n 1)))
...    acc)
...

==> 6

; --- parse ---

>>> (parse "(+ 1 2)")
...

==> (PROGN (+ 1 2))

; --- derivative function ---

>>> (d 5)
...

==> 0

>>> (d 'x)
...

==> 1

>>> (d '(+ x 3))
...

==> (+ 1 0)

>>> (d '(* x x))
...

==> (+ (* X 1) (* X 1))

>>> (d '(sin x))
...

==> (* (COS X) 1)

>>> (d '(expt x 3))
...

==> (* (* 3 (EXPT X 2)) 1)

; --- random produces correct types ---

>>> (integerp (random 10))
...

==> T

>>> (floatp (random 10.0))
...

==> T

; --- Additional arithmetic edge cases ---

>>> ;;; single argument to multiply
... (* 5)
...

==> 5

>>> ;;; negation of zero
... (- 0)
...

==> 0

>>> ;;; division produces exact fraction
... (/ 1 2)
...

==> 1/2

; --- Additional list operation edge cases ---

>>> ;;; append with 3+ lists
... (append '(1) '(2) '(3))
...

==> (1 2 3)

>>> ;;; sort on single-element list
... (sort (list 3) <)
...

==> (3)

; --- Lambda/macro with docstrings ---

>>> ;;; lambda with docstring still works
... ((lambda () "a docstring" 42))
...

==> 42

>>> ;;; defmacro with docstring still works
... (defmacro test-docmacro19 (x) "a docstring" `(+ ,x 1))
...

==> (MACRO TEST-DOCMACRO19 (X) ...)

>>> (test-docmacro19 5)
...

==> 6

; --- writef with map ---

>>> ;;; writef using map keys (uppercased)
... (writef "{X} {Y}" (make-dict (x 10) (y 20)))
10 20
==> "10 20"

; --- write!/writeLn! with multiple arguments ---

>>> ;;; write! with multiple args concatenates output
... (write! 1 2 3)
123
==> 3

>>> ;;; writeLn! with multiple args
... (writeLn! 4 5 6)
456

==> 6

; ============================================================
; test26-setq-atset.lisp
; Tests for setq (variable assignment) and at-set (indexed/keyed
; mutation of lists and maps).
; ============================================================

; --- 1. setq: basic assignment ---

>>> (setq sq1 99)
...

==> 99

>>> sq1
...

==> 99

>>> (setq sq1 100)
...

==> 100

>>> sq1
...

==> 100

>>> (setq sq2 3.14)
...

==> 3.14

>>> (setq sq3 "hello")
...

==> "hello"

>>> (setq sq4 '(a b c))
...

==> (A B C)

>>> (setq sq5 nil)
...

==> NIL

>>> (setq sq6 t)
...

==> T

; --- 2. setq: multiple pairs in one call ---

>>> ;;; returns the value of the last assignment
... (setq ma 1 mb 2 mc 3)
...

==> 3

>>> ma
...

==> 1

>>> mb
...

==> 2

>>> mc
...

==> 3

; --- 3. setq: scope behaviour ---

>>> ;;; setq inside let finds and updates the let-local binding
... (let ((lvar 5))
...    (setq lvar 99)
...    lvar)
...

==> 99

>>> ;;; the let-local did not leak into the global scope
... (boundp 'lvar)
...

==> NIL

>>> ;;; setq creates a new global when the name is not bound anywhere
... (setq sq-new 777)
...

==> 777

>>> sq-new
...

==> 777

; --- 4. setq: auto-names a lambda ---

>>> (setq sq-fn (lambda (x) (* x x)))
...

==> (FUNCTION SQ-FN (X) ...)

>>> (sq-fn 7)
...

==> 49

; --- 5. setq: error cases ---

>>> ;;; no arguments
... (setq)

%%% ERROR 'SETQ': At least 2 arguments expected.
%%% PRIMITIVE USAGE: (SETQ symbol1 sexpr1 symbol2 sexpr2 ...)
==>

>>> ;;; odd number of arguments
... (setq sq1 1 sq2)

%%% ERROR 'SETQ': An even number of arguments is expected.  Received 3.
%%% PRIMITIVE USAGE: (SETQ symbol1 sexpr1 symbol2 sexpr2 ...)
==>

>>> ;;; non-symbol lvalue
... (setq 42 1)

%%% ERROR 'SETQ': First of setf pair must be a symbol.
%%% PRIMITIVE USAGE: (SETQ symbol1 sexpr1 symbol2 sexpr2 ...)
==>

; --- 6. at-set: list basic mutation ---

>>> (setf asl (list 10 20 30 40 50))
...

==> (10 20 30 40 50)

>>> ;;; set first element; returns new value
... (at-set 0 asl 99)
...

==> 99

>>> ;;; mutation is in-place
... asl
...

==> (99 20 30 40 50)

>>> (at-set 2 asl 77)
...

==> 77

>>> asl
...

==> (99 20 77 40 50)

>>> (at-set 4 asl 55)
...

==> 55

>>> asl
...

==> (99 20 77 40 55)

>>> ;;; negative index follows Python semantics: -1 is the last element
... (at-set -1 asl 11)
...

==> 11

>>> asl
...

==> (99 20 77 40 11)

; --- 7. at-set: list aliasing (shared reference) ---

>>> (setf orig (list 1 2 3))
...

==> (1 2 3)

>>> (setf aliased orig)
...

==> (1 2 3)

>>> ;;; mutation through alias is visible via the original binding
... (at-set 0 aliased 99)
...

==> 99

>>> orig
...

==> (99 2 3)

; --- 8. at-set: map symbol keys ---

>>> (setf skmap (make-dict (x 10) (y 20)))
...

==> (DICT
   ("X" 10)
   ("Y" 20)
)

>>> ;;; quoted symbol key is uppercased to match the stored key
... (at-set 'x skmap 99)
...

==> 99

>>> skmap
...

==> (DICT
   ("X" 99)
   ("Y" 20)
)

>>> ;;; at-set adds a new key when the key is not already present
... (at-set 'z skmap 30)
...

==> 30

>>> skmap
...

==> (DICT
   ("X" 99)
   ("Y" 20)
   ("Z" 30)
)

; --- 9. at-set: map string keys ---

>>> (setf strmap (make-dict ("p" 1) ("q" 2)))
...

==> (DICT
   ("p" 1)
   ("q" 2)
)

>>> (at-set "p" strmap 88)
...

==> 88

>>> strmap
...

==> (DICT
   ("p" 88)
   ("q" 2)
)

; --- 10. at-set: error cases ---

>>> ;;; too few arguments
... (at-set 0 (list 1 2 3))

%%% ERROR 'AT-SET': 3 arguments expected.
%%% PRIMITIVE USAGE: (AT-SET keyOrIndex dictListOrStr newValue)
==>

>>> ;;; too many arguments
... (at-set 0 (list 1 2 3) 9 9)

%%% ERROR 'AT-SET': 3 arguments expected.
%%% PRIMITIVE USAGE: (AT-SET keyOrIndex dictListOrStr newValue)
==>

>>> ;;; second argument is an integer (not a list or map)
... (at-set 0 42 9)

%%% ERROR 'AT-SET': Invalid argument.  List or Dict expected.
%%% PRIMITIVE USAGE: (AT-SET keyOrIndex dictListOrStr newValue)
==>

>>> ;;; second argument is a string (strings are immutable)
... (at-set 0 "abc" 9)

%%% ERROR 'AT-SET': Invalid argument.  List or Dict expected.
%%% PRIMITIVE USAGE: (AT-SET keyOrIndex dictListOrStr newValue)
==>

>>> ;;; list index out of range
... (at-set 99 (list 1 2 3) 0)

%%% ERROR 'AT-SET': Invalid argument key/index.
%%% PRIMITIVE USAGE: (AT-SET keyOrIndex dictListOrStr newValue)
==>

>>> ;;; float index into a list
... (at-set 1.5 (list 1 2 3) 0)

%%% ERROR 'AT-SET': Invalid argument key/index.
%%% PRIMITIVE USAGE: (AT-SET keyOrIndex dictListOrStr newValue)
==>

; --- 11. setf macro expansion tests ---

>>> ;;; setf with a symbol expands to setq (auto-names a lambda)
... (setf sf-fn (lambda (x) (* x 3)))
...

==> (FUNCTION SF-FN (X) ...)

>>> (sf-fn 5)
...

==> 15

>>> ;;; setf with multiple pairs (progn of setqs)
... (setf sf-a 1 sf-b 2 sf-c 3)
...

==> 3

>>> sf-a
...

==> 1

>>> sf-b
...

==> 2

>>> sf-c
...

==> 3

>>> ;;; setf (at ...) form mutates a list in place
... (setf sf-lst (list 10 20 30))
...

==> (10 20 30)

>>> (setf (at 1 sf-lst) 99)
...

==> 99

>>> sf-lst
...

==> (10 99 30)

>>> ;;; setf (at ...) form mutates a map in place
... (setf sf-map (make-dict (x 1) (y 2)))
...

==> (DICT
   ("X" 1)
   ("Y" 2)
)

>>> (setf (at 'y sf-map) 77)
...

==> 77

>>> sf-map
...

==> (DICT
   ("X" 1)
   ("Y" 77)
)

>>> ;;; setf with mixed pairs: symbol then at-form
... (setf sf-x 0 (at 0 sf-lst) 55)
...

==> 55

>>> sf-x
...

==> 0

>>> sf-lst
...

==> (55 99 30)

; --- 12. setf macro error cases ---

>>> ;;; NIL lvalue
... (setf () 5)

%%% setf: lvalue cannot be NIL or ()
==>

>>> ;;; (at ...) form with wrong element count
... (setf (at 0) 5)

%%% setf: (at ...) place form expected 3 elements
==>

>>> ;;; struct accessor with extra instance argument
... (setf (sf-fn sf-a sf-b) 5)

%%% setf: struct accessor place must have exactly 1 instance argument
==>

>>> ;;; non-symbol non-list lvalue
... (setf 99 5)

%%% setf: unrecognized place form
==>

>>> ;;; zero args
... (setf)

%%% Too few positional arguments.
==>

>>> ;;; one arg (missing value)
... (setf sf-a)

%%% Too few positional arguments.
==>

; ============================================================
; test22-additional-coverage.lisp
; Additional edge case and coverage tests for primitives
; and library functions not exercised elsewhere.
; ============================================================

; --- mapcar edge cases ---

>>> ;;; mapcar on empty list
... (mapcar (lambda (x) (* x 2)) '())
...

==> NIL

>>> ;;; mapcar with lambda (double each)
... (mapcar (lambda (x) (* x 2)) '(1 2 3))
...

==> (2 4 6)

; --- remove edge cases ---

>>> ;;; remove non-existent element returns original
... (remove 5 '(1 2 3))
...

==> (1 2 3)

>>> ;;; remove from empty list
... (remove 1 '())
...

==> NIL

; --- when/unless with multiple body forms ---

>>> ;;; when true with multiple body forms returns last
... (when t 1 2)
...

==> 2

>>> ;;; when false with multiple body forms returns NIL
... (when nil 1 2)
...

==> NIL

>>> ;;; unless false with multiple body forms returns last
... (unless nil 1 2)
...

==> 2

>>> ;;; unless true with multiple body forms returns NIL
... (unless t 1 2)
...

==> NIL

; --- setf with at on map ---

>>> (setf smap22 (make-dict (a 1) (b 2)))
...

==> (DICT
   ("A" 1)
   ("B" 2)
)

>>> ;;; setf at on map mutates the value
... (setf (at 'a smap22) 99)
...

==> 99

>>> smap22
...

==> (DICT
   ("A" 99)
   ("B" 2)
)

; --- setf with at on list ---

>>> (setf slist22 (list 10 20 30))
...

==> (10 20 30)

>>> ;;; setf at on list mutates the element
... (setf (at 1 slist22) 99)
...

==> 99

>>> slist22
...

==> (10 99 30)

; --- copy-tree on map ---

>>> ;;; copy-tree preserves map structure
... (copy-tree (make-dict (a 1) (b 2)))
...

==> (DICT
   ("A" 1)
   ("B" 2)
)

; --- nth edge cases ---

>>> ;;; nth with negative index (Python-style)
... (nth -1 '(1 2 3))
...

==> 3

; --- sort with strings ---

>>> ;;; sort works on string lists
... (sort '("c" "a" "b") string<)
...

==> ("a" "b" "c")

; --- cons edge cases ---

>>> ;;; cons a list onto a list
... (cons '(1 2) '(3 4))
...

==> ((1 2) 3 4)

>>> ;;; cons NIL onto a list
... (cons nil '(1 2))
...

==> (NIL 1 2)

; --- multi-body control structures ---

>>> ;;; progn returns the last value
... (progn 1 2 3)
...

==> 3

>>> ;;; cond with multiple body forms returns last
... (cond (t 1 2 3))
...

==> 3

>>> ;;; case with multiple body forms returns last
... (case 1 (1 10 20 30))
...

==> 30

>>> ;;; while returns last body value
... (let ((n 1))
...    (while (> n 0)
...       (setf n (- n 1))
...       42))
...

==> 42

; --- defun with docstring ---

>>> ;;; defun with docstring
... (defun square22 (x) "Returns x squared" (* x x))
...

==> (FUNCTION SQUARE22 (X) ...)

>>> (square22 7)
...

==> 49

; --- average edge cases ---

>>> ;;; average of single value
... (average 5)
...

==> 5

>>> ;;; average with rationals
... (average 1/2 3/2)
...

==> 1

; --- string/ustring from symbol ---

>>> ;;; string converts symbol to string (programmer mode)
... (string 'abc)
...

==> "ABC"

>>> ;;; ustring converts symbol to string (user mode)
... (ustring 'abc)
...

==> "ABC"

; --- make-symbol from string ---

>>> ;;; make-symbol from a single string
... (make-symbol "hello-world")
...

==> HELLO-WORLD

; --- nested quote ---

>>> ;;; double-quoted symbol
... ''x
...

==> (QUOTE X)

>>> ;;; quote inside a quoted list
... '(a 'b c)
...

==> (A (QUOTE B) C)

; --- isqrt boundary values ---

>>> ;;; isqrt of 0
... (isqrt 0)
...

==> 0

>>> ;;; isqrt of 1
... (isqrt 1)
...

==> 1

>>> ;;; isqrt of 4 (perfect square)
... (isqrt 4)
...

==> 2

; --- signum with floats ---

>>> ;;; signum of positive float
... (signum 3.14)
...

==> 1

>>> ;;; signum of negative float
... (signum -0.5)
...

==> -1

; --- integer truncation ---

>>> ;;; integer truncation of negative float
... (integer -3.7)
...

==> -3

; --- atan2 (two-argument atan) ---

>>> ;;; atan with two arguments (atan2)
... (atan 1 1)
...

==> 0.7853981633974483

; --- length ---

>>> ;;; length counts top-level elements only
... (length '((1 2) 3))
...

==> 2

>>> ;;; length of a string
... (length "abc")
...

==> 3

>>> ;;; list-length alias still works
... (list-length '(1 2 3))
...

==> 3

; --- lambda with &rest used standalone ---

>>> ;;; lambda with &rest captures all args
... ((lambda (&rest args) args) 1 2 3)
...

==> (1 2 3)

; --- random zero edge cases ---

>>> ;;; random of 0 is an error (must be positive)
... (random 0)

%%% ERROR 'RANDOM': Argument expected to be positive.
%%% PRIMITIVE USAGE: (RANDOM integerOrFloat)
==>

>>> ;;; random of 0.0 is an error (must be positive)
... (random 0.0)

%%% ERROR 'RANDOM': Argument expected to be positive.
%%% PRIMITIVE USAGE: (RANDOM integerOrFloat)
==>

; --- make-symbol from string edge case ---

>>> ;;; make-symbol from ustring of symbol
... (make-symbol (ustring 'abc))
...

==> ABC

; --- car/cdr on empty list ---

>>> ;;; car of empty list is NIL
... (car '())
...

==> NIL

>>> ;;; cdr of empty list is NIL
... (cdr '())
...

==> NIL

; --- string repetition via * ---

>>> ;;; string times integer (Python repeat)
... (* "ab" 3)
...

==> "ababab"

; --- recursion-limit returns integer ---

>>> ;;; recursion-limit returns an integer
... (integerp (recursion-limit))
...

==> T

; --- boundp ---

>>> (setf bptest 42)
...

==> 42

>>> (boundp 'bptest)
...

==> T

>>> (boundp 'no-such-var-xyz)
...

==> NIL

; --- symbol-name ---

>>> (symbol-name 'hello)
...

==> "HELLO"

>>> (symbol-name 'car)
...

==> "CAR"

; --- error ---

>>> (error "something went wrong")

%%% something went wrong
==>

>>> ;;; error with positional format args
... (error "bad value: {0}" (list 42))

%%% bad value: 42
==>

>>> ;;; error with map format args
... (error "expected {TYPE} but got {GOT}" (make-dict (type "integer") (got "string")))

%%% expected integer but got string
==>

; ============================================================
; test23-coverage-gaps.lisp
; Edge case and happy path tests for under-tested features.
; ============================================================

; --- help happy path tests ---

>>> ;;; help with primitive shows usage and returns T
... (help +)
PRIMITIVE USAGE: (+ &rest numbers)

Returns the sum of numbers.
==> T

>>> ;;; help with another primitive
... (help car)
PRIMITIVE USAGE: (CAR list)

Returns the first item in a list.
==> T

>>> ;;; help with unknown topic string
... (help "unknown-topic")
Unknown topic: "UNKNOWN-TOPIC"
==> T

; --- alias macro tests ---

>>> ;;; alias creates a working alias for +
... (alias myadd23 +)

==> (PRIMITIVE + (&rest numbers) ...)

>>> ;;; alias for + works
... (myadd23 1 2)

==> 3

>>> ;;; alias creates a working alias for car
... (alias mycar23 car)

==> (PRIMITIVE CAR (list) ...)

>>> ;;; alias for car works
... (mycar23 '(1 2 3))

==> 1

; --- eval edge cases ---

>>> ;;; eval self-evaluating integer
... (eval 42)

==> 42

>>> ;;; eval self-evaluating string
... (eval "hello")

==> "hello"

>>> ;;; eval T symbol
... (eval 't)

==> T

>>> ;;; eval empty list (NIL)
... (eval '())

==> NIL

>>> ;;; eval special form through eval
... (eval '(if t 1 2))

==> 1

; --- parse edge cases ---

>>> ;;; parse quoted list
... (parse "'(1 2 3)")

==> (PROGN (QUOTE (1 2 3)))

; --- backquote edge cases ---

>>> ;;; backquote atom passthrough
... `42

==> 42

>>> ;;; backquote string passthrough
... `"hello"

==> "hello"

>>> ;;; backquote empty list
... `()

==> NIL

>>> ;;; splice empty list into backquote
... `(a ,@'() b)

==> (A B)

>>> ;;; splice non-empty list into backquote
... `(a ,@'(1 2) b)

==> (A 1 2 B)

; --- make-dict edge cases ---

>>> ;;; empty dict
... (make-dict)

==> (DICT
)

>>> ;;; empty dict is a dict
... (dictp (make-dict))

==> T

>>> ;;; dict values are evaluated
... (make-dict (a (+ 1 2)))

==> (DICT
   ("A" 3)
)

>>> ;;; at retrieves evaluated dict value
... (at 'a (make-dict (a (+ 1 2))))

==> 3

>>> ;;; dict with duplicate keys (last wins)
... (make-dict (a 1) (a 2))

==> (DICT
   ("A" 2)
)

>>> ;;; dict with integer keys
... (make-dict (1 "one") (2 "two"))

==> (DICT
   (1 "one")
   (2 "two")
)

>>> ;;; at with integer key on integer-keyed dict
... (at 1 (make-dict (1 "one") (2 "two")))

==> "one"

; --- at on strings ---

>>> ;;; at first char of string
... (at 0 "hello")

==> "h"

>>> ;;; at last char of string
... (at 4 "hello")

==> "o"

; --- sort edge cases ---

>>> ;;; sort with duplicates preserved
... (sort '(3 1 2 1) <)

==> (1 1 2 3)

>>> ;;; sort with rationals
... (sort (list 1/2 1/3 1/4) <)

==> (1/4 1/3 1/2)

>>> ;;; sort with mixed int/float
... (sort (list 1 2.5 3) <)

==> (1 2.5 3)

>>> ;;; sort empty list
... (sort '() <)

==> NIL

; --- predicate alias tests ---

>>> ;;; isFunction? on lambda returns T
... (isFunction? (lambda (x) x))

==> T

>>> ;;; isFunction? on integer returns NIL
... (isFunction? 42)

==> NIL

>>> ;;; isAtom? on integer returns T
... (isAtom? 42)

==> T

>>> ;;; isAtom? on list returns NIL
... (isAtom? '(1))

==> NIL

>>> ;;; isList? on list returns T
... (isList? '(1 2))

==> T

>>> ;;; isList? on integer returns NIL
... (isList? 42)

==> NIL

; --- math library edge cases ---

>>> ;;; evenp zero is even
... (evenp 0)

==> T

>>> ;;; oddp negative odd number
... (oddp -3)

==> T

>>> ;;; evenp type error on float
... (evenp 2.0)

%%% evenp: argument must be an integer

>>> ;;; oddp type error on float
... (oddp 3.0)

%%% oddp: argument must be an integer

>>> ;;; zerop float zero
... (zerop 0.0)

==> T

>>> ;;; minusp zero is not negative
... (minusp 0)

==> NIL

>>> ;;; plusp zero is not positive
... (plusp 0)

==> NIL

>>> ;;; minusp negative float
... (minusp -0.5)

==> T

>>> ;;; plusp positive rational
... (plusp 1/2)

==> T

; --- copy-list / copy-tree edge cases ---

>>> ;;; copy-list empty list
... (copy-list '())

==> NIL

>>> ;;; copy-tree empty list
... (copy-tree '())

==> NIL

>>> ;;; copy-tree atom passthrough
... (copy-tree 42)

==> 42

>>> ;;; copy-tree string passthrough
... (copy-tree "hello")

==> "hello"

>>> ;;; copy-tree independence: mutating original does not affect copy
... (let ((orig23 (list 1 (list 2 3) 4))
...       (cp23 (copy-tree (list 1 (list 2 3) 4))))
...    (setf (at 0 (at 1 orig23)) 99)
...    (at 1 cp23))

==> (2 3)

; --- mapcar with named functions ---

>>> ;;; mapcar with car extracts first elements
... (mapcar car '((1 2) (3 4) (5 6)))

==> (1 3 5)

>>> ;;; mapcar with cdr extracts tails
... (mapcar cdr '((1 2) (3 4)))

==> ((2) (4))

; --- apply / funcall edge cases ---

>>> ;;; apply with multiple prepended args
... (apply + 1 2 '(3 4))

==> 10

>>> ;;; apply with lambda
... (apply (lambda (a b) (+ a b)) '(3 7))

==> 10

>>> ;;; funcall with &rest lambda
... (funcall (lambda (&rest args) args) 1 2 3)

==> (1 2 3)

>>> ;;; funcall with library function reverse
... (funcall reverse '(3 2 1))

==> (1 2 3)

>>> ;;; funcall with library function list
... (funcall list 1 2 3)

==> (1 2 3)

; --- python edge cases ---

>>> ;;; python None returns None
... (python "None")

==> None

>>> ;;; python list expression returns Lisp list
... (python "[1,2,3]")

==> (1 2 3)

; --- equality across types ---

>>> ;;; int equals float
... (= 1 1.0)

==> T

>>> ;;; int equals rational
... (= 1 1/1)

==> T

>>> ;;; rational less than float
... (< 1/2 0.6)

==> T

; --- dotimes with negative count ---

>>> ;;; dotimes with negative count does zero iterations
... (dotimes (i -1) (write! i))

==> NIL

; --- multi-pair setf ---

>>> ;;; setf with multiple pairs
... (progn (setf sa23 10 sb23 20) (list sa23 sb23))

==> (10 20)

; --- struct interactions ---

>>> ;;; define a struct for interaction tests
... (defstruct point23 x y)

==> POINT23

>>> ;;; create a struct instance
... (setf pt23 (make-point23 :x 10 :y 20))

==> (DICT
   ("STRUCT-TYPE" POINT23)
   ("X" 10)
   ("Y" 20)
)

>>> ;;; read struct fields
... (point23-x pt23)

==> 10

>>> ;;; modify struct field via setf
... (setf (point23-x pt23) 99)

==> 99

>>> ;;; verify struct field was modified
... (point23-x pt23)

==> 99

>>> ;;; mapcar over struct accessor extracts field from list of structs
... (mapcar point23-x (list (make-point23 :x 1 :y 2) (make-point23 :x 3 :y 4)))

==> (1 3)

>>> ;;; closure capturing struct: lambda reads and modifies struct fields
... (let ((p23 (make-point23 :x 0 :y 0)))
...    (setf (point23-x p23) 42)
...    (point23-x p23))

==> 42

; --- string-capitalize ---

>>> (string-capitalize "hello world")
==> "Hello World"

>>> (string-capitalize "HELLO WORLD")
==> "Hello World"

>>> (string-capitalize "hello")
==> "Hello"

>>> (string-capitalize "")
==> ""

>>> (string-capitalize "already Capitalized words")
==> "Already Capitalized Words"

; --- gensym ---

>>> (symbolp (gensym))
==> T

>>> (symbolp (gensym "FOO"))
==> T

; each call returns a distinct symbol
>>> (let ((g1 (gensym)) (g2 (gensym)))
...   (not (eq g1 g2)))
==> T

; prefix is upper-cased like all symbols
>>> (let ((g (gensym "test")))
...   (string= (symbol-name g) (string-upcase (symbol-name g))))
==> T

; error: non-string prefix
>>> (gensym 42)
%%% ERROR 'GENSYM': Argument must be a string prefix.
%%% PRIMITIVE USAGE: (GENSYM &optional prefix)

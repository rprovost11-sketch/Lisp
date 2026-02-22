; ============================================================
; test23-coverage-gaps.lisp
; Edge case and happy path tests for under-tested features.
; ============================================================

; --- help happy path tests ---

>>> ;;; help with primitive shows usage and returns T
... (help +)
USAGE: (+ <number1> <number2> ...)

Returns the sum of numbers.
==> T

>>> ;;; help with another primitive
... (help car)
USAGE: (CAR <list>)

Returns the first item in a list.
==> T

>>> ;;; help with unknown topic string
... (help "unknown-topic")
Unknown topic: "UNKNOWN-TOPIC"
==> T

; --- alias macro tests ---

>>> ;;; alias creates a working alias for +
... (alias myadd23 +)

==> (+ <number1> <number2> ...)

>>> ;;; alias for + works
... (myadd23 1 2)

==> 3

>>> ;;; alias creates a working alias for car
... (alias mycar23 car)

==> (CAR <list>)

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

; --- map edge cases ---

>>> ;;; empty map
... (map)

==> (MAP
)

>>> ;;; empty map is a map
... (mapp (map))

==> T

>>> ;;; map values are evaluated
... (map (a (+ 1 2)))

==> (MAP
   ("A" 3)
)

>>> ;;; at retrieves evaluated map value
... (at 'a (map (a (+ 1 2))))

==> 3

>>> ;;; map with duplicate keys (last wins)
... (map (a 1) (a 2))

==> (MAP
   ("A" 2)
)

>>> ;;; map with integer keys
... (map (1 "one") (2 "two"))

==> (MAP
   (1 "one")
   (2 "two")
)

>>> ;;; at with integer key on integer-keyed map
... (at 1 (map (1 "one") (2 "two")))

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
... (sort '(3 1 2 1))

==> (1 1 2 3)

>>> ;;; sort with rationals
... (sort (list 1/2 1/3 1/4))

==> (1/4 1/3 1/2)

>>> ;;; sort with mixed int/float
... (sort (list 1 2.5 3))

==> (1 2.5 3)

>>> ;;; sort empty list
... (sort '())

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

==> (MAP
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

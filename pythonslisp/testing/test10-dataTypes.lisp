>>> (list (+ 1 3) '(apple pear "string") 22/7 'false)
...

==> (4 (APPLE PEAR "string") 22/7 FALSE)

>>> (first '(apple banana cake))
...

==> APPLE

>>> (rest '(apple banana cake donut))
...

==> (BANANA CAKE DONUT)

>>> (rest '(donkey))
...

==> NIL

>>> (cons 'apple '(banana orange pear))
...

==> (APPLE BANANA ORANGE PEAR)

>>> (remove 'orange '(apple banana orange pear))
...

==> (APPLE BANANA PEAR)

>>> (push! '(1 2 3 4) 6)
...

==> (1 2 3 4 6)

>>> (setf a '(1 2 3 4))
...

==> (1 2 3 4)

>>> a
...

==> (1 2 3 4)

>>> (pop! a)
...

==> 4

>>> a
...

==> (1 2 3)

>>> (setf mylist '("a" "b" "c" "d"))
...

==> ("a" "b" "c" "d")

>>> mylist
...

==> ("a" "b" "c" "d")

>>> (at 2 mylist)
...

==> "c"

>>> (setf (at 1 mylist) 1)
...

==> 1

>>> mylist
...

==> ("a" 1 "c" "d")

>>> (append mylist '(pi e))
...

==> ("a" 1 "c" "d" PI E)

>>> (hasvalue? '(a b c d) pi)
...

==> NIL

>>> (hasvalue? '(a b c d) 'c)
...

==> T

>>> (length nil)
...

==> 0

>>> (length '(a b c d))
...

==> 4

>>> (length "hello")
...

==> 5

>>> (reverse '(1 2 3 4 5))
...

==> (5 4 3 2 1)

>>> (setf mymap (make-dict ("a" 1) ("b" 2) ("c" 3) ) )
...

==> (MAP
   ("a" 1)
   ("b" 2)
   ("c" 3)
)

>>> mymap
...

==> (MAP
   ("a" 1)
   ("b" 2)
   ("c" 3)
)

>>> (at "b" mymap)
...

==> 2

>>> (setf (at "b" mymap) e)
...

==> 2.718281828459045

>>> mymap
...

==> (MAP
   ("a" 1)
   ("b" 2.718281828459045)
   ("c" 3)
)

>>> (hasValue? mymap 1)
...

==> T

>>> (hasValue? mymap 10)
...

==> NIL

>>> (update! mymap (make-dict ("b" 5) ("d" 10)))
...

==> (MAP
   ("a" 1)
   ("b" 5)
   ("c" 3)
   ("d" 10)
)

>>> mymap
...

==> (MAP
   ("a" 1)
   ("b" 5)
   ("c" 3)
   ("d" 10)
)

>>> (hasKey? mymap "a")
...

==> T

>>> (hasKey? mymap 3)
...

==> NIL

>>> ;;; Error: hasKey? requires a map as first argument
... (hasKey? 1 (make-dict))

%%% ERROR 'HASKEY?': Invalid argument 1.  Map expected.
%%% USAGE: (HASKEY? dict key)
==>

>>> ;;; Error: hasValue? requires a list or map as first argument
... (hasValue? 1 '(1 2))

%%% ERROR 'HASVALUE?': Invalid argument.  Argument 1 expected to be a list or map.
%%% USAGE: (HASVALUE? listOrDict value)
==>

>>> ;;; Error: update! requires map arguments
... (update! 1 (make-dict))

%%% ERROR 'UPDATE!': Argument 1 expected to be a map.
%%% USAGE: (UPDATE! dict1 dict2)
==>

>>> (update! (make-dict) 1)

%%% ERROR 'UPDATE!': Argument 2 expected to be a map.
%%% USAGE: (UPDATE! dict1 dict2)
==>

>>> (update!)

%%% ERROR 'UPDATE!': 2 arguments expected.
%%% USAGE: (UPDATE! dict1 dict2)
==>

; ========================
; type-of mainstream cases
; ------------------------

; nil → NULL
>>> (type-of nil)
...

==> NULL

; positive integer
>>> (type-of 42)
...

==> INTEGER

; negative integer
>>> (type-of -7)
...

==> INTEGER

; float
>>> (type-of 3.14)
...

==> FLOAT

; ratio (Fraction)
>>> (type-of (/ (rational 3) 4))
...

==> RATIO

; string
>>> (type-of "hello")
...

==> STRING

; symbol
>>> (type-of 'foo)
...

==> SYMBOL

; non-empty list
>>> (type-of '(1 2 3))
...

==> CONS

; plain dict no STRUCT-TYPE key DICT
>>> (type-of (make-dict (x 1) (y 2)))
...

==> DICT

; named function define it first, then query
>>> (defun fn14 (x) (* x x))
...

==> (FUNCTION FN14 (X) ... )

>>> (type-of fn14)
...

==> FUNCTION

; built-in macro
>>> (type-of when)
...

==> MACRO

; built-in primitive
>>> (type-of car)
...

==> PRIMITIVE

; ==================
; type-of edge cases
; ------------------

; T is a symbol, not a boolean
>>> (type-of t)
...

==> SYMBOL

; zero is still an integer
>>> (type-of 0)
...

==> INTEGER

; negative float
>>> (type-of -1.5)
...

==> FLOAT

; empty string
>>> (type-of "")
...

==> STRING

; quoted empty list is NIL → NULL type
>>> (type-of '())
...

==> NULL

; single-element list is still CONS
>>> (type-of (list 1))
...

==> CONS

; (rational 1) is 1/1 a Fraction, not an integer
>>> (type-of (rational 1))
...

==> RATIO

; anonymous lambda → FUNCTION
>>> (type-of (lambda (x) x))
...

==> FUNCTION

; result of type-of is always a symbol
>>> (symbolp (type-of 42))
...

==> T

; type-of on a type symbol → SYMBOL
>>> (type-of (type-of 42))
...

==> SYMBOL

; ===================
; type-of error cases
; -------------------

; zero arguments
>>> (type-of)
...

%%% ERROR 'TYPE-OF': 1 argument expected.
%%% USAGE: (TYPE-OF sexpr)
==>

; too many arguments
>>> (type-of 1 2)
...

%%% ERROR 'TYPE-OF': 1 argument expected.
%%% USAGE: (TYPE-OF sexpr)
==>

>>> (defstruct point (x 0) (y 0))
...

==> POINT

>>> (setf p0 (make-point))
...

==> (MAP
   ("STRUCT-TYPE" POINT)
   ("X" 0)
   ("Y" 0)
)

>>> (point-x p0)
...

==> 0

>>> (point-y p0)
...

==> 0

>>> (setf p1 (make-point :x 3 :y 4))
...

==> (MAP
   ("STRUCT-TYPE" POINT)
   ("X" 3)
   ("Y" 4)
)

>>> (point-x p1)
...

==> 3

>>> (point-y p1)
...

==> 4

>>> (setf p-partial (make-point :x 5))
...

==> (MAP
   ("STRUCT-TYPE" POINT)
   ("X" 5)
   ("Y" 0)
)

>>> (point-x p-partial)
...

==> 5

>>> (point-y p-partial)
...

==> 0

>>> (point-p p0)
...

==> T

>>> (point-p p1)
...

==> T

>>> (point-p 42)
...

==> NIL

>>> (point-p "hello")
...

==> NIL

>>> (point-p nil)
...

==> NIL

>>> (point-p '(1 2 3))
...

==> NIL

>>> (setf (point-x p1) 10)
...

==> 10

>>> (point-x p1)
...

==> 10

>>> (setf (point-y p1) 99)
...

==> 99

>>> (point-y p1)
...

==> 99

>>> (setf p2 (copy-point p1))
...

==> (MAP
   ("STRUCT-TYPE" POINT)
   ("X" 10)
   ("Y" 99)
)

>>> (setf (point-x p1) 0)
...

==> 0

>>> (point-x p1)
...

==> 0

>>> (point-x p2)
...

==> 10

>>> (setf (point-y p2) 1000)
...

==> 1000

>>> (point-y p2)
...

==> 1000

>>> (point-y p1)
...

==> 99

>>> (defstruct node value next)
...

==> NODE

>>> (setf n1 (make-node :value 42))
...

==> (MAP
   ("NEXT" NIL)
   ("STRUCT-TYPE" NODE)
   ("VALUE" 42)
)

>>> (node-value n1)
...

==> 42

>>> (node-next n1)
...

==> NIL

>>> (setf (node-next n1) 'done)
...

==> DONE

>>> (node-next n1)
...

==> DONE

>>> (defstruct person name age)
...

==> PERSON

>>> (setf bob (make-person :name "Bob" :age 30))
...

==> (MAP
   ("AGE" 30)
   ("NAME" "Bob")
   ("STRUCT-TYPE" PERSON)
)

>>> (person-name bob)
...

==> "Bob"

>>> (person-age bob)
...

==> 30

>>> (person-p bob)
...

==> T

>>> (point-p bob)
...

==> NIL

>>> (person-p p1)
...

==> NIL

>>> (person-p p0)
...

==> NIL

>>> (defstruct labeled (tag "none") (count 0))
...

==> LABELED

>>> (setf lbl (make-labeled))
...

==> (MAP
   ("COUNT" 0)
   ("STRUCT-TYPE" LABELED)
   ("TAG" "none")
)

>>> (labeled-tag lbl)
...

==> "none"

>>> (setf (labeled-tag lbl) "active")
...

==> "active"

>>> (labeled-tag lbl)
...

==> "active"

; type-of for struct instances
>>> (type-of p0)
...

==> POINT

>>> (type-of p1)
...

==> POINT

>>> (type-of bob)
...

==> PERSON

>>> (type-of n1)
...

==> NODE

>>> (type-of lbl)
...

==> LABELED

>>> (point-x 42)
...

%%% ERROR 'AT': Invalid argument.  List, Map, or String expected.
%%% USAGE: (AT keyOrIndex dictListOrStr)
==>

>>> (setf (point-x 42) 5)
...

%%% ERROR 'SET-ACCESSOR!': Argument 2 must be a struct instance.
%%% USAGE: (SET-ACCESSOR! accessor-symbol instance newValue)
==>

>>> (setf (point-x p1 p1) 5)
...

%%% setf: struct accessor place must have exactly 1 instance argument
==>

>>> (make-point :z 99)
...

%%% Error binding arguments in call to function "MAKE-POINT".
%%% Unexpected keyword found Z.
==>


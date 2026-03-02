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

>>> (hasvalue? pi '(a b c d))
...

==> NIL

>>> (hasvalue? 'c '(a b c d))
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

==> (DICT
   ("a" 1)
   ("b" 2)
   ("c" 3)
)

>>> mymap
...

==> (DICT
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

==> (DICT
   ("a" 1)
   ("b" 2.718281828459045)
   ("c" 3)
)

>>> (hasValue? 1 mymap)
...

==> T

>>> (hasValue? 10 mymap)
...

==> NIL

>>> (update! mymap (make-dict ("b" 5) ("d" 10)))
...

==> (DICT
   ("a" 1)
   ("b" 5)
   ("c" 3)
   ("d" 10)
)

>>> mymap
...

==> (DICT
   ("a" 1)
   ("b" 5)
   ("c" 3)
   ("d" 10)
)

>>> (hasKey? "a" mymap)
...

==> T

>>> (hasKey? 3 mymap)
...

==> NIL

>>> ;;; Error: hasKey? requires a map as second argument
... (hasKey? "a" 1)

%%% ERROR 'HASKEY?': Invalid argument 2.  Dict expected.
%%% PRIMITIVE USAGE: (HASKEY? key dict)
==>

>>> ;;; Error: hasValue? requires a list or map as second argument
... (hasValue? 2 1)

%%% ERROR 'HASVALUE?': Invalid argument.  Argument 2 expected to be a list or dict.
%%% PRIMITIVE USAGE: (HASVALUE? value listOrDict)
==>

>>> ;;; Error: update! requires map arguments
... (update! 1 (make-dict))

%%% ERROR 'UPDATE!': Argument 1 expected to be a dict.
%%% PRIMITIVE USAGE: (UPDATE! dict1 dict2)
==>

>>> (update! (make-dict) 1)

%%% ERROR 'UPDATE!': Argument 2 expected to be a dict.
%%% PRIMITIVE USAGE: (UPDATE! dict1 dict2)
==>

>>> (update!)

%%% ERROR 'UPDATE!': 2 arguments expected.
%%% PRIMITIVE USAGE: (UPDATE! dict1 dict2)
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

==> (FUNCTION FN14 (X) ...)

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
%%% PRIMITIVE USAGE: (TYPE-OF sexpr)
==>

; too many arguments
>>> (type-of 1 2)
...

%%% ERROR 'TYPE-OF': 1 argument expected.
%%% PRIMITIVE USAGE: (TYPE-OF sexpr)
==>

>>> (defstruct point (x 0) (y 0))
...

==> POINT

>>> (setf p0 (make-point))
...

==> (DICT
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

==> (DICT
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

==> (DICT
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

==> (DICT
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

==> (DICT
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

==> (DICT
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

==> (DICT
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

%%% ERROR 'AT': Invalid argument.  List, Dict, or String expected.
%%% PRIMITIVE USAGE: (AT keyOrIndex dictListOrStr)
==>

>>> (setf (point-x 42) 5)
...

%%% ERROR 'SET-ACCESSOR!': Argument 2 must be a struct instance.
%%% PRIMITIVE USAGE: (SET-ACCESSOR! accessor-symbol instance newValue)
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


; --- type-of: stream subtypes ---

>>> (type-of (make-string-output-stream))
==> STRING-STREAM

>>> (type-of (make-string-input-stream "hello"))
==> STRING-STREAM

; --- file-stream-p / string-stream-p ---

>>> (string-stream-p (make-string-output-stream))
==> T

>>> (file-stream-p (make-string-output-stream))
==> NIL

>>> (string-stream-p (make-string-input-stream "hi"))
==> T

>>> (string-stream-p 42)
==> NIL

>>> (file-stream-p 42)
==> NIL

; --- typep: basic types ---

>>> (typep 42 'integer)
==> T

>>> (typep 3.14 'float)
==> T

>>> (typep 1/3 'ratio)
==> T

>>> (typep 42 'rational)
==> T

>>> (typep 1/3 'rational)
==> T

>>> (typep 42 'number)
==> T

>>> (typep 3.14 'real)
==> T

>>> (typep "hello" 'string)
==> T

>>> (typep 'foo 'symbol)
==> T

>>> (typep nil 'null)
==> T

>>> (typep nil 'list)
==> T

>>> (typep (list 1 2) 'cons)
==> T

>>> (typep nil 'atom)
==> T

>>> (typep t 'boolean)
==> T

>>> (typep nil 'boolean)
==> T

>>> (typep 42 'boolean)
==> NIL

>>> (typep (make-string-output-stream) 'stream)
==> T

>>> (typep (make-string-output-stream) 'string-stream)
==> T

>>> (typep (make-string-output-stream) 'file-stream)
==> NIL

>>> (typep 42 't)
==> T

>>> (typep 42 'nil)
==> NIL

; --- typep: error ---

>>> (typep 42 "not-a-symbol")
%%% ERROR 'TYPEP': Argument 2 must be a type symbol.
%%% PRIMITIVE USAGE: (TYPEP object type-specifier)

; --- typecase ---

>>> (typecase 42
...   (string "str")
...   (integer "int")
...   (otherwise "other"))
==> "int"

>>> (typecase "hi"
...   (integer "int")
...   (string "str"))
==> "str"

>>> (typecase nil
...   (integer "int")
...   (null "null"))
==> "null"

; no matching clause returns nil
>>> (typecase 3.14
...   (integer "int")
...   (string "str"))
==> NIL

; t as catch-all
>>> (typecase 'foo
...   (integer "int")
...   (t "other"))
==> "other"

; --- etypecase ---

>>> (etypecase 42
...   (integer "int")
...   (string "str"))
==> "int"

>>> (etypecase "hello"
...   (integer "int")
...   (string "str"))
==> "str"

>>> (etypecase 3.14
...   (integer "int")
...   (string "str"))
%%% etypecase: no matching clause for value: 3.14

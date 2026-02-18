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

>>> (point-x 42)
...

%%% ERROR 'AT': Invalid argument.  List, Map, or String expected.
%%% USAGE: (AT <keyOrIndex> <mapListOrStr>)
==>

>>> (setf (point-x 42) 5)
...

%%% ERROR 'SETF': Expected a struct instance as argument to (POINT-X ...).
%%% USAGE: (SETF <symbol> <sexpr>)
==>

>>> (setf (point-x p1 p1) 5)
...

%%% ERROR 'SETF': Struct accessor setf expects exactly 1 instance argument, got 2.
%%% USAGE: (SETF <symbol> <sexpr>)
==>

>>> (make-point :z 99)
...

%%% Error binding arguments in call to function "MAKE-POINT".
%%% Unexpected keyword found Z.
==>


>>> (setf var1 35)
...

==> 35

>>> var1
...

==> 35

>>> (+ var1 5)
...

==> 40

>>> (+ 5 var1)
...

==> 40

>>> (setf var2 6.2)
...

==> 6.2

>>> var2
...

==> 6.2

>>> (- var2 var1)
...

==> -28.8

>>> var1
...

==> 35

>>> (setf myList '(apple banana orange cherry))
...

==> (APPLE BANANA ORANGE CHERRY)

>>> myList
...

==> (APPLE BANANA ORANGE CHERRY)

>>> (first myList)
...

==> APPLE

>>> (rest myList)
...

==> (BANANA ORANGE CHERRY)

>>> (if (= (first myList) 'apple)
... "It's true!")
...

==> "It's true!"

>>> (setf var1 "some string value")
...

==> "some string value"

>>> var1
...

==> "some string value"

>>> (setf var1 "some string value")
...

==> "some string value"

>>> var1
...

==> "some string value"

>>> (setf var1 "some string value")
...

==> "some string value"

>>> var1
...

==> "some string value"

>>> (let ()
...    (- var2)
...    (if (= var1 "my value")
...        (* 7 3)
...        (writeLn! var1)
...        )
...    var1
...    )
...
"some string value"

==> "some string value"

>>> (let ( (var1 -22/7) )
...    (writeLn! var1)
...    )
...
-22/7

==> -22/7

>>> var1
...

==> "some string value"

>>> var3
...

%%% Unbound Variable: VAR3.
==>

>>> (let ( (var3 "true") )
...    (writeLn! var3)
...    )
...
"true"

==> "true"

>>> var3
...

%%% Unbound Variable: VAR3.
==>

>>> (setf var1 15)
...

==> 15

>>> (setf var2 2)
...

==> 2

>>> (makunbound 'var3)
...

==> NIL

>>> (let ( (var3 'var3) )
...    (writeLn! var1)
...    (writeLn! var2)
...    (writeLn! var3)
...    (setf var2 -22/7)
...    (setf var3 "a value")
...    (writeLn! var1)
...    (writeLn! var2)
...    (writeLn! var3)
...    (let ( (var3 T) )
...       (writeLn! var1)
...       (writeLn! var2)
...       (writeLn! var3)
...       (symTab!)
...       )
...    )
...
15
2
VAR3
15
-22/7
"a value"
15
-22/7
T
==> NIL

>>> var1
...

==> 15

>>> var2
...

==> -22/7

>>> var3
...

%%% Unbound Variable: VAR3.
==>

>>> ;;; Error: makunbound requires exactly one symbol argument
... (makunbound 1 2)

%%% ERROR 'MAKUNBOUND': 1 argument expected.
%%% PRIMITIVE USAGE: (MAKUNBOUND symbol)
==>

>>> (makunbound 1)

%%% ERROR 'MAKUNBOUND': Argument expected to be a symbol.
%%% PRIMITIVE USAGE: (MAKUNBOUND symbol)
==>

>>> ;;; undef! is an alias for makunbound
... (setf _tmp_var_ 99)

==> 99

>>> (undef! '_tmp_var_)
...

==> NIL

>>> ;;; Error: setf requires an even number of arguments
... (setf myvar1 10 myvar2)

%%% Too few positional arguments.
==>

; --- defparameter ---

>>> (defparameter dp-x 42)
==> 42

>>> dp-x
==> 42

>>> ;;; defparameter always rebinds
... (defparameter dp-x 99)
==> 99

>>> dp-x
==> 99

; --- defvar ---

>>> (defvar dv-x 10)
==> NIL

>>> dv-x
==> 10

>>> ;;; defvar does NOT rebind if already bound
... (defvar dv-x 999)
==> NIL

>>> dv-x
==> 10

>>> ;;; defvar with no value initializes to nil
... (defvar dv-y)
==> NIL

>>> dv-y
==> NIL

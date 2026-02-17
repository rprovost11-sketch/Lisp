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

>>> (undef! var3)
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

>>> ;;; Error: undef! requires exactly one symbol argument
... (undef! 1 2)

%%% ERROR 'UNDEF!': 1 argument expected.
%%% USAGE: (UNDEF! <symbol>)
==>

>>> (undef! 1)

%%% ERROR 'UNDEF!': Argument expected to be a symbol.
%%% USAGE: (UNDEF! <symbol>)
==>

>>> ;;; Error: setf requires an even number of arguments
... (setf myvar1 10 myvar2)

%%% ERROR 'SETF': An even number of arguments is expected.  Received 3.
%%% USAGE: (SETF <symbol> <sexpr>)
==>


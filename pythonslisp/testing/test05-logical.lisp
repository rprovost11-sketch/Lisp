>>> (not 1)
...

==> NIL

>>> (not symbol)
...

==> NIL

>>> (not "string")
...

==> NIL

>>> (not "")
...

==> NIL

>>> (not nil)
...

==> T

>>> (not 1)
...

==> NIL

>>> (and t t t )
...

==> T

>>> (and t NIL t t t)
...

==> NIL

>>> (or nil nil nil nil)
...

==> NIL

>>> (or t nil nil nil)
...

==> T

>>> (not (and t t))
...

==> NIL

>>> (not (and t nil))
...

==> T

>>> (and 1 2 3)
...

==> 3

>>> (and 1 nil 3)
...

==> NIL

>>> (or nil 2)
...

==> 2

>>> (or nil nil)
...

==> NIL

>>> (or nil nil 3)
...

==> 3

>>> (if nil 42)
...

==> NIL

>>> (if nil 42 99)
...

==> 99

>>> (if t 1)
...

==> 1


>>> (and nil (writeLn! "should not print"))
...

==> NIL

>>> (and t (writeLn! "should print"))
"should print"

==> "should print"

>>> (or t (writeLn! "should not print"))
...

==> T

>>> (or nil (writeLn! "should print"))
"should print"

==> "should print"

>>> (and (writeLn! "first") nil (writeLn! "third"))
"first"

==> NIL

>>> (or (writeLn! "first") t (writeLn! "third"))
"first"

==> "first"


>>> ;;; Error: not requires exactly one argument
... (not)

%%% ERROR 'NOT': 1 argument expected.
%%% USAGE: (NOT <boolean>)
==>

>>> (not 1 2)

%%% ERROR 'NOT': 1 argument expected.
%%% USAGE: (NOT <boolean>)
==>

; Single-arg: returns the value of the form
>>> (and 42)
==> 42

>>> (or 42)
==> 42

; Zero-arg: (and) = T, (or) = NIL
>>> (and)
==> T

>>> (or)
==> NIL

; and returns value of last form (not just T)
>>> (and 1 2)
==> 2

; or returns value of first truthy form (not just T)
>>> (or nil 99)
==> 99


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

==> T

>>> (and 1 nil 3)
...

==> NIL

>>> (or nil 2)
...

==> T

>>> (or nil nil)
...

==> NIL

>>> (or nil nil 3)
...

==> T

>>> (if nil 42)
...

==> NIL

>>> (if nil 42 99)
...

==> 99

>>> (if t 1)
...

==> 1


;;; Test short-circuiting
>>> (and nil (writeLn! "should not print"))
...

==> NIL

>>> (and t (writeLn! "should print"))
"should print"

==> T

>>> (or t (writeLn! "should not print"))
...

==> T

>>> (or nil (writeLn! "should print"))
"should print"

==> T

>>> (and (writeLn! "first") nil (writeLn! "third"))
"first"

==> NIL

>>> (or (writeLn! "first") t (writeLn! "third"))
"first"

==> T


>>> ;;; Error: not requires exactly one argument
... (not)

%%% ERROR 'NOT': 1 argument expected.
%%% USAGE: (NOT <boolean>)
==>

>>> (not 1 2)

%%% ERROR 'NOT': 1 argument expected.
%%% USAGE: (NOT <boolean>)
==>

>>> ;;; Error: and/or require at least two arguments
... (and 1)

%%% ERROR 'AND': 2 or more arguments expected.
%%% USAGE: (AND <boolean1> <boolean2> ...)
==>

>>> (or 1)

%%% ERROR 'OR': 2 or more arguments expected.
%%% USAGE: (OR <boolean1> <boolean2> ...)
==>


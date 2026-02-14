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


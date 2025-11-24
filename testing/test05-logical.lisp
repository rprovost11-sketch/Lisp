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

>>> (and 1 1 1 )
...

==> T

>>> (and 1 0 1 1 1)
...

==> NIL

>>> (or 0 0 0 0)
...

==> NIL

>>> (or 1 0 0 0)
...

==> T

>>> (not (and 1 1))
...

==> NIL

>>> (not (and 1 0))
...

==> T


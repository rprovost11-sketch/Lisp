>>> (null 1)
...

==> NIL

>>> (null 6.7)
...

==> NIL

>>> (null nil)
...

==> T

>>> (null nan)
...

==> NIL

>>> (numberp 15)
...

==> T

>>> (numberp -3.8)
...

==> T

>>> (numberp pi)
...

==> T

>>> (numberp 2/3)
...

==> T

>>> (numberp aRandomSymbol)
...

==> NIL

>>> (numberp "hello")
...

==> NIL

>>> (symbolp x)
...

==> T

>>> (symbolp 13)
...

==> NIL

>>> (symbolp nil)
...

==> NIL

>>> (symbolp e)
...

==> NIL

>>> (symbolp xy)
...

==> T

>>> (atom nil)
...

==> T

>>> (atom 0)
...

==> T

>>> (atom 3/4)
...

==> T

>>> (numberp nil)
...

==> NIL

>>> (numberp pi)
...

==> T

>>> (numberp e)
...

==> T

>>> (numberp -2/3)
...

==> T

>>> (numberp 5.8)
...

==> T

>>> (numberp "junky string")
...

==> NIL

>>> (numberp randomSymbol)
...

==> NIL

>>> (numberp '())
...

==> NIL

>>> (atom nil)
...

==> T

>>> (atom e)
...

==> T

>>> (atom pi)
...

==> T

>>> (atom 1/2)
...

==> T

>>> (atom 2.8)
...

==> T

>>> (atom aSymbol)
...

==> T

>>> (atom "hello")
...

==> T

>>> (atom '())
...

==> T

>>> (listp nil)
...

==> T

>>> (listp 0)
...

==> NIL

>>> (listp pi)
...

==> NIL

>>> (listp -5/7)
...

==> NIL

>>> (listp 62.3)
...

==> NIL

>>> (listp '())
...

==> T

>>> (isMap? nil)
...

==> NIL

>>> (isMap? 0)
...

==> NIL

>>> (isMap? 0)
...

==> NIL

>>> (isMap? pi)
...

==> NIL

>>> (isMap? 7)
...

==> NIL

>>> (isMap? 52/3)
...

==> NIL

>>> (isMap? '())
...

==> NIL

>>> (isMap? "something")
...

==> NIL

>>> (stringp nil)
...

==> NIL

>>> (stringp 1)
...

==> NIL

>>> (stringp pi)
...

==> NIL

>>> (stringp 3)
...

==> NIL

>>> (stringp 4.7)
...

==> NIL

>>> (stringp 8/9)
...

==> NIL

>>> (stringp someSymbol)
...

==> NIL

>>> (stringp "some string")
...

==> T

>>> (stringp '())
...

==> NIL

>>> (functionp nil)
...

==> NIL

>>> (functionp 0)
...

==> NIL

>>> (functionp e)
...

==> NIL

>>> (functionp 2)
...

==> NIL

>>> (functionp -8.49)
...

==> NIL

>>> (functionp 7/8)
...

==> NIL

>>> (functionp aSymbol)
...

==> NIL

>>> (functionp "a string")
...

==> NIL

>>> (functionp '())
...

==> NIL



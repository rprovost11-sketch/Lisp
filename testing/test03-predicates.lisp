>>> (isNil? 1)
...

==> NIL

>>> (isNil? 6.7)
...

==> NIL

>>> (isNil? nil)
...

==> T

>>> (isNil? nan)
...

==> NIL

>>> (isNumber? 15)
...

==> T

>>> (isNumber? -3.8)
...

==> T

>>> (isNumber? pi)
...

==> T

>>> (isNumber? 2/3)
...

==> T

>>> (isNumber? aRandomSymbol)
...

==> NIL

>>> (isNumber? "hello")
...

==> NIL

>>> (isSymbol? x)
...

==> T

>>> (isSymbol? 13)
...

==> NIL

>>> (isSymbol? nil)
...

==> NIL

>>> (isSymbol? e)
...

==> NIL

>>> (isSymbol? xy)
...

==> T

>>> (isAtom? nil)
...

==> NIL

>>> (isAtom? 0)
...

==> T

>>> (isAtom? 3/4)
...

==> T

>>> (isNumber? nil)
...

==> NIL

>>> (isNumber? pi)
...

==> T

>>> (isNumber? e)
...

==> T

>>> (isNumber? -2/3)
...

==> T

>>> (isNumber? 5.8)
...

==> T

>>> (isNumber? "junky string")
...

==> NIL

>>> (isNumber? randomSymbol)
...

==> NIL

>>> (isNumber? '())
...

==> NIL

>>> (isAtom? nil)
...

==> NIL

>>> (isAtom? e)
...

==> T

>>> (isAtom? pi)
...

==> T

>>> (isAtom? 1/2)
...

==> T

>>> (isAtom? 2.8)
...

==> T

>>> (isAtom? aSymbol)
...

==> NIL

>>> (isAtom? "hello")
...

==> T

>>> (isAtom? '())
...

==> NIL

>>> (isList? nil)
...

==> T

>>> (isList? 0)
...

==> NIL

>>> (isList? pi)
...

==> NIL

>>> (isList? -5/7)
...

==> NIL

>>> (isList? 62.3)
...

==> NIL

>>> (isList? '())
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

>>> (isString? nil)
...

==> NIL

>>> (isString? 1)
...

==> NIL

>>> (isString? pi)
...

==> NIL

>>> (isString? 3)
...

==> NIL

>>> (isString? 4.7)
...

==> NIL

>>> (isString? 8/9)
...

==> NIL

>>> (isString? someSymbol)
...

==> NIL

>>> (isString? "some string")
...

==> T

>>> (isString? '())
...

==> NIL

>>> (isFunction? nil)
...

==> NIL

>>> (isFunction? 0)
...

==> NIL

>>> (isFunction? e)
...

==> NIL

>>> (isFunction? 2)
...

==> NIL

>>> (isFunction? -8.49)
...

==> NIL

>>> (isFunction? 7/8)
...

==> NIL

>>> (isFunction? aSymbol)
...

==> NIL

>>> (isFunction? "a string")
...

==> NIL

>>> (isFunction? '())
...

==> NIL



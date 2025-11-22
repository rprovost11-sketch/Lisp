>>> (isNil? 1)
...

==> 0

>>> (isNil? 6.7)
...

==> 0

>>> (isNil? nil)
...

==> 1

>>> (isNil? nan)
...

==> 0

>>> (isNumber? 15)
...

==> 1

>>> (isNumber? -3.8)
...

==> 1

>>> (isNumber? pi)
...

==> 1

>>> (isNumber? 2/3)
...

==> 1

>>> (isNumber? aRandomSymbol)
...

==> 0

>>> (isNumber? "hello")
...

==> 0

>>> (isSymbol? x)
...

==> 1

>>> (isSymbol? 13)
...

==> 0

>>> (isSymbol? nil)
...

==> 0

>>> (isSymbol? e)
...

==> 0

>>> (isSymbol? xy)
...

==> 1

>>> (isAtom? nil)
...

==> 0

>>> (isAtom? 0)
...

==> 1

>>> (isAtom? 3/4)
...

==> 1

>>> (isNumber? nil)
...

==> 0

>>> (isNumber? pi)
...

==> 1

>>> (isNumber? e)
...

==> 1

>>> (isNumber? -2/3)
...

==> 1

>>> (isNumber? 5.8)
...

==> 1

>>> (isNumber? "junky string")
...

==> 0

>>> (isNumber? randomSymbol)
...

==> 0

>>> (isNumber? '())
...

==> 0

>>> (isAtom? nil)
...

==> 0

>>> (isAtom? e)
...

==> 1

>>> (isAtom? pi)
...

==> 1

>>> (isAtom? 1/2)
...

==> 1

>>> (isAtom? 2.8)
...

==> 1

>>> (isAtom? aSymbol)
...

==> 0

>>> (isAtom? "hello")
...

==> 1

>>> (isAtom? '())
...

==> 0

>>> (isList? nil)
...

==> 1

>>> (isList? 0)
...

==> 0

>>> (isList? pi)
...

==> 0

>>> (isList? -5/7)
...

==> 0

>>> (isList? 62.3)
...

==> 0

>>> (isList? '())
...

==> 1

>>> (isMap? nil)
...

==> 0

>>> (isMap? 0)
...

==> 0

>>> (isMap? 0)
...

==> 0

>>> (isMap? pi)
...

==> 0

>>> (isMap? 7)
...

==> 0

>>> (isMap? 52/3)
...

==> 0

>>> (isMap? '())
...

==> 0

>>> (isMap? "something")
...

==> 0

>>> (isString? nil)
...

==> 0

>>> (isString? 1)
...

==> 0

>>> (isString? pi)
...

==> 0

>>> (isString? 3)
...

==> 0

>>> (isString? 4.7)
...

==> 0

>>> (isString? 8/9)
...

==> 0

>>> (isString? someSymbol)
...

==> 0

>>> (isString? "some string")
...

==> 1

>>> (isString? '())
...

==> 0

>>> (isFunction? nil)
...

==> 0

>>> (isFunction? 0)
...

==> 0

>>> (isFunction? e)
...

==> 0

>>> (isFunction? 2)
...

==> 0

>>> (isFunction? -8.49)
...

==> 0

>>> (isFunction? 7/8)
...

==> 0

>>> (isFunction? aSymbol)
...

==> 0

>>> (isFunction? "a string")
...

==> 0

>>> (isFunction? '())
...

==> 0



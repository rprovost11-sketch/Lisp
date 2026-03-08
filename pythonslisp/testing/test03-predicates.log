>>> (null 1)
...

==> NIL

>>> (null 6.7)
...

==> NIL

>>> (null nil)
...

==> T

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

>>> (numberp "hello")
...

==> NIL

>>> (symbolp 'x)
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

>>> (symbolp 'xy)
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

>>> (numberp 'randomSymbol)
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

>>> (atom 'aSymbol)
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

>>> (isDict? nil)
...

==> NIL

>>> (isDict? 0)
...

==> NIL

>>> (isDict? 0)
...

==> NIL

>>> (isDict? pi)
...

==> NIL

>>> (isDict? 7)
...

==> NIL

>>> (isDict? 52/3)
...

==> NIL

>>> (isDict? '())
...

==> NIL

>>> (isDict? "something")
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

>>> (stringp 'someSymbol)
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

>>> (functionp 'aSymbol)
...

==> NIL

>>> (functionp "a string")
...

==> NIL

>>> (functionp '())
...

==> NIL

>>> (functionp (lambda (x) x))
...

==> T

>>> (functionp list)
...

==> T

>>> (functionp reverse)
...

==> T

>>> (integerp 42)
...

==> T

>>> (integerp 3.14)
...

==> NIL

>>> (integerp 1/2)
...

==> NIL

>>> (rationalp 1/3)
...

==> T

>>> (rationalp 42)
...

==> T

>>> (rationalp 3.14)
...

==> NIL

>>> (floatp 3.14)
...

==> T

>>> (floatp 42)
...

==> NIL

>>> (floatp 1/2)
...

==> NIL

>>> (macrop when)
...

==> T

>>> (macrop incf)
...

==> T

>>> (macrop defstruct)
...

==> T

>>> (macrop car)
...

==> NIL

>>> (macrop +)
...

==> NIL

>>> (isInteger? 5)
...

==> T

>>> (isRational? 1/4)
...

==> T

>>> (isFloat? 2.5)
...

==> T

>>> (isNil? nil)
...

==> T

>>> (isNil? t)
...

==> NIL

>>> (isNumber? 42)
...

==> T

>>> (isNumber? "hello")
...

==> NIL

>>> (isString? "abc")
...

==> T

>>> (isSymbol? 'foo)
...

==> T



>>> ;;; Error: predicates require exactly one argument
... (numberp)

%%% ERROR 'NUMBERP': 1 argument expected.
%%% PRIMITIVE USAGE: (NUMBERP sexpr)
==>

>>> (numberp 1 2)

%%% ERROR 'NUMBERP': 1 argument expected.
%%% PRIMITIVE USAGE: (NUMBERP sexpr)
==>

>>> (integerp)

%%% ERROR 'INTEGERP': 1 argument expected.
%%% PRIMITIVE USAGE: (INTEGERP sexpr)
==>

>>> (floatp)

%%% ERROR 'FLOATP': 1 argument expected.
%%% PRIMITIVE USAGE: (FLOATP sexpr)
==>

>>> (rationalp)

%%% ERROR 'RATIONALP': 1 argument expected.
%%% PRIMITIVE USAGE: (RATIONALP sexpr)
==>

>>> (symbolp)

%%% ERROR 'SYMBOLP': 1 argument expected.
%%% PRIMITIVE USAGE: (SYMBOLP sexpr)
==>

>>> (listp)

%%% ERROR 'LISTP': 1 argument expected.
%%% PRIMITIVE USAGE: (LISTP sexpr)
==>

>>> (stringp)

%%% ERROR 'STRINGP': 1 argument expected.
%%% PRIMITIVE USAGE: (STRINGP sexpr)
==>

>>> (functionp)

%%% ERROR 'FUNCTIONP': 1 argument expected.
%%% PRIMITIVE USAGE: (FUNCTIONP sexpr)
==>

>>> (macrop)

%%% ERROR 'MACROP': 1 argument expected.
%%% PRIMITIVE USAGE: (MACROP sexpr)
==>

; --- consp ---

>>> (consp '(1 2 3))
==> T

>>> (consp '(1))
==> T

>>> (consp nil)
==> NIL

>>> (consp '())
==> NIL

>>> (consp 42)
==> NIL

>>> (consp "hello")
==> NIL

>>> (consp 'foo)
==> NIL

>>> (consp (cons 1 '(2)))
==> T

>>> ;;; consp error: no args
... (consp)

%%% ERROR 'CONSP': 1 argument expected.
%%% PRIMITIVE USAGE: (CONSP sexpr)
==>

>>> ;;; consp error: too many args
... (consp 1 2)

%%% ERROR 'CONSP': 1 argument expected.
%%% PRIMITIVE USAGE: (CONSP sexpr)
==>

; --- notany / notevery ---

>>> (notany oddp '(2 4 6))
==> T

>>> (notany oddp '(1 2 3))
==> NIL

>>> (notany oddp '())
==> T

>>> (notevery oddp '(1 3 5))
==> NIL

>>> (notevery oddp '(1 2 3))
==> T

>>> (notevery oddp '())
==> NIL

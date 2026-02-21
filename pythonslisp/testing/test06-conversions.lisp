>>> (float 3)
...

==> 3.0

>>> (float 1/3)
...

==> 0.3333333333333333

>>> (float "0.9")
...

==> 0.9

>>> (float -1/4)
...

==> -0.25

>>> (integer 6.982)

==> 6

>>> (integer "24")

==> 24

>>> (string 3.5 "Hello" 1/2 )
...

==> "3.5Hello1/2"

>>> (list pi 3/4 'asymbol "a string")
...

==> (3.141592653589793 3/4 ASYMBOL "a string")

>>> (rational 0.5)
...

==> 1/2

>>> (rational 3)
...

==> 3/1

>>> (rational 3/4)
...

==> 3/4

>>> (rational 1.5)
...

==> 3/2

==> "FOO"

>>> (string "hello")
...

==> "hello"

>>> (symbol "hello")
...

==> HELLO

>>> (symbol "MAKE-" "POINT")
...

==> MAKE-POINT


>>> ;;; Error: float rejects non-numeric strings
... (float "hello")

%%% ERROR 'FLOAT': Invalid argument.
%%% USAGE: (FLOAT <number>)
==>

>>> ;;; Error: rational requires exactly one argument
... (rational)

%%% ERROR 'RATIONAL': Exactly 1 argument expected.
%%% USAGE: (RATIONAL <number>)
==>

>>> (rational 1 2)

%%% ERROR 'RATIONAL': Exactly 1 argument expected.
%%% USAGE: (RATIONAL <number>)
==>

>>> ;;; Error: string/symbol require at least one argument
... (string)

%%% ERROR 'STRING': 1 or more arguments expected.
%%% USAGE: (STRING <object1> <object2> ...)
==>

>>> (symbol)

%%% ERROR 'SYMBOL': 1 or more string argument expected.
%%% USAGE: (SYMBOL <string1> <string2> ...)
==>


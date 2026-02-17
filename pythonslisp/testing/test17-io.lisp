>>> (write! 42)
42
==> 42

>>> (write! "hello")
"hello"
==> "hello"

>>> (writeLn! 42)
42

==> 42

>>> (writeLn! "hello")
"hello"

==> "hello"

>>> (uwrite! 42)
42
==> 42

>>> (uwrite! "hello")
hello
==> "hello"

>>> (uwriteLn! 42)
42

==> 42

>>> (uwriteLn! "hello")
hello

==> "hello"

>>> (writef "{0} {1}" (list 42 "hello"))
42 hello
==> "42 hello"

>>> (writef "x={0:d}" (list 99))
x=99
==> "x=99"

>>> (writef "{0} + {1} = {2}" (list 1 2 3))
1 + 2 = 3
==> "1 + 2 = 3"


>>> ;;; Error: write!/writeLn! require at least one argument
... (write!)

%%% ERROR 'WRITE!': 1 or more arguments expected.
%%% USAGE: (WRITE! <obj1> <obj2> ...)
==>

>>> (writeLn!)

%%% ERROR 'WRITELN!': 1 or more arguments expected.
%%% USAGE: (WRITELN! <obj1> <obj2> ...)
==>

>>> (uwrite!)

%%% ERROR 'UWRITE!': 1 or more arguments expected.
%%% USAGE: (UWRITE! <obj1> <obj2> ...)
==>

>>> (uwriteLn!)

%%% ERROR 'UWRITELN!': 1 or more arguments expected.
%%% USAGE: (UWRITELN! <obj1> <obj2> ...)
==>

>>> ;;; Error: writef requires exactly two arguments
... (writef)

%%% ERROR 'WRITEF': 2 arguments expected.
%%% USAGE: (WRITEF <formatString> <MapOrList>)
==>

>>> ;;; Error: writef first argument must be a format string
... (writef 1 '(a b))

%%% ERROR 'WRITEF': 1st argument expected to be a format string.
%%% USAGE: (WRITEF <formatString> <MapOrList>)
==>

>>> ;;; Error: writef second argument must be a list or map
... (writef "hello" 1)

%%% ERROR 'WRITEF': 2nd argument expected to be a list or map.
%%% USAGE: (WRITEF <formatString> <MapOrList>)
==>


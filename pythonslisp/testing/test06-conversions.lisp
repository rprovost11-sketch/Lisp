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

==> "3.5"Hello"1/2"

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

>>> (ustring 42)
...

==> "42"

>>> (ustring "hello")
...

==> "hello"

>>> (ustring 'foo)
...

==> "FOO"

>>> (string "hello")
...

==> ""hello""

>>> (make-symbol "hello")
...

==> HELLO

>>> (make-symbol "MAKE-POINT")
...

==> MAKE-POINT


>>> ;;; Error: float rejects non-numeric strings
... (float "hello")

%%% ERROR 'FLOAT': Invalid argument.
%%% USAGE: (FLOAT number)
==>

>>> ;;; Error: rational requires exactly one argument
... (rational)

%%% ERROR 'RATIONAL': Exactly 1 argument expected.
%%% USAGE: (RATIONAL number)
==>

>>> (rational 1 2)

%%% ERROR 'RATIONAL': Exactly 1 argument expected.
%%% USAGE: (RATIONAL number)
==>

>>> ;;; Error: string requires at least one argument
... (string)

%%% ERROR 'STRING': 1 or more arguments expected.
%%% USAGE: (STRING &rest objects)
==>

>>> ;;; Error: make-symbol requires exactly one argument
... (make-symbol)

%%% ERROR 'MAKE-SYMBOL': 1 argument expected.
%%% USAGE: (MAKE-SYMBOL string)
==>

; --- make-symbol happy-path tests ---

>>> (make-symbol "foo")
...

==> FOO

>>> (make-symbol "hello-world")
...

==> HELLO-WORLD

>>> (make-symbol "x")
...

==> X

>>> (make-symbol "abc123")
...

==> ABC123

>>> (make-symbol "+")
...

==> +

>>> (make-symbol "nil")
...

==> NIL

>>> (make-symbol "FOO")
...

==> FOO

>>> (make-symbol "foo-bar-baz")
...

==> FOO-BAR-BAZ

; --- string-upcase / string-downcase ---

>>> (string-upcase "hello")
...

==> "HELLO"

>>> (string-downcase "WORLD")
...

==> "world"

>>> (string-upcase "already UP")
...

==> "ALREADY UP"

>>> (string-downcase "mixed Case 123")
...

==> "mixed case 123"

; --- string-trim / string-left-trim / string-right-trim ---

>>> (string-trim " " "  hello  ")
...

==> "hello"

>>> (string-left-trim " " "  hello  ")
...

==> "hello  "

>>> (string-right-trim " " "  hello  ")
...

==> "  hello"

>>> (string-trim "xy" "xyhelloyx")
...

==> "hello"

; --- char-code / code-char ---

>>> (char-code "a")
...

==> 97

>>> (char-code "A")
...

==> 65

>>> (code-char 97)
...

==> "a"

>>> (code-char 65)
...

==> "A"

>>> ;;; round-trip
... (code-char (char-code "z"))
...

==> "z"


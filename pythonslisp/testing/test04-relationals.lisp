>>> (is? nil nil)
...

==> T

>>> (is? 1 0)
...

==> NIL

>>> (is? 1 1)
...

==> T

>>> (is? 3 3)
...

==> T

>>> (is? 3 3/1)
...

==> T

>>> (is? 3.0 3)
...

==> T

>>> (is? 5 6)
...

==> NIL

>>> (is? "one" "one")
...

==> T

>>> (is? "a string" 'aSymbol)
...

==> NIL

>>> (= 1 1)
...

==> T

>>> (= 1 0)
...

==> NIL

>>> (= 1 3/2)
...

==> NIL

>>> (= 1 nil)
...

==> NIL

>>> (= 1 symbol)
...

==> NIL

>>> (= 1 "string")
...

==> NIL

>>> (= "string" "string")
...

==> T

>>> (= "string1" "string2")
...

==> NIL

>>> (/= 1 1)
...

==> NIL

>>> (/= 0 6)
...

==> T

>>> (/= "string" "string")
...

==> NIL

>>> (/= "string1" "string2")
...

==> T

>>> (< 6 2)
...

==> NIL

>>> (< 4 4)
...

==> NIL

>>> (< 2 3)
...

==> T

>>> (< 2 4 6 8)
...

==> T

>>> (< 2 4 8 5)
...

==> NIL

>>> (= 4 4)
...

==> T

>>> (= 2 3)
...

==> NIL

>>> (= 4 4 4 4)
...

==> T

>>> (= 2 2 3 2)
...

==> NIL

>>> (<= 5 3)
...

==> NIL

>>> (<= 2 2)
...

==> T

>>> (<= 3 8)
...

==> T

>>> (<= 2 2 3 2 4)
...

==> NIL

>>> (<= 2 2 3 3 4)
...

==> T

>>> (/= 2 4)
...

==> T

>>> (/= 2 2)
...

==> NIL

>>> (/= 2 5 8 4)
...

==> T

>>> (/= 4 6 4 4)
...

==> NIL

>>> (> 5 3)
...

==> T

>>> (> 3 5)
...

==> NIL

>>> (> 5 5)
...

==> NIL

>>> (> 5 3 2)
...

==> T

>>> (> 5 3 3)
...

==> NIL

>>> (>= 5 3)
...

==> T

>>> (>= 5 5)
...

==> T

>>> (>= 3 5)
...

==> NIL

>>> (>= 5 5 4)
...

==> T

>>> (>= 5 5 6)
...

==> NIL


>>> ;;; Error: relational operators need at least 2 arguments
... (= 1)

%%% ERROR '=': 2 or more arguments expected.
%%% USAGE: (= <expr1> <expr2> ...)
==>

>>> (/= 1)

%%% ERROR '/=': 2 or more arguments expected.
%%% USAGE: (/= <expr1> <expr2> ...)
==>

>>> (< 1)

%%% ERROR '<': 2 or more arguments expected.
%%% USAGE: (< <expr1> <expr2> ...)
==>

>>> (<= 1)

%%% ERROR '<=': 2 or more arguments expected.
%%% USAGE: (<= <expr1> <expr2> ...)
==>

>>> (> 1)

%%% ERROR '>': 2 or more arguments expected.
%%% USAGE: (> <expr1> <expr2> ...)
==>

>>> (>= 1)

%%% ERROR '>=': 2 or more arguments expected.
%%% USAGE: (>= <expr1> <expr2> ...)
==>


; --- string comparisons ---

>>> (= "abc" "abc")
...

==> T

>>> (= "abc" "ABC")
...

==> NIL

>>> (< "abc" "abd")
...

==> T

>>> (> "z" "a")
...

==> T

>>> (string= "hello" "hello")
...

==> T

>>> (string< "apple" "banana")
...

==> T

>>> (string> "zebra" "apple")
...

==> T

>>> (string<= "abc" "abc")
...

==> T

>>> (string>= "b" "a")
...

==> T

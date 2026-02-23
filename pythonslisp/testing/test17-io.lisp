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

>>> ;;; writef with no second arg outputs the format string unchanged
... (writef "no substitution needed")
no substitution needed
==> "no substitution needed"

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

>>> ;;; Error: writef requires at least one argument
... (writef)

%%% ERROR 'WRITEF': 1 or 2 arguments expected.
%%% USAGE: (WRITEF <formatString> &optional <MapOrList>)
==>

>>> ;;; Error: writef first argument must be a format string
... (writef 1 '(a b))

%%% ERROR 'WRITEF': 1st argument expected to be a format string.
%%% USAGE: (WRITEF <formatString> &optional <MapOrList>)
==>

>>> ;;; Error: writef second argument must be a list or map
... (writef "hello" 1)

%%% ERROR 'WRITEF': 2nd argument expected to be a list or map.
%%% USAGE: (WRITEF <formatString> &optional <MapOrList>)
==>

; ============================================================
; Additional write!/uwrite!/writeLn!/uwriteLn! coverage
; ============================================================

; --- write! with various types ---

>>> ;;; write! with NIL
... (write! NIL)
NIL
==> NIL

>>> ;;; write! with T
... (write! T)
T
==> T

>>> ;;; write! with float
... (write! 3.14)
3.14
==> 3.14

>>> ;;; write! with fraction
... (write! 1/3)
1/3
==> 1/3

>>> ;;; write! with symbol
... (write! 'foo)
FOO
==> FOO

>>> ;;; write! with list
... (write! '(1 2 3))
(1 2 3)
==> (1 2 3)

>>> ;;; write! with nested list
... (write! '(a (b c) d))
(A (B C) D)
==> (A (B C) D)

>>> ;;; write! with empty list
... (write! '())
NIL
==> NIL

; --- uwrite! with various types ---

>>> ;;; uwrite! with NIL
... (uwrite! NIL)
NIL
==> NIL

>>> ;;; uwrite! with T
... (uwrite! T)
T
==> T

>>> ;;; uwrite! with float
... (uwrite! 3.14)
3.14
==> 3.14

>>> ;;; uwrite! with fraction
... (uwrite! 1/3)
1/3
==> 1/3

>>> ;;; uwrite! with symbol
... (uwrite! 'foo)
FOO
==> FOO

>>> ;;; uwrite! with list
... (uwrite! '(1 2 3))
(1 2 3)
==> (1 2 3)

; --- uwrite!/uwriteLn! with multiple arguments ---

>>> ;;; uwrite! with multiple args concatenates
... (uwrite! 1 2 3)
123
==> 3

>>> ;;; uwriteLn! with multiple args
... (uwriteLn! 4 5 6)
456

==> 6

; --- write! vs uwrite! string quoting ---

>>> ;;; write! with multiple strings keeps quotes
... (write! "a" "b" "c")
"a""b""c"
==> "c"

>>> ;;; uwrite! with multiple strings omits quotes
... (uwrite! "a" "b" "c")
abc
==> "c"

; --- mixed types in multi-arg ---

>>> ;;; write! with mixed types (string + int)
... (write! "x=" 42)
"x="42
==> 42

>>> ;;; uwrite! with mixed types (string + int)
... (uwrite! "x=" 42)
x=42
==> 42

; --- return value is last argument ---

>>> ;;; write! returns last of multiple int args
... (write! 10 20 30)
102030
==> 30

>>> ;;; write! returns last arg when string
... (write! 1 "end")
1"end"
==> "end"

>>> ;;; uwrite! returns last arg when string
... (uwrite! 1 "end")
1end
==> "end"

; --- writeLn!/uwriteLn! multi-arg ---

>>> ;;; writeLn! with multiple string args
... (writeLn! "a" "b")
"a""b"

==> "b"

>>> ;;; uwriteLn! with multiple string args
... (uwriteLn! "a" "b")
ab

==> "b"


; --- terpri ---

>>> (terpri)


==> NIL

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

==> NIL

>>> (is? 3.0 3)
...

==> NIL

>>> (is? 5 6)
...

==> NIL

>>> (is? "one" "one")
...

==> NIL

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

>>> (= 1 make-symbol)
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

>>> ;;; /= non-adjacent duplicate: 1st and 3rd are equal
... (/= 1 2 1)
...

==> NIL

>>> ;;; /= three all-distinct values
... (/= 1 2 3)
...

==> T

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

%%% ERROR '=': At least 2 arguments expected.
%%% USAGE: (= expr1 expr2 ...)
==>

>>> (/= 1)

%%% ERROR '/=': At least 2 arguments expected.
%%% USAGE: (/= expr1 expr2 ...)
==>

>>> (< 1)

%%% ERROR '<': At least 2 arguments expected.
%%% USAGE: (< expr1 expr2 ...)
==>

>>> (<= 1)

%%% ERROR '<=': At least 2 arguments expected.
%%% USAGE: (<= expr1 expr2 ...)
==>

>>> (> 1)

%%% ERROR '>': At least 2 arguments expected.
%%% USAGE: (> expr1 expr2 ...)
==>

>>> (>= 1)

%%% ERROR '>=': At least 2 arguments expected.
%%% USAGE: (>= expr1 expr2 ...)
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

; --- eq (identity) ---

>>> (eq 'foo 'foo)
==> T

>>> (eq 'foo 'bar)
==> NIL

>>> (eq nil nil)
==> T

>>> (eq t t)
==> T

; same object is eq to itself
>>> (let ((x '(1 2))) (eq x x))
==> T

; two separately constructed lists are not eq
>>> (eq '(1 2) '(1 2))
==> NIL

; is? is an alias for eq
>>> (is? nil nil)
==> T

>>> (is? '(1 2) '(1 2))
==> NIL

; --- eql ---

>>> (eql 'foo 'foo)
==> T

>>> (eql 1 1)
==> T

; different numeric types are not eql
>>> (eql 1 1.0)
==> NIL

>>> (eql 1/3 1/3)
==> T

; strings: eql is identity, not content
>>> (eql "hello" "hello")
==> NIL

; --- equal ---

; lists compared structurally
>>> (equal '(1 2 3) '(1 2 3))
==> T

>>> (equal '(1 (2 3)) '(1 (2 3)))
==> T

>>> (equal '(1 2) '(1 2 3))
==> NIL

; strings compared by content
>>> (equal "hello" "hello")
==> T

>>> (equal "Hello" "hello")
==> NIL

; numbers: equal uses eql at leaves same type required
>>> (equal 1 1)
==> T

>>> (equal 1 1.0)
==> NIL

; nil and empty list
>>> (equal nil nil)
==> T

>>> (equal nil '())
==> T

; symbols
>>> (equal 'foo 'foo)
==> T

>>> (equal 'foo 'bar)
==> NIL

; --- equalp ---

; lists
>>> (equalp '(1 2 3) '(1 2 3))
==> T

; case-insensitive strings
>>> (equalp "hello" "HELLO")
==> T

>>> (equalp "hello" "world")
==> NIL

; cross-type numeric comparison
>>> (equalp 1 1.0)
==> T

>>> (equalp 1 1)
==> T

>>> (equalp 1 2)
==> NIL

>>> (equalp nil nil)
==> T

; nested list with mixed string case
>>> (equalp '("a" "b") '("A" "B"))
==> T

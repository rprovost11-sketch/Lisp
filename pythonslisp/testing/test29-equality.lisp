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

; numbers: equal uses eql at leaves — same type required
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

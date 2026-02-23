; --- eql ---

>>> (eql 'foo 'foo)
==> T

>>> (eql 'foo 'bar)
==> NIL

>>> (eql 1 1)
==> T

>>> (eql 1 1.0)
==> NIL

>>> (eql 1/3 1/3)
==> T

>>> (eql nil nil)
==> T

>>> (eql t t)
==> T

>>> (eql "hello" "hello")
==> NIL

; --- catch and throw ---

; throw immediately returns value to catch; remaining body skipped
>>> (catch 'done (throw 'done 42) 99)
==> 42

; normal completion — no throw; returns last body form
>>> (catch 'done 1 2 3)
==> 3

; empty body returns NIL
>>> (catch 'tag)
==> NIL

; throw crosses function-call boundary (dynamic extent)
>>> (defun thrower () (throw 'escape 99))
==> (FUNCTION THROWER () ... )

>>> (catch 'escape (thrower) 0)
==> 99

; nested catches: inner tag matches — caught by inner; outer returns body after
>>> (catch 'outer
...    (catch 'inner
...       (throw 'inner 1))
...    2)
==> 2

; nested catches: outer tag — propagates past inner, caught by outer
>>> (catch 'outer
...    (catch 'inner
...       (throw 'outer 99))
...    42)
==> 99

; tag is evaluated at runtime
>>> (let ((tag 'my-tag)) (catch tag (throw tag 77)))
==> 77

; numeric tag uses eql (same type, same value)
>>> (catch 1 (throw 1 "hit") "miss")
==> "hit"

; eql distinguishes numeric types — mismatched tag is an unmatched throw
>>> (catch 1 (throw 1.0 "hit") "miss")
%%% throw: no catch for tag 1.0.

; throw with no matching catch — error
>>> (throw 'nowhere 42)
%%% throw: no catch for tag NOWHERE.

; throw requires exactly 2 args
>>> (throw 'foo)
%%% ERROR 'THROW': 2 arguments expected.
%%% USAGE: (THROW <tag> <result>)

; ============================================================
; test26-setq-atset.lisp
; Tests for setq (variable assignment) and at-set (indexed/keyed
; mutation of lists and maps).
; ============================================================

; --- 1. setq: basic assignment ---

>>> (setq sq1 99)
...

==> 99

>>> sq1
...

==> 99

>>> (setq sq1 100)
...

==> 100

>>> sq1
...

==> 100

>>> (setq sq2 3.14)
...

==> 3.14

>>> (setq sq3 "hello")
...

==> "hello"

>>> (setq sq4 '(a b c))
...

==> (A B C)

>>> (setq sq5 nil)
...

==> NIL

>>> (setq sq6 t)
...

==> T

; --- 2. setq: multiple pairs in one call ---

>>> ;;; returns the value of the last assignment
... (setq ma 1 mb 2 mc 3)
...

==> 3

>>> ma
...

==> 1

>>> mb
...

==> 2

>>> mc
...

==> 3

; --- 3. setq: scope behaviour ---

>>> ;;; setq inside let finds and updates the let-local binding
... (let ((lvar 5))
...    (setq lvar 99)
...    lvar)
...

==> 99

>>> ;;; the let-local did not leak into the global scope
... (boundp 'lvar)
...

==> NIL

>>> ;;; setq creates a new global when the name is not bound anywhere
... (setq sq-new 777)
...

==> 777

>>> sq-new
...

==> 777

; --- 4. setq: auto-names a lambda ---

>>> (setq sq-fn (lambda (x) (* x x)))
...

==> (FUNCTION SQ-FN (X) ... )

>>> (sq-fn 7)
...

==> 49

; --- 5. setq: error cases ---

>>> ;;; no arguments
... (setq)

%%% ERROR 'SETQ': At least 2 arguments expected.
%%% USAGE: (SETQ <symbol1> <sexpr1> <symbol2> <sexpr2> ...)
==>

>>> ;;; odd number of arguments
... (setq sq1 1 sq2)

%%% ERROR 'SETQ': An even number of arguments is expected.  Received 3.
%%% USAGE: (SETQ <symbol1> <sexpr1> <symbol2> <sexpr2> ...)
==>

>>> ;;; non-symbol lvalue
... (setq 42 1)

%%% ERROR 'SETQ': First of setf pair must be a symbol.
%%% USAGE: (SETQ <symbol1> <sexpr1> <symbol2> <sexpr2> ...)
==>

; --- 6. at-set: list — basic mutation ---

>>> (setf asl (list 10 20 30 40 50))
...

==> (10 20 30 40 50)

>>> ;;; set first element; returns new value
... (at-set 0 asl 99)
...

==> 99

>>> ;;; mutation is in-place
... asl
...

==> (99 20 30 40 50)

>>> (at-set 2 asl 77)
...

==> 77

>>> asl
...

==> (99 20 77 40 50)

>>> (at-set 4 asl 55)
...

==> 55

>>> asl
...

==> (99 20 77 40 55)

>>> ;;; negative index follows Python semantics: -1 is the last element
... (at-set -1 asl 11)
...

==> 11

>>> asl
...

==> (99 20 77 40 11)

; --- 7. at-set: list — aliasing (shared reference) ---

>>> (setf orig (list 1 2 3))
...

==> (1 2 3)

>>> (setf alias orig)
...

==> (1 2 3)

>>> ;;; mutation through alias is visible via the original binding
... (at-set 0 alias 99)
...

==> 99

>>> orig
...

==> (99 2 3)

; --- 8. at-set: map — symbol keys ---

>>> (setf skmap (map (x 10) (y 20)))
...

==> (MAP
   ("X" 10)
   ("Y" 20)
)

>>> ;;; quoted symbol key is uppercased to match the stored key
... (at-set 'x skmap 99)
...

==> 99

>>> skmap
...

==> (MAP
   ("X" 99)
   ("Y" 20)
)

>>> ;;; at-set adds a new key when the key is not already present
... (at-set 'z skmap 30)
...

==> 30

>>> skmap
...

==> (MAP
   ("X" 99)
   ("Y" 20)
   ("Z" 30)
)

; --- 9. at-set: map — string keys ---

>>> (setf strmap (map ("p" 1) ("q" 2)))
...

==> (MAP
   ("p" 1)
   ("q" 2)
)

>>> (at-set "p" strmap 88)
...

==> 88

>>> strmap
...

==> (MAP
   ("p" 88)
   ("q" 2)
)

; --- 10. at-set: error cases ---

>>> ;;; too few arguments
... (at-set 0 (list 1 2 3))

%%% ERROR 'AT-SET': Exactly 3 arguments expected.
%%% USAGE: (AT-SET <keyOrIndex> <mapListOrStr> <newValue>)
==>

>>> ;;; too many arguments
... (at-set 0 (list 1 2 3) 9 9)

%%% ERROR 'AT-SET': Exactly 3 arguments expected.
%%% USAGE: (AT-SET <keyOrIndex> <mapListOrStr> <newValue>)
==>

>>> ;;; second argument is an integer (not a list or map)
... (at-set 0 42 9)

%%% ERROR 'AT-SET': Invalid argument.  List or Map expected.
%%% USAGE: (AT-SET <keyOrIndex> <mapListOrStr> <newValue>)
==>

>>> ;;; second argument is a string (strings are immutable)
... (at-set 0 "abc" 9)

%%% ERROR 'AT-SET': Invalid argument.  List or Map expected.
%%% USAGE: (AT-SET <keyOrIndex> <mapListOrStr> <newValue>)
==>

>>> ;;; list index out of range
... (at-set 99 (list 1 2 3) 0)

%%% ERROR 'AT-SET': Invalid argument key/index.
%%% USAGE: (AT-SET <keyOrIndex> <mapListOrStr> <newValue>)
==>

>>> ;;; float index into a list
... (at-set 1.5 (list 1 2 3) 0)

%%% ERROR 'AT-SET': Invalid argument key/index.
%%% USAGE: (AT-SET <keyOrIndex> <mapListOrStr> <newValue>)
==>

; --- 11. setf macro — expansion tests ---

>>> ;;; setf with a symbol expands to setq (auto-names a lambda)
... (setf sf-fn (lambda (x) (* x 3)))
...

==> (FUNCTION SF-FN (X) ... )

>>> (sf-fn 5)
...

==> 15

>>> ;;; setf with multiple pairs (progn of setqs)
... (setf sf-a 1 sf-b 2 sf-c 3)
...

==> 3

>>> sf-a
...

==> 1

>>> sf-b
...

==> 2

>>> sf-c
...

==> 3

>>> ;;; setf (at ...) form mutates a list in place
... (setf sf-lst (list 10 20 30))
...

==> (10 20 30)

>>> (setf (at 1 sf-lst) 99)
...

==> 99

>>> sf-lst
...

==> (10 99 30)

>>> ;;; setf (at ...) form mutates a map in place
... (setf sf-map (map (x 1) (y 2)))
...

==> (MAP
   ("X" 1)
   ("Y" 2)
)

>>> (setf (at 'y sf-map) 77)
...

==> 77

>>> sf-map
...

==> (MAP
   ("X" 1)
   ("Y" 77)
)

>>> ;;; setf with mixed pairs: symbol then at-form
... (setf sf-x 0 (at 0 sf-lst) 55)
...

==> 55

>>> sf-x
...

==> 0

>>> sf-lst
...

==> (55 99 30)

; --- 12. setf macro — error cases ---

>>> ;;; NIL lvalue
... (setf () 5)

%%% setf: lvalue cannot be NIL or ()
==>

>>> ;;; (at ...) form with wrong element count
... (setf (at 0) 5)

%%% setf: (at ...) place form expected 3 elements
==>

>>> ;;; struct accessor with extra instance argument
... (setf (sf-fn sf-a sf-b) 5)

%%% setf: struct accessor place must have exactly 1 instance argument
==>

>>> ;;; non-symbol non-list lvalue
... (setf 99 5)

%%% setf: unrecognized place form
==>

>>> ;;; zero args
... (setf)

%%% Too few positional arguments.
==>

>>> ;;; one arg (missing value)
... (setf sf-a)

%%% Too few positional arguments.
==>

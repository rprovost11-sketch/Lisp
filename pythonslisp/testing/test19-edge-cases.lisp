; ============================================================
; test19-edge-cases.lisp
; Edge cases and boundary conditions across primitives.
; ============================================================

; --- Arithmetic edge cases ---

>>> ;;; (+) with no arguments returns 0
... (+)
...

==> 0

>>> ;;; expt with power 0 returns 1
... (expt 2 0)
...

==> 1

>>> ;;; expt 0^0 returns 1
... (expt 0 0)
...

==> 1

>>> ;;; abs of zero
... (abs 0)
...

==> 0

>>> ;;; abs of negative rational
... (abs -3/4)
...

==> 3/4

>>> ;;; gcd with no args returns 0
... (gcd)
...

==> 0

>>> ;;; lcm with no args returns 1
... (lcm)
...

==> 1

; --- Control structure edge cases ---

>>> ;;; while with initially false condition returns NIL
... (while nil (+ 1 2))
...

==> NIL

>>> ;;; foreach on empty list returns NIL
... (foreach x '() (write! x))
...

==> NIL

>>> ;;; doTimes with 0 iterations returns NIL
... (doTimes (i 0) (write! i))
...

==> NIL

>>> ;;; progn with no arguments returns NIL
... (progn)
...

==> NIL

>>> ;;; cond with no matching clause returns NIL
... (cond (nil 1) (nil 2))
...

==> NIL

>>> ;;; case with no matching clause returns NIL
... (case 5 (1 "one") (2 "two"))
...

==> NIL

>>> ;;; case with matching clause
... (case 2 (1 10) (2 20) (3 30))
...

==> 20

>>> ;;; cond with first match
... (cond (t 1) (t 2))
...

==> 1

>>> ;;; cond skips false, matches second
... (cond (nil 1) (t 2))
...

==> 2

; --- List operation edge cases ---

>>> ;;; at with negative index (Python-style)
... (at -1 (list 10 20 30))
...

==> 30

>>> ;;; at on a string
... (at 0 "hello")
...

==> "h"

>>> ;;; at on string with negative index
... (at -1 "hello")
...

==> "o"

>>> ;;; at with symbol key on map
... (at 'a (map (a 1) (b 2)))
...

==> 1

>>> ;;; sorted on empty list
... (sorted '())
...

==> NIL

>>> ;;; cons 3 onto empty list
... (cons 3 '())
...

==> (3)

>>> ;;; hasValue? on list (true)
... (hasValue? '(1 2 3) 2)
...

==> T

>>> ;;; hasValue? on list (false)
... (hasValue? '(1 2 3) 5)
...

==> NIL

>>> ;;; hasValue? on map (true)
... (hasValue? (map (a 1) (b 2)) 2)
...

==> T

>>> ;;; hasValue? on map (false)
... (hasValue? (map (a 1) (b 2)) 3)
...

==> NIL

>>> ;;; hasKey? on empty map
... (hasKey? (map) 'a)
...

==> NIL

; --- Map edge cases ---

>>> ;;; empty map
... (mapp (map))
...

==> T

>>> ;;; map with integer keys
... (at 1 (map (1 "a") (2 "b")))
...

==> "a"

; --- push!/pop! edge cases ---

>>> (setf plist (list 1 2 3))
...

==> (1 2 3)

>>> ;;; push! mutates and returns list
... (push! plist 4)
...

==> (1 2 3 4)

>>> plist
...

==> (1 2 3 4)

>>> ;;; pop! removes and returns last element
... (pop! plist)
...

==> 4

>>> plist
...

==> (1 2 3)

; --- at-delete on map with symbol key ---

>>> (setf tmap (map (a 10) (b 20) (c 30)))
...

==> (MAP
   ("A" 10)
   ("B" 20)
   ("C" 30)
)

>>> (at-delete 'a tmap)
...

==> T

>>> tmap
...

==> (MAP
   ("B" 20)
   ("C" 30)
)

; --- identity vs equality ---

>>> (setf lst1 (list 1 2))
...

==> (1 2)

>>> ;;; is? on same object returns T
... (is? lst1 lst1)
...

==> T

>>> ;;; is? on equal but different objects returns NIL
... (is? (list 1 2) (list 1 2))
...

==> NIL

; --- copy vs original ---

>>> (setf orig (list 1 2 3))
...

==> (1 2 3)

>>> (setf cop (copy orig))
...

==> (1 2 3)

>>> ;;; copy is not identity-equal
... (is? orig cop)
...

==> NIL

>>> ;;; copy is value-equal
... (= orig cop)
...

==> T

; --- deepCopy ---

>>> (deepCopy '(1 (2 3) 4))
...

==> (1 (2 3) 4)

; --- let edge cases ---

>>> ;;; let with bare symbol binds to NIL
... (let (x) x)
...

==> NIL

>>> ;;; let with single-element list binds to NIL
... (let ((x)) x)
...

==> NIL

>>> ;;; let with no body returns NIL
... (let ())
...

==> NIL

>>> ;;; let* with no body returns NIL
... (let* ())
...

==> NIL

; --- setf with multiple pairs ---

>>> (setf aa 10 bb 20)
...

==> 20

>>> aa
...

==> 10

>>> bb
...

==> 20

; --- integer conversion with base ---

>>> ;;; integer with base 16 (hex)
... (integer "ff" 16)
...

==> 255

>>> ;;; integer with base 2 (binary)
... (integer "10" 2)
...

==> 2

; --- string concatenation ---

>>> (string 1 2 3)
...

==> "123"

>>> (ustring 1 2 3)
...

==> "123"

; --- foreach accumulation ---

>>> (setf fsum 0)
...

==> 0

>>> (foreach x '(1 2 3 4 5) (setf fsum (+ fsum x)))
...

==> 15

>>> fsum
...

==> 15

; --- doTimes accumulation ---

>>> (setf dsum 0)
...

==> 0

>>> (doTimes (i 5) (setf dsum (+ dsum i)))
...

==> 10

>>> dsum
...

==> 10

; --- while with accumulation ---

>>> (let ((n 3) (acc 0))
...    (while (> n 0)
...       (setf acc (+ acc n))
...       (setf n (- n 1)))
...    acc)
...

==> 6

; --- parse ---

>>> (parse "(+ 1 2)")
...

==> (PROGN (+ 1 2))

; --- derivative function ---

>>> (d 5)
...

==> 0

>>> (d 'x)
...

==> 1

>>> (d '(+ x 3))
...

==> (+ 1 0)

>>> (d '(* x x))
...

==> (+ (* X 1) (* X 1))

>>> (d '(sin x))
...

==> (* (COS X) 1)

>>> (d '(expt x 3))
...

==> (* (* 3 (EXPT X 2)) 1)

; --- random produces correct types ---

>>> (integerp (random 10))
...

==> T

>>> (floatp (random 10.0))
...

==> T

; --- Additional arithmetic edge cases ---

>>> ;;; single argument to multiply
... (* 5)
...

==> 5

>>> ;;; negation of zero
... (- 0)
...

==> 0

>>> ;;; division produces float
... (/ 1 2)
...

==> 0.5

; --- Additional list operation edge cases ---

>>> ;;; append with 3+ lists
... (append '(1) '(2) '(3))
...

==> (1 2 3)

>>> ;;; sorted on single-element list
... (sorted (list 3))
...

==> (3)

; --- Lambda/macro with docstrings ---

>>> ;;; lambda with docstring still works
... ((lambda () "a docstring" 42))
...

==> 42

>>> ;;; defmacro with docstring still works
... (defmacro test-docmacro19 (x) "a docstring" `(+ ,x 1))
...

==> (Macro TEST-DOCMACRO19 (X) ... )

>>> (test-docmacro19 5)
...

==> 6

; --- writef with map ---

>>> ;;; writef using map keys (uppercased)
... (writef "{X} {Y}" (map (x 10) (y 20)))
10 20
==> "10 20"

; --- write!/writeLn! with multiple arguments ---

>>> ;;; write! with multiple args concatenates output
... (write! 1 2 3)
123
==> 3

>>> ;;; writeLn! with multiple args
... (writeLn! 4 5 6)
456

==> 6

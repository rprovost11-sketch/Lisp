; ============================================================
; test22-additional-coverage.lisp
; Additional edge case and coverage tests for primitives
; and library functions not exercised elsewhere.
; ============================================================

; --- mapcar edge cases ---

>>> ;;; mapcar on empty list
... (mapcar (lambda (x) (* x 2)) '())
...

==> NIL

>>> ;;; mapcar with lambda (double each)
... (mapcar (lambda (x) (* x 2)) '(1 2 3))
...

==> (2 4 6)

; --- remove edge cases ---

>>> ;;; remove non-existent element returns original
... (remove 5 '(1 2 3))
...

==> (1 2 3)

>>> ;;; remove from empty list
... (remove 1 '())
...

==> NIL

; --- when/unless with multiple body forms ---

>>> ;;; when true with multiple body forms returns last
... (when t 1 2)
...

==> 2

>>> ;;; when false with multiple body forms returns NIL
... (when nil 1 2)
...

==> NIL

>>> ;;; unless false with multiple body forms returns last
... (unless nil 1 2)
...

==> 2

>>> ;;; unless true with multiple body forms returns NIL
... (unless t 1 2)
...

==> NIL

; --- setf with at on map ---

>>> (setf smap22 (map (a 1) (b 2)))
...

==> (MAP
   ("A" 1)
   ("B" 2)
)

>>> ;;; setf at on map mutates the value
... (setf (at 'a smap22) 99)
...

==> 99

>>> smap22
...

==> (MAP
   ("A" 99)
   ("B" 2)
)

; --- setf with at on list ---

>>> (setf slist22 (list 10 20 30))
...

==> (10 20 30)

>>> ;;; setf at on list mutates the element
... (setf (at 1 slist22) 99)
...

==> 99

>>> slist22
...

==> (10 99 30)

; --- deepCopy on map ---

>>> ;;; deepCopy preserves map structure
... (deepCopy (map (a 1) (b 2)))
...

==> (MAP
   ("A" 1)
   ("B" 2)
)

; --- nth edge cases ---

>>> ;;; nth with negative index (Python-style)
... (nth -1 '(1 2 3))
...

==> 3

; --- sorted with strings ---

>>> ;;; sorted works on string lists
... (sorted '("c" "a" "b"))
...

==> ("a" "b" "c")

; --- cons edge cases ---

>>> ;;; cons a list onto a list
... (cons '(1 2) '(3 4))
...

==> ((1 2) 3 4)

>>> ;;; cons NIL onto a list
... (cons nil '(1 2))
...

==> (NIL 1 2)

; --- multi-body control structures ---

>>> ;;; progn returns the last value
... (progn 1 2 3)
...

==> 3

>>> ;;; cond with multiple body forms returns last
... (cond (t 1 2 3))
...

==> 3

>>> ;;; case with multiple body forms returns last
... (case 1 (1 10 20 30))
...

==> 30

>>> ;;; while returns last body value
... (let ((n 1))
...    (while (> n 0)
...       (setf n (- n 1))
...       42))
...

==> 42

; --- defun with docstring ---

>>> ;;; defun with docstring
... (defun square22 (x) "Returns x squared" (* x x))
...

==> (FUNCTION SQUARE22 (X) ... )

>>> (square22 7)
...

==> 49

; --- average edge cases ---

>>> ;;; average of single value
... (average 5)
...

==> 5.0

>>> ;;; average with rationals
... (average 1/2 3/2)
...

==> 1/1

; --- string from symbol ---

>>> ;;; string converts symbol to string (programmer mode)
... (string 'abc)
...

==> "ABC"

; --- symbol from concatenated strings ---

>>> ;;; symbol from multiple strings
... (symbol "hello" "-" "world")
...

==> HELLO-WORLD

; --- nested quote ---

>>> ;;; double-quoted symbol
... ''x
...

==> (QUOTE X)

>>> ;;; quote inside a quoted list
... '(a 'b c)
...

==> (A (QUOTE B) C)

; --- isqrt boundary values ---

>>> ;;; isqrt of 0
... (isqrt 0)
...

==> 0

>>> ;;; isqrt of 1
... (isqrt 1)
...

==> 1

>>> ;;; isqrt of 4 (perfect square)
... (isqrt 4)
...

==> 2

; --- signum with floats ---

>>> ;;; signum of positive float
... (signum 3.14)
...

==> 1

>>> ;;; signum of negative float
... (signum -0.5)
...

==> -1

; --- integer truncation ---

>>> ;;; integer truncation of negative float
... (integer -3.7)
...

==> -3

; --- atan2 (two-argument atan) ---

>>> ;;; atan with two arguments (atan2)
... (atan 1 1)
...

==> 0.7853981633974483

; --- list-length with nested list ---

>>> ;;; list-length counts top-level elements only
... (list-length '((1 2) 3))
...

==> 2

; --- lambda with &rest used standalone ---

>>> ;;; lambda with &rest captures all args
... ((lambda (&rest args) args) 1 2 3)
...

==> (1 2 3)

; --- random zero edge cases ---

>>> ;;; random of 0 returns 0
... (random 0)
...

==> 0

>>> ;;; random of 0.0 returns 0.0
... (random 0.0)
...

==> 0.0

; --- symbol from symbol edge case ---

>>> ;;; symbol from symbol (converts via string)
... (symbol 'abc)
...

==> ABC

; --- car/cdr on empty list ---

>>> ;;; car of empty list is NIL
... (car '())
...

==> NIL

>>> ;;; cdr of empty list is NIL
... (cdr '())
...

==> NIL

; --- string repetition via * ---

>>> ;;; string times integer (Python repeat)
... (* "ab" 3)
...

==> "ababab"

; --- recursion-limit primitive removed (CEK machine has no recursion limit) ---

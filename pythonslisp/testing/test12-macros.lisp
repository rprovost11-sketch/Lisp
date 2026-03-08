>>> (setf name 'john)
...

==> JOHN

>>> `(go ,name go)
...

==> (GO JOHN GO)

>>> `( name ,name age ,(+ 3 7))
...

==> (NAME JOHN AGE 10)

>>> `,name
...

==> JOHN

>>> (setf full '(r h l))
...

==> (R H L)

>>> `( ,@full )
...

==> (R H L)

; --- nested backquotes ---

>>> ;;; inner backquote preserved as data no commas
... `(a `b)
...

==> (A (BACKQUOTE B))

>>> ;;; inner comma preserved as form (belongs to inner backquote)
... `(a `(b ,name))
...

==> (A (BACKQUOTE (B (COMMA NAME))))

>>> ;;; double-comma: outer comma evaluated now, inner comma preserved
... `(a `(b ,,name))
...

==> (A (BACKQUOTE (B (COMMA JOHN))))

>>> ;;; macro-generating-macro using nested backquote
... (defmacro make-adder-macro (name amount)
...    `(defmacro ,name (x)
...       `(+ ,x ,,amount)))
...

==> (MACRO MAKE-ADDER-MACRO (NAME AMOUNT) ...)

>>> (make-adder-macro add10 10)
...

==> (MACRO ADD10 (X) ...)

>>> (add10 5)
...

==> 15

>>> (defmacro zero (var) `(setf ,var 0))
...

==> (MACRO ZERO (VAR) ...)

>>> x
...

%%% Unbound Variable: X.
==> 

>>> (zero x)
...

==> 0

>>> x
...

==> 0

>>> (defmacro foo (a &rest b) `(a ,a ,@b))
...

==> (MACRO FOO (A &REST B) ...)

>>> (macroexpand '(foo 1 (a b c) (x y z)))
...

==> (A 1 (A B C) (X Y Z))

>>> (macroexpand '(foo 1 "a" "b" "c"))
...

==> (A 1 "a" "b" "c")

>>> ;;; macroexpand fully expands through multiple macro layers
... (macroexpand '(unless t 42))
...

==> (IF (NOT T) (PROGN 42))

>>> ;;; macroexpand-1 stops after one expansion step
... (macroexpand-1 '(unless t 42))
...

==> (WHEN (NOT T) 42)

>>> ;;; macroexpand-1 on a non-macro returns form unchanged
... (macroexpand-1 '(if t 42))
...

==> (IF T 42)

>>> ;;; macroexpand on a non-macro returns form unchanged
... (macroexpand '(if t 42))
...

==> (IF T 42)

>>> ;;; macroexpand on non-list returns unchanged
... (macroexpand 1)
...

==> 1

>>> ;;; macroexpand on empty list returns NIL
... (macroexpand '())
...

==> NIL

>>> ;;; macroexpand on non-macro head returns form unchanged
... (macroexpand '(1))
...

==> (1)

>>> ;;; Error: macroexpand requires exactly one argument
... (macroexpand)

%%% ERROR 'MACROEXPAND': 1 argument expected.
%%% PRIMITIVE USAGE: (MACROEXPAND 'form)
==>

>>> ;;; Error: macroexpand-1 requires exactly one argument
... (macroexpand-1)

%%% ERROR 'MACROEXPAND-1': 1 argument expected.
%%% PRIMITIVE USAGE: (MACROEXPAND-1 'form)
==>

; ============================================================
; Macro lambda list destructuring
; ============================================================

>>> ;;; basic required destructuring
... (defmacro test-destruct1 ((a b) c)
...    `(list ,a ,b ,c))

==> (MACRO TEST-DESTRUCT1 ((A B) C) ...)

>>> (test-destruct1 (10 20) 30)

==> (10 20 30)

>>> ;;; nested destructuring
... (defmacro test-destruct2 ((a (b c)) d)
...    `(list ,a ,b ,c ,d))

==> (MACRO TEST-DESTRUCT2 ((A (B C)) D) ...)

>>> (test-destruct2 (1 (2 3)) 4)

==> (1 2 3 4)

>>> ;;; &optional inside nested pattern — arg supplied
... (defmacro test-destruct3 ((a &optional b) c)
...    `(list ,a ,b ,c))

==> (MACRO TEST-DESTRUCT3 ((A &OPTIONAL B) C) ...)

>>> (test-destruct3 (1 2) 3)

==> (1 2 3)

>>> ;;; &optional inside nested pattern — arg defaulted
... (test-destruct3 (1) 3)

==> (1 NIL 3)

>>> ;;; &rest inside nested pattern
... (defmacro test-destruct4 ((a &rest bs) c)
...    `(list ,a ',bs ,c))

==> (MACRO TEST-DESTRUCT4 ((A &REST BS) C) ...)

>>> (test-destruct4 (1 2 3) 4)

==> (1 (2 3) 4)

>>> ;;; destructuring with remaining required params
... (defmacro test-destruct5 (x (a b) y)
...    `(list ,x ,a ,b ,y))

==> (MACRO TEST-DESTRUCT5 (X (A B) Y) ...)

>>> (test-destruct5 0 (1 2) 3)

==> (0 1 2 3)


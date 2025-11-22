>>> (def! 'name 'john)
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

>>> (def! 'full '(r h l))
...

==> (R H L)

>>> `( ,@full )
...

==> (R H L)

>>> (defmacro!! zero (var) `(def!! ,var 0))
...

==> (Macro ZERO (VAR) ... )

>>> x
...

==> X

>>> (zero x)
...

==> 0

>>> x
...

==> 0

>>> (defmacro!! foo (a &rest b) `(a ,a ,@b))
...

==> (Macro FOO (A &REST B) ... )

>>> (macroexpand '(foo 1 (a b c) (x y z)))
...

==> ((A 1 (A B C) (X Y Z)))

>>> (macroexpand '(foo 1 "a" "b" "c"))
...

==> ((A 1 "a" "b" "c"))

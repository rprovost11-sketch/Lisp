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

>>> (defmacro zero (var) `(setf ,var 0))
...

==> (Macro ZERO (VAR) ... )

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

==> (Macro FOO (A &REST B) ... )

>>> (macroexpand '(foo 1 (a b c) (x y z)))
...

==> ((A 1 (A B C) (X Y Z)))

>>> (macroexpand '(foo 1 "a" "b" "c"))
...

==> ((A 1 "a" "b" "c"))

>>> ;;; Error: macroexpand requires exactly one argument
... (macroexpand)

%%% ERROR 'MACROEXPAND': Exactly 1 argument expected.
%%% USAGE: (MACROEXPAND '(<macroName> <arg1> <arg2> ...))
==>

>>> ;;; Error: macroexpand argument must be a list
... (macroexpand 1)

%%% ERROR 'MACROEXPAND': Argument 1 expected to be a list.
%%% USAGE: (MACROEXPAND '(<macroName> <arg1> <arg2> ...))
==>

>>> ;;; Error: macroexpand with non-macro
... (macroexpand '(1))

%%% ERROR 'MACROEXPAND': Badly formed list expression.  The first element should evaluate to a macro.
%%% USAGE: (MACROEXPAND '(<macroName> <arg1> <arg2> ...))
==>


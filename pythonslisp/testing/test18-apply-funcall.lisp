; ============================================================
; test18-apply-funcall.lisp
; Tests for apply, funcall, eval, and python primitives.
; ============================================================

; --- funcall: basic usage ---

>>> ;;; funcall with a primitive
... (funcall + 1 2 3)
...

==> 6

>>> ;;; funcall with car
... (funcall car '(a b c))
...

==> A

>>> ;;; funcall with a lambda
... (funcall (lambda (x) (* x x)) 5)
...

==> 25

>>> ;;; funcall with a named function
... (funcall list 1 2 3)
...

==> (1 2 3)

; --- apply: basic usage ---

>>> ;;; apply with a list of args
... (apply + '(1 2 3))
...

==> 6

>>> ;;; apply with extra args prepended to list
... (apply + 1 2 '(3 4))
...

==> 10

>>> ;;; apply with empty list
... (apply + '())
...

==> 0

>>> ;;; apply with a named function
... (apply list '(a b c))
...

==> (A B C)

>>> ;;; apply with extra args and named function
... (apply cons 'a '((b c)))
...

==> (A B C)

; --- eval ---

>>> ;;; eval on a quoted expression
... (eval '(+ 10 20))
...

==> 30

; --- python ---

>>> ;;; python evaluates a python expression
... (python "3 + 4")
...

==> 7

>>> ;;; python with string expression
... (python "'hello' + ' world'")
...

==> "hello world"

; --- recursion-limit primitive removed (CEK machine has no recursion limit) ---

; --- funcall errors ---

>>> ;;; Error: funcall with no arguments
... (funcall)

%%% ERROR 'FUNCALL': 1 or more arguments expected
%%% USAGE: (FUNCALL <fnNameSymbol> <arg1> <arg2> ...)
==>

; --- apply errors ---

>>> ;;; Error: apply with no arguments
... (apply)

%%% ERROR 'APPLY': At least 2 arguments expected to apply.
%%% USAGE: (APPLY <function> &rest <args> <argsList>)
==>

>>> ;;; Error: apply last arg not a list
... (apply + 1)

%%% ERROR 'APPLY': Last argument expected to be a list.
%%% USAGE: (APPLY <function> &rest <args> <argsList>)
==>

>>> ;;; Error: apply with a special form
... (apply 'if '(t 1 2))

%%% ERROR 'APPLY': First argument may not be a special form.
%%% USAGE: (APPLY <function> &rest <args> <argsList>)
==>

; --- python errors ---

>>> ;;; Error: python with no arguments
... (python)

%%% ERROR 'PYTHON': 1 string argument expected by python.
%%% USAGE: (PYTHON <string>)
==>

>>> ;;; Error: python with non-string
... (python 1)

%%% ERROR 'PYTHON': Argument expected to be a string.
%%% USAGE: (PYTHON <string>)
==>

; --- eval errors ---

>>> ;;; Error: eval with no arguments
... (eval)

%%% ERROR 'EVAL': 1 argument expected.
%%% USAGE: (EVAL <sexpr>)
==>

>>> ;;; Error: eval with too many arguments
... (eval 1 2)

%%% ERROR 'EVAL': 1 argument expected.
%%% USAGE: (EVAL <sexpr>)
==>



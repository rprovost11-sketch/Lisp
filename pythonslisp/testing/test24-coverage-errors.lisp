; ============================================================
; test24-coverage-errors.lisp
; Error cases for under-tested error paths.
; ============================================================

; --- backquote argument count errors ---

>>> ;;; backquote with no arguments
... (backquote)

%%% ERROR 'BACKQUOTE': 1 argument expected.
%%% USAGE: (BACKQUOTE <sexpr>)
==>

>>> ;;; backquote with too many arguments
... (backquote 1 2)

%%% ERROR 'BACKQUOTE': 1 argument expected.
%%% USAGE: (BACKQUOTE <sexpr>)
==>

; --- log domain errors ---

>>> ;;; log of zero is domain error
... (log 0)

%%% ERROR 'LOG': Invalid argument.
%%% USAGE: (LOG <number> &optional ( <base> e ))
==>

>>> ;;; log of negative is domain error
... (log -1)

%%% ERROR 'LOG': Invalid argument.
%%% USAGE: (LOG <number> &optional ( <base> e ))
==>

; --- eval argument count errors ---

>>> ;;; eval with no arguments
... (eval)

%%% ERROR 'EVAL': 1 argument expected.
%%% USAGE: (EVAL <sexpr>)
==>

; --- funcall errors ---

>>> ;;; funcall with no arguments
... (funcall)

%%% ERROR 'FUNCALL': 1 or more arguments expected
%%% USAGE: (FUNCALL <fnNameSymbol> <arg1> <arg2> ...)
==>

; --- at errors on strings ---

>>> ;;; at out of range on string
... (at 5 "hi")

%%% ERROR 'AT': Invalid argument key/index.
%%% USAGE: (AT <keyOrIndex> <mapListOrStr>)
==>

>>> ;;; at on empty string
... (at 0 "")

%%% ERROR 'AT': Invalid argument key/index.
%%% USAGE: (AT <keyOrIndex> <mapListOrStr>)
==>

>>> ;;; at with missing key on map
... (at 'missing (map (a 1)))

%%% ERROR 'AT': Invalid argument key/index.
%%% USAGE: (AT <keyOrIndex> <mapListOrStr>)
==>

; --- at-delete additional errors ---

>>> ;;; at-delete out of range on list
... (at-delete 10 '(1 2))

%%% ERROR 'AT-DELETE': Bad index or key into collection.
%%% USAGE: (AT-DELETE <keyOrIndex> <mapOrList>)
==>

>>> ;;; at-delete missing key on map
... (at-delete 'missing (map (a 1)))

%%% ERROR 'AT-DELETE': Bad index or key into collection.
%%% USAGE: (AT-DELETE <keyOrIndex> <mapOrList>)
==>

; --- conversion errors ---

>>> ;;; float on list (non-numeric)
... (float (list 1))

%%% ERROR 'FLOAT': Invalid argument.
%%% USAGE: (FLOAT <number>)
==>

>>> ;;; rational on list (non-numeric)
... (rational (list 1))

%%% ERROR 'RATIONAL': Invalid argument.
%%% USAGE: (RATIONAL <number>)
==>

>>> ;;; symbol with empty string
... (symbol "")

%%% ERROR 'SYMBOL': The resulting string "" is not a valid Lisp symbol.
%%% USAGE: (SYMBOL <string1> <string2> ...)
==>

; --- python errors ---

>>> ;;; python with non-string argument
... (python 42)

%%% ERROR 'PYTHON': Argument expected to be a string.
%%% USAGE: (PYTHON <string>)
==>


; --- library function argument count errors ---

>>> ;;; evenp with no arguments
... (evenp)

%%% Error binding arguments in call to function "EVENP".
%%% Too few positional arguments.
==>

>>> ;;; oddp with no arguments
... (oddp)

%%% Error binding arguments in call to function "ODDP".
%%% Too few positional arguments.
==>

>>> ;;; zerop with no arguments
... (zerop)

%%% Error binding arguments in call to function "ZEROP".
%%% Too few positional arguments.
==>

>>> ;;; minusp with no arguments
... (minusp)

%%% Error binding arguments in call to function "MINUSP".
%%% Too few positional arguments.
==>

>>> ;;; plusp with no arguments
... (plusp)

%%% Error binding arguments in call to function "PLUSP".
%%% Too few positional arguments.
==>

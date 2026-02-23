; ============================================================
; test20-error-cases.lisp
; Error cases for primitives not covered by other test files.
; ============================================================

; --- Arithmetic errors ---

>>> ;;; (*) with no args
... (*)

%%% ERROR '*': Invalid argument.
%%% USAGE: (* <number1> <number2> ...)
==>

>>> ;;; (-) with no args
... (-)

%%% ERROR '-': Invalid argument.
%%% USAGE: (- <number1> <number2> ...)
==>

>>> ;;; (/) with no args
... (/)

%%% ERROR '/': Invalid argument.
%%% USAGE: (/ <number1> <number2> ...)
==>

>>> ;;; integer division by zero
... (// 7 0)

%%% ERROR '//': division by zero
%%% USAGE: (// <number1> <number2>)
==>

>>> ;;; modulo by zero
... (mod 7 0)

%%% ERROR 'MOD': division by zero
%%% USAGE: (MOD <number1> <number2>)
==>

>>> ;;; integer division with non-number
... (// "a" 2)

%%% ERROR '//': Invalid argument.
%%% USAGE: (// <number1> <number2>)
==>

>>> ;;; modulo with non-number
... (mod "a" 2)

%%% ERROR 'MOD': Invalid argument.
%%% USAGE: (MOD <number1> <number2>)
==>

>>> ;;; negation of non-number
... (- "a")

%%% ERROR '-': Invalid argument.
%%% USAGE: (- <number1> <number2> ...)
==>

>>> ;;; division with non-number
... (/ "a" 2)

%%% ERROR '/': Invalid argument.
%%% USAGE: (/ <number1> <number2> ...)
==>

>>> ;;; min with no args
... (min)

%%% ERROR 'MIN': Invalid argument.
%%% USAGE: (MIN <number1> <number2> ...)
==>

>>> ;;; max with no args
... (max)

%%% ERROR 'MAX': Invalid argument.
%%% USAGE: (MAX <number1> <number2> ...)
==>

; --- Trig domain errors ---

>>> ;;; asin out of domain
... (asin 2)

%%% ERROR 'ASIN': Invalid argument.
%%% USAGE: (ASIN <number>)
==>

>>> ;;; acos out of domain
... (acos 2)

%%% ERROR 'ACOS': Invalid argument.
%%% USAGE: (ACOS <number>)
==>

; --- Random errors ---

>>> ;;; random with negative
... (random -1)

%%% ERROR 'RANDOM': Argument expected to be non-negative.
%%% USAGE: (RANDOM <integerOrFloat>)
==>

>>> ;;; random with string
... (random "a")

%%% ERROR 'RANDOM': Invalid argument type.
%%% USAGE: (RANDOM <integerOrFloat>)
==>

>>> ;;; random with no args
... (random)

%%% ERROR 'RANDOM': Exactly 1 number argument expected.
%%% USAGE: (RANDOM <integerOrFloat>)
==>

>>> ;;; random with too many args
... (random 1 2)

%%% ERROR 'RANDOM': Exactly 1 number argument expected.
%%% USAGE: (RANDOM <integerOrFloat>)
==>

; --- Relational type errors ---

>>> ;;; less-than with incomparable types
... (< "a" 1)

%%% ERROR '<': Invalid argument.  Arguments are not comparable.
%%% USAGE: (< <expr1> <expr2> ...)
==>

>>> ;;; greater-than with incomparable types
... (> "a" 1)

%%% ERROR '>': Invalid argument.  Arguments are not comparable.
%%% USAGE: (> <expr1> <expr2> ...)
==>

>>> ;;; less-equal with incomparable types
... (<= "a" 1)

%%% ERROR '<=': Invalid argument.  Arguments are not comparable.
%%% USAGE: (<= <expr1> <expr2> ...)
==>

>>> ;;; greater-equal with incomparable types
... (>= "a" 1)

%%% ERROR '>=': Invalid argument.  Arguments are not comparable.
%%% USAGE: (>= <expr1> <expr2> ...)
==>

; --- List operation errors ---

>>> ;;; push! with non-list
... (push! 1 2)

%%% ERROR 'PUSH!': 1st argument expected to be a list.
%%% USAGE: (PUSH! <list> <value>)
==>

>>> ;;; pop! with non-list
... (pop! 1)

%%% ERROR 'POP!': 1st argument expected to be a list.
%%% USAGE: (POP! <list>)
==>

>>> ;;; pop! on empty list
... (pop! '())

%%% ERROR 'POP!': Invalid argument.
%%% USAGE: (POP! <list>)
==>

>>> ;;; cons with non-list second arg
... (cons 1 2)

%%% ERROR 'CONS': 2nd argument expected to be a list.
%%% USAGE: (CONS <obj> <list>)
==>

>>> ;;; at-insert with non-integer index
... (at-insert "a" '(1 2) 3)

%%% ERROR 'AT-INSERT': Argument 1 expected to be an integer index.
%%% USAGE: (AT-INSERT <index> <list> <newItem>)
==>

>>> ;;; at-insert with non-list
... (at-insert 0 1 3)

%%% ERROR 'AT-INSERT': Argument 2 expected to be a list.
%%% USAGE: (AT-INSERT <index> <list> <newItem>)
==>

>>> ;;; at on non-collection
... (at 0 42)

%%% ERROR 'AT': Invalid argument.  List, Map, or String expected.
%%% USAGE: (AT <keyOrIndex> <mapListOrStr>)
==>

>>> ;;; at-delete with bad index
... (at-delete 5 '(1 2 3))

%%% ERROR 'AT-DELETE': Bad index or key into collection.
%%% USAGE: (AT-DELETE <keyOrIndex> <mapOrList>)
==>

>>> ;;; at-delete on non-collection
... (at-delete 1 1)

%%% ERROR 'AT-DELETE': Argument 2 expected to be a list or map.
%%% USAGE: (AT-DELETE <keyOrIndex> <mapOrList>)
==>

>>> ;;; sort with incomparable types
... (sort (list 3 "a" 1))

%%% ERROR 'SORT': Cannot sort a list with incomparable types.
%%% USAGE: (SORT <list>)
==>

; --- Arg count errors for list ops ---

>>> (push!)

%%% ERROR 'PUSH!': 2 arguments expected.
%%% USAGE: (PUSH! <list> <value>)
==>

>>> (push! '() 1 2)

%%% ERROR 'PUSH!': 2 arguments expected.
%%% USAGE: (PUSH! <list> <value>)
==>

>>> (pop!)

%%% ERROR 'POP!': 1 argument expected.
%%% USAGE: (POP! <list>)
==>

>>> (pop! '() 1)

%%% ERROR 'POP!': 1 argument expected.
%%% USAGE: (POP! <list>)
==>

>>> (at)

%%% ERROR 'AT': 2 arguments expected.
%%% USAGE: (AT <keyOrIndex> <mapListOrStr>)
==>

>>> (at 1)

%%% ERROR 'AT': 2 arguments expected.
%%% USAGE: (AT <keyOrIndex> <mapListOrStr>)
==>

>>> (at 1 2 3)

%%% ERROR 'AT': 2 arguments expected.
%%% USAGE: (AT <keyOrIndex> <mapListOrStr>)
==>

>>> (at-delete)

%%% ERROR 'AT-DELETE': Exactly 2 arguments expected.
%%% USAGE: (AT-DELETE <keyOrIndex> <mapOrList>)
==>

>>> (at-delete 1)

%%% ERROR 'AT-DELETE': Exactly 2 arguments expected.
%%% USAGE: (AT-DELETE <keyOrIndex> <mapOrList>)
==>

>>> (at-insert)

%%% ERROR 'AT-INSERT': Exactly 3 arguments expected.
%%% USAGE: (AT-INSERT <index> <list> <newItem>)
==>

>>> (at-insert 1)

%%% ERROR 'AT-INSERT': Exactly 3 arguments expected.
%%% USAGE: (AT-INSERT <index> <list> <newItem>)
==>

>>> (at-insert 1 2)

%%% ERROR 'AT-INSERT': Exactly 3 arguments expected.
%%% USAGE: (AT-INSERT <index> <list> <newItem>)
==>

; --- hasValue? / hasKey? / update! errors ---

>>> (hasValue?)

%%% ERROR 'HASVALUE?': 2 arguments expected.
%%% USAGE: (HASVALUE? <listOrMap> <value>)
==>

>>> ;;; hasValue? on non-collection
... (hasValue? 1 2)

%%% ERROR 'HASVALUE?': Invalid argument.  Argument 1 expected to be a list or map.
%%% USAGE: (HASVALUE? <listOrMap> <value>)
==>

>>> (hasKey?)

%%% ERROR 'HASKEY?': 2 arguments expected.
%%% USAGE: (HASKEY? <map> <key>)
==>

>>> ;;; hasKey? with non-map first arg
... (hasKey? 1 (map))

%%% ERROR 'HASKEY?': Invalid argument 1.  Map expected.
%%% USAGE: (HASKEY? <map> <key>)
==>

>>> (update!)

%%% ERROR 'UPDATE!': 2 arguments expected.
%%% USAGE: (UPDATE! <map1> <map2>)
==>

>>> ;;; update! with non-map first arg
... (update! 1 (map))

%%% ERROR 'UPDATE!': Argument 1 expected to be a map.
%%% USAGE: (UPDATE! <map1> <map2>)
==>

>>> ;;; update! with non-map second arg
... (update! (map) 1)

%%% ERROR 'UPDATE!': Argument 2 expected to be a map.
%%% USAGE: (UPDATE! <map1> <map2>)
==>

; --- Map errors ---

>>> ;;; map with mixed key types
... (map (a 1) (2 2))

%%% ERROR 'MAP': All keys in a map must be the same type. Entry 2 is int, expected str.
%%% USAGE: (MAP (<key1> <val1>) (<key2> <val2>) ...)
==>

; --- Control structure errors ---

>>> (while)

%%% Too few positional arguments.
==>

>>> ;;; while with no body
... (while t)

%%% while: at least one body expression is required
==>

>>> (dotimes)

%%% Too few positional arguments.
==>

>>> ;;; dotimes with no body
... (dotimes (i 3))

%%% dotimes: at least one body expression is required
==>

>>> ;;; dotimes control list must be a list
... (dotimes i 1)

%%% dotimes: first argument must be a (var count) list
==>

>>> ;;; dotimes control list must have 2 elements
... (dotimes (i 3 4) 1)

%%% dotimes: first argument must have exactly 2 elements
==>

>>> ;;; dotimes variable must be a symbol
... (dotimes (1 3) 1)

%%% dotimes: loop variable must be a symbol
==>

>>> ;;; dotimes count must be integer
... (dotimes (i "a") 1)

%%% dotimes: count must be an integer
==>

>>> (dolist)

%%% Too few positional arguments.
==>

>>> ;;; dolist with bad control spec
... (dolist x (write! x))

%%% dolist: first argument must be a (variable list) control spec
==>

>>> ;;; dolist variable must be a symbol
... (dolist (1 '(a b)) (write! 1))

%%% dolist: control spec variable must be a symbol
==>

>>> ;;; dolist list must evaluate to a list
... (dolist (x 42) (write! x))

%%% dolist: list must evaluate to a list
==>

>>> (case)

%%% ERROR 'CASE': 2 or more arguments expected.
%%% USAGE: (CASE <sexpr> (<val1> <body1>) (<val2> <body2>) ...)
==>

; --- let / let* errors ---

>>> (let)

%%% ERROR 'LET': 2 or more arguments expected.
%%% USAGE: (LET ( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <sexpr1> <sexpr2> ...))
==>

>>> ;;; let first arg must be list
... (let 1 2)

%%% ERROR 'LET': The first argument to let expected to be a list of variable initializations.
%%% USAGE: (LET ( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <sexpr1> <sexpr2> ...))
==>

>>> ;;; let var spec first element must be symbol
... (let ((1 2)) 1)

%%% ERROR 'LET': First element of a variable initializer pair expected to be a symbol.
%%% USAGE: (LET ( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <sexpr1> <sexpr2> ...))
==>

>>> ;;; let var spec must be 1 or 2 elements
... (let ((x 1 2)) 1)

%%% ERROR 'LET': Variable initializer spec expected to be 1 or 2 elements long.
%%% USAGE: (LET ( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <sexpr1> <sexpr2> ...))
==>

>>> (let*)

%%% ERROR 'LET*': 2 or more arguments expected.
%%% USAGE: (LET* ( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <sexpr1> <sexpr2> ...))
==>

>>> ;;; let* first arg must be list
... (let* 1 2)

%%% ERROR 'LET*': The first argument to let expected to be a list of variable initializations.
%%% USAGE: (LET* ( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <sexpr1> <sexpr2> ...))
==>

; --- Macro / lambda errors ---

>>> (defmacro)

%%% ERROR 'DEFMACRO': 3 or more arguments expected.
%%% USAGE: (DEFMACRO <symbol> <lambda-list> &optional <sexpr1> <sexpr2> ...)
==>

>>> ;;; defmacro first arg must be symbol
... (defmacro 1 () 1)

%%% ERROR 'DEFMACRO': Argument 1 expected to be a symbol.
%%% USAGE: (DEFMACRO <symbol> <lambda-list> &optional <sexpr1> <sexpr2> ...)
==>

>>> ;;; defmacro second arg must be list
... (defmacro foo 1 1)

%%% ERROR 'DEFMACRO': Argument 2 expected to be a list of params.
%%% USAGE: (DEFMACRO <symbol> <lambda-list> &optional <sexpr1> <sexpr2> ...)
==>

>>> (lambda)

%%% ERROR 'LAMBDA': 1 or more arguments expected.
%%% USAGE: (LAMBDA <lambda-list> <sexpr1> <sexpr2> ...)
==>

>>> ;;; lambda with no body — valid in CL, returns NIL when called
... (lambda ())

==> (FUNCTION  () ... )

; --- Quote / backquote errors ---

>>> (quote)

%%% ERROR 'QUOTE': 1 argument expected.
%%% USAGE: (QUOTE <sexpr>)
==>

>>> (quote 1 2)

%%% ERROR 'QUOTE': 1 argument expected.
%%% USAGE: (QUOTE <sexpr>)
==>

>>> ;;; comma outside backquote
... ,x

%%% ERROR 'COMMA': COMMA can only occur inside a BACKQUOTE.
%%% USAGE: (COMMA <sexpr>)
==>

>>> ;;; comma-at outside backquote
... ,@x

%%% ERROR 'COMMA-AT': COMMA-AT can only occur inside a BACKQUOTE.
%%% USAGE: (COMMA-AT <sexpr>)
==>

>>> ;;; comma-at result must be a list
... `(a ,@1 c)

%%% ERROR 'COMMA-AT': Argument 1 must evaluate to a List.
%%% USAGE: (COMMA-AT <sexpr>)
==>

; --- Macroexpand errors ---

>>> (macroexpand)

%%% ERROR 'MACROEXPAND': Exactly 1 argument expected.
%%% USAGE: (MACROEXPAND '(<macroName> <arg1> <arg2> ...))
==>

>>> ;;; macroexpand non-list returns unchanged
... (macroexpand 1)

==> 1

>>> ;;; macroexpand non-macro head returns form unchanged
... (macroexpand '(1))

==> (1)

; --- setf / undef! / symtab! errors ---

>>> (setf)

%%% Too few positional arguments.
==>

>>> ;;; setf with odd number of args
... (setf x)

%%% Too few positional arguments.
==>

>>> ;;; setf with 3 args (odd)
... (setf x 1 y)

%%% Too few positional arguments.
==>

>>> (undef!)

%%% ERROR 'UNDEF!': 1 argument expected.
%%% USAGE: (UNDEF! <symbol>)
==>

>>> ;;; undef! with non-symbol
... (undef! 1)

%%% ERROR 'UNDEF!': Argument expected to be a symbol.
%%% USAGE: (UNDEF! <symbol>)
==>

>>> ;;; undef! with too many args
... (undef! 'a 'b)

%%% ERROR 'UNDEF!': 1 argument expected.
%%% USAGE: (UNDEF! <symbol>)
==>

>>> ;;; symtab! with args
... (symtab! 1)

%%% ERROR 'SYMTAB!': 0 arguments expected.
%%% USAGE: (SYMTAB! )
==>

; --- Conversion errors ---

>>> ;;; integer from invalid string
... (integer "hello")

%%% ERROR 'INTEGER': Invalid argument.
%%% USAGE: (INTEGER <number> &optional (<base> 10))
==>

>>> (string)

%%% ERROR 'STRING': 1 or more arguments expected.
%%% USAGE: (STRING <object1> <object2> ...)
==>

>>> (ustring)

%%% ERROR 'USTRING': 1 or more arguments expected.
%%% USAGE: (USTRING <object1> <object2> ...)
==>

>>> (symbol)

%%% ERROR 'SYMBOL': 1 or more string argument expected.
%%% USAGE: (SYMBOL <string1> <string2> ...)
==>

>>> ;;; symbol from numeric string is not valid
... (symbol 1)

%%% ERROR 'SYMBOL': The resulting string "1" is not a valid Lisp symbol.
%%% USAGE: (SYMBOL <string1> <string2> ...)
==>

; --- Parse errors ---

>>> (parse)

%%% ERROR 'PARSE': 1 string argument expected.
%%% USAGE: (PARSE <string>)
==>

>>> ;;; parse with non-string
... (parse 1)

%%% ERROR 'PARSE': Argument expected to be a string.
%%% USAGE: (PARSE <string>)
==>

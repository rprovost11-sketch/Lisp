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

>>> ;;; lambda with no body valid in CL, returns NIL when called
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

>>> ;;; comma-at at top level of backquote: splice requires a list context
... `,@(list 1 2)

%%% Ill-placed ,@ (COMMA-AT): splice requires a list context.
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

; ============================================================
; test21-missing-errors.lisp
; Error condition tests for gaps not covered by other test files.
; ============================================================

; --- SETF additional error paths ---

>>> ;;; setf with NIL lvalue
... (setf () 5)

%%% setf: lvalue cannot be NIL or ()
==>

>>> ;;; setf (at ...) form with wrong element count
... (setf (at 1) 5)

%%% setf: (at ...) place form expected 3 elements
==>

>>> ;;; setf with unrecognized accessor (not in defsetf registry)
... (setf (blah 42) 5)

%%% ERROR 'SET-ACCESSOR!': No setf expander registered for BLAH.
%%% USAGE: (SET-ACCESSOR! <accessor-symbol> <instance> <newValue>)
==>

; --- IF error paths ---

>>> ;;; if with no arguments
... (if)

%%% ERROR 'IF': 2 or 3 arguments expected.
%%% USAGE: (IF <cond> <conseq> &optional <alt>)
==>

>>> ;;; if with too many arguments
... (if t 1 2 3)

%%% ERROR 'IF': 2 or 3 arguments expected.
%%% USAGE: (IF <cond> <conseq> &optional <alt>)
==>

; --- COND additional error paths ---

>>> ;;; cond entry that is not a list
... (cond 1)

%%% ERROR 'COND': Entry 1 must be a non-empty list.
%%% USAGE: (COND (<cond1> <body1>) (<cond2> <body2>) ...)
==>

>>> ;;; cond entry with condition but no body
... (cond (t))

==> NIL

; --- CASE additional error paths ---

>>> ;;; case with no clauses
... (case 1)

%%% ERROR 'CASE': 2 or more arguments expected.
%%% USAGE: (CASE <sexpr> (<val1> <body1>) (<val2> <body2>) ...)
==>

>>> ;;; case entry that is not a list
... (case 1 2)

%%% ERROR 'CASE': Entry 1 must be a non-empty list.
%%% USAGE: (CASE <sexpr> (<val1> <body1>) (<val2> <body2>) ...)
==>

>>> ;;; case entry with value but no body
... (case 1 (1))

==> NIL

; --- MAP additional error path ---

>>> ;;; map entry with single element (not a pair)
... (map (1))

%%% ERROR 'MAP': Entry 1 does not contain a (key value) pair.
%%% USAGE: (MAP (<key1> <val1>) (<key2> <val2>) ...)
==>

; --- MACROEXPAND additional error path ---

>>> ;;; macroexpand empty list returns NIL
... (macroexpand '())

==> NIL

; --- DEFMACRO docstring-only error ---

>>> ;;; defmacro with docstring but no body
... (defmacro foo () "docstring")

%%% ERROR 'DEFMACRO': At least one body expression expected after docstring.
%%% USAGE: (DEFMACRO <symbol> <lambda-list> &optional <sexpr1> <sexpr2> ...)
==>

; --- LAMBDA docstring-only error ---

>>> ;;; lambda with docstring but no body valid in CL, returns NIL when called
... (lambda () "docstring")

==> (FUNCTION  () ... )

; --- LET additional error path ---

>>> ;;; let with non-symbol non-list initializer
... (let (1) x)

%%% ERROR 'LET': Variable initializer spec expected to be a symbol or a list.
%%% USAGE: (LET ( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <sexpr1> <sexpr2> ...))
==>

; --- LET* additional error paths ---

>>> ;;; let* first element of initializer pair must be symbol
... (let* ((1 2)) 1)

%%% ERROR 'LET*': First element of a variable initializer pair expected to be a symbol.
%%% USAGE: (LET* ( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <sexpr1> <sexpr2> ...))
==>

>>> ;;; let* initializer spec too long
... (let* ((x 1 2)) 1)

%%% ERROR 'LET*': Variable initializer spec expected to be 1 or 2 elements long.
%%% USAGE: (LET* ( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <sexpr1> <sexpr2> ...))
==>

>>> ;;; let* with non-symbol non-list initializer
... (let* (1) x)

%%% ERROR 'LET*': Variable initializer spec expected to be a symbol or a list.
%%% USAGE: (LET* ( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <sexpr1> <sexpr2> ...))
==>

; --- DOLIST additional error path ---

>>> ;;; dolist with no body expressions is allowed (returns NIL)
... (dolist (x '(1 2)))
...

==> NIL

; --- Arithmetic argument count errors ---

>>> ;;; sin with no arguments
... (sin)

%%% ERROR 'SIN': 1 argument expected.
%%% USAGE: (SIN <radians>)
==>

>>> ;;; sin with too many arguments
... (sin 1 2)

%%% ERROR 'SIN': 1 argument expected.
%%% USAGE: (SIN <radians>)
==>

>>> ;;; cos with no arguments
... (cos)

%%% ERROR 'COS': 1 argument expected.
%%% USAGE: (COS <radians>)
==>

>>> ;;; cos with too many arguments
... (cos 1 2)

%%% ERROR 'COS': 1 argument expected.
%%% USAGE: (COS <radians>)
==>

>>> ;;; asin with no arguments
... (asin)

%%% ERROR 'ASIN': 1 argument expected.
%%% USAGE: (ASIN <number>)
==>

>>> ;;; acos with no arguments
... (acos)

%%% ERROR 'ACOS': 1 argument expected.
%%% USAGE: (ACOS <number>)
==>

>>> ;;; integer division with too many arguments
... (// 1 2 3)

%%% ERROR '//': 2 arguments expected.
%%% USAGE: (// <number1> <number2>)
==>

>>> ;;; modulo with too many arguments
... (mod 1 2 3)

%%% ERROR 'MOD': 2 arguments expected.
%%% USAGE: (MOD <number1> <number2>)
==>

; --- Arithmetic type errors ---

>>> ;;; gcd with non-number
... (gcd "a" 2)

%%% ERROR 'GCD': Invalid argument.
%%% USAGE: (GCD <integer1> <integer2> ...)
==>

>>> ;;; lcm with non-number
... (lcm "a" 2)

%%% ERROR 'LCM': Invalid argument.
%%% USAGE: (LCM <integer1> <integer2> ...)
==>

>>> ;;; min with non-number
... (min "a" 1)

%%% ERROR 'MIN': Invalid argument.
%%% USAGE: (MIN <number1> <number2> ...)
==>

>>> ;;; max with non-number
... (max "a" 1)

%%% ERROR 'MAX': Invalid argument.
%%% USAGE: (MAX <number1> <number2> ...)
==>

>>> ;;; expt with non-number base
... (expt "a" 2)

%%% ERROR 'EXPT': Invalid argument type.  Arguments expected to be numbers.
%%% USAGE: (EXPT <base> <power>)
==>

>>> ;;; cos with non-number
... (cos "a")

%%% ERROR 'COS': Invalid argument.
%%% USAGE: (COS <radians>)
==>

>>> ;;; sin with non-number
... (sin "a")

%%% ERROR 'SIN': Invalid argument.
%%% USAGE: (SIN <radians>)
==>

; --- Additional math argument count errors ---

>>> ;;; expt with no arguments
... (expt)

%%% ERROR 'EXPT': Exactly two arguments expected.
%%% USAGE: (EXPT <base> <power>)
==>

>>> ;;; expt with one argument
... (expt 1)

%%% ERROR 'EXPT': Exactly two arguments expected.
%%% USAGE: (EXPT <base> <power>)
==>

>>> ;;; expt with three arguments
... (expt 1 2 3)

%%% ERROR 'EXPT': Exactly two arguments expected.
%%% USAGE: (EXPT <base> <power>)
==>

>>> ;;; log with no arguments
... (log)

%%% ERROR 'LOG': 1 or 2 arguments expected.
%%% USAGE: (LOG <number> &optional <base>)
==>

>>> ;;; log with too many arguments
... (log 1 2 3)

%%% ERROR 'LOG': 1 or 2 arguments expected.
%%% USAGE: (LOG <number> &optional <base>)
==>

>>> ;;; log with non-number
... (log "a")

%%% ERROR 'LOG': Invalid argument.
%%% USAGE: (LOG <number> &optional <base>)
==>

; --- Predicate argument count errors ---

>>> ;;; atom with no arguments
... (atom)

%%% ERROR 'ATOM': 1 argument expected.
%%% USAGE: (ATOM <sexpr>)
==>

>>> ;;; atom with too many arguments
... (atom 1 2)

%%% ERROR 'ATOM': 1 argument expected.
%%% USAGE: (ATOM <sexpr>)
==>

>>> ;;; mapp with no arguments
... (mapp)

%%% ERROR 'MAPP': 1 argument expected.
%%% USAGE: (MAPP <sexpr>)
==>

>>> ;;; mapp with too many arguments
... (mapp 1 2)

%%% ERROR 'MAPP': 1 argument expected.
%%% USAGE: (MAPP <sexpr>)
==>

; --- eq / is? argument count errors (is? is an alias for eq) ---

>>> ;;; is? with no arguments
... (is?)

%%% ERROR 'EQ': 2 arguments expected.
%%% USAGE: (EQ <a> <b>)
==>

>>> ;;; is? with one argument
... (is? 1)

%%% ERROR 'EQ': 2 arguments expected.
%%% USAGE: (EQ <a> <b>)
==>

>>> ;;; is? with too many arguments
... (is? 1 2 3)

%%% ERROR 'EQ': 2 arguments expected.
%%% USAGE: (EQ <a> <b>)
==>

; --- Conversion argument count errors ---

>>> ;;; float with no arguments
... (float)

%%% ERROR 'FLOAT': 1 argument expected.
%%% USAGE: (FLOAT <number>)
==>

>>> ;;; float with too many arguments
... (float 1 2)

%%% ERROR 'FLOAT': 1 argument expected.
%%% USAGE: (FLOAT <number>)
==>

>>> ;;; integer with no arguments
... (integer)

%%% ERROR 'INTEGER': 1 or 2 arguments expected.
%%% USAGE: (INTEGER <number> &optional (<base> 10))
==>

>>> ;;; integer with too many arguments
... (integer 1 2 3)

%%% ERROR 'INTEGER': 1 or 2 arguments expected.
%%% USAGE: (INTEGER <number> &optional (<base> 10))
==>

>>> ;;; rational with non-number
... (rational "hello")

%%% ERROR 'RATIONAL': Invalid argument.
%%% USAGE: (RATIONAL <number>)
==>

; --- CAR/CDR argument count errors ---

>>> ;;; car with no arguments
... (car)

%%% ERROR 'CAR': 1 argument expected.
%%% USAGE: (CAR <list>)
==>

>>> ;;; car with too many arguments
... (car 1 2)

%%% ERROR 'CAR': 1 argument expected.
%%% USAGE: (CAR <list>)
==>

>>> ;;; cdr with no arguments
... (cdr)

%%% ERROR 'CDR': 1 argument expected.
%%% USAGE: (CDR <list>)
==>

>>> ;;; cdr with too many arguments
... (cdr 1 2)

%%% ERROR 'CDR': 1 argument expected.
%%% USAGE: (CDR <list>)
==>

; --- ATAN additional error paths ---

>>> ;;; atan with too many arguments
... (atan 1 2 3)

%%% ERROR 'ATAN': 1 or 2 arguments expected.
%%% USAGE: (ATAN <number1> &optional <number2>)
==>

>>> ;;; atan with non-number
... (atan "a")

%%% ERROR 'ATAN': Invalid argument.
%%% USAGE: (ATAN <number1> &optional <number2>)
==>

; --- EVAL additional error path ---

>>> ;;; eval with too many arguments
... (eval 1 2)

%%% ERROR 'EVAL': 1 argument expected.
%%% USAGE: (EVAL <sexpr>)
==>

; --- NTH out-of-range error ---

>>> ;;; nth with out-of-range index
... (nth 10 '(1 2 3))

%%% ERROR 'AT': Invalid argument key/index.
%%% USAGE: (AT <keyOrIndex> <mapListOrStr>)
==>

; --- WRITEF format error ---

>>> ;;; writef with out-of-range format index
... (writef "{2}" (list 1))

%%% ERROR 'WRITEF': Format error: Replacement index 2 out of range for positional args tuple
%%% USAGE: (WRITEF <formatString> &optional <MapOrList> <stream>)
==>

; --- SETF AT: container type and key/index errors ---

>>> ;;; setf at with non-list non-map container
... (setf (at 0 42) 5)

%%% ERROR 'AT-SET': Invalid argument.  List or Map expected.
%%% USAGE: (AT-SET <keyOrIndex> <mapListOrStr> <newValue>)
==>

>>> ;;; setf at with out-of-range index
... (setf (at 99 (list 1 2)) 5)

%%% ERROR 'AT-SET': Invalid argument key/index.
%%% USAGE: (AT-SET <keyOrIndex> <mapListOrStr> <newValue>)
==>

; --- DEFMACRO: empty body without docstring ---

>>> ;;; defmacro with empty body (no docstring)
... (defmacro emptyb22 ())

%%% ERROR 'DEFMACRO': At least one body expression expected.
%%% USAGE: (DEFMACRO <symbol> <lambda-list> &optional <sexpr1> <sexpr2> ...)
==>

; --- APPLY additional error paths ---

>>> ;;; apply with unbound symbol
... (apply 'nonexistent-fn-xyz '(1 2))

%%% ERROR 'APPLY': First argument "NONEXISTENT-FN-XYZ" expected to be the name of a callable.
%%% USAGE: (APPLY <function> &rest <args> <argsList>)
==>

>>> ;;; apply with non-symbol non-callable first arg
... (apply 42 '(1 2))

%%% ERROR 'APPLY': First argument expected to be a symbol.
%%% USAGE: (APPLY <function> &rest <args> <argsList>)
==>

; --- ASIN/ACOS: too many arguments ---

>>> ;;; asin with too many arguments
... (asin 0 1)

%%% ERROR 'ASIN': 1 argument expected.
%%% USAGE: (ASIN <number>)
==>

>>> ;;; acos with too many arguments
... (acos 0 1)

%%% ERROR 'ACOS': 1 argument expected.
%%% USAGE: (ACOS <number>)
==>

; --- MAP: invalid key type ---

>>> ;;; map with list as key
... (map ('(1) 2))

%%% ERROR 'MAP': Entry 1 has an invalid <key> type.
%%% USAGE: (MAP (<key1> <val1>) (<key2> <val2>) ...)
==>

; --- Division: zero by zero ---

>>> ;;; division zero by zero
... (/ 0 0)

%%% ERROR '/': division by zero
%%% USAGE: (/ <number1> <number2> ...)
==>

; --- Conversion: symbol arguments ---

>>> ;;; float with symbol argument
... (float 'abc)

%%% ERROR 'FLOAT': Invalid argument.
%%% USAGE: (FLOAT <number>)
==>

>>> ;;; integer with symbol argument
... (integer 'abc)

%%% ERROR 'INTEGER': Invalid argument.
%%% USAGE: (INTEGER <number> &optional (<base> 10))
==>

; --- APPEND: non-list argument in middle ---

>>> ;;; append with non-list in middle
... (append '(1) 2 '(3))

%%% ERROR 'APPEND': Invalid argument.
%%% USAGE: (APPEND <list1> <list2> ...)
==>

; --- DEFSETF-INTERNAL errors ---

>>> ;;; defsetf-internal with no arguments
... (defsetf-internal)

%%% ERROR 'DEFSETF-INTERNAL': 2 arguments expected.
%%% USAGE: (DEFSETF-INTERNAL <accessor-symbol> <field-symbol>)
==>

>>> ;;; defsetf-internal with non-symbol arguments
... (defsetf-internal 1 2)

%%% ERROR 'DEFSETF-INTERNAL': Both arguments must be symbols.
%%% USAGE: (DEFSETF-INTERNAL <accessor-symbol> <field-symbol>)
==>

; --- HELP errors ---

>>> ;;; help with too many arguments
... (help 1 2)

%%% ERROR 'HELP': Too many arguments.  Received 2
%%% USAGE: (HELP &optional callableSymbol)
==>

>>> ;;; help with non-callable argument
... (help 42)

%%% ERROR 'HELP': First argument expected to be a callable.
%%% USAGE: (HELP &optional callableSymbol)
==>

; --- SORT: non-list argument ---

>>> ;;; sort with non-list argument
... (sort 'abc)

%%% ERROR 'SORT': Argument 1 expected to be a list.
%%% USAGE: (SORT <list>)
==>

; --- DOLIST: non-list evaluated argument ---

>>> ;;; dolist with non-list as collection
... (dolist (x 42) (write! x))

%%% dolist: list must evaluate to a list
==>

; --- COND: bad entry in non-first position ---

>>> ;;; cond with non-list entry in second position
... (cond (nil 1) 2)

%%% ERROR 'COND': Entry 2 must be a non-empty list.
%%% USAGE: (COND (<cond1> <body1>) (<cond2> <body2>) ...)
==>

; --- CASE: bad entry in non-first position ---

>>> ;;; case with non-list entry in second position (non-matching first)
... (case 99 (1 "one") 2)

%%% ERROR 'CASE': Entry 2 must be a non-empty list.
%%% USAGE: (CASE <sexpr> (<val1> <body1>) (<val2> <body2>) ...)

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
%%% USAGE: (LOG <number> &optional <base>)
==>

>>> ;;; log of negative is domain error
... (log -1)

%%% ERROR 'LOG': Invalid argument.
%%% USAGE: (LOG <number> &optional <base>)
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

; --- recursion-limit errors ---

>>> ;;; recursion-limit with non-integer
... (recursion-limit "abc")

%%% ERROR 'RECURSION-LIMIT': Argument must be an integer.
%%% USAGE: (RECURSION-LIMIT &optional <newLimit>)
==>

>>> ;;; recursion-limit with too many args
... (recursion-limit 1 2)

%%% ERROR 'RECURSION-LIMIT': Only one optional arg is allowed.
%%% USAGE: (RECURSION-LIMIT &optional <newLimit>)
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

; --- readall errors ---

>>> (readall)

%%% ERROR 'READALL': 1 argument expected.
%%% USAGE: (READALL <stream>)
==>

>>> (readall 42)

%%% ERROR 'READALL': Argument expected to be a stream.
%%% USAGE: (READALL <stream>)
==>

; --- tmpdir and path-join errors ---

>>> (tmpdir 1)

%%% ERROR 'TMPDIR': 0 arguments expected.
%%% USAGE: (TMPDIR )
==>

>>> (path-join)

%%% ERROR 'PATH-JOIN': 1 or more arguments expected.
%%% USAGE: (PATH-JOIN <path1> <path2> ...)
==>

>>> (path-join 42)

%%% ERROR 'PATH-JOIN': Argument 1 expected to be a string.
%%% USAGE: (PATH-JOIN <path1> <path2> ...)
==>

>>> (path-join "a" 42)

%%% ERROR 'PATH-JOIN': Argument 2 expected to be a string.
%%% USAGE: (PATH-JOIN <path1> <path2> ...)
==>

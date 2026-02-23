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

%%% ERROR 'COND': Entry 1 does not contain a (<cond:expr> <body:expr>) pair.
%%% USAGE: (COND (<cond1> <body1>) (<cond2> <body2>) ...)
==>

>>> ;;; cond entry with condition but no body
... (cond (t))

%%% ERROR 'COND': Entry 1 expects at least one body expression.
%%% USAGE: (COND (<cond1> <body1>) (<cond2> <body2>) ...)
==>

; --- CASE additional error paths ---

>>> ;;; case with no clauses
... (case 1)

%%% ERROR 'CASE': At least one case expected.
%%% USAGE: (CASE <sexpr> (<val1> <body1>) (<val2> <body2>) ...)
==>

>>> ;;; case entry that is not a list
... (case 1 2)

%%% ERROR 'CASE': Entry 1 does not contain a (<val> <body>) pair.
%%% USAGE: (CASE <sexpr> (<val1> <body1>) (<val2> <body2>) ...)
==>

>>> ;;; case entry with value but no body
... (case 1 (1))

%%% ERROR 'CASE': Case body expected.
%%% USAGE: (CASE <sexpr> (<val1> <body1>) (<val2> <body2>) ...)
==>

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

>>> ;;; lambda with docstring but no body
... (lambda () "docstring")

%%% ERROR 'LAMBDA': At least one body expression expected after docstring.
%%% USAGE: (LAMBDA <lambda-list> <sexpr1> <sexpr2> ...)
==>

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

; --- IS? argument count errors ---

>>> ;;; is? with no arguments
... (is?)

%%% ERROR 'IS?': 2 arguments expected.
%%% USAGE: (IS? <expr1> <expr2>)
==>

>>> ;;; is? with one argument
... (is? 1)

%%% ERROR 'IS?': 2 arguments expected.
%%% USAGE: (IS? <expr1> <expr2>)
==>

>>> ;;; is? with too many arguments
... (is? 1 2 3)

%%% ERROR 'IS?': 2 arguments expected.
%%% USAGE: (IS? <expr1> <expr2>)
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
%%% USAGE: (WRITEF <formatString> &optional <MapOrList>)
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

%%% ERROR 'COND': Entry 2 does not contain a (<cond:expr> <body:expr>) pair.
%%% USAGE: (COND (<cond1> <body1>) (<cond2> <body2>) ...)
==>

; --- CASE: bad entry in non-first position ---

>>> ;;; case with non-list entry in second position (non-matching first)
... (case 99 (1 "one") 2)

%%% ERROR 'CASE': Entry 2 does not contain a (<val> <body>) pair.
%%% USAGE: (CASE <sexpr> (<val1> <body1>) (<val2> <body2>) ...)

Python's Lisp
=============

Common Lisp programming language

The best python lisp implementation!

Implemented entirely in python 3.14.

Embed lisp in python code and python in lisp code; or interact with lisp in the included repl.


Lisp Documentation
==================

Goals:  Implement a substantial subset of the features of common lisp.

Features
- A full featured listener with session logging and resumption, interpreter
  reboot, and interpreter testing.
- Function definition with stack and recursion support.
- Common Lisp macro definition and the macroexpand primitive.
- &optional, &rest, &key and &aux parameter support for both functions and macros.
- Support for lisp closures.
- Robust error handling and reporting.
- Support for special forms.
- Includes runtime libraries written in lisp.
- Easily integrates lisp into python code.
- Call into python code from lisp.

Design Features (the code not lisp)
- Very simple tree-walk interpreter.
- Fully object oriented.
- Designed to be easily modified and extended.
- Prioritize code readability over code performance.
- Prioritize implementing something in the runtime library over in python.
- Full suite of tests.
- Strings support the full range of python escape sequences.
- Fully implemented in python 3.14.
- Implemented from scratch (Didn't start from anyone else's code.  AI was not
  used or consulted with).
- No external package dependencies.
- Complete lexical analyzer and LL(1) recursive descent parser.


### USAGE ###
=============

Run the repl.

          > python3 -m pythonslisp

Execute a lisp source file.

          > python3 -m pythonslisp <lispSourceFile.lisp>


API: Using Lisp as a Package
============================

The easiest way to use the package is to import then instantiate a
LispInterpreter, passing it info on where to find a runtime library, then use
its eval() functions to call into lisp.  Everything is persistent across calls
to these eval functions so you can mix and match which one you use at any given
time.

The example below simply defines a fibonacci function in lisp, then calls the
function from python and prints the results.

          from pythonslisp.LispInterpreter import LispInterpreter
          from pythonslisp.LispAST import prettyPrintSExpr

          interp = LispInterpreter( runtimeLibraryDir='pythonslisp/lib' )
          interp.rawEval( '''(defun fibo (num)
                                 (if (< num 2)
                                     1
                                     (+ (fibo (- num 1))
                                        (fibo (- num 2)))))''' )

          def fibo_wrapper( nth ):
             return interp.rawEval(f'(fibo {nth})')

          print( fibo_wrapper(10) )
          print( prettyPrintSExpr(fibo_wrapper(15)) )

If the lisp code returns an int,float,fraction,string,list or dict then it just
returns the python versions of those things.  However, most calls to rawEval will
return the AST of an sexpression.  A subsequent call to prettyPrintSExpr(ast)
will convert the complex structure to a python string representation.

LispInterpreter's eval functions can handle any number of lisp expressions
in the string argument.  The return value is always the value of the result
of evaluating the last expression in the string.

Calling python from lisp is even easier.  Just call the python lisp primitive
and pass it some python code in a lisp string.

          # Simply call python
          interp.rawEval( '(python "3 + 4")' )

          # Passing values from lisp
          interp.rawEval( '(setf num 3)' )
          interp.rawEval( '(python (string num " + 4"))' )

Primarily your program will interact with pythonslisp.LispAST,
pythonslisp.LispInterpreter, pythonslisp.LispParser and pythonslisp.Parser.
Methods prefixed by _ are considered private.  Private methods are
implementation details for the given class and probably not useful to you.
Moreover, given that they are not intended for public use, they are likely to
change without notice in future revisions.  The remaining functions and methods,
those without _, are public and are useful to the python programmer.  Note that
the public interface for the class pythonslisp.Parser.LispLexer is actually
found in the base class pythonslisp.Parser.Lexer.  LispLexer only implements
private methods needed by the base class.

Note that eval() functions in the interpreter that include the label
'instrumented' are designed for testing the performance of the interpreter.
These special versions of eval() return a tuple of three values: return value,
parse time in seconds, evaluation time in seconds.  They are probably not that
useful to the python developer.  They are used by the listener's repl to
report performance characteristics during interactive sessions.

Modification of the Package
===========================

Customizations are possible.  One way to get started easily is to write some
new primitives into LispInterpreter.  Following are some notes I've
accumulated regarding modifying the package.

Regarding Exceptions in the Interpreter and AST
- During s-expression evaluation, you want to catch all the low level python
  exceptions and re-raise higher level lisp exceptions with information useful
  to the lisp programmer.  However, you generally don't want to wrap a call to
  LispInterpreter._lEval() (or a call to something that calls _lEval()) in a try
  block.  Doing so might prevent the higher level exceptions from percolating up
  and being caught by the listener so it can report them to the lisp programmer
  as verious kinds of lisp errors.  Ideally no low level python exceptions
  would ever reach the try block in the Listener class's repl.

### THE LANGUAGE ###
====================

Lexemes
   Comments
      Comments extend from ';' through '\n'.

   Literals
      NumberLiteral:  ['+'|'-'] ('0' .. '9')+
                         ( '/' ('0' .. '9')+
                         | 'e' ['+'|'-'] ('0' .. '9')+
                         | '.' ('0' .. '9')+ [ 'e' ['+'|'-'] ('0' .. '9')+ ]
                         )   # Supports integers, floats and fractions
      StringLiteral:  '"' (^('"'))* '"'   # Supports python escape sequences
      Symbol:         'a..zA..Z+-~!$%^&*_=\\/?<>#|'
                      { 'a..zA..Z+-~!$%^&*_=\\/?<>#|0..9' }

   Predefined Symbols
         'nil', 't', 'e', 'pi'

Grammar
   Start:
      Object* EOF

   Object:
      NumberLiteral | StringLiteral | Symbol | List | '#' | '|' | ':' | '[' | ']'
      | "'" Object | "`" Object | "," Object | ",@" Object

   List:
      '(' Object* ')'

### LISP PRIMITIVES ###
=======================

    Primitives marked with ! are specific to Python's Lisp (not common lisp).

### Values ###
NIL                                     ;; an empty list. Stands for false.
T                                       ;; a symbol to stand for true.
PI                                      ;; the constant pi.
E                                       ;; the constant e.

### Variable Definition ###
(defmacro name (<param1> <param2> ...)  ;; Define and return a macro.
        <sexpr1> <sexpr2> ...)
(expandmacro '(<macroName> <arg1> <arg2> ...))
                                        ;; Perform the expansion of the macro
                                        ;;    and return the results of those
                                        ;;    expansions in a list.
(setf <symbol> <expr> )                 ;; Update a variable's value.  The
                                        ;;    search for the variable begins
                                        ;;    locally and proceeds to search
                                        ;;    ever less local scopes until
                                        ;;    the global scope is searched.
                                        ;;    If the variable is located in this
                                        ;;    search its value is updated.  If
                                        ;;    it's not located a global is
                                        ;;    defined and set to the value.
! (undef! '<symbol>)                    ;; Undefine the global definition
                                        ;;    for a symbol.
! (symtab!)                             ;; Print a symbol tab.  Each scope on a
                                        ;;    separate line.  Local scope first.
                                        ;;    Global scope last.

### Control Structures ###
(lambda (<arg1> <arg2> ...) <expr1> <expr2> ...)
                                        ;; Return a lambda function.
                                        ;;    When evaluating such a function
                                        ;;    the body (the exprs) are evaluated
                                        ;;    within a nested scope.
(let ( (<var1> <expr1>) (<var2> <expr2>) ... ) <expr1> <expr2> ... )
                                        ;; Execute code in a nested scope.
                                        ;;    var1,var2,... are local variables
                                        ;;    bound to initial values.  Initial
                                        ;;    values are not evaluated in order
                                        ;;    and are not evaluated in the
                                        ;;    new local scope.
(let* ( (<var1> <expr1>) (<var2> <expr2>) ... ) <expr1> <expr2> ... )
                                        ;; Execute code in a nested scope.
                                        ;;    var1,var2,... are local variables
                                        ;;    bound to initial values.  Initial
                                        ;;    values ARE evaluated in order
                                        ;;    and inside the new local scope.
(progn <sexpr1> <sexpr2> ...)           ;; Evaluate each sexpression in turn.
                                        ;;    Returns the result of the last
                                        ;;    sexpr.
(if <cond> <conseq> &optional <alt>)    ;; If cond evaluates to true evaluates
                                        ;;    conseq otherwise evaluates alt.
(cond (<cond1> <body1>) (<cond2> <body2>) ...)
                                        ;; Evaluates each cond in order until
                                        ;;    one evaluates to true.  Then
                                        ;;    evaluates each expr in body and
                                        ;;    returns the result of the last
                                        ;;    expr evaluated.  All remaining
                                        ;;    conds and bodys are skipped.
(case <expr> (<val1> <body1>) (<val2> <body2>) ...)
                                        ;; Evaluates expr.  Finds the first
                                        ;;    val that equals expr's val.  Then
                                        ;;    evaluates each expr in body and
                                        ;;    returns the result of the last
                                        ;;    expr evaluated.  All remaining
                                        ;;    cases are skipped.
! (while <conditionExpr> <sexpr1> <sexpr2)
                                        ;; Perform a loop over the body sexprs.
                                        ;;    Before iteration conditionExpr is
                                        ;;    evaluated.  If conditionExpr is
                                        ;;    true the iteration occurs.
                                        ;;    However if conditionExpr evaluates
                                        ;;    to false, while returns the result
                                        ;;    recent body evaluation.
! (doTimes (<variable> <countExpr>) <sexpr1> <sexpr2> ...)
                                        ;; Performs a loop countExpr times.
                                        ;;    For each iternation of the loop
                                        ;;    variable is set to the loop number
                                        ;;    starting with 0 for the first loop
                                        ;;    The value of the last sexpr
                                        ;;    evaluated is returned.
! (forEach <variable> <list> <sexpr1> <sexpr2> ...)
                                        ;; Each iteration assigns a list element
                                        ;;    to symbol, and evaluates expr.
(quote <expr>)                          ;; Returns <expr> without evaluating it.
(backquote <expr>)                      ;; Similar to quote, but allows comma
                                        ;;    and comma-at expressions within
                                        ;;    expr.
(comma <expr>)                          ;; Must occur within a backquote expr
                                        ;;    or it's an error.  Evaluates expr
                                        ;;    (even if within a quoted expr) and
                                        ;;    returns it to the enclosing expr.
(comma-at <expr>)                       ;; Must occur within a backquote expr
                                        ;;    or it's an error.  Evaluates expr.
                                        ;;    result must be a list.  Inserts
                                        ;;    the elements of the resulting list
                                        ;;    into the enclosing list.

(apply <fnOrName> <arg1> <arg2> ... <listOfMoreArgs>)
                                        ;; insert arg1,arg2,... in front of
                                        ;; listOfMoreArgs, then apply the
                                        ;; function to the whole list of args.
! (funcall <fnNameSym> <fnArg1> <fnArg2> ...)
                                        ;; Call a function with the args provided
(apply <fn> <arg1> <arg2> ... <list>)   ;; Apply fn to the list of args.
(eval <expr>)                           ;; Evaluate <expr> in the current scope.
! (parse <string>)                      ;; Parse the string as an sexpression
                                        ;;    and returns the sexpression.
! (python <pythonCodeString>)           ;; Execute some python code from lisp.

### List & Map Manipulation ###
(car <list>)                            ;; Return the first item in the list.
(cdr <list>)                            ;; Return the list minus the first item.
(cons <obj> <list>)                     ;; Return a new list with <obj> inserted
                                        ;;    into the front of list.
! (push! '<list> '<value>)              ;; Push a value onto the back of a list.
! (pop! '<list>)                        ;; Pop and returns the last value of the
                                        ;;    list.
! (at <keyOrIndex> <mapListOrStr>)      ;; Return the value at a specified index
                                        ;;    or key of a list, map or string.
Use: (setf (at <key> <map>) <newValueExpr>)
Use: (setf (at <idx> <lst>) <newValueExpr>)
Use: (setf (at <idx> <str>) <newValueExpr>)
! (at-delete <keyOrIndex> <mapOrList>)  ;; Delete the item from the map or list
                                        ;;    specified by the keyOrIndex.
! (at-insert <index> <list> <newItem>)  ;; Insert newItem into list at position
                                        ;;    index.
(append <list-1> <list-2> ...)          ;; Return a new list with the lists
                                        ;;    merged.  Order is retained.
! (hasValue? '<listOrMap> '<value>)     ;; Returns T if the list/map contains
                                        ;;    value.
! (map '( (<key1> <val1>) (<key2> <val2>) ...))
                                        ;; Construct a map of key-value pairs.
! (update! <map1> <map2>)               ;; Update map1's data with map2's.
! (hasKey? <map> <key>)                 ;; Return T if key is in the map.
! (sorted <list>)                       ;; Return a sorted version of list.

### Arithmetic Operations ###
(+ <expr1> <expr2> ...)                 ;; Returns the sum of numbers or
                                        ;;    concatenates strings.
(- <expr1> <expr2> ...)                 ;; Returns the difference.
(* <expr1> <expr2> ...)                 ;; Returns the product.
(/ <expr1> <expr2> ...)                 ;; Returns the quotient.
! (// <expr1> <expr2>)                  ;; Returns the integer division.
(mod <expr1> <expr2>)                   ;; Returns the integer remainder.
(gcd <int1> <int2> ...)                 ;; Returns the gcd of the arguments.
(lcm <int1> <int2> ...)                 ;; Returns the lcm of the arguments.
(log <x> [<base>])                      ;; Returns the log of x.  If base if not
                                        ;;    provided, e is used.
(expt <base> <power>)                   ;; Returns base raised to power.
(sin <radians>)                         ;; Returns the sin of radians.
(cos <radians>)                         ;; Returns the cos of radians.
(asin <number>)                         ;; Returns the asin (in radians).
(acos <number>)                         ;; Returns the acos (in radians).
(atan <number>)                         ;; Returns the atan (in radians).
(min <val1> <val2> ...)                 ;; Returns the lowest of the values.
(max <val1> <val2> ...)                 ;; Returns the highest of the values.
(random <num>)                          ;; Return a random int or float between
                                        ;;    0 and num.

### Predicates ###
(numberp  <expr>)                       ;; Returns t if expr is a number.
(integerp <expr>)                       ;; Returns t if expr is an integer.
(rationalp <expr>)                      ;; Returns t if expr is an integer
                                        ;;    or a fraction.
(symbolp  <expr>)                       ;; Returns t if expr is a symbol.
(atom <expr>)                           ;; Returns t if expr in { int, float,
                                        ;;   fraction, string, nil, symbol }.
(listp <expr>)                          ;; Returns t if expr is a list.
! (isMap?  <expr>)                      ;; Returns t if expr is a map.
(stringp  <expr>)                       ;; Returns t if expr is a string.
(functionp <expr>)                      ;; Returns t if expr is a function.

### Relational Operators ###
! (is? <val1> <val2>)                   ;; Are the two values the same object?
(=   <val1> <val2> ...)                 ;; Are all the values equal?
(/=  <val1> <val2> ...)                 ;; Are all the values unequal?
(<   <val1> <val2> ...)                 ;; Are all the values less than?
(<=  <val1> <val2> ...)                 ;; Are all the values less than or equal
(>   <val1> <val2> ...)                 ;; Are all the values more than?
(>=  <val1> <val2> ...)                 ;; Are all the values more than or equal

### Logical Operators ###
(not <val>)                             ;; Returns the negation of val.
(and <val1> <val2> ...)                 ;; Returns the logical and of vals.
(or  <val1> <val2> ...)                 ;; Returns the logical or of vals.

### Type Conversion ###
(float <val>)                           ;; Returns val as a float.  val can be
                                        ;;    any number type or a string
                                        ;;    containing a lisp float.
(rational <val>)                        ;; Returns val as a fraction.
! (integer <val> [<base>])              ;; Returns val as an int.  val can be
                                        ;;    any number type or a string
                                        ;;    containing a lisp integer.
(string <expr1> <expr2> ...)            ;; Returns the concatenation of the
                                        ;;    repr string results of the
                                        ;;    arguments.
! (ustring <expr1> <expr2> ...)         ;; Returns the concatenation of the
                                        ;;    str string results of the
                                        ;;    arguments.
(symbol <expr1> <expr2> ...)            ;; Concatenate the strings and use the
                                        ;;    result to define a symbol.

### I/O ###
! (writef <formatStr> <mapOrList>)      ;; Write formatted text.  Takes a
                                        ;;    Python format string and a map
                                        ;;    of values to fill the string.
! (write! <object1> <object2> ...)      ;; Write text rep of objects to the
                                        ;;    screeen/stream.
! (writeLn! <object1> <object2> ...)    ;; Write a line of text to the
                                        ;;    screen/stream.
! (uwrite! <object1> <object2> ...)     ;; Write objects in a format suitable
                                        ;;    for an end user.
! (uwriteLn! <object1> <object2> ...)
! (read!)                               ;; Read input from keyboard/stream.

Library Functions & Macros
==========================

caar, cadr, cdar, cddr, caaar, caadr, cadar, caddr, cdaar, cdadr, cddar, cdddr,
caaaar, caaadr, caadar, caaddr, cadaar, cadadr, caddar, cadddr, cdaaar,
cdaadr, cdadar, cdaddr, cddaar, cddadr, cdddar, cddddr, first, second, third,
fourth, fifth, sixth, seventh, eighth, ninth, tenth, rest
(nth <index> <list>)                    ;; Returns the nth item of the list
(null <sexpr>)                          ;; Is the result of the sexpr nil?
(list &rest <elts>)                     ;; Returns a new list of evaluated
                                        ;;    expressions.
! (remove '<symbol> <list>)             ;; Remove a symbol from a list.
(list-length '<list>)                   ;; Return the length of a list.
! (reverse '<list>)                     ;; Return a new list with list reversed.
! (copy <list>)                         ;; Return a copy of a list.
! (deepCopy <list>)                     ;; Return a deepcopy of a list.
! (read_prompt promptStr)               ;; Write a prompt and read a value.
(tree-equal <tree1> <tree2>)            ;; Are two deeply nested lists equal?
(evenp intVal)                          ;; Is the intval even?
(oddp intVal)                           ;; Is the intval odd?
(plusp numVal)                          ;; Is the number positive?
(minusp numVal)                         ;; Is the number negative?
(zerop numVal)                          ;; Is the number zero?
(signum num)                            ;; Compute the sign of the number.
(sqrt num)                              ;; Compute the square root.
(isqrt num)                             ;; Compute the integer square root.
(exp num)                               ;; Compute e raised to num.
(abs num)                               ;; Compute the absolute valute.
(tan <radians>)                         ;; Returns the tan of radians.
! (fact n)                              ;; compute the factorial of n.
! (fib n)                               ;; return the nth fibonacci number.
(gensym &optional (<prefix-str> 'G'))   ;; Generate a unique symbol.  The
                                        ;;    default is to set <prefix-str> to
                                        ;;    G.  The unique symbol is the
                                        ;;    <prefix-str> followed by digits.

Library Macros
==============

(defun name (<param1> <param2> ...)     ;; Define and return a global function.
        <expr1> <expr2> ...)
(incf var &optional (<delta> 1))        ;; increment a variable by 1 or delta.
(decf var &optional (<delta> 1))        ;; decrement a variable by 1 or delta.

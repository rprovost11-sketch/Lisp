Python's Lisp
=============

Common Lisp programming language

The best python lisp implementation!

Implemented entirely in python 3.9 and above.

Embed lisp in python code and python in lisp code; or interact with lisp in the included repl.


Lisp Documentation
==================

Goals:  Implement a substantial subset of the features of common lisp.

Features
- Full macro system: defmacro, macroexpand, and a runtime library written in Lisp
- Complete lambda lists: &optional, &rest, &key, and &aux for functions and macros
- Closures, tail call optimization, and call/cc
- Condition system: handler-case, signal, make-condition
- Multiple return values: values, multiple-value-bind
- Embed Lisp in Python or call Python from Lisp
- Full interactive REPL with persistent history and session logging

Design Features
- Implemented from scratch with no external dependencies
- Extensible: add new primitives or Lisp definitions without modifying the interpreter
- Complete lexical analyzer and LL(1) recursive descent parser
- Full test suite


A Taste of the Language
=======================

Closures and higher-order functions:

          ; make-adder returns a closure over n
          (defun make-adder (n)
            (lambda (x) (+ x n)))

          (setf add10 (make-adder 10))
          (mapcar add10 '(1 2 3 4 5))   ;==> (11 12 13 14 15)

          ; filter and transform in one pipeline
          (mapcar (lambda (x) (* x x))
                  (remove-if 'oddp '(1 2 3 4 5 6)))  ;==> (4 16 36)

Macros that write code:

          ; swap! expands to a let at compile time -- no helper function needed
          (defmacro swap! (a b)
            `(let ((tmp ,a))
               (setf ,a ,b)
               (setf ,b tmp)))

          (setf x 1  y 100)
          (swap! x y)
          x   ;==> 100
          y   ;==> 1

Structs with generated constructors, predicates, and accessors:

          (defstruct point x y)

          (setf p (make-point :x 3 :y 4))
          (point-p p)              ;==> T
          (point-x p)              ;==> 3
          (setf (point-x p) 10)
          (list (point-x p) (point-y p))   ;==> (10 4)


### CLI USAGE ###

Run the repl.

          > python3 -m pythonslisp

Execute a lisp source file.

          > python3 -m pythonslisp <lispSourceFile.lisp>

For various information requests there are.

          > python3 -m pythonslisp (-h|--help|-v|--version)

Note that the repl provides access to two separate online documentation systems.
There is the listener command help system accessible by typing ']help' in the
repl.  Then there is the Lisp online help system (LOHS) accessible by evaluating
the sexpression '(help)'.

These two help systems document access to different things.  The listener
command help system provides documentation for commands recognized by the
listener from inside the repl.  The LOHS provides access to documentation for
all lisp callables (primitives, functions and macros) as well as to various help
topics of interest to the lisp programmer.

The LOHS is dynamic.  As the user defines new functions and macros their
documentation becomes available in the help system.  Specifically the
documentation system will display a "function header" which includes the
function name and its lambda list (formal parameter list).  This will be
followed by any text in the documentation string coded by the lisp programmer.

If you want a lisp startup script to run whenever the interpreter is
initialized/reinitialized you can add .pythonslisp_rc to your home directory.

API: Using Lisp as a Package
============================

The easiest way to use the package is to import then instantiate an
Interpreter.  Call reboot to initialize the interpreter and load the
runtime.  If at any time you want completely reset the interpreter you can call
reboot() again.  Next use the interpreter's eval() functions to call into lisp.
Everything is persistent across calls to these eval functions so you can mix
and match which one you use at any given time.

The example below simply defines a fibonacci function in lisp, then calls the
function from python and prints the results.

          from pythonslisp.Interpreter import Interpreter
          from pythonslisp.AST import prettyPrintSExpr

          interp = Interpreter( )
          interp.reboot( )
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
returns the python versions of those things.  Calls to rawEval will
return the AST of an sexpression.  A subsequent call to prettyPrintSExpr(ast)
will convert the complex structure to a python string representation.

The interpreter's eval functions can handle any number of lisp expressions
in the string argument.  The return value is always the value of the result
of evaluating the last expression in the string.

Calling python from lisp is just as easy.  Just call the python lisp primitive
and pass it some python code in a lisp string.

          # Simply call python
          interp.rawEval( '(python "3 + 4")' )

          # Passing values from lisp
          interp.rawEval( '(setf num 3)' )
          interp.rawEval( '(python (string num " + 4"))' )

Primarily your program will interact with pythonslisp.AST,
pythonslisp.Interpreter, and pythonslisp.Parser.  Methods prefixed by _ are
private implementation details subject to change; all other methods and
attributes form the public interface.

eval() functions in the interpreter that include the label 'instrumented' are
designed for testing the performance of the interpreter.  These special
versions of eval() return a tuple of three values: return value, parse time in
seconds, evaluation time in seconds.  They are used by the listener's repl to
report performance characteristics during interactive sessions.

To extend or modify the interpreter, evaluate '(help "MODIFYING")' in the REPL.

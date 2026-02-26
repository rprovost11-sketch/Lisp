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
- Listener now has expression history with up/down arrow which is persistent
  across sessions.
- Function definition with stack and recursion support.
- Common Lisp macro definition and the macroexpand primitive.
- &optional, &rest, &key and &aux parameter support for functions and macros.
- Support for function closures.
- Tail Call Optimization
- Robust error handling and reporting.
- Support for special operators
- Includes a portion of the runtime library written in lisp.
- Easily integrates lisp into python code.
- Call into python code from lisp.

Design Features (the code not lisp)
- Very simple looping tree-walk interpreter.
- Fully object oriented.
- Designed to be easily modified and extended.
- Prioritize code readability and organization over code performance.
- Prioritize implementing something in the lisp runtime library over in python.
- Full suite of tests.
- Strings support the full range of python escape sequences.
- Fully implemented in python 3.14.
- Implemented from scratch (Didn't start from anyone else's code).
- No external package dependencies.
- Complete lexical analyzer and LL(1) recursive descent parser.


### CLI USAGE ###
=================

Run the repl.

          > python3 -m pythonslisp

Execute a lisp source file.

          > python3 -m pythonslisp <lispSourceFile.lisp>

For various information requests there are.

          > python3 -m pythonslisp (-h|--help|-v|--version)

Note that the repl provides access to two online documentation systems.  There 
is the listener command help system accessible by typing ']help' in the repl.
Then there is the Lisp online help system (LOHS) accessible by evaluating the
sexpression '(help)'.

These two help systems provide access to different things.  The listener
command help system provides documentation for commands recognized by the
listener.  The LOHS provides access to documentation for all lisp callables
(primitives, functions and macros) as well as to various help topics of
interest to the lisp programmer.

The LOHS is dynamic.  As the user defines new functions and macros their
documentation becomes available in the help system.  Specifically the
documentation system will display a "function header" which includes the
function name and its lambda list (formal parameter list).  This will be
followed by any text in the documentation string coded by the lisp programmer.


API: Using Lisp as a Package
============================

The easiest way to use the package is to import then instantiate a
LispInterpreter.  Next, if you want access to the functions and macros defined
in the lisp runtime library, you want to call reboot() which creates and loads
up the global environemt with primitives, functions and macros.  Next use its
eval() functions to call into lisp.  Everything is persistent across calls to
these eval functions so you can mix and match which one you use at any given
time.

The example below simply defines a fibonacci function in lisp, then calls the
function from python and prints the results.

          from pythonslisp.LispInterpreter import LispInterpreter
          from pythonslisp.LispAST import prettyPrintSExpr

          interp = LispInterpreter( )
          interp.reboot()
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

LispInterpreter's eval functions can handle any number of lisp expressions
in the string argument.  The return value is always the value of the result
of evaluating the last expression in the string.

Calling python from lisp is just as easy.  Just call the python lisp primitive
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
change without notice in future revisions.

The remaining functions and methods, those without _, are public and are useful
to the python programmer.  Note that the public interface for the class
pythonslisp.Parser.LispLexer is actually found in the base class
pythonslisp.Parser.Lexer.  LispLexer only implements private methods needed by
the base class.

eval() functions in the interpreter that include the label 'instrumented' are
designed for testing the performance of the interpreter.  These special
versions of eval() return a tuple of three values: return value, parse time in
seconds, evaluation time in seconds.  They are probably not that useful to the
python developer.  They are used by the listener's repl to report performance
characteristics during interactive sessions.

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
  and being caught by the listener for reporting to the lisp programmer as
  various kinds of lisp errors.  Ideally no low level python exceptions
  would ever reach the try block in the Listener class's repl.

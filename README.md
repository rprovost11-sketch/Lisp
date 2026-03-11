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
- A full featured listener with session logging and resumption, interpreter
  reboot, and interpreter testing.
- Listener now has expression history with up/down arrow which is persistent
  across sessions.
- Function definition with stack and recursion support.
- Common Lisp macro definition and the macroexpand primitive.
- &optional, &rest, &key and &aux parameter support for functions and macros.
- Support for function closures.
- Tail Call Optimization
- call/cc
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
- Fully implemented in python 3.9 and above.
- Implemented from scratch (Didn't start from anyone else's code).
- No external package dependencies.
- Complete lexical analyzer and LL(1) recursive descent parser.


### CLI USAGE ###

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
pythonslisp.Interpreter, and pythonslisp.Parser.

Methods prefixed by _ are considered private.  Private methods are
implementation details for the given class and probably not useful to you.
Moreover, given that they are not intended for public use, they are likely to
change without notice in future revisions.

The remaining functions and methods, those without _, are public and are useful
to the python programmer.  Note that the public interface for the class
pythonslisp.Parser.Lexer is actually found in the base class
pythonslisp.ltk.ParserBase.LexerBase.  Lexer only implements private methods
needed by the base class.

eval() functions in the interpreter that include the label 'instrumented' are
designed for testing the performance of the interpreter.  These special
versions of eval() return a tuple of three values: return value, parse time in
seconds, evaluation time in seconds.  They are probably not that useful to the
python developer.  They are used by the listener's repl to report performance
characteristics during interactive sessions.

Modification of the Package
===========================

Customizations are possible.  One way to get started easily is to write some
new primitives into the interpreter.  Following are some notes I've
accumulated regarding modifying the package.

Regarding Exceptions in the Interpreter and AST
- During s-expression evaluation, you want to catch all the low level python
  exceptions and re-raise higher level lisp exceptions with information useful
  to the lisp programmer.  However, you generally don't want to wrap a call to
  Interpreter._lEval() (or a call to something that calls _lEval()) in a try
  block.  Doing so might prevent the higher level exceptions from percolating up
  and being caught by the listener for reporting to the lisp programmer as
  various kinds of lisp errors.  Ideally no low level python exceptions
  would ever reach the try block in the Listener class's repl.

- Create new primitives (functions that can't be implemented in lisp) using
  the same pattern found in the files in the extensions subdirectory.  Start
  with the following imports
  
from typing import Any

from pythonslisp.ltk.EnvironmentBase import EnvironmentBase
from pythonslisp.AST import LSymbol, LCallable, LFunction, L_NIL, eql
from pythonslisp.AST import prettyPrintSExpr, prettyPrint
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimeError, LRuntimePrimError, Thrown, ReturnFrom
from pythonslisp.extensions import LambdaListMode

  Add any additional imports for modules you'll need.

  Next implement the register function.

def register(primitive) -> None:

  Its body is a series of primitive function definitions (written in python).
  Each function is headed by the primitive decorator.

   @primitive( 'lambda', '(lambda-list &rest body)', preEvalArgs=False )
   def LP_lambda( ctx: Context, env: EnvironmentBase, args: list[Any] ) -> Any:
      """Creates and returns an unnamed lambda function.  When evaluating such a
function the body exprs are evaluated within a nested scope.  This primitive
captures the environment it is defined in to allow for closures.  The first body
expression can be a documentation string."""
      funcParams, *funcBody = args
      if funcBody and isinstance(funcBody[0], str):
         docString, *funcBody = funcBody
      else:
         docString = ''
      return LFunction( LSymbol(""), funcParams, docString, funcBody, capturedEnvironment=env )

  The primitive decorator is complex.  It's first parameter is the name of the
  primitive inside the lisp interpreter.  The third parameter is the
  preEvalArgs boolean.  If this is set to true (the default) then all
  arguments are evaluated prior to calling your function.  If it's false
  all arguments will be passed to you unevaluated.  This is primarily to allow
  some special operators to be defined this way.
  
  The second parameter is a lambda-list which the decorator may use in one of
  three ways, determined by the value passed for the lambdaListMode parameter.
  
  A value of LambdaListMode.DOC_ONLY tells the interpreter that the supplied
  lambda list is to be used for documentation purposes only.  Key arguments
  args_min and args_max must also be supplied.  For this case your primitive
  body must parse the arguments manually before any processing can happen.
  Generally, use this if the lambda list has special syntax that can't be
  expressed clearly through a syntactically correct lambda list.
  
  A value of LambdaListMode.ARITY_ONLY tells the decorator to use the lambda
  list argument to auto-compute args_min and args_max but otherwise use the
  lambda list for documentation purposes only.  For this case your primitive
  body must parse the arguments manually before any processing can happen.  Use
  this for most cases.  It's the default value of the parameter.
  
  Finally a value of LambdaListMode.FULL_BINDING tells the decorator to
  auto-compute args_min and args_max; and moreover at evaluation time use the
  lambda list argument to call the interpreter's argument binding routines just
  prior to calling your primitive.  All parameters will then be bound to
  argument values and exist in the python function's env parameter.  In this
  case your python function will begin by looking up and caching locally each of
  the parameters in the lambda list prior to any processing.  E.g.
  
     filename = env.lookup('FILENAME')
     direction = env.lookup('DIRECTION')
  
  Use FULL_BINDING only for primitives with complex but otherwise syntactically
  correct lambda lists.  The built-in argument binding machinery is big and
  complex.  Parsing arguments manually in each primitive is usually faster.
  
  The python function's documentation string is important.  It becomes the
  primitive's documentation text in the help system.
  
  Don't forget that every python primitive must return a value.
  
  With access to the environment, to all the important lisp routines in the
  context object, and access to any python package you could want, you should
  be able to define primitives into lisp to implement any new functionality
  that you desire.
  
  Extensions can be loaded from inside lisp using two primitives.
  load-extension takes any number of string file paths and loads each one:
  .py files must export register(primitive); .lisp files are parsed and
  evaluated.  load-extension-dirs takes any number of string directory paths
  and loads each directory: all .py files alphabetically, then all .lisp files
  alphabetically.

  **Note:** A fully annotated example extension covering all three LambdaListMode
  cases is available at `pythonslisp/examples/my_extension.py`. It includes the
  complete set of imports you are likely to need and can be used as a starting
  point for writing your own primitives.

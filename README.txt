# Lisp-Py
Lisp programming language - Implemented purely in Python 3.14

Lisp is intended as an toy interpreter for a very simple Lisp dialect.

Lisp is implemented entirely in python 3.14.

Goals
==========
Lisp-like syntax.
Macro-free.
Constant evaluation semantic. (Every expression is evaluated in exactly
   the same semantic.  Arguments are each evaluated in turn, then the
   results of theose evaulations are pushed onto the stack in reverse order).
The current implementation uses a tree-walking interpreter.  I expect this to change.
All functions are lambdas.

To get an idea of how the language looks take a look at Lybrary.lisp in the source files.

Uses
==========
Educational Only.

HOW TO USE
==========
Just needs Python 3.14.

Download.

execute:  python Lisp.py


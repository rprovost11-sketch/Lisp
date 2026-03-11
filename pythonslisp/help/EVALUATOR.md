# The Evaluator

Python's Lisp is a **tree-walk interpreter**.  To evaluate an expression it
walks the abstract syntax tree (AST) and interprets each node directly --
no bytecode, no compilation step.

The AST uses standard Python types.  Numbers and strings are Python `int`,
`float`, `Fraction`, or `str`.  Symbols are `LSymbol` objects.  Lists are
Python `list`.  The environment is a plain dictionary mapping names to values.

## A Minimal Lisp Evaluator

The following self-contained Python program implements a working Lisp
evaluator.  It strips away all the machinery of the full interpreter and
shows just the essential structure.

```python
env = {}       # the global environment: maps names to values

def lEval( expr, env ):
   # A string is a variable name -- look it up in the environment.
   if isinstance(expr, str):
      return env[expr]
   # Any other non-list atom (int, float, ...) evaluates to itself.
   elif not isinstance(expr, list):
      return expr

   if len(expr) == 0:
      return []               # empty list evaluates to itself (NIL)

   head = expr[0]

   # Special forms: the head names a form that controls its own evaluation.
   # 'if' must NOT evaluate both branches -- only the one that is taken.
   if head == 'if':
      condValue = lEval( expr[1], env )
      return lEval( expr[ 2 if condValue else 3 ], env )

   # 'setq' must NOT evaluate the variable name -- only the value expression.
   elif head == 'setq':
      var, valExpr = expr[1:]
      val = lEval(valExpr, env)
      env[var] = val
      return val

   # Regular function call: evaluate everything -- the head and all arguments --
   # then call the resulting function with the resulting argument values.
   fn, *evaluatedArgs = [ lEval(subExpr, env) for subExpr in expr ]
   return fn( evaluatedArgs, env )
```

## Primitives

Primitive functions are plain Python functions stored in the environment
by name.  From the evaluator's perspective they are just values that happen
to be callable.

```python
def LP_add( argsList, env ):
   return argsList[0] + argsList[1]
env['+'] = LP_add

def LP_sub( argsList, env ):
   return argsList[0] - argsList[1]
env['-'] = LP_sub

def LP_isEqualTo( argsList, env ):
   return 1 if argsList[0] == argsList[1] else 0
env['='] = LP_isEqualTo
```

## Equivalent Lisp

The following expressions exercise all three evaluator paths.

```python
def evalLispExpr( expr ):
   print( f'==> {lEval( expr, env )}' )

def main():
   evalLispExpr( ['setq', 'a', ['+', 1, 1]] )           # ==> 2
   evalLispExpr( ['+', ['-', 10, 7], 'a'] )             # ==> 5
   evalLispExpr( ['if', ['=', 'a', 2], ['+', 'a', 1], ['-', 'a', 1]] )  # ==> 3

if __name__ == '__main__':
   main()
```

```lisp
; setq is a special form: the name 'a' is NOT evaluated, only the value.
(setq a (+ 1 1))                ; ==> 2

; Regular function call: +, (- 10 7), and a are all evaluated first.
(+ (- 10 7) a)                  ; ==> 5

; if is a special form: only ONE branch is evaluated, never both.
(if (= a 2) (+ a 1) (- a 1))    ; ==> 3
```

## What the Real Interpreter Adds

The `lEval` above is the complete conceptual core.  The real `_lEval` in
`Interpreter.py` extends it with:

- **More special forms** inlined for performance: `let`, `let*`, `progn`,
  `cond`, `case`, `funcall`, `apply`
- **Tail-call optimization (TCO)**: the evaluator loops instead of recursing
  for tail positions, so deeply recursive Lisp code does not overflow
  Python's call stack
- **Macro expansion**: before function dispatch, macro calls are expanded
  inline and re-evaluated in the same loop iteration
- **Continuations, tracing, and full argument binding** for the complete
  feature set

**Note:** The complete working code from this document is available as a standalone script in `pythonslisp/examples/evaluator.py`. Run it with `python pythonslisp/examples/evaluator.py`.

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

def lEval( env, expr ):
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
      condValue = lEval( env, expr[1] )
      return lEval( env, expr[ 2 if condValue else 3 ] )

   # 'setq' must NOT evaluate the variable name -- only the value expression.
   elif head == 'setq':
      var, valExpr = expr[1:]
      val = lEval(env, valExpr)
      env[var] = val
      return val

   # Regular function call: evaluate everything -- the head and all arguments --
   # then call the resulting function with the resulting argument values.
   fn, *evaluatedArgs = [ lEval(env, subExpr) for subExpr in expr ]
   return fn( evaluatedArgs, env )
```

## Primitives

Primitive functions are plain Python functions stored in the environment
by name.  From the evaluator's perspective they are just values that happen
to be callable.

```python
def LP_add( args, env ):
   return sum(args)
env['+'] = LP_add

def LP_sub( args, env ):
   if len(args) == 1:
      return -1 * args[0]
   return args[0] - sum(args[1:])
env['-'] = LP_sub

def LP_isEqualTo( args, env ):
   prior = None
   for mbr in args:
      if prior is not None and prior != mbr:
         return 0             # a falsy value
      prior = mbr
   return 1                   # a truthy value
env['='] = LP_isEqualTo
```

## Equivalent Lisp

The following expressions exercise all three evaluator paths.

```python
def evalLispExpr( expr ):
   print( f'==> {lEval( env, expr )}' )

def main():
   evalLispExpr( ['setq', 'a', ['+', 1, 1]] )           # ==> 2
   evalLispExpr( ['+', 2, ['-', 10, 7], 'a'] )           # ==> 7
   evalLispExpr( ['if', ['=', 'a', 2], ['+', 'a', 1], ['-', 'a', 1]] )  # ==> 3
```

```lisp
; setq is a special form: the name 'a' is NOT evaluated, only the value.
(setq a (+ 1 1))                ; ==> 2

; Regular function call: +, 2, (- 10 7), and a are all evaluated first.
(+ 2 (- 10 7) a)                ; ==> 7

; if is a special form: only ONE branch is evaluated, never both.
(if (= a 2) (+ a 1) (- a 1))   ; ==> 3
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

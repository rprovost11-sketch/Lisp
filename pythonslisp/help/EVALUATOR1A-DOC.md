# Evaluator 1a - The Minimal Recursive Evaluator

*Continues in `EVALUATOR1B-DOC`: closures and lexical scoping.*

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

   elif head == 'progn':
      for sub in expr[1:-1]:
         lEval( sub, env )             # non-tail forms: recurse
      return lEval( expr[-1], env )

   # 'setq' must NOT evaluate the variable name -- only the value expression.
   elif head == 'setq':
      var, valExpr = expr[1:]
      val = lEval(valExpr, env)
      env[var] = val
      return val

   # 'quote' returns its argument unevaluated.
   elif head == 'quote':
      return expr[1]

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
# the global environment: maps names to values
global_env = {
   '+':     lambda args: args[0] + args[1],
   '-':     lambda args: args[0] - args[1],
   '*':     lambda args: args[0] * args[1],
   '=':     lambda args: 1 if args[0] == args[1] else 0,
   '<':     lambda args: 1 if args[0] <  args[1] else 0,
}
```

## Equivalent Lisp

The following expressions exercise all three evaluator paths.

```python
def evalLispExpr( expr ):
   print( f'==> {lEval( expr, global_env )}' )

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

## Running the Example

The complete working code is in `pythonslisp/examples/IttyBittyLisp1a.py`.

```
python pythonslisp/examples/IttyBittyLisp1a.py
```

*Next: `EVALUATOR1B-DOC` adds lexical scopes, `let`, and closures.*

## Challenges

- **Add list primitives.** Add `car`, `cdr`, `cons`, `list`, and `null?` to
  `global_env` as Python lambdas.  With these plus `quote` you can write
  list-processing programs without touching `lEval` at all -- which tells
  you something about where the boundary between the evaluator and the
  library really is.

- **Add `not`, `and`, `or`.** `not` is a straightforward primitive, but
  `and` and `or` require short-circuit evaluation: `(and x y)` must not
  evaluate `y` if `x` is false.  That means they cannot be plain lambdas.
  They need to be special forms.  What does that boundary tell you about
  the difference between a primitive and a special form?

- **Add `cond`.** Implement `(cond (test1 result1) (test2 result2) ...)` as
  a special form -- it's just nested `if`, but written as a loop over
  clauses inside `lEval`.  Notice that you are writing the transformation
  *in Python* rather than in Lisp.  That distinction -- built-in special
  form versus user-defined macro -- will matter later.

- **Improve error messages.** An unbound variable currently raises a Python
  `KeyError`.  Catch it and produce a friendlier message.  Then look for
  the other places a bad program can crash the Python runtime and consider
  where the right boundary for error handling is.

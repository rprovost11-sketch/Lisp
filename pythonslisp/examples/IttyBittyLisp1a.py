"""
IttyBittyLisp1a - The simplest possible Lisp evaluator.

The AST is hand-written as nested Python lists -- no parser.  The environment
is a plain Python dict -- no scoping, no closures.  Every recursive call pushes
a new Python stack frame, so deeply recursive Lisp programs will overflow
Python's ~1000-frame limit.

This file is the starting point for a series that progressively adds features:

  IttyBittyLisp1a.py  (this file) -- bare evaluator, flat dict environment
  IttyBittyLisp1b.py              -- adds closures, let, lexical scoping
  IttyBittyLisp2.py               -- adds tail-call optimization (TCO) via looping
  IttyBittyLisp3.py               -- replaces recursion with an explicit CEK machine
  IttyBittyParser.py              -- adds a source-string parser to complete the pipeline

Run with: python pythonslisp/examples/IttyBittyLisp1a.py
"""


# ---------------------------------------------------------------------------
# The recursive evaluator
# ---------------------------------------------------------------------------

def lEval( expr, env ):
   # Python strings represent Lisp symbols -- there are no string values in
   # this Lisp.  Look up the symbol's value in the environment dict.
   if isinstance(expr, str):
      return env[expr]

   # Anything that isn't a list (int, float, ...) is self-evaluating.
   elif not isinstance(expr, list):
      return expr

   # Empty list is NIL.
   elif len(expr) == 0:
      return []

   head = expr[0]

   # (if cond then else) -- evaluate cond, then pick the appropriate branch.
   if head == 'if':
      condValue = lEval( expr[1], env )
      return lEval( expr[2] if condValue else expr[3], env )

   # (progn e1 e2 ... eN) -- evaluate each form in order; return the last.
   elif head == 'progn':
      for sub in expr[1:-1]:
         lEval( sub, env )             # non-tail forms: side effects only
      return lEval( expr[-1], env )    # tail form: its value is the result

   # (setq var valExpr) -- evaluate valExpr, bind it to var in env, return it.
   elif head == 'setq':
      var, valExpr = expr[1:]
      val = lEval(valExpr, env)
      env[var] = val
      return val

   # (quote datum) -- return datum unevaluated.
   elif head == 'quote':
      return expr[1]

   # Function call: evaluate every sub-expression (function + all arguments),
   # then call the resulting Python callable with the argument list.
   fn, *evaluatedArgs = [ lEval(subExpr, env) for subExpr in expr ]
   return fn( evaluatedArgs )


# ---------------------------------------------------------------------------
# Primitives and global environment
# ---------------------------------------------------------------------------

global_env = {
   '+':     lambda args: args[0] + args[1],
   '-':     lambda args: args[0] - args[1],
   '*':     lambda args: args[0] * args[1],
   '=':     lambda args: 1 if args[0] == args[1] else 0,
   '<':     lambda args: 1 if args[0] <  args[1] else 0,
}


# ---------------------------------------------------------------------------
# Helpers and demo
# ---------------------------------------------------------------------------

def evalLispExpr( expr ):
   print( f'>>> {expr}\n' )
   returnVal = lEval( expr, global_env )
   print( f'==> {returnVal}\n' )

def main() -> None:
   # Self-evaluating atom: a number evaluates to itself.
   evalLispExpr( 42 )                                              # 42

   # setq: bind a variable, return the value.
   evalLispExpr( ['setq', 'a', ['+', 1, 1]] )                     # (setq a (+ 1 1))

   # Symbol lookup: a bare variable evaluates to its current value.
   evalLispExpr( 'a' )                                             # a

   # Arithmetic primitives.
   evalLispExpr( ['+', ['-', 10, 7], 'a'] )                       # (+ (- 10 7) a)
   evalLispExpr( ['*', 3, 4] )                                     # (* 3 4)

   # Comparison: = and < return 1 (true) or 0 (false).
   evalLispExpr( ['=', 'a', 2] )                                   # (= a 2)
   evalLispExpr( ['<', 2, 5] )                                     # (< 2 5)  -> 1
   evalLispExpr( ['<', 5, 2] )                                     # (< 5 2)  -> 0

   # if: evaluate condition, then pick the matching branch.
   evalLispExpr( ['if', ['=', 'a', 2], ['+', 'a', 1], ['-', 'a', 1]] )
                                                                    # (if (= a 2) (+ a 1) (- a 1))

   # progn: evaluate a sequence of forms; return the value of the last one.
   evalLispExpr( ['progn', ['setq', 'b', 10], ['+', 'b', 5]] )    # (progn (setq b 10) (+ b 5))

   # quote: return a datum unevaluated -- suppresses evaluation entirely.
   evalLispExpr( ['quote', ['a', 'b', 'c']] )                      # (quote (a b c))

   # NIL: the empty list evaluates to itself.
   evalLispExpr( [] )                                              # ()

if __name__ == '__main__':
   main( )


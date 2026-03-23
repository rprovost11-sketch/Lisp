def lEval( expr, env ):
   if isinstance(expr, str):   # In this lisp there are no strings.  So, python strs hold lisp symbols.
      return env[expr]
   elif not isinstance(expr, list):
      return expr
   elif len(expr) == 0:
      return []

   head = expr[0]
   if head == 'if':
      condValue = lEval( expr[1], env )
      return lEval( expr[ 2 if condValue else 3 ], env )

   elif head == 'progn':
      for sub in expr[1:-1]:
         lEval( sub, env )             # non-tail forms: recurse
      return lEval( expr[-1], env )

   elif head == 'setq':
      var, valExpr = expr[1:]
      val = lEval(valExpr, env)
      env[var] = val
      return val

   elif head == 'quote':
      return expr[1]

   fn, *evaluatedArgs = [ lEval(subExpr, env) for subExpr in expr ]
   return fn( evaluatedArgs )

global_env = {
   '+':     lambda args: args[0] + args[1],
   '-':     lambda args: args[0] - args[1],
   '*':     lambda args: args[0] * args[1],
   '=':     lambda args: 1 if args[0] == args[1] else 0,
   '<':     lambda args: 1 if args[0] <  args[1] else 0,
}

def evalLispExpr( expr ):
   print( f'>>> {expr}\n' )
   returnVal = lEval( expr, global_env )
   print( f'==> {returnVal}\n' )

def main() -> None:
   evalLispExpr( ['setq', 'a', ['+', 1, 1]] )
   evalLispExpr( ['+', ['-', 10, 7], 'a'] )
   evalLispExpr( ['if', ['=', 'a', 2], ['+', 'a', 1], ['-', 'a', 1]] )

if __name__ == '__main__':
   main( )


env = {}   # The global environment.  Maps names to values.

def lEval( expr, env ):
   if isinstance(expr, str):   # In this lisp there are no strings.  So, python strs hold lisp symbols.
      return env[expr]
   elif not isinstance(expr, list):
      return expr

   if len(expr) == 0:
      return []
   
   head = expr[0]
   if head == 'if':
      condValue = lEval( expr[1], env )
      return lEval( expr[ 2 if condValue else 3 ], env )
   elif head == 'setq':
      var, valExpr = expr[1:]
      val = lEval(valExpr, env)
      env[var] = val
      return val

   fn, *evaluatedArgs = [ lEval(subExpr, env) for subExpr in expr ]
   return fn( evaluatedArgs, env )

def LP_add( argsList, env ):
   return argsList[0] + argsList[1]
env['+'] = LP_add            # store LP_add in the environment under +

def LP_sub( argsList, env ):
   return argsList[0] - argsList[1]
env['-'] = LP_sub            # store LP_sub in the environment under -

def LP_isEqualTo( argsList, env ):
   return 1 if argsList[0] == argsList[1] else 0
env['='] = LP_isEqualTo      # store LP_isEqualTo in the environment under =

def evalLispExpr( expr ):
   print( f'>>> {expr}\n' )
   returnVal = lEval( expr, env )
   print( f'==> {returnVal}\n' )

def main() -> None:
   evalLispExpr( ['setq', 'a', ['+', 1, 1]] )
   evalLispExpr( ['+', ['-', 10, 7], 'a'] )
   evalLispExpr( ['if', ['=', 'a', 2], ['+', 'a', 1], ['-', 'a', 1]] )

if __name__ == '__main__':
   main( )


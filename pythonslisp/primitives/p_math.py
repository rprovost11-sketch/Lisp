import functools
import math
import random as _random
from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.LispAST import LNUMBER
from pythonslisp.LispExceptions import LispRuntimeFuncError


def register(primitive, primitiveDict: dict) -> None:
   # Math constants live here alongside the math primitives
   primitiveDict['PI'] = math.pi
   primitiveDict['E']  = math.e

   @primitive( '+', '<number1> <number2> ...' )
   def LP_add( env: Environment, *args ) -> Any:
      """Returns the sum of numbers."""
      try:
         return sum(args)
      except TypeError:
         raise LispRuntimeFuncError( LP_add, 'Invalid argument.' )

   @primitive( '-', '<number1> <number2> ...' )
   def LP_sub( env: Environment, *args ) -> Any:
      """Returns the difference of numbers."""
      try:
         if len(args) == 1:
            arg = args[0]
            if not isinstance( arg, LNUMBER ):
               raise TypeError( )
            return -1 * arg
         else:
            return functools.reduce( lambda x,y: x - y, args )
      except TypeError:
         raise LispRuntimeFuncError( LP_sub, 'Invalid argument.' )

   @primitive( '*', '<number1> <number2> ...' )
   def LP_mul( env: Environment, *args ) -> Any:
      """Returns the product of numbers."""
      try:
         return functools.reduce( lambda x,y: x * y, iter(args) )
      except TypeError:
         raise LispRuntimeFuncError( LP_mul, 'Invalid argument.' )

   @primitive( '/', '<number1> <number2> ...' )
   def LP_div( env: Environment, *args ) -> Any:
      """Returns the quotient of numbers."""
      try:
         return functools.reduce( lambda x,y: x / y, iter(args) )
      except TypeError:
         raise LispRuntimeFuncError( LP_div, 'Invalid argument.' )
      except ZeroDivisionError:
         raise LispRuntimeFuncError( LP_div, 'division by zero' )

   @primitive( '//', '<number1> <number2>' )
   def LP_intdiv( env: Environment, *args ) -> Any:
      """Return the integer division of two numbers."""
      if len(args) != 2:
         raise LispRuntimeFuncError( LP_intdiv, '2 arguments expected.' )
      try:
         return args[0] // args[1]
      except TypeError:
         raise LispRuntimeFuncError( LP_intdiv, 'Invalid argument.' )
      except ZeroDivisionError:
         raise LispRuntimeFuncError( LP_intdiv, 'division by zero' )

   @primitive( 'mod', '<number1> <number2>' )
   def LP_moddiv( env: Environment, *args ) -> Any:
      """Returns the integer remainder of division of two numbers."""
      if len(args) != 2:
         raise LispRuntimeFuncError( LP_moddiv, '2 arguments expected.' )
      try:
         return args[0] % args[1]
      except TypeError:
         raise LispRuntimeFuncError( LP_moddiv, 'Invalid argument.' )
      except ZeroDivisionError:
         raise LispRuntimeFuncError( LP_moddiv, 'division by zero' )

   @primitive( 'gcd', '<integer1> <integer2> ...' )
   def LP_gcd( env: Environment, *args ) -> Any:
      """Returns the greatest common divisor of some integers."""
      try:
         return math.gcd( *args )
      except TypeError:
         raise LispRuntimeFuncError( LP_gcd, 'Invalid argument.' )

   @primitive( 'lcm', '<integer1> <integer2> ...' )
   def LP_lcm( env: Environment, *args ) -> Any:
      """Returns the least common multiple of some integers."""
      try:
         return math.lcm( *args )
      except TypeError:
         raise LispRuntimeFuncError( LP_lcm, 'Invalid argument.' )

   @primitive( 'log', '<number> &optional <base>' )
   def LP_log( env: Environment, *args ) -> Any:
      """Returns the logarithm of a number.  With one argument, returns the natural
logarithm (base e).  An optional second argument specifies the base."""
      numArgs = len(args)
      if not( 1 <= numArgs <= 2 ):
         raise LispRuntimeFuncError( LP_log, '1 or 2 arguments expected.' )

      try:
         num,*rest = args
         base = math.e if len(rest) == 0 else rest[0]
         return math.log(num,base)
      except (ValueError, TypeError):
         raise LispRuntimeFuncError( LP_log, 'Invalid argument.' )

   @primitive( 'expt', '<base> <power>' )
   def LP_expt( env: Environment, *args ) -> Any:
      """Returns base raised to a power."""
      try:
         base,power = args
         result = base ** power
      except ValueError:
         raise LispRuntimeFuncError( LP_expt, 'Exactly two arguments expected.' )
      except TypeError:
         raise LispRuntimeFuncError( LP_expt, 'Invalid argument type.  Arguments expected to be numbers.' )

      return result.real if isinstance(result, complex) else result

   @primitive( 'sin', '<radians>' )
   def LP_sin( env: Environment, *args ) -> Any:
      """Returns the sine of radians."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_sin, '1 argument expected.' )
      try:
         return math.sin(args[0])
      except (TypeError, ValueError):
         raise LispRuntimeFuncError( LP_sin, 'Invalid argument.' )

   @primitive( 'cos', '<radians>' )
   def LP_cos( env: Environment, *args ) -> Any:
      """Returns the cosine of radians."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_cos, '1 argument expected.' )
      try:
         return math.cos(args[0])
      except (TypeError, ValueError):
         raise LispRuntimeFuncError( LP_cos, 'Invalid argument.' )

   @primitive( 'asin', '<number>' )
   def LP_asin( env: Environment, *args ) -> Any:
      """Returns the arcsine of a number in radians."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_asin, '1 argument expected.' )
      try:
         return math.asin(args[0])
      except (TypeError, ValueError):
         raise LispRuntimeFuncError( LP_asin, 'Invalid argument.' )

   @primitive( 'acos', '<number>' )
   def LP_acos( env: Environment, *args ) -> Any:
      """Returns the arccosine of a number in radians."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_acos, '1 argument expected.' )
      try:
         return math.acos(args[0])
      except (TypeError, ValueError):
         raise LispRuntimeFuncError( LP_acos, 'Invalid argument.' )

   @primitive( 'atan', '<number1> &optional <number2>' )
   def LP_atan( env: Environment, *args ) -> Any:
      """Returns the arctangent of one or two numbers in radians."""
      numArgs = len(args)
      if numArgs == 1:
         try:
            return math.atan( args[0] )
         except (TypeError, ValueError):
            raise LispRuntimeFuncError( LP_atan, 'Invalid argument.' )
      elif numArgs == 2:
         try:
            return math.atan2( args[0], args[1] )
         except (TypeError, ValueError):
            raise LispRuntimeFuncError( LP_atan, 'Invalid argument.' )
      else:
         raise LispRuntimeFuncError( LP_atan, '1 or 2 arguments expected.' )

   @primitive( 'floor', '<number> &optional <divisor>' )
   def LP_floor( env: Environment, *args ) -> Any:
      """Returns the largest integer <= number (or <= number/divisor).
Returns only the primary value; remainder is discarded."""
      if len(args) not in (1, 2):
         raise LispRuntimeFuncError( LP_floor, '1 or 2 arguments expected.' )
      try:
         if len(args) == 1:
            return math.floor( args[0] )
         else:
            return math.floor( args[0] / args[1] )
      except (TypeError, ValueError, ZeroDivisionError) as e:
         raise LispRuntimeFuncError( LP_floor, f'Invalid argument: {e}' )

   @primitive( 'ceiling', '<number> &optional <divisor>' )
   def LP_ceiling( env: Environment, *args ) -> Any:
      """Returns the smallest integer >= number (or >= number/divisor).
Returns only the primary value; remainder is discarded."""
      if len(args) not in (1, 2):
         raise LispRuntimeFuncError( LP_ceiling, '1 or 2 arguments expected.' )
      try:
         if len(args) == 1:
            return math.ceil( args[0] )
         else:
            return math.ceil( args[0] / args[1] )
      except (TypeError, ValueError, ZeroDivisionError) as e:
         raise LispRuntimeFuncError( LP_ceiling, f'Invalid argument: {e}' )

   @primitive( 'round', '<number> &optional <divisor>' )
   def LP_round( env: Environment, *args ) -> Any:
      """Rounds number to the nearest integer (ties go to even, per CL).
Returns only the primary value; remainder is discarded."""
      if len(args) not in (1, 2):
         raise LispRuntimeFuncError( LP_round, '1 or 2 arguments expected.' )
      try:
         if len(args) == 1:
            return round( args[0] )
         else:
            return round( args[0] / args[1] )
      except (TypeError, ValueError, ZeroDivisionError) as e:
         raise LispRuntimeFuncError( LP_round, f'Invalid argument: {e}' )

   @primitive( 'min', '<number1> <number2> ...' )
   def LP_min( env: Environment, *args ) -> Any:
      """Returns the smallest of a set of numbers."""
      try:
         return min( *args )
      except (TypeError, ValueError):
         raise LispRuntimeFuncError( LP_min, 'Invalid argument.' )

   @primitive( 'max', '<number1> <number2> ...' )
   def LP_max( env: Environment, *args ) -> Any:
      """Returns the largest of a set of numbers."""
      try:
         return max( *args )
      except (TypeError, ValueError):
         raise LispRuntimeFuncError( LP_max, 'Invalid argument.' )

   @primitive( 'random', '<integerOrFloat>' )
   def LP_random( env: Environment, *args ) -> Any:
      """Returns a random int or float in the range 0 <= n <= arg.  If the argument
is an int the random number will be an int, if the argument is a float the
random number will be a float."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_random, 'Exactly 1 number argument expected.' )
      num = args[0]

      if isinstance( num, int ):
         if num < 0:
            raise LispRuntimeFuncError( LP_random, 'Argument expected to be non-negative.' )
         return _random.randint(0, num)
      elif isinstance( num, float ):
         return _random.uniform(0.0, num)
      else:
         raise LispRuntimeFuncError( LP_random, 'Invalid argument type.' )

from __future__ import annotations
import functools
import math
import random as _random
from fractions import Fraction
from typing import Any

from pythonslisp.ltk.EnvironmentBase import EnvironmentBase
from pythonslisp.AST import LNUMBER, LMultipleValues
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimePrimError


def _frac_or_int( frac: Fraction ):
   """Return frac as an int if it has no remainder, otherwise as a Fraction."""
   return int(frac) if frac.denominator == 1 else frac


def register(primitive) -> None:

   @primitive( '+', '(&rest numbers)' )
   def LP_add( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns the sum of numbers."""
      try:
         return sum(args)
      except TypeError:
         raise LRuntimePrimError( LP_add, 'Invalid argument.' )

   @primitive( '-', '(number &rest numbers)' )
   def LP_sub( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns the difference of numbers."""
      try:
         if len(args) == 1:
            arg = args[0]
            if not isinstance( arg, LNUMBER ):
               raise TypeError( )
            return -1 * arg
         return args[0] - sum(args[1:])
      except TypeError:
         raise LRuntimePrimError( LP_sub, 'Invalid argument.' )

   @primitive( '*', '(number &rest numbers)' )
   def LP_mul( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns the product of numbers."""
      try:
         return functools.reduce( lambda x,y: x * y, iter(args) )
      except TypeError:
         raise LRuntimePrimError( LP_mul, 'Invalid argument.' )

   @primitive( '/', '(number &rest more-numbers)', min_args=1 )
   def LP_div( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns the quotient of numbers.  With one argument returns the reciprocal.
Integer and Fraction inputs produce exact Fraction results (simplified to int
when the denominator is 1).  Any float input yields a float result."""
      try:
         if len(args) == 1:
            n = args[0]
            if isinstance(n, float):
               return 1.0 / n
            return _frac_or_int( Fraction(1) / Fraction(n) )
         if any( isinstance(a, float) for a in args ):
            return functools.reduce( lambda x, y: x / y, args )
         fracs  = [ Fraction(a) for a in args ]
         result = functools.reduce( lambda x, y: x / y, fracs )
         return _frac_or_int( result )
      except (TypeError, ValueError):
         raise LRuntimePrimError( LP_div, 'Invalid argument.' )
      except ZeroDivisionError:
         raise LRuntimePrimError( LP_div, 'division by zero' )

   @primitive( '//', '(dividend divisor)' )
   def LP_intdiv( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Return the integer division of two numbers."""
      try:
         return args[0] // args[1]
      except TypeError:
         raise LRuntimePrimError( LP_intdiv, 'Invalid argument.' )
      except ZeroDivisionError:
         raise LRuntimePrimError( LP_intdiv, 'division by zero' )

   @primitive( 'mod', '(number divisor)' )
   def LP_moddiv( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns the integer remainder of division of two numbers."""
      try:
         return args[0] % args[1]
      except TypeError:
         raise LRuntimePrimError( LP_moddiv, 'Invalid argument.' )
      except ZeroDivisionError:
         raise LRuntimePrimError( LP_moddiv, 'division by zero' )

   @primitive( 'gcd', '(&rest integers)' )
   def LP_gcd( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns the greatest common divisor of some integers."""
      try:
         return math.gcd( *args )
      except TypeError:
         raise LRuntimePrimError( LP_gcd, 'Invalid argument.' )

   @primitive( 'lcm', '(&rest integers)' )
   def LP_lcm( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns the least common multiple of some integers."""
      try:
         return math.lcm( *args )
      except TypeError:
         raise LRuntimePrimError( LP_lcm, 'Invalid argument.' )

   @primitive( 'log', '(number &optional (base e))' )
   def LP_log( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns the logarithm of a number.  With one argument, returns the natural
logarithm (base e).  An optional second argument specifies the base."""
      try:
         num,*rest = args
         base = math.e if len(rest) == 0 else rest[0]
         return math.log(num,base)
      except (ValueError, TypeError):
         raise LRuntimePrimError( LP_log, 'Invalid argument.' )

   @primitive( 'expt', '(base power)' )
   def LP_expt( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns base raised to a power."""
      base, power = args
      try:
         result = base ** power
      except TypeError:
         raise LRuntimePrimError( LP_expt, 'Invalid argument type.  Arguments expected to be numbers.' )

      return result.real if isinstance(result, complex) else result

   @primitive( 'sin', '(radians)' )
   def LP_sin( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns the sine of radians."""
      try:
         return math.sin(args[0])
      except (TypeError, ValueError):
         raise LRuntimePrimError( LP_sin, 'Invalid argument.' )

   @primitive( 'cos', '(radians)' )
   def LP_cos( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns the cosine of radians."""
      try:
         return math.cos(args[0])
      except (TypeError, ValueError):
         raise LRuntimePrimError( LP_cos, 'Invalid argument.' )

   @primitive( 'asin', '(number)' )
   def LP_asin( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns the arcsine of a number in radians."""
      try:
         return math.asin(args[0])
      except (TypeError, ValueError):
         raise LRuntimePrimError( LP_asin, 'Invalid argument.' )

   @primitive( 'acos', '(number)' )
   def LP_acos( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns the arccosine of a number in radians."""
      try:
         return math.acos(args[0])
      except (TypeError, ValueError):
         raise LRuntimePrimError( LP_acos, 'Invalid argument.' )

   @primitive( 'atan', '(number1 &optional number2)' )
   def LP_atan( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns the arctangent of one or two numbers in radians."""
      try:
         if len(args) == 1:
            return math.atan( args[0] )
         else:
            return math.atan2( args[0], args[1] )
      except (TypeError, ValueError):
         raise LRuntimePrimError( LP_atan, 'Invalid argument.' )

   @primitive( 'floor', '(number &optional divisor)' )
   def LP_floor( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns two values: the largest integer <= number (or <= number/divisor),
and the remainder (number - quotient*divisor).  In scalar context only the
quotient is used."""
      try:
         if len(args) == 1:
            q = math.floor( args[0] )
            r = args[0] - q
         else:
            q = math.floor( args[0] / args[1] )
            r = args[0] - q * args[1]
         return LMultipleValues( [q, r] )
      except (TypeError, ValueError, ZeroDivisionError) as e:
         raise LRuntimePrimError( LP_floor, f'Invalid argument: {e}' )

   @primitive( 'ceiling', '(number &optional divisor)' )
   def LP_ceiling( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns two values: the smallest integer >= number (or >= number/divisor),
and the remainder (number - quotient*divisor).  In scalar context only the
quotient is used."""
      try:
         if len(args) == 1:
            q = math.ceil( args[0] )
            r = args[0] - q
         else:
            q = math.ceil( args[0] / args[1] )
            r = args[0] - q * args[1]
         return LMultipleValues( [q, r] )
      except (TypeError, ValueError, ZeroDivisionError) as e:
         raise LRuntimePrimError( LP_ceiling, f'Invalid argument: {e}' )

   @primitive( 'round', '(number &optional divisor)' )
   def LP_round( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns two values: number rounded to the nearest integer (ties go to
even, per CL) (or rounded quotient of number/divisor), and the remainder
(number - quotient*divisor).  In scalar context only the quotient is used."""
      try:
         if len(args) == 1:
            q = round( args[0] )
            r = args[0] - q
         else:
            q = round( args[0] / args[1] )
            r = args[0] - q * args[1]
         return LMultipleValues( [q, r] )
      except (TypeError, ValueError, ZeroDivisionError) as e:
         raise LRuntimePrimError( LP_round, f'Invalid argument: {e}' )

   @primitive( 'truncate', '(number &optional divisor)' )
   def LP_truncate( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns two values: number truncated toward zero (or truncated quotient
of number/divisor), and the remainder (number - quotient*divisor).
In scalar context only the quotient is used."""
      try:
         if len(args) == 1:
            q = math.trunc( args[0] )
            r = args[0] - q
         else:
            q = math.trunc( args[0] / args[1] )
            r = args[0] - q * args[1]
         return LMultipleValues( [q, r] )
      except (TypeError, ValueError, ZeroDivisionError) as e:
         raise LRuntimePrimError( LP_truncate, f'Invalid argument: {e}' )

   @primitive( 'min', '(&rest numbers)' )
   def LP_min( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns the smallest of a set of numbers."""
      try:
         return min( *args )
      except (TypeError, ValueError):
         raise LRuntimePrimError( LP_min, 'Invalid argument.' )

   @primitive( 'max', '(&rest numbers)' )
   def LP_max( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns the largest of a set of numbers."""
      try:
         return max( *args )
      except (TypeError, ValueError):
         raise LRuntimePrimError( LP_max, 'Invalid argument.' )

   @primitive( 'random', '(integerOrFloat)' )
   def LP_random( ctx: Context, env: Environment, args: list[Any] ) -> Any:
      """Returns a random number in [0, n).  n must be positive.
For integer n returns a random integer in [0, n).
For float n returns a random float in [0.0, n)."""
      num = args[0]

      if isinstance( num, int ):
         if num <= 0:
            raise LRuntimePrimError( LP_random, 'Argument expected to be positive.' )
         return _random.randrange( num )
      elif isinstance( num, float ):
         if num <= 0.0:
            raise LRuntimePrimError( LP_random, 'Argument expected to be positive.' )
         return _random.uniform( 0.0, num )
      else:
         raise LRuntimePrimError( LP_random, 'Invalid argument type.' )

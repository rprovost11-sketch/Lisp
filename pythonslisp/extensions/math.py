from __future__ import annotations
import functools
import math
import random as _random
from fractions import Fraction
from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.AST import LNUMBER, LMultipleValues, got_str
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimePrimError, LRuntimeUsageError
from pythonslisp.extensions import primitive


def _frac_or_int( frac: Fraction ):
   """Return frac as an int if it has no remainder, otherwise as a Fraction."""
   return int(frac) if frac.denominator == 1 else frac


def _bad_number_arg( args, prim ):
   """After TypeError from a bulk numeric operation, find and report the first non-number argument."""
   for i, arg in enumerate(args, 1):
      if not isinstance(arg, LNUMBER):
         raise LRuntimeUsageError(prim, f'Invalid argument {i}. NUMBER expected{got_str(arg)}.')
   raise LRuntimeUsageError(prim, 'Invalid argument.')


@primitive( '+', '(&rest numbers)' )
def LP_add( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the sum of numbers."""
   try:
      return sum(args)
   except TypeError:
      _bad_number_arg(args, LP_add)

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
      _bad_number_arg(args, LP_sub)

@primitive( '*', '(number &rest numbers)' )
def LP_mul( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the product of numbers."""
   try:
      return functools.reduce( lambda x,y: x * y, iter(args) )
   except TypeError:
      _bad_number_arg(args, LP_mul)

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
      _bad_number_arg(args, LP_div)
   except ZeroDivisionError:
      raise LRuntimePrimError( LP_div, 'division by zero')

@primitive( '//', '(dividend divisor)' )
def LP_intdiv( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Return the integer division of two numbers."""
   try:
      return args[0] // args[1]
   except TypeError:
      _bad_number_arg(args[:2], LP_intdiv)
   except ZeroDivisionError:
      raise LRuntimePrimError( LP_intdiv, 'division by zero')

@primitive( 'mod', '(number divisor)' )
def LP_mod( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the modulus of two numbers.  The sign of the result matches
the sign of the divisor (second argument)."""
   try:
      return args[0] % args[1]
   except TypeError:
      _bad_number_arg(args[:2], LP_mod)
   except ZeroDivisionError:
      raise LRuntimePrimError( LP_mod, 'division by zero')

@primitive( 'rem', '(number divisor)' )
def LP_rem( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the remainder of division of two numbers.  The sign of the
result matches the sign of the dividend (first argument)."""
   try:
      a, b = args
      m = a % b
      if m == 0 or (a >= 0) == (b >= 0):
         return m
      return m - b
   except TypeError:
      _bad_number_arg(args[:2], LP_rem)
   except ZeroDivisionError:
      raise LRuntimePrimError( LP_rem, 'division by zero')

@primitive( 'gcd', '(&rest integers)' )
def LP_gcd( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the greatest common divisor of some integers."""
   try:
      return math.gcd( *args )
   except TypeError:
      _bad_number_arg(args, LP_gcd)

@primitive( 'lcm', '(&rest integers)' )
def LP_lcm( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the least common multiple of some integers."""
   try:
      return math.lcm( *args )
   except TypeError:
      _bad_number_arg(args, LP_lcm)

@primitive( 'log', '(number &optional (base e))' )
def LP_log( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the logarithm of a number.  With one argument, returns the natural
logarithm (base e).  An optional second argument specifies the base."""
   try:
      num,*rest = args
      base = math.e if len(rest) == 0 else rest[0]
      return math.log(num,base)
   except TypeError:
      _bad_number_arg(args, LP_log)
   except ValueError:
      raise LRuntimePrimError( LP_log, 'Invalid argument 1. Invalid value for logarithm.')

@primitive( 'expt', '(base power)' )
def LP_expt( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns base raised to a power."""
   base, power = args
   try:
      result = base ** power
   except TypeError:
      _bad_number_arg([base, power], LP_expt)

   return result.real if isinstance(result, complex) else result

@primitive( 'sin', '(radians)' )
def LP_sin( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the sine of radians."""
   try:
      return math.sin(args[0])
   except (TypeError, ValueError):
      raise LRuntimeUsageError( LP_sin, 'Invalid argument 1. Number expected.' )

@primitive( 'cos', '(radians)' )
def LP_cos( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the cosine of radians."""
   try:
      return math.cos(args[0])
   except (TypeError, ValueError):
      raise LRuntimeUsageError( LP_cos, 'Invalid argument 1. Number expected.' )

@primitive( 'asin', '(number)' )
def LP_asin( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the arcsine of a number in radians."""
   try:
      return math.asin(args[0])
   except (TypeError, ValueError):
      raise LRuntimeUsageError( LP_asin, 'Invalid argument 1. Number expected.' )

@primitive( 'acos', '(number)' )
def LP_acos( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the arccosine of a number in radians."""
   try:
      return math.acos(args[0])
   except (TypeError, ValueError):
      raise LRuntimeUsageError( LP_acos, 'Invalid argument 1. Number expected.' )

@primitive( 'atan', '(number1 &optional number2)' )
def LP_atan( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the arctangent of one or two numbers in radians."""
   try:
      if len(args) == 1:
         return math.atan( args[0] )
      else:
         return math.atan2( args[0], args[1] )
   except (TypeError, ValueError):
      _bad_number_arg(args, LP_atan)

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
   except (TypeError, ValueError):
      _bad_number_arg(args[:2], LP_floor)
   except ZeroDivisionError:
      raise LRuntimePrimError( LP_floor, 'Invalid argument 2. Division by zero.')

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
   except (TypeError, ValueError):
      _bad_number_arg(args[:2], LP_ceiling)
   except ZeroDivisionError:
      raise LRuntimePrimError( LP_ceiling, 'Invalid argument 2. Division by zero.')

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
   except (TypeError, ValueError):
      _bad_number_arg(args[:2], LP_round)
   except ZeroDivisionError:
      raise LRuntimePrimError( LP_round, 'Invalid argument 2. Division by zero.')

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
   except (TypeError, ValueError):
      _bad_number_arg(args[:2], LP_truncate)
   except ZeroDivisionError:
      raise LRuntimePrimError( LP_truncate, 'Invalid argument 2. Division by zero.')

@primitive( 'min', '(&rest numbers)' )
def LP_min( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the smallest of a set of numbers."""
   try:
      return min( *args )
   except (TypeError, ValueError):
      _bad_number_arg(args, LP_min)

@primitive( 'max', '(&rest numbers)' )
def LP_max( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the largest of a set of numbers."""
   try:
      return max( *args )
   except (TypeError, ValueError):
      _bad_number_arg(args, LP_max)

@primitive( 'random', '(integerOrFloat)' )
def LP_random( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a random number in [0, n).  n must be positive.
For integer n returns a random integer in [0, n).
For float n returns a random float in [0.0, n)."""
   num = args[0]

   if isinstance( num, int ):
      if num <= 0:
         raise LRuntimePrimError( LP_random, 'Invalid argument 1. Argument expected to be positive.')
      return _random.randrange( num )
   elif isinstance( num, float ):
      if num <= 0.0:
         raise LRuntimePrimError( LP_random, 'Invalid argument 1. Argument expected to be positive.')
      return _random.uniform( 0.0, num )
   else:
      raise LRuntimeUsageError( LP_random, f'Invalid argument 1. INTEGER or FLOAT expected{got_str(num)}.' )

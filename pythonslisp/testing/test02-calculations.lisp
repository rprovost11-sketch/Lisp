>>> (+ 3 4)
...

==> 7

>>> (+ 5 -1)
...

==> 4

>>> (+ 2 3.1)
...

==> 5.1

>>> (+ 2.1 3)
...

==> 5.1

>>> (+ 3 1/2)
...

==> 7/2

>>> (+ 1/2 3)
...

==> 7/2

>>> (+ 1 2)
...

==> 3

>>> (+ 9 2 5 6)
...

==> 22

>>> (+ 1/2 1/3)
...

==> 5/6

>>> (+ 2 0.5 1/2)
...

==> 3.0

>>> (+ 4)
...

==> 4

>>> (+ 1 2 3 4 5)
...

==> 15

>>> ( - 1067 60)
...

==> 1007

>>> (- 5 2.5
... )
...

==> 2.5

>>> (- 15 8)
...

==> 7

>>> (- 10 2 3)
...

==> 5

>>> (- 4)
...

==> -4

>>> (* 9 6)
...

==> 54

>>> (* 2 5 3 4)
...

==> 120

>>> (* 2 5 4 3)
...

==> 120

>>> (* 1/2 1/3 1/10)
...

==> 1/60

>>> (* 2 3 4)
...

==> 24

>>> (/ 16 1/2)
...

==> 32

>>> (// 29 7)
...

==> 4

>>> (mod 29 7)
...

==> 1

>>> (integer 5.893072)
...

==> 5

>>> (integer pi)
...

==> 3

>>> (integer e)
...

==> 2

>>> (abs 3)
...

==> 3

>>> (abs -1/2)
...

==> 1/2

>>> (log 5/32 10)
...

==> -0.8061799739838872

>>> (expt 2 8)
...

==> 256

>>> (expt 3 2)
...

==> 9

>>> (expt 2 4)
...

==> 16

>>> (expt 10 3)
...

==> 1000

>>> (expt 10 -2)
...

==> 0.01

>>> (expt 9 1/2)
...

==> 3.0

>>> (expt 16 1/2)
...

==> 4.0

>>> (expt 1/4 1/2)
...

==> 0.5

>>> (sin 3)
...

==> 0.1411200080598672

>>> (cos -52.1)
...

==> -0.2606749092650805

>>> (tan 32/35)
...

==> 1.297810006713956

>>> (float 1/4)
...

==> 0.25

>>> (float 1/100)
...

==> 0.01

>>> (* 2 1000 1000 1000)
...

==> 2000000000

>>> (float (* 2 1000 1000 1000))
...

==> 2000000000.0

>>> (* 2 1/1000 1/1000)
...

==> 1/500000

>>> (float (* 2 1/1000 1/1000))
...

==> 2e-06

>>> (min 0 -1/2 3.69)
...

==> -1/2

>>> (max -1/2 3.69 0)
...

==> 3.69

>>> (+ 3 (- 7 4))
...

==> 6

>>> (/ 12 2 3)
...

==> 2

>>> (/ 10 4)
...

==> 5/2

>>> (log 100)
...

==> 4.605170185988092

>>> (asin 0)
...

==> 0.0

>>> (asin 1)
...

==> 1.5707963267948966

>>> (acos 0)
...

==> 1.5707963267948966

>>> (acos 1)
...

==> 0.0

>>> (atan 0)
...

==> 0.0

>>> (atan 1)
...

==> 0.7853981633974483

>>> (atan 0 1)
...

==> 0.0


>>> ;;; Error: non-numeric argument to +
... (+ "a" 1)

%%% ERROR '+': Invalid argument.
%%% USAGE: (+ <number1> <number2> ...)
==>

>>> ;;; reciprocal: (/ n) = 1/n
... (/ 4)
...

==> 1/4

>>> ;;; (/ 1) = 1
... (/ 1)
...

==> 1

>>> ;;; (/ 2.0) = float reciprocal
... (/ 2.0)
...

==> 0.5

>>> ;;; Error: division by zero
... (/ 1 0)

%%% ERROR '/': division by zero
%%% USAGE: (/ <number1> <number2> ...)
==>

>>> ;;; Error: expt requires exactly two arguments
... (expt 2)

%%% ERROR 'EXPT': Exactly two arguments expected.
%%% USAGE: (EXPT <base> <power>)
==>

>>> ;;; Error: log requires at least one argument
... (log)

%%% ERROR 'LOG': 1 or 2 arguments expected.
%%% USAGE: (LOG <number> &optional <base>)
==>

>>> ;;; Error: non-numeric argument to log
... (log "a")

%%% ERROR 'LOG': Invalid argument.
%%% USAGE: (LOG <number> &optional <base>)
==>

>>> ;;; Error: non-numeric argument to sin
... (sin "a")

%%% ERROR 'SIN': Invalid argument.
%%% USAGE: (SIN <radians>)
==>

>>> ;;; Error: atan with no arguments
... (atan)

%%% ERROR 'ATAN': 1 or 2 arguments expected.
%%% USAGE: (ATAN <number1> &optional <number2>)
==>


; --- Arithmetic edge cases ---

>>> ;;; (+) with no arguments returns 0
... (+)
...

==> 0

>>> ;;; expt with power 0 returns 1
... (expt 2 0)
...

==> 1

>>> ;;; expt 0^0 returns 1
... (expt 0 0)
...

==> 1

>>> ;;; abs of zero
... (abs 0)
...

==> 0

>>> ;;; abs of negative rational
... (abs -3/4)
...

==> 3/4

>>> ;;; gcd with no args returns 0
... (gcd)
...

==> 0

>>> ;;; lcm with no args returns 1
... (lcm)
...

==> 1

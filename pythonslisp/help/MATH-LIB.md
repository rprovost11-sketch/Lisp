# Math Library

The math library consists of primitive operations (built in to the
interpreter) and a collection of Lisp-defined functions loaded at startup.
Together they cover arithmetic, number theory, trigonometry, rounding,
predicates, and common utilities.

---

## Arithmetic Primitives

### Basic operations

```lisp
(+ 1 2 3)       ;==> 6        ; variadic
(- 10 3)        ;==> 7        ; also (- 5) ==> -5  (negate)
(* 2 3 4)       ;==> 24       ; variadic
(/ 10 4)        ;==> 5/2      ; exact rational result
(// 10 3)       ;==> 3        ; integer (floor) division
(mod 10 3)      ;==> 1        ; modulo (sign follows divisor)
```

Division always returns an exact fraction when the result is not an integer.
Use `float` to convert: `(float (/ 10 4))` → `2.5`.

### Exponentiation and roots

```lisp
(expt 2 10)     ;==> 1024     ; integer expt
(expt 2.0 0.5)  ;==> 1.4142...  ; float
(sqrt 9.0)      ;==> 3.0      ; from math.lisp
(isqrt 17)      ;==> 4        ; integer square root (floor)
(exp 1.0)       ;==> 2.7182...  ; e^x  (from math.lisp)
```

### Logarithms

```lisp
(log 1.0)           ;==> 0.0        ; natural log (base e)
(log 100.0 10.0)    ;==> 2.0        ; log base 10
(log 8.0 2.0)       ;==> 3.0        ; log base 2
(ln 2.71828)        ;==> ~1.0       ; alias for (log x) in math.lisp
```

### Number theory

```lisp
(gcd 12 8)      ;==> 4
(gcd 12 8 6)    ;==> 2        ; variadic
(lcm 4 6)       ;==> 12
(lcm 4 6 10)    ;==> 60       ; variadic
```

---

## Rounding — Multiple Return Values

`floor`, `ceiling`, `round`, and `truncate` each return **two values**:
the quotient and the remainder.  Use `multiple-value-bind` to capture both,
or just evaluate them for the primary (quotient) value.

```lisp
(floor 7 2)       ;==> 3  1      ; floor division: q=3, r=1
(ceiling 7 2)     ;==> 4  -1     ; ceiling division
(round 7 2)       ;==> 4  -1     ; round to nearest even
(truncate 7 2)    ;==> 3  1      ; truncate toward zero

; Single-argument forms divide by 1
(floor 3.7)       ;==> 3  0.7
(ceiling 3.2)     ;==> 4  -0.8
(round 2.5)       ;==> 2  0.5    ; bankers' rounding
(truncate -3.7)   ;==> -3  -0.7

; Capture both values
(multiple-value-bind (q r) (floor 17 5)
  (ustring q " remainder " r))
;==> "3 remainder 2"
```

See MULTIPLE-VALUES for more on multiple return values.

---

## Min, Max, and Absolute Value

```lisp
(min 3 1 4 1 5)   ;==> 1
(max 3 1 4 1 5)   ;==> 5
(abs -7)          ;==> 7        ; from math.lisp
(abs 3.5)         ;==> 3.5
(signum -4)       ;==> -1       ; from math.lisp: -1, 0, or 1
(signum 0)        ;==> 0
(signum 7.5)      ;==> 1
```

---

## Trigonometry

All trigonometric functions work in **radians**.

```lisp
(sin PI)         ;==> ~0.0
(cos 0.0)        ;==> 1.0
(tan PI)         ;==> ~0.0  (tan is defined in math.lisp)
(asin 1.0)       ;==> ~1.5708  (π/2)
(acos 1.0)       ;==> 0.0
(atan 1.0)       ;==> ~0.7854  (π/4)
(atan 1.0 1.0)   ;==> ~0.7854  ; 2-arg form: atan(y/x) with correct quadrant
```

The constants `PI` and `E` are defined in math.lisp:

```lisp
PI    ;==> 3.141592653589793
E     ;==> 2.718281828459045
```

---

## Predicates

These predicates are all defined in math.lisp:

```lisp
; Zero, positive, negative
(zerop 0)       ;==> T
(plusp 3)       ;==> T
(minusp -2)     ;==> T

; Even and odd (integers only)
(evenp 4)       ;==> T
(oddp  3)       ;==> T

; Aliases (alternative spellings)
(isZero?    0)  ;==> T
(isPositive? 5) ;==> T
(isNegative? -1);==> T
(isEven? 6)     ;==> T
(isOdd? 7)      ;==> T
```

From types.py — general numeric type predicates:

```lisp
(numberp  42)    ;==> T    ; any number
(integerp 42)    ;==> T    ; integer
(floatp   3.14)  ;==> T    ; float
(rationalp 1/3)  ;==> T    ; integer or ratio
```

---

## Increment and Decrement

`incf` and `decf` mutate a place in the environment and return the new value.

```lisp
(setf n 5)
(incf n)      ;==> 6  (n is now 6)
(incf n 3)    ;==> 9  (n is now 9)
(decf n)      ;==> 8
(decf n 4)    ;==> 4
```

---

## Random Numbers

```lisp
(random 10)        ; random integer in [0, 10)
(random 1.0)       ; random float in [0.0, 1.0)
```

---

## Average

```lisp
(average 1 2 3 4 5)   ;==> 3
(average 1.0 2.0)     ;==> 1.5
```

---

## Type Conversion

```lisp
(float 3)          ;==> 3.0       ; integer → float
(integer 3.7)      ;==> 3         ; float → integer (truncate)
(integer "42")     ;==> 42        ; parse string
(integer "ff" 16)  ;==> 255       ; parse in given base
(rational 0.5)     ;==> 1/2       ; float → exact fraction
(rational 3)       ;==> 3/1
```

---

## Comparison Operators

```lisp
(= 1 1 1)     ;==> T     ; all equal
(/= 1 2 1)    ;==> NIL   ; 1st and 3rd equal — NIL
(< 1 2 3)     ;==> T
(<= 1 1 2)    ;==> T
(> 3 2 1)     ;==> T
(>= 3 3 2)    ;==> T
```

All comparison operators accept two or more arguments and test all adjacent
pairs (or all pairs for `/=`).

---

## Quick Reference

| Expression | Meaning |
|---|---|
| `(+ a b ...)` | Sum |
| `(- a b)` | Subtract; `(- a)` negate |
| `(* a b ...)` | Product |
| `(/ a b)` | Exact division (fraction) |
| `(// a b)` | Integer division |
| `(mod a b)` | Modulo |
| `(expt b e)` | b to the power e |
| `(sqrt x)` | Square root |
| `(isqrt n)` | Integer square root |
| `(abs x)` | Absolute value |
| `(signum x)` | Sign: -1, 0, or 1 |
| `(gcd a b ...)` | Greatest common divisor |
| `(lcm a b ...)` | Least common multiple |
| `(log x)` | Natural log |
| `(log x base)` | Log in given base |
| `(exp x)` | e^x |
| `(sin x)` / `(cos x)` / `(tan x)` | Trig (radians) |
| `(atan y x)` | Two-arg arctangent |
| `(floor n d)` | Floor division → quotient, remainder |
| `(ceiling n d)` | Ceiling division → quotient, remainder |
| `(round n d)` | Nearest-even rounding → quotient, remainder |
| `(truncate n d)` | Truncate toward zero → quotient, remainder |
| `(min a b ...)` / `(max a b ...)` | Minimum/maximum |
| `(random n)` | Random number in [0, n) |
| `(average a b ...)` | Arithmetic mean |
| `(incf place)` | Increment by 1 |
| `(decf place)` | Decrement by 1 |
| `PI` / `E` | Math constants |
| `(zerop x)` `(plusp x)` `(minusp x)` | Zero/positive/negative |
| `(evenp n)` `(oddp n)` | Even/odd |

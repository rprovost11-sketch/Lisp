# Strings

*Quick reference: `(help "strings")` — Full documentation: this file.*


Strings in Python's Lisp are immutable sequences of characters represented
as Python str values.  String literals use double quotes with standard
escape sequences (`\n`, `\t`, `\\`, `\"`).

---

## Creating Strings

### String literals

```lisp
"hello"             ; a string
"line one\nline two"  ; with newline
"tab\there"         ; with tab
"say \"hi\""        ; embedded quotes
```

### Constructing strings from values

```lisp
; string — programmer-readable (strings are quoted, symbols in caps)
(string "hello")    ;==> "\"hello\""
(string 42)         ;==> "42"
(string 'foo)       ;==> "FOO"

; ustring — user-readable (strings unquoted, no escaping)
(ustring "hello")   ;==> "hello"
(ustring 42 " + " 1 " = " 43)  ;==> "42 + 1 = 43"
(ustring 'foo)      ;==> "FOO"
```

`ustring` is the preferred way to build display strings from mixed values.

---

## Case Conversion

```lisp
(string-upcase   "Hello World")  ;==> "HELLO WORLD"
(string-downcase "Hello World")  ;==> "hello world"
(string-capitalize "hello world fun")  ;==> "Hello World Fun"
```

`string-capitalize` follows CL word-boundary rules: the first
letter of each word (sequence of alphanumeric chars) is uppercased, the
rest lowercased.

### Capitalizing a region

```lisp
(string-capitalize "hello world" :start 6)  ;==> "hello World"
(string-capitalize "hello world" :start 0 :end 5) ;==> "Hello world"
```

---

## Trimming Whitespace and Characters

```lisp
; Trim from both ends
(string-trim " \t" "  hello  ")   ;==> "hello"

; Trim from left only
(string-left-trim " " "  hello  ")  ;==> "hello  "

; Trim from right only
(string-right-trim " " "  hello  ") ;==> "  hello"

; The first argument is a bag of characters to remove (not a prefix)
(string-trim "abc" "abcXYZcba")   ;==> "XYZ"
```

---

## Character Operations

```lisp
; Single character as a one-character string
(char-code "A")    ;==> 65    ; Unicode code point
(char-code "a")    ;==> 97
(code-char 65)     ;==> "A"   ; code point → character string
(code-char 10)     ;==> "\n"  ; newline
```

The `char` macro (defined in core.lisp) retrieves a character by index:

```lisp
(char "hello" 0)   ;==> "h"
(char "hello" 4)   ;==> "o"
```

---

## Subsequences and Length

```lisp
(length "hello")           ;==> 5
(subseq "hello world" 6)   ;==> "world"
(subseq "hello world" 0 5) ;==> "hello"
```

---

## Searching and Testing

```lisp
; Check containment (returns position or nil)
(position #\h "hello")    ; note: use string form

; Using find on a list of chars — convert first or use predicates
(some (lambda (c) (string= c "e")) '("h" "e" "l"))  ;==> T

; String comparison — use standard predicates
(string= "abc" "abc")   ; NOTE: Python's Lisp uses equal for strings
(equal "abc" "abc")     ;==> T
(equal "abc" "ABC")     ;==> NIL
```

For case-insensitive comparison:

```lisp
(equal (string-downcase "Hello") (string-downcase "hello"))  ;==> T
```

---

## Splitting and Joining

Python's Lisp does not include built-in split/join, but you can build them:

```lisp
; Join a list of strings with a separator
(defun join (sep lst)
  (if (null lst)
      ""
      (reduce (lambda (a b) (ustring a sep b)) lst)))

(join ", " '("apple" "banana" "cherry"))
;==> "apple, banana, cherry"
```

---

## Formatted Output

### writef — Python str.format() syntax

```lisp
(writef "Hello, {0}!\n" (list "World"))
(writef "{0} + {1} = {2}\n" (list 1 2 3))
(writef "{name} is {age}\n" (map (name "Alice") (age 30)))
```

`writef` returns the formatted string and also writes it to the current
output (or a stream if supplied as the third argument):

```lisp
(writef "{0}\n" (list "hello") f)  ; write to stream f
```

### write! and uwrite!

```lisp
; Programmer-readable (strings shown with quotes)
(write! "hello")    ; prints "hello" (with the quotes)
(write-line "hello")  ; adds newline

; User-readable (no quotes, escapes decoded)
(uwrite! "hello")   ; prints hello
(uwrite-line "hello" " " "world")  ; prints hello world
```

### with-output-to-string — capture as a string

```lisp
(with-output-to-string (s)
  (writef "x = {0}\n" (list 42) s)
  (writef "y = {0}\n" (list 99) s))
;==> "x = 42\ny = 99\n"
```

---

## Parsing Strings as Numbers

```lisp
(integer "42")       ;==> 42
(integer "ff" 16)    ;==> 255
(float "3.14")       ;==> 3.14
```

---

## Reading S-expressions from Strings

```lisp
(with-input-from-string (s "(+ 1 2)")
  (read s))
;==> (+ 1 2)    ; unevaluated AST
```

See STREAMS for more string stream operations.

---

## Quick Reference

| Expression | Meaning |
|---|---|
| `"text"` | String literal |
| `(ustring v1 v2 ...)` | User-readable concatenation |
| `(string v1 v2 ...)` | Programmer-readable concatenation |
| `(string-upcase s)` | ALL CAPS |
| `(string-downcase s)` | all lower |
| `(string-capitalize s)` | Title Case |
| `(string-trim bag s)` | Strip chars from both ends |
| `(string-left-trim bag s)` | Strip from left |
| `(string-right-trim bag s)` | Strip from right |
| `(char-code c)` | Unicode code point of char |
| `(code-char n)` | Char string from code point |
| `(char s i)` | Character at index i |
| `(length s)` | String length |
| `(subseq s start end)` | Substring |
| `(equal s1 s2)` | String equality |
| `(writef fmt args)` | Formatted string output |
| `(integer s)` / `(float s)` | Parse string as number |

---

*See `(help "strings")` for the condensed quick reference.*

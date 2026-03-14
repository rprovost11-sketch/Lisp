# Streams

*Quick reference: `(help "streams")` — Full documentation: this file.*


A stream is an object that represents a source or destination for character
data.  File streams connect to files on disk.  String streams read from or
write to an in-memory string.  The standard streams connect to the terminal.
All streams share the same set of reading, writing, and inspection primitives.

---

## Standard Streams

Seven standard stream variables are initialized at startup.  They follow
the Common Lisp naming conventions.

| Variable | Default | Purpose |
|---|---|---|
| `*standard-input*` | stdin | Default input |
| `*standard-output*` | stdout | Default output |
| `*error-output*` | stderr | Error messages |
| `*terminal-io*` | stdout | Direct terminal I/O |
| `*debug-io*` | stderr | Debugger I/O |
| `*query-io*` | stdout | Interactive queries |
| `*trace-output*` | stdout | Trace output |

The primitives `(stdin)`, `(stdout)`, and `(stderr)` return the underlying
system streams directly.

```lisp
(write-line (stdout) "hello")    ; write to stdout explicitly
(write-line *error-output* "!")  ; write to stderr via variable
```

---

## File Streams

### with-open-file — preferred for file I/O

`with-open-file` opens a file, binds it to a variable, evaluates the body,
and closes the file when the body finishes.  This is the preferred way to
work with files because it guarantees the file is closed.

```lisp
(with-open-file (stream "filespec" open-options...)
  body...)
```

**Reading a file:**

```lisp
(with-open-file (f "data.txt")   ; :direction :input is the default
  (readall f))                   ; read entire contents as a string
```

**Writing a file:**

```lisp
(with-open-file (f "output.txt" :direction :output)
  (write-line f "line one")
  (write-line f "line two"))
```

**Appending to a file:**

```lisp
(with-open-file (f "log.txt" :direction :output :if-exists :append)
  (write-line f "new entry"))
```

### open and close

Use `open` and `close` when you need to manage the stream lifetime manually.

```lisp
(setf f (open "data.txt"))          ; open for reading
(setf line (read-line f nil nil))   ; read one line
(close f)
```

`open` keyword arguments:

| Keyword | Default | Values |
|---|---|---|
| `:direction` | `:input` | `:input`, `:output` |
| `:if-exists` | `:supersede` | `:supersede`, `:append`, `:error`, `nil` |
| `:if-does-not-exist` | `:error` | `:error`, `nil` |

`:if-exists nil` and `:if-does-not-exist nil` both cause `open` to return
NIL silently rather than signaling an error.

```lisp
; Open only if file exists, return NIL otherwise
(setf f (open "maybe.txt" :if-does-not-exist nil))
(when f
  (let ((content (readall f)))
    (close f)
    content))
```

---

## String Streams

String streams let you read from or write to an in-memory string using the
same primitives as file I/O.

### with-output-to-string — preferred for string output

Evaluates body forms with a variable bound to a fresh string output stream,
then returns everything written to that stream as a single string.

```lisp
(with-output-to-string (s)
  (write-line s "hello")
  (writef "value = {0}\n" (list 42) s))
;==> "hello\nvalue = 42\n"
```

### with-input-from-string — preferred for string input

Creates a readable stream from a string, binds it to a variable, and
evaluates body forms.

```lisp
(with-input-from-string (s "line one\nline two\n")
  (list (read-line s) (read-line s)))
;==> ("line one" "line two")

; Optional start and end delimit a substring of the source
(with-input-from-string (s "abcdefgh" 2 5)
  (readall s))
;==> "cde"
```

### Manual string streams

```lisp
; Output stream
(setf out (make-string-output-stream))
(write-line out "first")
(write-line out "second")
(get-output-stream-string out)   ;==> "first\nsecond\n"
; get-output-stream-string clears the buffer; stream stays open
(get-output-stream-string out)   ;==> ""

; Input stream
(setf in (make-string-input-stream "hello world"))
(read-line in)   ;==> "hello world"
```

---

## Reading from Streams

All read primitives accept an optional stream argument.  When omitted or
NIL, they read from standard input.

### read-line

Reads one line and returns it as a string, without the trailing newline.

```lisp
(read-line stream)                       ; error at EOF
(read-line stream nil nil)               ; return NIL at EOF
(read-line stream nil "default-value")   ; return given value at EOF
```

```lisp
(with-open-file (f "data.txt")
  (for (line (read-line f nil nil)) line (read-line f nil nil)
    (uwrite-line line)))
```

### read-char

Reads and returns a single character as a one-character string.

```lisp
(read-char stream)           ; error at EOF
(read-char stream nil nil)   ; return NIL at EOF
```

### read

Reads and returns one complete s-expression from the stream, without
evaluating it.  On seekable streams (files and string streams) the stream
position advances past the consumed expression.

```lisp
(with-input-from-string (s "(+ 1 2) (+ 3 4)")
  (list (read s) (read s)))
;==> ((+ 1 2) (+ 3 4))
```

This makes `read` useful for parsing Lisp data files or simple serialized
formats:

```lisp
; data.lisp contains: (name "Alice") (age 30) (city "London")
(with-open-file (f "data.lisp")
  (let ((result nil))
    (for (expr (read f nil nil)) expr (read f nil nil)
      (push! result expr))
    (reverse result)))
```

### readall

Reads the entire remaining contents of a stream as a single string.

```lisp
(with-open-file (f "notes.txt")
  (readall f))
```

---

## Writing to Streams

All write primitives accept an optional stream as their first argument.
When omitted, output goes to the current output stream.

### write! and write-line

Print values in **programmer-readable** form — strings are quoted and escape
sequences are shown.  `write-line` appends a newline.

```lisp
(write! "hello")           ; prints "hello" (with quotes)
(write-line "hello")         ; prints "hello"\n
(write-line f "hello" 42)    ; write to stream f
```

### uwrite! and uwrite-line

Print values in **user-readable** form — strings are printed without quotes
and escape sequences are decoded.  `uwrite-line` appends a newline.

```lisp
(uwrite! "hello")          ; prints hello (no quotes)
(uwrite-line "hello" " " "world")  ; prints hello world\n
```

### writef

Writes a formatted string using Python `str.format()` syntax.  Returns the
string that was written.

```lisp
(writef "Hello, {0}!\n" (list "World"))
(writef "x={x}, y={y}\n" (map (x 10) (y 20)))
(writef "done\n" f)                     ; write to stream f
(writef "{0} of {1}\n" (list 3 10) f)  ; format and write to f
```

### terpri

Outputs a newline.  Returns NIL.

```lisp
(terpri)      ; newline to current output
(terpri f)    ; newline to stream f
```

---

## Stream Predicates

```lisp
(open-stream-p stream)        ; T if open, NIL if closed
(closed stream)               ; T if closed, NIL if open
(input-stream-p stream)       ; T if readable
(output-stream-p stream)      ; T if writable
(interactive-stream-p stream) ; T if connected to a terminal
```

Aliases available for the three directional predicates:

```lisp
(readable stream)    ; same as input-stream-p
(writable stream)    ; same as output-stream-p
(isatty stream)      ; same as interactive-stream-p
```

`type-of` identifies the stream kind:

```lisp
(type-of (open "f.txt"))           ;==> FILE-STREAM
(type-of (make-string-output-stream)) ;==> STRING-STREAM
```

---

## flush

Forces any buffered output to be written.

```lisp
(flush)          ; flush stdout
(flush stream)   ; flush a specific stream
```

---

## Practical Patterns

### Process a file line by line

```lisp
(with-open-file (f "data.txt")
  (let ((count 0))
    (for (line (read-line f nil nil)) line (read-line f nil nil)
      (setf count (+ count 1))
      (uwrite-line line))
    count))
```

### Capture output as a string

```lisp
(defun list->csv (lst)
  (with-output-to-string (s)
    (dolist (item lst)
      (writef "{0}," (list (ustring item)) s))
    (terpri s)))

(list->csv '(apple banana cherry))
;==> "APPLE,BANANA,CHERRY,\n"
```

### Parse s-expressions from a string

```lisp
(defun read-all-from-string (str)
  (with-input-from-string (s str)
    (let ((exprs nil))
      (for (e (read s nil nil)) e (read s nil nil)
        (push! exprs e))
      (reverse exprs))))

(read-all-from-string "(+ 1 2) (* 3 4) (- 9 5)")
;==> ((+ 1 2) (* 3 4) (- 9 5))
```

### Write a report to a file

```lisp
(defun write-report (filename data)
  (with-open-file (f filename :direction :output)
    (write-line f "Report")
    (write-line f "======")
    (dolist (row data)
      (writef "{name}: {value}\n" row f))))
```

### Check a file exists before opening

```lisp
(let ((f (open "config.txt" :if-does-not-exist nil)))
  (if f
      (let ((content (readall f)))
        (close f)
        content)
      "default config"))
```

---

## Quick Reference

| Expression | Meaning |
|---|---|
| `(with-open-file (v "f") body...)` | Open file, bind to v, close on exit |
| `(with-output-to-string (v) body...)` | Capture writes to v as a string |
| `(with-input-from-string (v str) body...)` | Read from str via stream v |
| `(open "f" :direction :output)` | Open file for writing |
| `(close stream)` | Close a stream |
| `(read-line stream nil nil)` | Read line; NIL at EOF |
| `(read-char stream nil nil)` | Read one character; NIL at EOF |
| `(read stream nil nil)` | Read one s-expression; NIL at EOF |
| `(readall stream)` | Read entire stream contents |
| `(write! stream val...)` | Write programmer-readable output |
| `(write-line stream val...)` | Same with trailing newline |
| `(uwrite! stream val...)` | Write user-readable output |
| `(uwrite-line stream val...)` | Same with trailing newline |
| `(writef fmt list stream)` | Write formatted string |
| `(terpri stream)` | Write a newline |
| `(flush stream)` | Flush buffered output |
| `(open-stream-p stream)` | T if open |
| `(input-stream-p stream)` | T if readable |
| `(output-stream-p stream)` | T if writable |

---

*See `(help "streams")` for the condensed quick reference.*

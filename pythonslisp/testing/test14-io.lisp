>>> (write! 42)
42
==> 42

>>> (write! "hello")
"hello"
==> "hello"

>>> (writeLn! 42)
42

==> 42

>>> (writeLn! "hello")
"hello"

==> "hello"

>>> (uwrite! 42)
42
==> 42

>>> (uwrite! "hello")
hello
==> "hello"

>>> (uwriteLn! 42)
42

==> 42

>>> (uwriteLn! "hello")
hello

==> "hello"

>>> (writef "{0} {1}" (list 42 "hello"))
42 hello
==> "42 hello"

>>> (writef "x={0:d}" (list 99))
x=99
==> "x=99"

>>> (writef "{0} + {1} = {2}" (list 1 2 3))
1 + 2 = 3
==> "1 + 2 = 3"

>>> ;;; writef with no second arg outputs the format string unchanged
... (writef "no substitution needed")
no substitution needed
==> "no substitution needed"

>>> ;;; write!/writeLn! with no arguments are valid no-ops returning NIL
... (write!)
==> NIL

>>> (writeLn!)
==> NIL

>>> (uwrite!)
==> NIL

>>> (uwriteLn!)
==> NIL

>>> ;;; Error: writef requires at least one argument
... (writef)

%%% ERROR 'WRITEF': 1 to 3 arguments expected.
%%% USAGE: (WRITEF formatString &optional dictOrList stream)
==>

>>> ;;; Error: writef first argument must be a format string
... (writef 1 '(a b))

%%% ERROR 'WRITEF': 1st argument expected to be a format string.
%%% USAGE: (WRITEF formatString &optional dictOrList stream)
==>

>>> ;;; Error: writef second argument must be a list or map
... (writef "hello" 1)

%%% ERROR 'WRITEF': 2nd argument expected to be a list, dict or stream.
%%% USAGE: (WRITEF formatString &optional dictOrList stream)
==>

; ============================================================
; Additional write!/uwrite!/writeLn!/uwriteLn! coverage
; ============================================================

; --- write! with various types ---

>>> ;;; write! with NIL
... (write! NIL)
NIL
==> NIL

>>> ;;; write! with T
... (write! T)
T
==> T

>>> ;;; write! with float
... (write! 3.14)
3.14
==> 3.14

>>> ;;; write! with fraction
... (write! 1/3)
1/3
==> 1/3

>>> ;;; write! with symbol
... (write! 'foo)
FOO
==> FOO

>>> ;;; write! with list
... (write! '(1 2 3))
(1 2 3)
==> (1 2 3)

>>> ;;; write! with nested list
... (write! '(a (b c) d))
(A (B C) D)
==> (A (B C) D)

>>> ;;; write! with empty list
... (write! '())
NIL
==> NIL

; --- uwrite! with various types ---

>>> ;;; uwrite! with NIL
... (uwrite! NIL)
NIL
==> NIL

>>> ;;; uwrite! with T
... (uwrite! T)
T
==> T

>>> ;;; uwrite! with float
... (uwrite! 3.14)
3.14
==> 3.14

>>> ;;; uwrite! with fraction
... (uwrite! 1/3)
1/3
==> 1/3

>>> ;;; uwrite! with symbol
... (uwrite! 'foo)
FOO
==> FOO

>>> ;;; uwrite! with list
... (uwrite! '(1 2 3))
(1 2 3)
==> (1 2 3)

; --- uwrite!/uwriteLn! with multiple arguments ---

>>> ;;; uwrite! with multiple args concatenates
... (uwrite! 1 2 3)
123
==> 3

>>> ;;; uwriteLn! with multiple args
... (uwriteLn! 4 5 6)
456

==> 6

; --- write! vs uwrite! string quoting ---

>>> ;;; write! with multiple strings keeps quotes
... (write! "a" "b" "c")
"a""b""c"
==> "c"

>>> ;;; uwrite! with multiple strings omits quotes
... (uwrite! "a" "b" "c")
abc
==> "c"

; --- mixed types in multi-arg ---

>>> ;;; write! with mixed types (string + int)
... (write! "x=" 42)
"x="42
==> 42

>>> ;;; uwrite! with mixed types (string + int)
... (uwrite! "x=" 42)
x=42
==> 42

; --- return value is last argument ---

>>> ;;; write! returns last of multiple int args
... (write! 10 20 30)
102030
==> 30

>>> ;;; write! returns last arg when string
... (write! 1 "end")
1"end"
==> "end"

>>> ;;; uwrite! returns last arg when string
... (uwrite! 1 "end")
1end
==> "end"

; --- writeLn!/uwriteLn! multi-arg ---

>>> ;;; writeLn! with multiple string args
... (writeLn! "a" "b")
"a""b"

==> "b"

>>> ;;; uwriteLn! with multiple string args
... (uwriteLn! "a" "b")
ab

==> "b"


; --- terpri ---

>>> (terpri)


==> NIL


; ============================================================
; flush with no argument (flushes stdout)
; ============================================================

>>> (flush)
==> T

; ============================================================
; tmpdir and path-join
; ============================================================

>>> ;;; tmpdir returns a non-empty string
... (stringp (tmpdir))
==> T

>>> (> (length (tmpdir)) 0)
==> T

>>> ;;; path-join returns a string longer than either component
... (stringp (path-join "a" "b"))
==> T

>>> (> (length (path-join "a" "b")) 2)
==> T

; ============================================================
; streamp: NIL for non-stream types
; ============================================================

>>> (streamp 42)
==> NIL

>>> (streamp "hello")
==> NIL

>>> (streamp nil)
==> NIL

>>> (streamp '(a b c))
==> NIL

; ============================================================
; open-write, stream predicates, write to stream, close
; ============================================================

>>> (setf st14w (open-write (path-join (tmpdir) "test14-stream.tmp")))
==> #<STREAM>

>>> ;;; streamp: T for an open stream
... (streamp st14w)
==> T

>>> ;;; type-of: STREAM
... (type-of st14w)
==> STREAM

>>> (writable st14w)
==> T

>>> (readable st14w)
==> NIL

>>> (isatty st14w)
==> NIL

>>> (closed st14w)
==> NIL

>>> ;;; uwriteLn! to stream: no stdout output, returns last arg
... (uwriteLn! st14w "hi")
==> "hi"

>>> (uwriteLn! st14w "bye")
==> "bye"

>>> (flush st14w)
==> T

>>> (close st14w)
==> T

>>> (closed st14w)
==> T

; ============================================================
; open-read, read lines, EOF detection
; ============================================================

>>> (setf st14r (open-read (path-join (tmpdir) "test14-stream.tmp")))
==> #<STREAM>

>>> (readable st14r)
==> T

>>> (writable st14r)
==> NIL

>>> (closed st14r)
==> NIL

>>> ;;; "hi\n" has length 3
... (= (length (readLn! st14r)) 3)
==> T

>>> ;;; "bye\n" has length 4
... (= (length (readLn! st14r)) 4)
==> T

>>> ;;; past EOF returns empty string
... (= (readLn! st14r) "")
==> T

>>> (close st14r)
==> T

; ============================================================
; terpri, write!, writeLn!, writef to stream
; ============================================================

>>> (setf st14w (open-write (path-join (tmpdir) "test14-stream.tmp")))
==> #<STREAM>

>>> ;;; terpri to stream: writes newline, returns NIL, no stdout output
... (terpri st14w)
==> NIL

>>> ;;; write! to stream: writes programmer-format, no newline
... (write! st14w 7)
==> 7

>>> ;;; writeLn! to stream: writes programmer-format with trailing newline
... (writeLn! st14w "ok")
==> "ok"

>>> ;;; writef 3-arg: format + list + stream
... (writef "n={0}" (list 5) st14w)
==> "n=5"

>>> ;;; writef 2-arg: format + stream (no substitutions)
... (writef "end" st14w)
==> "end"

>>> (close st14w)
==> T

>>> (setf st14r (open-read (path-join (tmpdir) "test14-stream.tmp")))
==> #<STREAM>

>>> ;;; line 1: "\n" from terpri, length 1
... (= (length (readLn! st14r)) 1)
==> T

>>> ;;; line 2: "7" + '"ok"\n' = '7"ok"\n', length 6
... (= (length (readLn! st14r)) 6)
==> T

>>> ;;; rest of file: "n=5end" (no newline), length 6
... (= (length (readLn! st14r)) 6)
==> T

>>> (= (readLn! st14r) "")
==> T

>>> (close st14r)
==> T

; ============================================================
; open-append: appends to existing file
; ============================================================

>>> ;;; use open-write to create a fresh file
... (setf st14a (open-write (path-join (tmpdir) "test14-append.tmp")))
==> #<STREAM>

>>> (uwriteLn! st14a "first")
==> "first"

>>> (close st14a)
==> T

>>> ;;; open-append adds to the existing content
... (setf st14a (open-append (path-join (tmpdir) "test14-append.tmp")))
==> #<STREAM>

>>> (writable st14a)
==> T

>>> (uwriteLn! st14a "second")
==> "second"

>>> (close st14a)
==> T

>>> (setf st14r (open-read (path-join (tmpdir) "test14-append.tmp")))
==> #<STREAM>

>>> ;;; "first\n" has length 6
... (= (length (readLn! st14r)) 6)
==> T

>>> ;;; "second\n" has length 7
... (= (length (readLn! st14r)) 7)
==> T

>>> (= (readLn! st14r) "")
==> T

>>> (close st14r)
==> T

; ============================================================
; readall: reads entire file contents as one string
; ============================================================

>>> (setf st14w (open-write (path-join (tmpdir) "test14-stream.tmp")))
==> #<STREAM>

>>> (uwriteLn! st14w "hello")
==> "hello"

>>> (uwriteLn! st14w "world")
==> "world"

>>> (close st14w)
==> T

>>> (setf st14r (open-read (path-join (tmpdir) "test14-stream.tmp")))
==> #<STREAM>

>>> ;;; readall returns "hello\nworld\n", length 12
... (= (length (readall st14r)) 12)
==> T

>>> ;;; after readall, subsequent readLn! returns empty string (EOF)
... (= (readLn! st14r) "")
==> T

>>> (close st14r)
==> T

; ============================================================
; save and load
; ============================================================

>>> ;;; save returns NIL; writes prettyPrintSExpr of each object on its own line
... (save (path-join (tmpdir) "test14-save.tmp") 99)
==> NIL

>>> ;;; load returns (PROGN expr1 expr2 ...)
... (setf loaded14 (load (path-join (tmpdir) "test14-save.tmp")))
==> (PROGN 99)

>>> (first loaded14)
==> PROGN

>>> (length loaded14)
==> 2

>>> (at 1 loaded14)
==> 99

>>> ;;; save multiple objects
... (save (path-join (tmpdir) "test14-save.tmp") 1 2 3)
==> NIL

>>> (setf loaded14 (load (path-join (tmpdir) "test14-save.tmp")))
==> (PROGN 1 2 3)

>>> (length loaded14)
==> 4

>>> ;;; save a quoted list
... (save (path-join (tmpdir) "test14-save.tmp") '(a b c))
==> NIL

>>> (setf loaded14 (load (path-join (tmpdir) "test14-save.tmp")))
==> (PROGN (A B C))

>>> (at 1 loaded14)
==> (A B C)

; ============================================================
; Error cases: arity errors
; ============================================================

>>> (streamp)

%%% ERROR 'STREAMP': 1 argument expected.
%%% USAGE: (STREAMP sexpr)
==>

>>> (open-read)

%%% ERROR 'OPEN-READ': 1 or 2 arguments expected.
%%% USAGE: (OPEN-READ filename &optional encoding)
==>

>>> (open-write)

%%% ERROR 'OPEN-WRITE': 1 or 2 arguments expected.
%%% USAGE: (OPEN-WRITE filename &optional encoding)
==>

>>> (open-append)

%%% ERROR 'OPEN-APPEND': 1 or 2 arguments expected.
%%% USAGE: (OPEN-APPEND filename &optional encoding)
==>

>>> (close)

%%% ERROR 'CLOSE': 1 argument expected.
%%% USAGE: (CLOSE stream)
==>

>>> (readable)

%%% ERROR 'READABLE': 1 argument expected.
%%% USAGE: (READABLE stream)
==>

>>> (writable)

%%% ERROR 'WRITABLE': 1 argument expected.
%%% USAGE: (WRITABLE stream)
==>

>>> (closed)

%%% ERROR 'CLOSED': 1 argument expected.
%%% USAGE: (CLOSED stream)
==>

>>> (isatty)

%%% ERROR 'ISATTY': 1 argument expected.
%%% USAGE: (ISATTY stream)
==>

>>> (save)

%%% ERROR 'SAVE': At least 1 argument expected.
%%% USAGE: (SAVE filename &rest objects)
==>

>>> (load)

%%% ERROR 'LOAD': 1 argument expected.
%%% USAGE: (LOAD fileName)
==>

; ============================================================
; Error cases: type errors
; ============================================================

>>> ;;; non-string filename
... (open-read 42)

%%% ERROR 'OPEN-READ': 1st argument expected to be a filename string.
%%% USAGE: (OPEN-READ filename &optional encoding)
==>

>>> (open-write 42)

%%% ERROR 'OPEN-WRITE': 1st argument expected to be a filename string.
%%% USAGE: (OPEN-WRITE filename &optional encoding)
==>

>>> ;;; file not found
... (open-read "no-such-file-42.tmp")

%%% ERROR 'OPEN-READ': File not found "no-such-file-42.tmp".
%%% USAGE: (OPEN-READ filename &optional encoding)
==>

>>> (close 42)

%%% ERROR 'CLOSE': Argument expected to be a stream.
%%% USAGE: (CLOSE stream)
==>

>>> (readable 42)

%%% ERROR 'READABLE': Argument expected to be a stream.
%%% USAGE: (READABLE stream)
==>

>>> (writable 42)

%%% ERROR 'WRITABLE': Argument expected to be a stream.
%%% USAGE: (WRITABLE stream)
==>

>>> (closed 42)

%%% ERROR 'CLOSED': Argument expected to be a stream.
%%% USAGE: (CLOSED stream)
==>

>>> (isatty 42)

%%% ERROR 'ISATTY': Argument expected to be a stream.
%%% USAGE: (ISATTY stream)
==>

>>> (flush 42)

%%% ERROR 'FLUSH': Argument expected to be a stream.
%%% USAGE: (FLUSH &optional stream)
==>

>>> (save 42)

%%% ERROR 'SAVE': 1st argument expected to be a filename.
%%% USAGE: (SAVE filename &rest objects)
==>

>>> (load 42)

%%% ERROR 'LOAD': Argument expected to be a filename.
%%% USAGE: (LOAD fileName)
==>

>>> (load "no-such-file-43.tmp")

%%% ERROR 'LOAD': File not found "no-such-file-43.tmp".
%%% USAGE: (LOAD fileName)
==>

; ============================================================
; Error cases: write to read-only stream / read from write-only stream
; ============================================================

>>> (setf st14r (open-read (path-join (tmpdir) "test14-stream.tmp")))
==> #<STREAM>

>>> (write! st14r "hello")

%%% ERROR 'WRITE!': Stream is not writable.
%%% USAGE: (WRITE! &optional stream &rest objects)
==>

>>> (writeLn! st14r "hello")

%%% ERROR 'WRITELN!': Stream is not writable.
%%% USAGE: (WRITELN! &optional stream &rest objects)
==>

>>> (uwrite! st14r "hello")

%%% ERROR 'UWRITE!': Stream is not writable.
%%% USAGE: (UWRITE! &optional stream &rest objects)
==>

>>> (uwriteLn! st14r "hello")

%%% ERROR 'UWRITELN!': Stream is not writable.
%%% USAGE: (UWRITELN! &optional stream &rest objects)
==>

>>> (terpri st14r)

%%% ERROR 'TERPRI': Stream is not writable.
%%% USAGE: (TERPRI &optional stream)
==>

>>> (writef "hello" st14r)

%%% ERROR 'WRITEF': Stream is not writable.
%%% USAGE: (WRITEF formatString &optional dictOrList stream)
==>

>>> (close st14r)
==> T

>>> (setf st14w (open-write (path-join (tmpdir) "test14-stream.tmp")))
==> #<STREAM>

>>> (readLn! st14w)

%%% ERROR 'READLN!': Stream is not readable.
%%% USAGE: (READLN! &optional stream)
==>

>>> (close st14w)
==> T

; --- with-open-file ---

>>> ;;; write to a file then read it back
... (with-open-file (f "_wof_test_.txt" output)
...    (uwrite! f "hello from with-open-file")
...    (terpri f))
==> NIL

>>> ;;; read it back: readLn includes trailing newline, length = 26
... (with-open-file (f "_wof_test_.txt")
...    (length (readLn! f)))
==> 26

>>> ;;; content starts with expected prefix
... (with-open-file (f "_wof_test_.txt")
...    (subseq (readLn! f) 0 5))
==> "hello"

>>> ;;; append mode
... (with-open-file (f "_wof_test_.txt" append)
...    (uwrite! f "second line")
...    (terpri f))
==> NIL

>>> ;;; with-open-file returns last body value
... (with-open-file (f "_wof_test_.txt")
...    (readLn! f)
...    42)
==> 42

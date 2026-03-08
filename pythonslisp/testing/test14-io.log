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
%%% PRIMITIVE USAGE: (WRITEF formatString &optional dictOrList stream)
==>

>>> ;;; Error: writef first argument must be a format string
... (writef 1 '(a b))

%%% ERROR 'WRITEF': 1st argument expected to be a format string.
%%% PRIMITIVE USAGE: (WRITEF formatString &optional dictOrList stream)
==>

>>> ;;; Error: writef second argument must be a list or map
... (writef "hello" 1)

%%% ERROR 'WRITEF': 2nd argument expected to be a list, dict or stream.
%%% PRIMITIVE USAGE: (WRITEF formatString &optional dictOrList stream)
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
; open :direction :output, stream predicates, write to stream, close
; ============================================================

>>> (setf st14w (open (path-join (tmpdir) "test14-stream.tmp") :direction :output))
==> #<STREAM>

>>> ;;; streamp: T for an open stream
... (streamp st14w)
==> T

>>> ;;; type-of: FILE-STREAM
... (type-of st14w)
==> FILE-STREAM

>>> (output-stream-p st14w)
==> T

>>> (input-stream-p st14w)
==> NIL

>>> (interactive-stream-p st14w)
==> NIL

>>> (open-stream-p st14w)
==> T

>>> ;;; uwriteLn! to stream: no stdout output, returns last arg
... (uwriteLn! st14w "hi")
==> "hi"

>>> (uwriteLn! st14w "bye")
==> "bye"

>>> (flush st14w)
==> T

>>> (close st14w)
==> T

>>> (open-stream-p st14w)
==> NIL

; ============================================================
; open (default :direction :input), read lines, EOF detection
; ============================================================

>>> (setf st14r (open (path-join (tmpdir) "test14-stream.tmp")))
==> #<STREAM>

>>> (input-stream-p st14r)
==> T

>>> (output-stream-p st14r)
==> NIL

>>> (open-stream-p st14r)
==> T

>>> ;;; read-line returns "hi" (length 2)
... (= (length (read-line st14r)) 2)
==> T

>>> ;;; read-line returns "bye" (length 3)
... (= (length (read-line st14r)) 3)
==> T

>>> ;;; past EOF returns NIL when eof-error-p is NIL
... (= (read-line st14r nil nil) nil)
==> T

>>> (close st14r)
==> T

; ============================================================
; terpri, write!, writeLn!, writef to stream
; ============================================================

>>> (setf st14w (open (path-join (tmpdir) "test14-stream.tmp") :direction :output))
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

>>> (setf st14r (open (path-join (tmpdir) "test14-stream.tmp")))
==> #<STREAM>

>>> ;;; line 1: empty line from terpri (length 0)
... (= (length (read-line st14r)) 0)
==> T

>>> ;;; line 2: 7"ok" without trailing newline (length 5)
... (= (length (read-line st14r)) 5)
==> T

>>> ;;; rest of file: "n=5end" (no trailing newline), length 6
... (= (length (read-line st14r)) 6)
==> T

>>> (= (read-line st14r nil nil) nil)
==> T

>>> (close st14r)
==> T

; ============================================================
; open :if-exists :append — appends to existing file
; ============================================================

>>> ;;; use open :direction :output to create a fresh file
... (setf st14a (open (path-join (tmpdir) "test14-append.tmp") :direction :output))
==> #<STREAM>

>>> (uwriteLn! st14a "first")
==> "first"

>>> (close st14a)
==> T

>>> ;;; :if-exists :append adds to the existing content
... (setf st14a (open (path-join (tmpdir) "test14-append.tmp") :direction :output :if-exists :append))
==> #<STREAM>

>>> (output-stream-p st14a)
==> T

>>> (uwriteLn! st14a "second")
==> "second"

>>> (close st14a)
==> T

>>> (setf st14r (open (path-join (tmpdir) "test14-append.tmp")))
==> #<STREAM>

>>> ;;; read-line returns "first" (length 5)
... (= (length (read-line st14r)) 5)
==> T

>>> ;;; read-line returns "second" (length 6)
... (= (length (read-line st14r)) 6)
==> T

>>> (= (read-line st14r nil nil) nil)
==> T

>>> (close st14r)
==> T

; ============================================================
; readall: reads entire file contents as one string
; ============================================================

>>> (setf st14w (open (path-join (tmpdir) "test14-stream.tmp") :direction :output))
==> #<STREAM>

>>> (uwriteLn! st14w "hello")
==> "hello"

>>> (uwriteLn! st14w "world")
==> "world"

>>> (close st14w)
==> T

>>> (setf st14r (open (path-join (tmpdir) "test14-stream.tmp")))
==> #<STREAM>

>>> ;;; readall returns "hello\nworld\n", length 12
... (= (length (readall st14r)) 12)
==> T

>>> ;;; after readall, subsequent read-line returns NIL (EOF)
... (= (read-line st14r nil nil) nil)
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
%%% PRIMITIVE USAGE: (STREAMP sexpr)
==>

>>> (open)

%%% ERROR 'OPEN': At least 1 argument expected.
%%% PRIMITIVE USAGE: (OPEN filespec &key (direction :input) (if-exists :supersede) (if-does-not-exist :error))
==>

>>> (close)

%%% ERROR 'CLOSE': At least 1 argument expected.
%%% PRIMITIVE USAGE: (CLOSE stream &key (abort nil))
==>

>>> (open-stream-p)

%%% ERROR 'OPEN-STREAM-P': 1 argument expected.
%%% PRIMITIVE USAGE: (OPEN-STREAM-P stream)
==>

>>> (input-stream-p)

%%% ERROR 'INPUT-STREAM-P': 1 argument expected.
%%% PRIMITIVE USAGE: (INPUT-STREAM-P stream)
==>

>>> (output-stream-p)

%%% ERROR 'OUTPUT-STREAM-P': 1 argument expected.
%%% PRIMITIVE USAGE: (OUTPUT-STREAM-P stream)
==>

>>> (interactive-stream-p)

%%% ERROR 'INTERACTIVE-STREAM-P': 1 argument expected.
%%% PRIMITIVE USAGE: (INTERACTIVE-STREAM-P stream)
==>

>>> (save)

%%% ERROR 'SAVE': At least 1 argument expected.
%%% PRIMITIVE USAGE: (SAVE filename &rest objects)
==>

>>> (load)

%%% ERROR 'LOAD': 1 argument expected.
%%% PRIMITIVE USAGE: (LOAD fileName)
==>

; ============================================================
; Error cases: type errors
; ============================================================

>>> ;;; non-string filename
... (open 42)

%%% ERROR 'OPEN': 1st argument expected to be a filename string.
%%% PRIMITIVE USAGE: (OPEN filespec &key (direction :input) (if-exists :supersede) (if-does-not-exist :error))
==>

>>> ;;; file not found
... (open "no-such-file-42.tmp")

%%% ERROR 'OPEN': File not found "no-such-file-42.tmp".
%%% PRIMITIVE USAGE: (OPEN filespec &key (direction :input) (if-exists :supersede) (if-does-not-exist :error))
==>

>>> ;;; bad :direction value
... (open (path-join (tmpdir) "test14-stream.tmp") :direction :bad)

%%% ERROR 'OPEN': :direction must be :input or :output.
%%% PRIMITIVE USAGE: (OPEN filespec &key (direction :input) (if-exists :supersede) (if-does-not-exist :error))
==>

>>> ;;; create a local file for the :if-exists :error test
... (close (open "_wof_test_ifexists_.tmp" :direction :output))
==> T

>>> ;;; :if-exists :error raises an error when file already exists
... (open "_wof_test_ifexists_.tmp" :direction :output :if-exists :error)

%%% ERROR 'OPEN': File already exists "_wof_test_ifexists_.tmp".
%%% PRIMITIVE USAGE: (OPEN filespec &key (direction :input) (if-exists :supersede) (if-does-not-exist :error))
==>


>>> ;;; :if-exists nil returns nil when file exists
... (open (path-join (tmpdir) "test14-stream.tmp") :direction :output :if-exists nil)
==> NIL

>>> ;;; :if-does-not-exist nil returns nil when file missing
... (open "no-such-file-42.tmp" :if-does-not-exist nil)
==> NIL

>>> (close 42)

%%% ERROR 'CLOSE': Argument expected to be a stream.
%%% PRIMITIVE USAGE: (CLOSE stream &key (abort nil))
==>

>>> (open-stream-p 42)

%%% ERROR 'OPEN-STREAM-P': Argument expected to be a stream.
%%% PRIMITIVE USAGE: (OPEN-STREAM-P stream)
==>

>>> (input-stream-p 42)

%%% ERROR 'INPUT-STREAM-P': Argument expected to be a stream.
%%% PRIMITIVE USAGE: (INPUT-STREAM-P stream)
==>

>>> (output-stream-p 42)

%%% ERROR 'OUTPUT-STREAM-P': Argument expected to be a stream.
%%% PRIMITIVE USAGE: (OUTPUT-STREAM-P stream)
==>

>>> (interactive-stream-p 42)

%%% ERROR 'INTERACTIVE-STREAM-P': Argument expected to be a stream.
%%% PRIMITIVE USAGE: (INTERACTIVE-STREAM-P stream)
==>

>>> (flush 42)

%%% ERROR 'FLUSH': Argument expected to be a stream.
%%% PRIMITIVE USAGE: (FLUSH &optional stream)
==>

>>> (save 42)

%%% ERROR 'SAVE': 1st argument expected to be a filename.
%%% PRIMITIVE USAGE: (SAVE filename &rest objects)
==>

>>> (load 42)

%%% ERROR 'LOAD': Argument expected to be a filename.
%%% PRIMITIVE USAGE: (LOAD fileName)
==>

>>> (load "no-such-file-43.tmp")

%%% ERROR 'LOAD': File not found "no-such-file-43.tmp".
%%% PRIMITIVE USAGE: (LOAD fileName)
==>

; ============================================================
; Error cases: write to read-only stream / read from write-only stream
; ============================================================

>>> (setf st14r (open (path-join (tmpdir) "test14-stream.tmp")))
==> #<STREAM>

>>> (write! st14r "hello")

%%% ERROR 'WRITE!': Stream is not writable.
%%% PRIMITIVE USAGE: (WRITE! &optional stream &rest objects)
==>

>>> (writeLn! st14r "hello")

%%% ERROR 'WRITELN!': Stream is not writable.
%%% PRIMITIVE USAGE: (WRITELN! &optional stream &rest objects)
==>

>>> (uwrite! st14r "hello")

%%% ERROR 'UWRITE!': Stream is not writable.
%%% PRIMITIVE USAGE: (UWRITE! &optional stream &rest objects)
==>

>>> (uwriteLn! st14r "hello")

%%% ERROR 'UWRITELN!': Stream is not writable.
%%% PRIMITIVE USAGE: (UWRITELN! &optional stream &rest objects)
==>

>>> (terpri st14r)

%%% ERROR 'TERPRI': Stream is not writable.
%%% PRIMITIVE USAGE: (TERPRI &optional stream)
==>

>>> (writef "hello" st14r)

%%% ERROR 'WRITEF': Stream is not writable.
%%% PRIMITIVE USAGE: (WRITEF formatString &optional dictOrList stream)
==>

>>> (close st14r)
==> T

>>> (setf st14w (open (path-join (tmpdir) "test14-stream.tmp") :direction :output))
==> #<STREAM>

>>> (read-line st14w)

%%% ERROR 'READ-LINE': Stream is not readable.
%%% PRIMITIVE USAGE: (READ-LINE &optional stream (eof-error-p t) eof-value recursive-p)
==>

>>> (close st14w)
==> T

; --- with-open-file ---

>>> ;;; write to a file then read it back
... (with-open-file (f "_wof_test_.txt" :direction :output)
...    (uwrite! f "hello from with-open-file")
...    (terpri f))
==> NIL

>>> ;;; read it back: read-line strips trailing newline, length = 25
... (with-open-file (f "_wof_test_.txt")
...    (length (read-line f)))
==> 25

>>> ;;; content starts with expected prefix
... (with-open-file (f "_wof_test_.txt")
...    (subseq (read-line f) 0 5))
==> "hello"

>>> ;;; append mode
... (with-open-file (f "_wof_test_.txt" :direction :output :if-exists :append)
...    (uwrite! f "second line")
...    (terpri f))
==> NIL

>>> ;;; with-open-file returns last body value
... (with-open-file (f "_wof_test_.txt")
...    (read-line f)
...    42)
==> 42

; ============================================================
; make-string-output-stream / get-output-stream-string — happy paths
; ============================================================

>>> ;;; make-string-output-stream returns a stream object
... (setf ss14 (make-string-output-stream))
==> #<STREAM>

>>> ;;; streamp: T for a string stream
... (streamp ss14)
==> T

>>> ;;; type-of: STRING-STREAM
... (type-of ss14)
==> STRING-STREAM

>>> ;;; string streams are writable
... (output-stream-p ss14)
==> T

>>> ;;; string streams are also readable (StringIO supports both)
... (input-stream-p ss14)
==> T

>>> ;;; string streams are not ttys
... (interactive-stream-p ss14)
==> NIL

>>> ;;; not closed initially
... (open-stream-p ss14)
==> T

>>> ;;; flush is a no-op on a string stream, returns T
... (flush ss14)
==> T

>>> ;;; get-output-stream-string on a fresh empty stream returns ""
... (get-output-stream-string ss14)
==> ""

>>> ;;; uwrite! to string stream: no stdout output, returns last value
... (uwrite! ss14 "hello")
==> "hello"

>>> ;;; accumulate more content
... (uwrite! ss14 " world")
==> " world"

>>> ;;; get-output-stream-string retrieves full accumulated content
... (get-output-stream-string ss14)
==> "hello world"

>>> ;;; CL clear semantics: buffer is empty after retrieval
... (get-output-stream-string ss14)
==> ""

>>> ;;; write after clearing accumulates again
... (uwrite! ss14 "second")
==> "second"

>>> (get-output-stream-string ss14)
==> "second"

>>> ;;; write! uses programmer format — strings get extra surrounding quotes
... (write! ss14 "quoted")
==> "quoted"

>>> ;;; "quoted" = 8 chars including the surrounding quote characters
... (= (length (get-output-stream-string ss14)) 8)
==> T

>>> ;;; uwriteLn! adds a trailing newline: "line\n" = 5 chars
... (uwriteLn! ss14 "line")
==> "line"

>>> (= (length (get-output-stream-string ss14)) 5)
==> T

>>> ;;; writeLn! programmer format with newline: '"line"\n' = 7 chars
... (writeLn! ss14 "line")
==> "line"

>>> (= (length (get-output-stream-string ss14)) 7)
==> T

>>> ;;; terpri writes a single newline, returns NIL, no stdout output
... (terpri ss14)
==> NIL

>>> (= (length (get-output-stream-string ss14)) 1)
==> T

>>> ;;; writef 3-arg: format + list + string stream
... (writef "{0}+{1}" (list 1 2) ss14)
==> "1+2"

>>> (get-output-stream-string ss14)
==> "1+2"

>>> ;;; uwrite! with multiple args: returns last, all written
... (uwrite! ss14 1 2 3)
==> 3

>>> (get-output-stream-string ss14)
==> "123"

>>> ;;; uwrite! with numbers and symbols
... (uwrite! ss14 'foo)
==> FOO

>>> (get-output-stream-string ss14)
==> "FOO"

>>> ;;; close returns T, open-stream-p then returns NIL
... (close ss14)
==> T

>>> (open-stream-p ss14)
==> NIL

>>> ;;; fresh one-liner: open, write, retrieve, all in one expression
... (let ((s (make-string-output-stream)))
...    (uwrite! s "alpha")
...    (uwrite! s "beta")
...    (get-output-stream-string s))
==> "alphabeta"

>>> ;;; CL clear in a single let: two gets return first/second half
... (let ((s (make-string-output-stream)))
...    (uwrite! s "first")
...    (get-output-stream-string s)
...    (uwrite! s "second")
...    (get-output-stream-string s))
==> "second"

; ============================================================
; make-string-output-stream / get-output-stream-string — error paths
; ============================================================

>>> ;;; make-string-output-stream takes no arguments
... (make-string-output-stream 42)

%%% ERROR 'MAKE-STRING-OUTPUT-STREAM': 0 arguments expected.
%%% PRIMITIVE USAGE: (MAKE-STRING-OUTPUT-STREAM)
==>

>>> ;;; get-output-stream-string requires exactly 1 argument
... (get-output-stream-string)

%%% ERROR 'GET-OUTPUT-STREAM-STRING': 1 argument expected.
%%% PRIMITIVE USAGE: (GET-OUTPUT-STREAM-STRING string-stream)
==>

>>> (get-output-stream-string (make-string-output-stream) (make-string-output-stream))

%%% ERROR 'GET-OUTPUT-STREAM-STRING': 1 argument expected.
%%% PRIMITIVE USAGE: (GET-OUTPUT-STREAM-STRING string-stream)
==>

>>> ;;; get-output-stream-string rejects a non-string-stream value
... (get-output-stream-string 42)

%%% ERROR 'GET-OUTPUT-STREAM-STRING': Argument must be a string stream (created by make-string-output-stream).
%%% PRIMITIVE USAGE: (GET-OUTPUT-STREAM-STRING string-stream)
==>

>>> ;;; get-output-stream-string rejects a file stream
... (setf ss14f (open (path-join (tmpdir) "test14-stream.tmp") :direction :output))
==> #<STREAM>

>>> (get-output-stream-string ss14f)

%%% ERROR 'GET-OUTPUT-STREAM-STRING': Argument must be a string stream (created by make-string-output-stream).
%%% PRIMITIVE USAGE: (GET-OUTPUT-STREAM-STRING string-stream)
==>

>>> (close ss14f)
==> T

>>> ;;; get-output-stream-string on a closed string stream is an error
... (setf ss14c (make-string-output-stream))
==> #<STREAM>

>>> (close ss14c)
==> T

>>> (get-output-stream-string ss14c)

%%% ERROR 'GET-OUTPUT-STREAM-STRING': String stream is closed.
%%% PRIMITIVE USAGE: (GET-OUTPUT-STREAM-STRING string-stream)
==>

; ============================================================
; make-string-input-stream — happy paths
; ============================================================

>>> ;;; make-string-input-stream returns a readable stream
... (setf si14 (make-string-input-stream "hello world"))
==> #<STREAM>

>>> (streamp si14)
==> T

>>> (input-stream-p si14)
==> T

>>> ;;; read-line on a string with no actual newlines returns full content (length 11)
... (= (length (read-line si14)) 11)
==> T

>>> ;;; past end returns NIL when eof-error-p is NIL
... (= (read-line si14 nil nil) nil)
==> T

>>> (close si14)
==> T

>>> ;;; read-line with actual newlines: read line by line, no trailing newlines
... (let* ((str (with-output-to-string (s) (uwrite! s "hello") (terpri s) (uwrite! s "world") (terpri s)))
...        (si  (make-string-input-stream str)))
...    (list (= (length (read-line si)) 5)
...          (= (length (read-line si)) 5)
...          (= (read-line si nil nil) nil)))
==> (T T T)

>>> ;;; readall reads entire content in one call
... (readall (make-string-input-stream "abc"))
==> "abc"

>>> ;;; start/end optional args constrain the substring used
... (readall (make-string-input-stream "hello world" 6))
==> "world"

>>> (readall (make-string-input-stream "hello world" 0 5))
==> "hello"

; ============================================================
; make-string-input-stream — error paths
; ============================================================

>>> ;;; arity: requires at least 1 argument
... (make-string-input-stream)

%%% ERROR 'MAKE-STRING-INPUT-STREAM': 1 to 3 arguments expected.
%%% PRIMITIVE USAGE: (MAKE-STRING-INPUT-STREAM string &optional (start 0) end)
==>

>>> ;;; too many arguments
... (make-string-input-stream "a" 0 1 2)

%%% ERROR 'MAKE-STRING-INPUT-STREAM': 1 to 3 arguments expected.
%%% PRIMITIVE USAGE: (MAKE-STRING-INPUT-STREAM string &optional (start 0) end)
==>

>>> ;;; non-string argument
... (make-string-input-stream 42)

%%% ERROR 'MAKE-STRING-INPUT-STREAM': 1st argument expected to be a string.
%%% PRIMITIVE USAGE: (MAKE-STRING-INPUT-STREAM string &optional (start 0) end)
==>

; ============================================================
; with-output-to-string
; ============================================================

>>> ;;; basic accumulation
... (with-output-to-string (s)
...    (uwrite! s "hello")
...    (uwrite! s " world"))
==> "hello world"

>>> ;;; terpri adds newline
... (= (length (with-output-to-string (s) (terpri s))) 1)
==> T

>>> ;;; write! uses programmer format: "quoted" with surrounding quotes is 8 chars
... (= (length (with-output-to-string (s) (write! s "quoted"))) 8)
==> T

; ============================================================
; close with :abort keyword (CL compatibility)
; ============================================================

>>> ;;; close with :abort nil (default) — same as plain close
... (let ((s (make-string-output-stream)))
...    (close s :abort nil)
...    (open-stream-p s))
==> NIL

>>> ;;; close with :abort t — accepted, ignored; stream still closed
... (let ((s (make-string-output-stream)))
...    (close s :abort t)
...    (open-stream-p s))
==> NIL

; ============================================================
; backward-compatible aliases: readable, writable, isatty, closed
; ============================================================

>>> ;;; readable is alias for input-stream-p
... (let ((s (open (path-join (tmpdir) "test14-stream.tmp"))))
...    (let ((r (readable s)))
...       (close s)
...       r))
==> T

>>> ;;; writable is alias for output-stream-p
... (let ((s (open (path-join (tmpdir) "test14-stream.tmp") :direction :output)))
...    (let ((r (writable s)))
...       (close s)
...       r))
==> T

>>> ;;; isatty is alias for interactive-stream-p
... (isatty (make-string-output-stream))
==> NIL

>>> ;;; closed is defun: (not (open-stream-p s))
... (let ((s (make-string-output-stream)))
...    (list (closed s)
...          (progn (close s) (closed s))))
==> (NIL T)

; ============================================================
; standard stream variables
; ============================================================

>>> ;;; *standard-input* is a stream
... (streamp *standard-input*)
==> T

>>> (streamp *standard-output*)
==> T

>>> (streamp *error-output*)
==> T

>>> (streamp *terminal-io*)
==> T

>>> (streamp *debug-io*)
==> T

>>> (streamp *query-io*)
==> T

>>> (streamp *trace-output*)
==> T

>>> ;;; *standard-output* is writable
... (output-stream-p *standard-output*)
==> T

>>> ;;; *standard-input* is readable
... (input-stream-p *standard-input*)
==> T

; ============================================================
; with-input-from-string
; ============================================================

>>> ;;; basic: read from a string
... (with-input-from-string (s "hello world")
...    (read-line s))
==> "hello world"

>>> ;;; with start offset
... (with-input-from-string (s "hello world" 6)
...    (read-line s))
==> "world"

>>> ;;; with start and end
... (with-input-from-string (s "hello world" 0 5)
...    (read-line s))
==> "hello"

>>> ;;; returns last body form
... (with-input-from-string (s "hello")
...    (read-line s)
...    42)
==> 42

>>> ;;; stream is closed after body
... (let ((captured nil))
...    (with-input-from-string (s "test")
...       (setf captured s))
...    (open-stream-p captured))
==> NIL

; ============================================================
; read-line — CL-compatible: no trailing newline
; ============================================================

>>> ;;; read-line from a string stream returns line without trailing newline
... (let ((str (with-output-to-string (s) (uwrite! s "hello") (terpri s) (uwrite! s "world") (terpri s))))
...    (let ((st (make-string-input-stream str)))
...       (list (read-line st) (read-line st))))
==> ("hello" "world")

>>> ;;; read-line nil eof: returns NIL at end of file when eof-error-p is NIL
... (let ((s (make-string-input-stream "hi")))
...    (read-line s)
...    (read-line s nil nil))
==> NIL

>>> ;;; read-line nil custom eof-value: returns given value at EOF
... (let ((s (make-string-input-stream "x")))
...    (read-line s)
...    (read-line s nil :eof))
==> :EOF

>>> ;;; read-line at EOF with eof-error-p T (default) signals error
... (read-line (make-string-input-stream ""))
%%% read-line: end of file.
==>

>>> ;;; read-line strips exactly one trailing newline: result has correct length
... (let ((str (with-output-to-string (s) (uwrite! s "abc") (terpri s))))
...    (let ((st (make-string-input-stream str)))
...       (length (read-line st nil nil))))
==> 3

>>> ;;; uwriteLn! adds a newline: result is 6 chars ("hello" + newline)
... (let ((out (make-string-output-stream)))
...    (uwriteLn! out "hello")
...    (= (length (get-output-stream-string out)) 6))
==> T

>>> ;;; read-line: stream argument type error
... (read-line 42)
%%% ERROR 'READ-LINE': Argument 1 must be a stream.
%%% PRIMITIVE USAGE: (READ-LINE &optional stream (eof-error-p t) eof-value recursive-p)
==>

; ============================================================
; read-char — reads one character at a time
; ============================================================

>>> ;;; read-char returns a one-character string
... (let ((s (make-string-input-stream "abc")))
...    (list (read-char s) (read-char s) (read-char s)))
==> ("a" "b" "c")

>>> ;;; read-char at EOF with eof-error-p NIL returns eof-value
... (let ((s (make-string-input-stream "")))
...    (read-char s nil :done))
==> :DONE

>>> ;;; read-char at EOF with eof-error-p T signals error
... (read-char (make-string-input-stream ""))
%%% read-char: end of file.
==>

>>> ;;; read-char: stream argument type error
... (read-char 42)
%%% ERROR 'READ-CHAR': Argument 1 must be a stream.
%%% PRIMITIVE USAGE: (READ-CHAR &optional stream (eof-error-p t) eof-value recursive-p)
==>

; ============================================================
; read — reads one s-expression from a stream
; ============================================================

>>> ;;; read parses a symbol
... (let ((s (make-string-input-stream "foo")))
...    (read s))
==> FOO

>>> ;;; read parses a number
... (let ((s (make-string-input-stream "42")))
...    (read s))
==> 42

>>> ;;; read parses a list
... (let ((s (make-string-input-stream "(a b c)")))
...    (read s))
==> (A B C)

>>> ;;; read repositions stream: second read sees next expression
... (let ((s (make-string-input-stream "42 hello")))
...    (list (read s) (read s)))
==> (42 HELLO)

>>> ;;; read handles leading whitespace correctly
... (let ((s (make-string-input-stream "   99")))
...    (read s))
==> 99

>>> ;;; read returns nested list correctly
... (let ((s (make-string-input-stream "(1 (2 3) 4)")))
...    (read s))
==> (1 (2 3) 4)

>>> ;;; read at EOF with eof-error-p NIL returns eof-value
... (let ((s (make-string-input-stream "")))
...    (read s nil :done))
==> :DONE

>>> ;;; read at EOF with eof-error-p T signals error
... (read (make-string-input-stream ""))
%%% read: end of file.
==>

>>> ;;; read: stream argument type error
... (read 42)
%%% ERROR 'READ': Argument 1 must be a stream.
%%% PRIMITIVE USAGE: (READ &optional stream (eof-error-p t) eof-value recursive-p)
==>

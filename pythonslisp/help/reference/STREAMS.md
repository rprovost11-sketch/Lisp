# Streams Quick Reference

## Opening Streams

| Expression | Meaning |
|---|---|
| `(with-open-file (v "f") body...)` | Open file, bind to v, close on exit |
| `(with-output-to-string (v) body...)` | Capture writes to v as a string |
| `(with-input-from-string (v str) body...)` | Read from string via stream v |
| `(open "f")` | Open for reading |
| `(open "f" :direction :output)` | Open for writing |
| `(open "f" :direction :output :if-exists :append)` | Append |
| `(open "f" :if-does-not-exist nil)` | Return NIL if not found |
| `(close stream)` | Close a stream |

## Reading

| Expression | Meaning |
|---|---|
| `(read-line stream nil nil)` | Read line; NIL at EOF |
| `(read-char stream nil nil)` | Read one character; NIL at EOF |
| `(read stream nil nil)` | Read one s-expression (unevaluated); NIL at EOF |
| `(readall stream)` | Read entire remaining contents as string |

## Writing

| Expression | Meaning |
|---|---|
| `(write! stream val...)` | Programmer-readable output (strings quoted) |
| `(write-line stream val...)` | Same with trailing newline |
| `(uwrite! stream val...)` | User-readable output (strings unquoted) |
| `(uwrite-line stream val...)` | Same with trailing newline |
| `(writef fmt list stream)` | Formatted string output |
| `(terpri stream)` | Write a newline |
| `(flush stream)` | Flush buffered output |

## Predicates

| Expression | Meaning |
|---|---|
| `(open-stream-p stream)` | T if open |
| `(input-stream-p stream)` | T if readable |
| `(output-stream-p stream)` | T if writable |
| `(interactive-stream-p stream)` | T if connected to a terminal |

## Standard Stream Variables

`*standard-input*`  `*standard-output*`  `*error-output*`  `*terminal-io*`

See `(help "streams-doc")` for full documentation and examples.

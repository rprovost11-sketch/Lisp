# Strings Quick Reference

## Building Strings

| Expression | Meaning |
|---|---|
| `(ustring v1 v2 ...)` | User-readable concatenation (strings unquoted) |
| `(string v1 v2 ...)` | Programmer-readable concatenation (strings quoted) |
| `(writef "{0} and {1}" (list a b))` | Python str.format() style |

## Case and Whitespace

| Expression | Meaning |
|---|---|
| `(string-upcase s)` | ALL CAPS copy |
| `(string-downcase s)` | all lower copy |
| `(string-capitalize s)` | Title Case copy (CL word-boundary rules) |
| `(string-capitalize s :start n :end m)` | Capitalize only region [n, m) |
| `(string-trim bag s)` | Strip chars-in-bag from both ends |
| `(string-left-trim bag s)` | Strip from left only |
| `(string-right-trim bag s)` | Strip from right only |

## Characters and Subsequences

| Expression | Meaning |
|---|---|
| `(char-code c)` | Unicode code point of single-character string |
| `(code-char n)` | Single-character string from code point |
| `(char s i)` | Character string at index i |
| `(length s)` | String length |
| `(subseq s start end)` | Substring [start, end) |
| `(equal s1 s2)` | String equality (case-sensitive) |

## Parsing

| Expression | Meaning |
|---|---|
| `(integer s)` | Parse string as integer |
| `(integer s 16)` | Parse hex string |
| `(float s)` | Parse string as float |

See `(help "strings-doc")` for full documentation and examples.

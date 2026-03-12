# Looping Quick Reference

## Loop Macros

| Form | Use |
|---|---|
| `(while cond body...)` | Repeat while cond is non-NIL |
| `(dotimes (i n) body...)` | i from 0 to n-1 |
| `(dotimes (i n result) body...)` | As above; return result after loop |
| `(dolist (x lst) body...)` | x bound to each element of lst |
| `(dolist (x lst result) body...)` | As above; return result after loop |
| `(for (var init) cond next body...)` | C-style loop |
| `(return val)` | Early exit from any loop macro |

## Higher-Order Functions

| Expression | Meaning |
|---|---|
| `(mapcar fn lst...)` | Transform each element; return new list |
| `(mapc fn lst...)` | Side-effect map; return first list |
| `(reduce fn lst)` | Fold list to single value (left to right) |
| `(remove-if pred lst)` | List with matching elements removed |
| `(remove-if-not pred lst)` | List with only matching elements kept |
| `(every pred lst...)` | T if pred holds for all elements |
| `(some pred lst...)` | First truthy pred result, or NIL |

## TCO Tail Recursion Pattern

```lisp
(defun sum-list (lst &optional (acc 0))
  (if (null lst)
      acc
      (sum-list (cdr lst) (+ acc (car lst)))))
```

Recursive call must be in **tail position** (last thing before returning).

See `(help "looping-doc")` for full documentation and examples.

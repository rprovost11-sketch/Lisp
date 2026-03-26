# Sequences

*Quick reference: `(help "sequences")` - Full documentation: this file.*


Python's Lisp sequences are lists and strings.  This document covers the
full library of functions for creating, accessing, searching, filtering,
transforming, and sorting lists.  String-specific operations are in STRINGS.
See also LOOPING for iteration macros.

---

## Building and Combining Lists

```lisp
(list 1 2 3)              ;==> (1 2 3)
(cons 0 '(1 2 3))         ;==> (0 1 2 3)  -- prepend
(append '(1 2) '(3 4))    ;==> (1 2 3 4)  -- new list
(append '(1 2) '(3 4) '(5)) ;==> (1 2 3 4 5)  -- variadic
(reverse '(1 2 3))        ;==> (3 2 1)
```

---

## Basic Access

```lisp
(car '(1 2 3))        ;==> 1            ; first element
(cdr '(1 2 3))        ;==> (2 3)        ; rest of list
(at 0 '(10 20 30))    ;==> 10           ; by index
(at 2 '(10 20 30))    ;==> 30
(length '(a b c))     ;==> 3
(subseq '(a b c d) 1 3)  ;==> (B C)    ; from 1 inclusive to 3 exclusive
(subseq '(a b c d) 2)    ;==> (C D)    ; from 2 to end
```

Named position accessors (macros):

```lisp
(first  '(a b c d e))   ;==> A
(second '(a b c d e))   ;==> B
(third  '(a b c d e))   ;==> C
; fourth ... tenth also defined
```

---

## Mutation

```lisp
(setf lst '(10 20 30))
(push! lst 40)         ; append to back → (10 20 30 40)
(pop! lst)             ;==> 40         ; remove from back
(at-set 1 lst 99)      ; set index 1 → (10 99 30)
(at-delete 1 lst)      ; remove index 1 → (10 30)
(at-insert 1 lst 55)   ; insert 55 at index 1 → (10 55 30)
```

---

## Searching

### member - find a tail by value

```lisp
(member 3 '(1 2 3 4 5))       ;==> (3 4 5)   -- tail at first match
(member 9 '(1 2 3))            ;==> NIL
(member "b" '("a" "b" "c") :test equal)   ;==> ("b" "c")
```

### find and find-if - return the matching element

```lisp
(find 3 '(1 2 3 4))                ;==> 3
(find 9 '(1 2 3))                   ;==> NIL
(find-if 'evenp '(1 3 4 5 6))      ;==> 4
(find-if 'evenp '(1 3 5))           ;==> NIL

; Keywords: :from-end, :start, :end, :key
(find 3 '(1 2 3 4 3) :from-end t)  ;==> 3  (rightmost)
(find-if 'plusp '(-1 -2 3 4) :start 2)  ;==> 3
```

### assoc - look up a key in an association list

```lisp
(assoc 'b '((a 1) (b 2) (c 3)))   ;==> (B 2)
(assoc 'z '((a 1) (b 2)))          ;==> NIL

; Custom test
(assoc "b" '(("a" 1) ("b" 2)) :test equal)  ;==> ("b" 2)
```

### position and position-if - return the index

```lisp
(position 3 '(1 2 3 4))            ;==> 2
(position 9 '(1 2 3))               ;==> NIL
(position-if 'evenp '(1 3 4 6))    ;==> 2
(position 3 '(1 3 2 3) :from-end t)  ;==> 3
```

---

## Counting

```lisp
(count 3 '(1 3 2 3 3))             ;==> 3
(count-if 'oddp '(1 2 3 4 5))      ;==> 3
(count-if 'evenp '(1 2 3 4) :start 1 :end 3)  ;==> 1
```

---

## Filtering

### remove - remove matching elements

```lisp
(remove 3 '(1 3 2 3 4))            ;==> (1 2 4)
(remove 3 '(1 3 2 3 4) :count 1)   ;==> (1 2 3 4)  -- only one
```

### remove-if and remove-if-not

```lisp
(remove-if 'oddp '(1 2 3 4 5 6))      ;==> (2 4 6)
(remove-if-not 'oddp '(1 2 3 4 5 6))  ;==> (1 3 5)

; :from-end with :count removes from the right
(remove-if 'oddp '(1 2 3 4 5) :count 1 :from-end t)
;==> (1 2 3 4)
```

---

## Substituting Elements

```lisp
(substitute 0 3 '(1 3 2 3 4))         ;==> (1 0 2 0 4)
(substitute 0 3 '(1 3 2 3 4) :count 1);==> (1 0 2 3 4)
(substitute-if 0 'oddp '(1 2 3 4 5))  ;==> (0 2 0 4 0)
```

---

## Sorting

```lisp
(sort '(3 1 4 1 5 9 2 6) '<)          ;==> (1 1 2 3 4 5 6 9)
(sort '(3 1 4 1 5) '>)                 ;==> (5 4 3 1 1)
(sort '("banana" "apple" "cherry") 'string<)
;==> ("apple" "banana" "cherry")

; Sort by a key
(sort '((3 "c") (1 "a") (2 "b"))
      '<
      :key 'car)
;==> ((1 "a") (2 "b") (3 "c"))
```

`sort` returns a new list; the original is unmodified.

---

## Mapping and Folding

### mapcar - transform each element

```lisp
(mapcar '1+ '(1 2 3 4))            ;==> (2 3 4 5)
(mapcar (lambda (x) (* x x)) '(1 2 3 4 5))
;==> (1 4 9 16 25)

; Multiple lists - stops at shortest
(mapcar '+ '(1 2 3) '(10 20 30))   ;==> (11 22 33)
```

### mapc - side-effect version of mapcar

```lisp
(mapc 'print '(a b c))
; prints: A B C
;==> (A B C)  -- returns first list, not results
```

### reduce - fold to a single value

```lisp
(reduce '+ '(1 2 3 4 5))      ;==> 15
(reduce '* '(1 2 3 4 5))      ;==> 120
(reduce 'max '(3 1 4 1 5 9))  ;==> 9
(reduce 'cons '(1 2 3) :initial-value nil)
;==> (1 2 3)  -- note: reduce doesn't natively take :initial-value;
             ;    use a wrapper function
```

### every and some

```lisp
(every 'oddp '(1 3 5))    ;==> T
(every 'oddp '(1 2 5))    ;==> NIL
(some 'evenp '(1 3 4 5))  ;==> T    -- returns the truthy value
(some 'evenp '(1 3 5))    ;==> NIL

; Multi-sequence forms
(every '< '(1 2 3) '(4 5 6))   ;==> T
```

---

## Dicts as Sequences

Dicts (maps) support some sequence operations:

```lisp
(setf d (map (a 1) (b 2) (c 3)))
(length d)           ;==> 3
(has-key-p 'b d)       ;==> T
(has-value-p 2 d)      ;==> T
(at 'b d)            ;==> 2
(at-set 'b d 99)     ; mutates d
(at-delete 'a d)     ; mutates d, returns T
(update! d (map (x 10)))  ; merges into d
```

---

## Quick Reference

| Expression | Meaning |
|---|---|
| `(car lst)` / `(cdr lst)` | First element / rest |
| `(cons x lst)` | Prepend x |
| `(append l1 l2 ...)` | Concatenate lists |
| `(length seq)` | Number of elements |
| `(at i lst)` | Element at index i |
| `(subseq lst start end)` | Subsequence |
| `(reverse lst)` | Reversed copy |
| `(push! lst x)` | Append x to list (mutate) |
| `(pop! lst)` | Remove and return last element |
| `(member x lst)` | Tail starting at x, or NIL |
| `(assoc key alist)` | First pair with matching car |
| `(find x lst)` | First matching element |
| `(find-if pred lst)` | First element satisfying pred |
| `(position x lst)` | Index of first match |
| `(count x lst)` | Number of occurrences |
| `(remove x lst)` | Copy without x |
| `(remove-if pred lst)` | Copy without matching elements |
| `(remove-if-not pred lst)` | Keep only matching elements |
| `(substitute new old lst)` | Replace occurrences |
| `(sort lst pred)` | Sorted copy |
| `(mapcar fn lst...)` | Transform each element |
| `(mapc fn lst...)` | Side-effect map |
| `(reduce fn lst)` | Fold to single value |
| `(every pred lst...)` | T if pred holds for all |
| `(some pred lst...)` | First truthy pred result |

---

*See `(help "sequences")` for the condensed quick reference.*

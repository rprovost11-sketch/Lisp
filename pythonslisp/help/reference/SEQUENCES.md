# Sequences Quick Reference

## Building and Access

| Expression | Meaning |
|---|---|
| `(list v...)` | Build a list |
| `(cons x lst)` | Prepend x to list |
| `(append l1 l2 ...)` | Concatenate lists |
| `(reverse lst)` | Reversed copy |
| `(car lst)` / `(cdr lst)` | First element / rest |
| `(at i lst)` | Element at index i |
| `(subseq lst start end)` | Subsequence [start, end) |
| `(length seq)` | Number of elements |
| `first` `second` ... `tenth` | Named position accessors |

## Mutation

| Expression | Meaning |
|---|---|
| `(push! lst x)` | Append x to list (mutates) |
| `(pop! lst)` | Remove and return last element |
| `(at-set i lst val)` | Set element at index (mutates) |
| `(at-delete i lst)` | Remove element at index (mutates) |
| `(at-insert i lst x)` | Insert x at index (mutates) |

## Searching

| Expression | Meaning |
|---|---|
| `(member x lst)` | Tail starting at first match, or NIL |
| `(assoc key alist)` | First pair with matching car |
| `(find x lst)` | First matching element |
| `(find-if pred lst)` | First element satisfying pred |
| `(position x lst)` | Index of first match |
| `(position-if pred lst)` | Index of first element satisfying pred |
| `(count x lst)` | Number of occurrences |
| `(count-if pred lst)` | Count of elements satisfying pred |

## Filtering and Transforming

| Expression | Meaning |
|---|---|
| `(remove x lst)` | Copy without x |
| `(remove-if pred lst)` | Copy with matching elements removed |
| `(remove-if-not pred lst)` | Keep only matching elements |
| `(substitute new old lst)` | Replace occurrences of old with new |
| `(substitute-if new pred lst)` | Replace where pred is true |
| `(sort lst pred)` | Sorted copy; `pred` is less-than test |
| `(sort lst pred :key fn)` | Sort by key function |

## Mapping and Folding

| Expression | Meaning |
|---|---|
| `(mapcar fn lst...)` | Transform each element; return new list |
| `(mapc fn lst...)` | Side-effect map; return first list |
| `(reduce fn lst)` | Fold left to single value |
| `(every pred lst...)` | T if pred holds for all |
| `(some pred lst...)` | First truthy result, or NIL |

All searching/filtering functions accept `:test`, `:key`, `:from-end`, `:start`, `:end`, `:count` keywords where applicable.

See `(help "sequences-doc")` for full documentation and examples.

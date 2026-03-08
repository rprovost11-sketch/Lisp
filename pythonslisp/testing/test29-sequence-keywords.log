; test29 — CL sequence functions with keyword arguments
; Covers member, assoc, find, find-if, position, position-if,
;         count, count-if, remove, remove-if, remove-if-not,
;         substitute, substitute-if

; ── member ────────────────────────────────────────────────────────────────────

; basic: symbols use eql (same as old = for symbols)
>>> (member 'b '(a b c))
...

==> (B C)

>>> (member 2 '(1 2 3 4))
...

==> (2 3 4)

>>> (member 9 '(1 2 3))
...

==> NIL

; eql default: strings are NOT eql unless same object
>>> (member "hi" '("hi" "there"))
...

==> NIL

; :test equal allows string matching
>>> (member "hi" '("hi" "there") :test equal)
...

==> ("hi" "there")

; :key extracts before comparing
>>> (member 2 '((1 a) (2 b) (3 c)) :key car)
...

==> ((2 B) (3 C))

; ── assoc ─────────────────────────────────────────────────────────────────────

>>> (assoc 'b '((a 1) (b 2) (c 3)))
...

==> (B 2)

>>> (assoc 'z '((a 1) (b 2)))
...

==> NIL

; :test equal for string keys
>>> (assoc "b" '(("a" 1) ("b" 2) ("c" 3)) :test equal)
...

==> ("b" 2)

; :key applied to each pair's key (keys are lists so car works)
>>> (assoc 2 '(((0 1) a) ((2 3) b) ((4 5) c)) :key car)
...

==> ((2 3) B)

; ── find ──────────────────────────────────────────────────────────────────────

>>> (find 3 '(1 2 3 4 5))
...

==> 3

>>> (find 9 '(1 2 3))
...

==> NIL

; :from-end — rightmost match
>>> (find 2 '(1 2 3 2 1) :from-end t)
...

==> 2

; :start and :end bound the search
>>> (find 1 '(1 2 3 1) :start 1)
...

==> 1

>>> (find 3 '(1 2 3 4) :end 2)
...

==> NIL

; :key
>>> (find 3 '((1 a) (2 b) (3 c)) :key car)
...

==> (3 C)

; ── find-if ───────────────────────────────────────────────────────────────────

>>> (find-if numberp '(a b 3 c))
...

==> 3

>>> (find-if numberp '(a b c))
...

==> NIL

; :from-end
>>> (find-if evenp '(1 2 3 4 5))
...

==> 2

>>> (find-if evenp '(1 2 3 4 5) :from-end t)
...

==> 4

; :start
>>> (find-if evenp '(2 3 4 5) :start 1)
...

==> 4

; :key
>>> (find-if evenp '((1 a) (2 b) (3 c)) :key car)
...

==> (2 B)

; ── position ──────────────────────────────────────────────────────────────────

>>> (position 'b '(a b c))
...

==> 1

>>> (position 9 '(1 2 3))
...

==> NIL

; :from-end — rightmost index
>>> (position 'a '(a b a c a))
...

==> 0

>>> (position 'a '(a b a c a) :from-end t)
...

==> 4

; :start / :end
>>> (position 'a '(a b a c a) :start 1)
...

==> 2

>>> (position 'a '(a b a c a) :start 1 :end 2)
...

==> NIL

; :key
>>> (position 3 '((1 x) (2 y) (3 z)) :key car)
...

==> 2

; ── position-if ───────────────────────────────────────────────────────────────

>>> (position-if numberp '(a b 3 c 5))
...

==> 2

>>> (position-if numberp '(a b c))
...

==> NIL

; :from-end
>>> (position-if evenp '(1 2 3 4 5) :from-end t)
...

==> 3

; :start
>>> (position-if evenp '(2 3 4 5) :start 1)
...

==> 2

; ── count ─────────────────────────────────────────────────────────────────────

>>> (count 'a '(a b a c a))
...

==> 3

>>> (count 9 '(1 2 3))
...

==> 0

; :start / :end
>>> (count 'a '(a b a c a) :start 1 :end 4)
...

==> 1

; :key
>>> (count 1 '((1 x) (1 y) (2 z)) :key car)
...

==> 2

; :test
>>> (count "x" '("x" "y" "x") :test equal)
...

==> 2

; ── count-if ──────────────────────────────────────────────────────────────────

>>> (count-if evenp '(1 2 3 4 5 6))
...

==> 3

>>> (count-if numberp '(a b c))
...

==> 0

; :start / :end
>>> (count-if evenp '(2 3 4 5 6) :start 1 :end 4)
...

==> 1

; :key
>>> (count-if evenp '((1 a) (2 b) (3 c) (4 d)) :key car)
...

==> 2

; ── remove ────────────────────────────────────────────────────────────────────

; basic (backward-compatible with old Lisp version)
>>> (remove 'b '(a b c))
...

==> (A C)

>>> (remove 'a '(a b c))
...

==> (B C)

; removes ALL matching (no :count)
>>> (remove 'a '(a b a c a))
...

==> (B C)

; :test
>>> (remove "x" '("x" "y" "x") :test equal)
...

==> ("y")

; :count
>>> (remove 'a '(a b a c a) :count 2)
...

==> (B C A)

; :from-end with :count — remove from right
>>> (remove 'a '(a b a c a) :count 2 :from-end t)
...

==> (A B C)

; :start / :end — only consider bounded region
>>> (remove 'a '(a b a c a) :start 1 :end 4)
...

==> (A B C A)

; :key
>>> (remove 2 '((1 x) (2 y) (3 z) (2 w)) :key car)
...

==> ((1 X) (3 Z))

; ── remove-if ─────────────────────────────────────────────────────────────────

>>> (remove-if evenp '(1 2 3 4 5))
...

==> (1 3 5)

>>> (remove-if numberp '(a 1 b 2 c))
...

==> (A B C)

; :count
>>> (remove-if evenp '(1 2 3 4 5 6) :count 2)
...

==> (1 3 5 6)

; :from-end with :count
>>> (remove-if evenp '(1 2 3 4 5 6) :count 2 :from-end t)
...

==> (1 2 3 5)

; :start / :end
>>> (remove-if evenp '(2 3 4 5 6) :start 1 :end 4)
...

==> (2 3 5 6)

; :key
>>> (remove-if evenp '((1 a) (2 b) (3 c) (4 d)) :key car)
...

==> ((1 A) (3 C))

; ── remove-if-not ─────────────────────────────────────────────────────────────

>>> (remove-if-not evenp '(1 2 3 4 5 6))
...

==> (2 4 6)

>>> (remove-if-not numberp '(a 1 b 2 c))
...

==> (1 2)

; :count limits removals (removes at most 2 failing elements)
>>> (remove-if-not evenp '(1 2 3 4 5 6) :count 2)
...

==> (2 4 5 6)

; ── substitute ────────────────────────────────────────────────────────────────

>>> (substitute 'x 'a '(a b a c a))
...

==> (X B X C X)

>>> (substitute 0 99 '(1 2 3))
...

==> (1 2 3)

; :count
>>> (substitute 'x 'a '(a b a c a) :count 2)
...

==> (X B X C A)

; :from-end with :count
>>> (substitute 'x 'a '(a b a c a) :count 2 :from-end t)
...

==> (A B X C X)

; :start / :end
>>> (substitute 'x 'a '(a b a c a) :start 1 :end 4)
...

==> (A B X C A)

; :test
>>> (substitute 'match "hi" '("hi" "bye" "hi") :test equal)
...

==> (MATCH "bye" MATCH)

; :key
>>> (substitute '(99 z) 2 '((1 x) (2 y) (3 z)) :key car)
...

==> ((1 X) (99 Z) (3 Z))

; ── substitute-if ─────────────────────────────────────────────────────────────

>>> (substitute-if 0 evenp '(1 2 3 4 5))
...

==> (1 0 3 0 5)

>>> (substitute-if 0 evenp '(1 3 5))
...

==> (1 3 5)

; :count
>>> (substitute-if 0 evenp '(1 2 3 4 5 6) :count 2)
...

==> (1 0 3 0 5 6)

; :from-end with :count
>>> (substitute-if 0 evenp '(1 2 3 4 5 6) :count 2 :from-end t)
...

==> (1 2 3 0 5 0)

; :key
>>> (substitute-if '(99) evenp '((1 a) (2 b) (3 c) (4 d)) :key car)
...

==> ((1 A) (99) (3 C) (99))

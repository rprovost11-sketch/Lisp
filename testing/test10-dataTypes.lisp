>>> (list (+ 1 3) '(apple pear "string") 22/7 false)
...

==> (4 (APPLE PEAR "string") 22/7 FALSE)

>>> (first '(apple banana cake))
...

==> APPLE

>>> (rest '(apple banana cake donut))
...

==> (BANANA CAKE DONUT)

>>> (first '())
...

==> NIL

>>> (rest '(donkey))
...

==> NIL

>>> (cons 'apple '(banana orange pear))
...

==> (APPLE BANANA ORANGE PEAR)

>>> (remove 'orange '(apple banana orange pear))
...

==> (APPLE BANANA PEAR)

>>> (push! '(1 2 3 4) 6)
...

==> (1 2 3 4 6)

>>> (set! 'a '(1 2 3 4))
...

==> (1 2 3 4)

>>> a
...

==> (1 2 3 4)

>>> (pop! a)
...

==> 4

>>> a
...

==> (1 2 3)

>>> (set! 'mylist '("a" "b" "c" "d"))
...

==> ("a" "b" "c" "d")

>>> mylist
...

==> ("a" "b" "c" "d")

>>> (at mylist 2)
...

==> "c"

>>> (atset! mylist 1 1)
...

==> 1

>>> mylist
...

==> ("a" 1 "c" "d")

>>> (append mylist '(pi e))
...

==> ("a" 1 "c" "d" PI E)

>>> (hasvalue? '(a b c d) pi)
...

==> NIL

>>> (hasvalue? '(a b c d) 'c)
...

==> T

>>> (length nil)
...

==> 0

>>> (length '(a b c d))
...

==> 4

>>> (reverse '(1 2 3 4 5))
...

==> (5 4 3 2 1)

>>> (set! 'mymap (map ("a" 1) ("b" 2) ("c" 3) ) )
...

==> (MAP
   ("a" 1)
   ("b" 2)
   ("c" 3)
)

>>> mymap
...

==> (MAP
   ("a" 1)
   ("b" 2)
   ("c" 3)
)

>>> (at mymap "b")
...

==> 2

>>> (atset! mymap "b" e)
...

==> 2.718281828459045

>>> mymap
...

==> (MAP
   ("a" 1)
   ("b" 2.718281828459045)
   ("c" 3)
)

>>> (hasValue? mymap 1)
...

==> T

>>> (hasValue? mymap 10)
...

==> NIL

>>> (update! mymap (map ("b" 5) ("d" 10)))
...

==> (MAP
   ("a" 1)
   ("b" 5)
   ("c" 3)
   ("d" 10)
)

>>> mymap
...

==> (MAP
   ("a" 1)
   ("b" 5)
   ("c" 3)
   ("d" 10)
)

>>> (hasKey? mymap "a")
...

==> T

>>> (hasKey? mymap 3)
...

==> NIL

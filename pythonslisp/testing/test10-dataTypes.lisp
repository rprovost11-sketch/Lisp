>>> (list (+ 1 3) '(apple pear "string") 22/7 'false)
...

==> (4 (APPLE PEAR "string") 22/7 FALSE)

>>> (first '(apple banana cake))
...

==> APPLE

>>> (rest '(apple banana cake donut))
...

==> (BANANA CAKE DONUT)

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

>>> (setf a '(1 2 3 4))
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

>>> (setf mylist '("a" "b" "c" "d"))
...

==> ("a" "b" "c" "d")

>>> mylist
...

==> ("a" "b" "c" "d")

>>> (at 2 mylist)
...

==> "c"

>>> (setf (at 1 mylist) 1)
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

>>> (list-length nil)
...

==> 0

>>> (list-length '(a b c d))
...

==> 4

>>> (reverse '(1 2 3 4 5))
...

==> (5 4 3 2 1)

>>> (setf mymap (map ("a" 1) ("b" 2) ("c" 3) ) )
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

>>> (at "b" mymap)
...

==> 2

>>> (setf (at "b" mymap) e)
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

>>> ;;; Error: hasKey? requires a map as first argument
... (hasKey? 1 (map))

%%% ERROR 'HASKEY?': Invalid argument 1.  Map expected.
%%% USAGE: (HASKEY? <map> <key>)
==>

>>> ;;; Error: hasValue? requires a list or map as first argument
... (hasValue? 1 '(1 2))

%%% ERROR 'HASVALUE?': Invalid argument.  Argument 1 expected to be a list or map.
%%% USAGE: (HASVALUE? <listOrMap> <value>)
==>

>>> ;;; Error: update! requires map arguments
... (update! 1 (map))

%%% ERROR 'UPDATE!': Argument 1 expected to be a map.
%%% USAGE: (UPDATE! <map1> <map2>)
==>

>>> (update! (map) 1)

%%% ERROR 'UPDATE!': Argument 2 expected to be a map.
%%% USAGE: (UPDATE! <map1> <map2>)
==>

>>> (update!)

%%% ERROR 'UPDATE!': 2 arguments expected.
%%% USAGE: (UPDATE! <map1> <map2>)
==>


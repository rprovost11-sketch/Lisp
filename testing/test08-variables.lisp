>>> (setf var1 35)
...

==> 35

>>> var1
...

==> 35

>>> (+ var1 5)
...

==> 40

>>> (+ 5 var1)
...

==> 40

>>> var2
...

==> VAR2

>>> (setf var2 6.2)
...

==> 6.2

>>> var2
...

==> 6.2

>>> (- var2 var1)
...

==> -28.8

>>> var1
...

==> 35

>>> (setf myList '(apple banana orange cherry))
...

==> (APPLE BANANA ORANGE CHERRY)

>>> myList
...

==> (APPLE BANANA ORANGE CHERRY)

>>> (first myList)
...

==> APPLE

>>> (rest myList)
...

==> (BANANA ORANGE CHERRY)

>>> (if (= (first myList) apple)
... "It's true!")
...

==> "It's true!"

>>> (setf var1 "some string value")
...

==> "some string value"

>>> var1
...

==> "some string value"

>>> (setf var1 "some string value")
...

==> "some string value"

>>> var1
...

==> "some string value"

>>> (setf var1 "some string value")
...

==> "some string value"

>>> var1
...

==> "some string value"

>>> (let ()
...    (- var2)
...    (if (= var1 "my value")
...        (* 7 3)
...        (writeLn! var1)
...        )
...    var1
...    )
...
"some string value"

==> "some string value"

>>> (let ( (var1 -22/7) )
...    (writeLn! var1)
...    )
...
-22/7

==> -22/7

>>> var1
...

==> "some string value"

>>> var3
...

==> VAR3

>>> (let ( (var3 true) )
...    (writeLn! var3)
...    )
...
TRUE

==> TRUE

>>> var3
...

==> VAR3

>>> (setf var1 15)
...

==> 15

>>> (setf var2 2)
...

==> 2

>>> (undef! 'var3)
...

==> NIL

>>> (let ( (var3 'var3) )
...    (writeLn! var1)
...    (writeLn! var2)
...    (writeLn! var3)
...    (setf var2 -22/7)
...    (setf var3 "a value")
...    (writeLn! var1)
...    (writeLn! var2)
...    (writeLn! var3)
...    (let ( (var3 true) )
...       (writeLn! var1)
...       (writeLn! var2)
...       (writeLn! var3)
...       (symTab!)
...       )
...    )
...
15
2
VAR3
15
-22/7
a value
15
-22/7
TRUE
Symbol Table Dump:  Inner-Most Scope First
------------------------------------------
['VAR3']
['VAR3']
['*', '+', '-', '-INF', '/', '//', '<', '<=', '<>', '=', '>', '>=', 'ABS', 'AND', 'APPLY', 'APPLYANDFLATTEN', 'AT', 'ATSET!', 'BLOCK', 'CASE', 'CONCAT', 'CONCAT-AUX', 'COND', 'CONS', 'CONSEQ1', 'CONSEQ2', 'COPY', 'COS', 'D', 'DEEPCOPY', 'DEFUN', 'DIG', 'E', 'EQUAL?', 'EVAL', 'EX', 'EXI', 'EXP', 'FACT', 'FIB', 'FIRST', 'FLOAT', 'FM', 'FMI', 'FOREACH', 'FORMAT', 'HASKEY?', 'HASVALUE?', 'IF', 'INF', 'IS?', 'ISATOM?', 'ISEVEN?', 'ISFUNCTION?', 'ISLIST?', 'ISMAP?', 'ISNIL?', 'ISNUMBER?', 'ISODATE', 'ISODD?', 'ISSTRING?', 'ISSYMBOL?', 'JOIN', 'LAM', 'LIST', 'LOG', 'MAP', 'MAX', 'MIN', 'MOD', 'NAN', 'NOT', 'NIL', 'OR', 'PI', 'POP!', 'POW', 'PUSH!', 'QUOTE', 'READ!', 'READLN!', 'READ_PROMPT', 'REMOVE', 'REST', 'REVERSE', 'REVERSE-AUX', 'SET!', 'SIN', 'SIZE', 'STRING', 'SYMTAB!', 'TAN', 'TRUNC', 'UNDEF!', 'UPDATE!', 'VAR1', 'VAR2', 'WRITE!', 'WRITELN!']

==> NIL

>>> var1
...

==> 15

>>> var2
...

==> -22/7

>>> var3
...

==> VAR3

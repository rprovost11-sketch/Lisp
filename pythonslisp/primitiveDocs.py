defmacroDoc = ( '<symbol> ( <paramList> ) <sexpr1> <sexpr2> ...',
                """Define and return a new globally named macro.""" )

macrodxpandDoc = ( '\'(<macroName> <arg1> <arg2> ...)',
                   """Perform the expansion of a macro and return the results of those
expansions in a list.""" )

setfDoc = ( '<symbol> <sexpr>',
            """Update a variable's value.  The search fo the variable begins locally
and proceeds to search ever less local scopes until the global scope is searched.
If the variable is located in this search its value is updated.  If it's not located
a new global is defined and set the value.

Alternate usage: (setf (at <keyOrIndex> <mapOrList>) <newValue>""" )

undefDoc = ( '<symbol>',
             """Undefine the global definition for a symbol.""" )

symtabDoc = ( '',
              """Print the environment.  Each scope is printed on a separate line.
Local scope is first; global scope last.""" )

lambdaDoc = ( '( <paramList> ) <sexpr1> <sexpr2> ...',
              """Create and return a lambda function.  When evaluating such a function
the body (the exprs) are evaluated within a nested scope.""" )

letDoc = ( '( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <sexpr1> <sexpr2> ...)',
           """Execute code in a nested scope.  var1,var2,... are local variables bound
to initial values.  Initial values are not evaluated in order and are not evaluated
in the new local scope.""" )

letstarDoc = ( '( (<var1> <sexpr1>) (<var2> <sexpr2>) ...) <sexpr1> <sexpr2> ...)',
               """Execute code in a nested scope.  var1,var2,... are local variables
bound to initial values.  Initial values are evaluated in order and inside the new
local scope.""" )

prognDoc = ( '<sexpr1> <sexpr2> ...',
             """Evaluate each sexpression in turn.  Returns the result of the last
sexpr evaluation.""" )

ifDoc = ( '<cond> <conseq> &optional <alt>',
          """If evaluates the condition.  If t then consequence is evaluated and its
result returned, otherwise alt is evaluated and its result returned.""" )

condDoc = ( '(<cond1> <body1>) (<cond2> <body2>) ...',
            """Evaluates each cond in order until one evaluates to true.  Then evaluates
each expr in body and returns the result of the last expr evaluated.  All remaining
conds and bodys are skipped.""" )

caseDoc = ( '<sexpr> (<val1> <body1>) (<val2> <body2>) ...',
            """Evaluates expr.  Finds the first val that equals expr's val.  Then
evaluates each expr in body and returns the resolt of the last expr evaluated.
All remaining cases are skipped.""" )

quoteDoc = ( '<sexpr>',
             """Returns expr without evaluating it.""" )

backquoteDoc = ( '<sexpr>',
                 """Similar to quote but allows command comma-at expressions within expr.""" )

commaDoc = ( '<sexpr>',
             """Must occur within a backquote expr or it's an error.  Evaluates expr
(even if within a quoted expr) and returns it to the enclosing expr.""" )

comma_atDoc = ( '<sexpr>',
                """Must occur within a backquote expr or it's an error.  Evaluates expr.
Result must be a list.  Inserts the elements of the resulting list into
the enclosing list.""" )

whileDoc = ( '<cond> <sexpr1> <sexpr2> ...',
             """Perform a loop over the body sexprs.  Before iteration conditionExpr is
evaluated.  If conditionExpr is t the iteration occurs.  However if conditionExpr
evaluates to nil, it returns the result of the last body evaluation.""" )

dotimesDoc = ( '(<var> <integer>) <sexpr1> <sexpr2> ...',
               """Performs a loop countExpr times.  For each iteration of the loop variable
is set to the loop number (starting with 0 for the first loop).  The value
of the last sexpr evaluated is returned.""" )

foreachDoc = ( '<variable> <list> <sexpr1> <sexpr2> ...',
               """Perform a loop over the elements of list.  On each iteration var is set
to the next element in the list, then the expr's are evaluated in order.  Returns
the result of the very last evaluation.""" )

funcallDoc = ( '<fnNameSymbol> <arg1> <arg2> ...',
               """Call a function with the args provided.""" )

evalDoc = ( '<sexpr>',
            """Evaluate expr in the current scope.""" )

applyDoc = ( '<function> &rest <args> <argsList>',
             """Insert arg1,arg2,... in the front of listOfMoreArgs, then apply the
function the the whole list of args.""" )

parseDoc = ( '<string>',
             """Parse the string as an sexpression and return the sexpression.""" )

pythonDoc = ( '<string>',
              """Execute some python code from Lisp.""" )

# =======================
# List & Map Manipulation
# -----------------------
mapDoc = ( '(<key1> <val1>) (<key2> <val2>) ...',
           """Construct a map of key-value pairs.""" )

carDoc = ( '<list>',
           """Return the first item in a list.""" )

cdrDoc = ( '<list>',
           """Return a copy of the list minus the first element.""" )

consDoc = ( '<obj> <list>',
            """Return a copy of list with obj inserted into the front of the list.""" )

pushDoc = ( '<list> <value>',
            """Push a value onto the back of a list.""" )

popDoc = ( '<list>',
           """Pop and return the last value of a list.""" )

atDoc = ( '<keyOrIndex> <mapListOrStr>',
          """Return the value at a specified index of a list, or specified key of a map.""" )

atDeleteDoc = ( '<keyOrIndex> <mapOrList>',
                """Delete the item from the map or list specifed by keyOrIndex.""" )

atInsertDoc = ( '<index> <list> <newItem>',
                """Insert newItem into list as position index.""" )

appendDoc = ( '<list1> <list2> ...',
              """Return a new list with the lists merged.  Order is retained.""" )

hasValueDoc = ( '<listOrMap> <value>',
                """Returns t if the list/map contains value.""" )

updateDoc = ( '<map1> <map2>',
              """Update map1's data with map2's.""" )

hasKeyDoc = ( '<map> <key>',
              """Returns t if the key is in the map.""" )

sortedDoc = ( '<list>',
              """Return a copy of the list sorted.""" )

# =====================
# Arithmetic Operations
# ---------------------
addDoc = ( '<number1> <number2> ...',
           """Returns the sum of numbers.""" )

subDoc = ( '<number1> <number2> ...',
           """Returns the difference of numbers.""" )

mulDoc = ( '<number1> <number2> ...',
           """Returns the product of numbers.""" )

divDoc = ( '<number1> <number2> ...',
           """Returns the quotient of numbers.""" )

intdivDoc = ( '<number1> <number2>',
              """Return the integer division of numbers.""" )

moddivDoc = ( '<number1> <number2>',
              """Returns the integer remainder of division of two numbers.""" )

gcdDoc = ( '<integer1> <integer2> ...',
           """Returns the greatest common divisor of a list of integers.""" )

lcmDoc = ( '<integer1> <integer2> ...',
           """Returns the least common multiple of a list of integers.""" )

logDoc = ( '<number> &optional (<base> 10)',
           """Returns the log of a number.  The default is to use a log of base 10 but
a second argument can be provided to specify a different base.""" )

exptDoc = ( '<base> <power>',
            """Returns base raised to a power.""" )

sinDoc = ( '<radians>',
           """Returns the sine of a number in radians.""" )

cosDoc = ( '<radians>',
           """Returns the cosine of a number in radians.""" )

asinDoc = ( '<number>',
            """Returns the arcsine of a number in radians.""" )

acosDoc = ( '<number>',
            """Returns the arccosine of a number in radians.""" )

atanDoc = ( '<number1> &optional <number2>',
            """Returns the tangent or one or two numbers.""" )

minDoc = ( '<number1> <number2> ...',
           """Returns the lowest number of a set of numbers.""" )

maxDoc = ( '<number1> <number2> ...',
           """Returns the largest number of a set of numbers.""" )

randomDoc = ( '<integerOrFloat>',
              """Returns a random int or float (whichever is the same type as the argument)
between 0 and num inclusive.""" )

# ==========
# Predicates
# ----------
numberpDoc = ( '<sexpr>',
               """Returns t if expr is a number otherwise nil.""" )

integerpDoc = ( '<sexpr>',
                """Returns t if expr is an integer otherwise nil.""" )

rationalpDoc = ( '<sexpr>',
                 """Returns t if expr is an integer or fraction otherwise nil.""" )

floatpDoc = ( '<sexpr>',
              """Returns t if expr is a float otherwise nil.""" )

symbolpDoc = ( '<sexpr>',
               """Returns t if expr is a symbol otherwise nil.""" )

atomDoc = ( '<sexpr>',
            """Returns t if expr is an atom (int,float,string,map or nil).""" )

listpDoc = ( '<sexpr>',
             """Returns t if expr is a list otherwise nil.""" )

isMapDoc = ( '<sexpr>',
             """Returns t if expr is a map otherwise nil.""" )

stringpDoc = ( '<sexpr>',
               """Returns t if expr is a string otherwise nil.""" )

functionpDoc = ( '<sexpr>',
                 """Returns t if expr is a function otherwise nil.""" )

macropDoc = ( '<sexpr>',
              """Returns t if expr is a macro otherwise nil.""" )

# ====================
# Relational Operators
# --------------------
isDoc = ( '<expr1> <expr2>',
          """Returns t if the two values are the same object otherwise nil.""" )

equalDoc = ( '<expr1> <expr2> ...',
             """Returns t if the two exprs are the same value otherwise nil.""" )

notEqualDoc = ( '<expr1> <expr2> ...',
                """Returns t if the two exprs are different values otherwise nil.""" )

lessDoc = ( '<expr1> <expr2> ...',
            """Returns t if the arguments are in ascending order.""" )

lessOrEqualDoc = ( '<expr1> <expr2> ...',
                   """Returns t if the adjacent arguments are less-than-or-equal otherwise nil.""" )

greaterDoc = ( '<expr1> <expr2> ...',
               """Returns t if the arguments are in descending order otherwise nil.""" )

greateOrEqualDoc = ( '<expr1> <expr2> ...',
                     """Returns t if the adjacent arguments are greater-than-or-equal otherwise nil.""" )

# =================
# Logical Operators
# -----------------
notDoc = ( '<boolean>',
           """Returns t if the argument is nil otherwise returns nil.""" )

andDoc = ( '<boolean1> <boolean2> ...',
           """Returns t if all arguments are non-nil.  Short-circuits: stops evaluating
arguments upon encountering the first nill.""" )

orDoc = ( '<boolean1> <boolean2> ...',
          """Returns t if at least one argument is non-nil.  Short-circuits:  stops
evaluating upon encountering the first non-nil value.""" )

# ===============
# Type Conversion
# ---------------
floatDoc = ( '<number>',
             """Returns val as a float.  Val can be any number type or a string containing
a valid lisp float.""")

integerDoc = ( '<number> &optional (<base> 10)',
               """Returns val as an integer.  Val can be any number type or a string
containing a valid lisp integer.""" )

rationalDoc = ( '<number>',
                """Returns its argument as a fraction.  Val can be any number or a string
containg a valid lisp number that can be expressed as a fraction.""" )

stringDoc = ( '<object1> <object2> ...',
              """PrettyPrint's as programmer readable strings each argument object and
concatenates the results to form a new string.""" )

ustringDoc = ( '<object1> <object2> ...',
               """PrettyPrint's as user readable strings each argument object and
concatenates the results to form a new string.""" )

symbolDoc = ( '<string1> <string2> ...',
              """PrettyPrint's as programmer readable strings each argument object and
concatenates the results to form a new string which is used to define a new
symbol object.""" )

# ===============
# I/O
# ---------------
writefDoc = ( '<formatString> <MapOrList>',
              """Write formatted text.  Takes a python f-string and a map or list of
values for the f-string.  Returns the formatted output string.""" )

writeDoc = ( '<obj1> <obj2> ...',
             """Sequentially prettyPrints in programmer readable text the objects listed.
Returns the last value printed.""" )

writelnDoc = ( '<obj1> <obj2> ...',
               """Sequentially prettyPrints in programmer readable text the object listed.
Terminate the output with a newline character.  Returns the last value printed.""" )

uwriteDoc = ( '<obj1> <obj2> ...',
              """Sequentially prettyPrints in user readable text the objects listed.
Returns the last value printed.""" )

uwritelnDoc = ( '<obj1> <obj2> ...',
                """Sequentially prettyPrints in user readable text the object listed.
Terminate the output with a newline character.  Returns the last value printed.""" )

readlnDoc = ( '',
              """Reads and returns text input from the keyboard.  This function blocks
while it waits for the input.""" )

# ===============
# System Level
# ---------------
recursionlimitDoc = ( '&optional <newLimit>',
                      """Get or set the system recursion limit.  The higher the integer
argument the deeper the recursion will be allowed to go.  If setting,
returns newLimit upon success otherwise nil.""" )

helpDoc = ( '&optional callableSymbol',
            """Prints a set of tables for all the globally defined symbols currently
available in Python's Lisp; or prints the usage and documentation for a
specific primitive, function or macro.""" )

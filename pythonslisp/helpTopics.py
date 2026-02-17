topics: dict[str, str] = { }

topics['LANGUAGE'] = """Lexemes
   Comments
      Comments extend from ';' through '\n'.

   Literals
      NumberLiteral:  ['+'|'-'] ('0' .. '9')+
                         ( '/' ('0' .. '9')+
                         | 'e' ['+'|'-'] ('0' .. '9')+
                         | '.' ('0' .. '9')+ [ 'e' ['+'|'-'] ('0' .. '9')+ ]
                         )   # Supports integers, floats and fractions
      StringLiteral:  '"' { ^["] } '"'   # Supports all python escape sequences
      Symbol:         'a..zA..Z+-~!$%^&*_=\\/?<>#|'
                      { 'a..zA..Z+-~!$%^&*_=\\/?<>#|0..9' }

   Predefined Symbols
         'nil', 't', 'e', 'pi'

Grammar
   Start:
      { Object } EOF

   Object:
      NumberLiteral | StringLiteral | Symbol | List | '#' | '|' | ':' | '[' | ']'
      | "'" Object | "`" Object | "," Object | ",@" Object

   List:
      '(' Object* ')'
   
Notes
- parentheses () indicate grouping
- brackets [] surround optional parts
- braces {} surround parts that can occur 0 or more times in a row
- plus + follows parts that must occur 1 or more times in a row
- pipes | separate alternatives
- two dots .. separate parts that indicate a range
- caret brackets ^[ ... ] define a character class that includes
  all printable characters not found among those listed between the brackets."""

topics['LAMBDA-LIST'] = """lambdaList -> ( {var}*
                [ &optional {var | (var [initForm [svar]])}* ]
                [ &rest var ]
                [ &key {var | ({var | (keyword var)} [initForm [svar]]) }* 
                       [&allow-other-keys] ]
                [ &aux {var | (var [initForm])}* ] )

Notes:
- parentheses are taken literally as actual parentheses in lisp
- braces indicate grouping
- brackets surround optional parts
- pipes separate alternative parts
- asterisks indicate zero of more of the form it follows
- symbols beginning with & are taken literally
- the ordering of sections is not flexible"""


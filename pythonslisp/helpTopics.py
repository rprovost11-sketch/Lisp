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
      StringLiteral:  '"' (^('"'))* '"'   # Supports python escape sequences
      Symbol:         'a..zA..Z+-~!$%^&*_=\\/?<>#|'
                      { 'a..zA..Z+-~!$%^&*_=\\/?<>#|0..9' }

   Predefined Symbols
         'nil', 't', 'e', 'pi'

Grammar
   Start:
      Object* EOF

   Object:
      NumberLiteral | StringLiteral | Symbol | List | '#' | '|' | ':' | '[' | ']'
      | "'" Object | "`" Object | "," Object | ",@" Object

   List:
      '(' Object* ')'"""

topics['LAMBDA-LIST'] = """lambdaList -> ( {var}*
                [ &optional {var | (var [initForm [svar]])}* ]
                [ &rest var ]
                [ &key {var | ({var | (keyword var)} [initForm [svar]]) }* 
                       [&allow-other-keys] ]
                [ &aux {var | (var [initForm])}* ] )

Notes:
- braces indicate grouping
- brackets surround optional parts
- pipes separate alternative parts
- asterisk indicates zero of more of the form it follows
- parenthesis are taken listerally as actual parenthesis in lisp
- symbols preceded by & are taken literally
- The ordering of sections is not flexible"""


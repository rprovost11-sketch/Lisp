from fractions import Fraction
from typing import Any

from pythonslisp.Parser import Scanner, Parser, ParseError
from pythonslisp.LispAST import LList, LSymbol, LPrimitive

"""
The Language
------------
Lexemes
   Comments
      Comments extend from ';' through '\n'.

   Literals
      NumberLiteral:  ['+'|'-'] ('0' .. '9')+
                         ( '/' ('0' .. '9')+
                         | 'e' ['+'|'-'] ('0' .. '9')+
                         | '.' ('0' .. '9')+ [ 'e' ['+'|'-'] ('0' .. '9')+ ]
                         )
      StringLiteral:  '"' (^('"'))* '"'
      Symbol:         'a..zA..Z+-~!$%^&*_=\\/?<>'
                      { 'a..zA..Z+-~!$%^&*_=\\/?<>0..9' }

   Predefined Symbols
         'nil', 't', 'e', 'pi', 'inf', 'nan'

Grammar
   Start:
      Object* EOF

   Object:
      NumberLiteral | StringLiteral | Symbol | List | '#' | '|' | ':' | '[' | ']'
      | "'" Object | "`" Object | "," Object | ",@" Object

   List:
      '(' Object* ')'
"""

class LispScanner( Scanner ):
   WHITESPACE     = ' \t\n\r'
   SIGN           = '+-'
   DIGIT          = '0123456789'
   OCTAL_DIGIT    = '01234567'
   HEX_DIGIT      = DIGIT + 'ABCDEFabcdef'
   SIGN_OR_DIGIT  = SIGN + DIGIT
   ALPHA_LOWER    = 'abcdefghijklmnopqrstuvwxyz'
   ALPHA_UPPER    = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
   ALPHA          = ALPHA_LOWER + ALPHA_UPPER
   SYMBOL_OTHER   = '~!$%^&*_=\\/?<>:'
   SYMBOL_FIRST   = ALPHA + SIGN + SYMBOL_OTHER
   SYMBOL_REST    = ALPHA + SIGN + SYMBOL_OTHER + DIGIT

   EOF_TOK            =   0

   SYMBOL_TOK         = 101    # Value Objects
   STRING_TOK         = 102
   INTEGER_TOK        = 111
   FLOAT_TOK          = 112
   FRAC_TOK           = 121

   OPEN_BRACKET_TOK   = 201    # Paired Symbols
   CLOSE_BRACKET_TOK  = 202
   OPEN_PAREN_TOK     = 211
   CLOSE_PAREN_TOK    = 212

   POUND_SIGN_TOK     = 501
   PIPE_TOK           = 502
   COLON_TOK          = 503
   SINGLE_QUOTE_TOK   = 504
   COMMA_TOK          = 505
   COMMA_AT_TOK       = 506
   BACK_QUOTE_TOK     = 507

   def __init__( self, ) -> None:
      super( ).__init__( )

   def _scanNextToken( self ) -> int:
      buf = self.buffer

      try:
         self._skipWhitespaceAndComments( )

         nextChar = buf.peekNextChar( )
         if nextChar == '':
            return LispScanner.EOF_TOK
         #elif nextChar == '[':
            #buf.markStartOfLexeme( )
            #buf.consume( )
            #return LispScanner.OPEN_BRACKET_TOK
         #elif nextChar == ']':
            ##buf.markStartOfLexeme( )
            #buf.consume( )
            #return LispScanner.CLOSE_BRACKET_TOK
         elif nextChar == '(':
            #buf.markStartOfLexeme( )
            buf.consume( )
            return LispScanner.OPEN_PAREN_TOK
         elif nextChar == ')':
            #buf.markStartOfLexeme( )
            buf.consume( )
            buf.scanLineTxt( )
            return LispScanner.CLOSE_PAREN_TOK
         #elif nextChar == '#':
            ##buf.markStartOfLexeme( )
            #buf.consume( )
            #return LispScanner.POUND_SIGN_TOK
         #elif nextChar == '|':
            ##buf.markStartOfLexeme( )
            #buf.consume( )
            #return LispScanner.PIPE_TOK
         #elif nextChar == ':':
            ##buf.markStartOfLexeme( )
            #buf.consume( )
            #nextChar = buf.peekNextChar( )
            #return LispScanner.COLON_TOK
         elif nextChar == "'":
            #buf.markStartOfLexeme( )
            buf.consume( )
            nextChar = buf.peekNextChar( )
            return LispScanner.SINGLE_QUOTE_TOK
         elif nextChar == '`':
            #buf.markStartOfLexeme( )
            buf.consume( )
            nextChar = buf.peekNextChar( )
            return LispScanner.BACK_QUOTE_TOK
         elif nextChar == ',':
            #buf.markStartOfLexeme( )
            buf.consume( )
            nextChar = buf.peekNextChar( )
            if nextChar == '@':
               buf.consume( )
               nextChar = buf.peekNextChar( )
               return LispScanner.COMMA_AT_TOK
            return LispScanner.COMMA_TOK
         elif nextChar == '"':
            return self._scanStringLiteral( )
         elif nextChar in LispScanner.SIGN_OR_DIGIT:
            return self._scanNumOrSymbol( )
         elif nextChar in LispScanner.SYMBOL_FIRST:
            return self._scanSymbol( )
         else:
            raise ParseError( self, 'Unknown Token' )

      except ParseError:
         raise

      except:
         return LispScanner.EOF_TOK

   def _scanStringLiteral( self ) -> int:
      buf = self.buffer

      # Scan in the initial quote
      nextChar = buf.peekNextChar( )
      if nextChar != '"':
         raise ParseError( self, '\'"\' expected.' )
      buf.markStartOfLexeme( )
      buf.consume( )

      nextChar =  buf.peekNextChar( )
      while (nextChar != '"') and (nextChar != ''):  # Loop until we reach the end quote.
         if nextChar == '\\':
            self._consumeStringEscapeSequence( )
         else:
            buf.consume( )

         nextChar = buf.peekNextChar( )

      # Scan past the final quote
      if nextChar != '"':
         raise ParseError( self, '\'"\' expected.  A string literal may be unterminated.' )
      buf.consume( )

      return LispScanner.STRING_TOK

   def _consumeStringEscapeSequence( self ) -> None:
      buf = self.buffer

      nextChar = buf.peekNextChar( )
      if nextChar != '\\':
         raise ParseError( self,  '\\ expected.' )
      buf.consume( )             # Consume the \

      nextChar = buf.peekNextChar( )
      if nextChar in LispScanner.OCTAL_DIGIT:
         # consume an octal number up to a 3 digits
         numCharsConsumed = buf.consumePast( LispScanner.OCTAL_DIGIT, maxCharsToConsume=3 )
         if (numCharsConsumed < 0) or (numCharsConsumed > 3):
            raise ParseError( self, '1 to 3 octacl digits expected following escape character \\.' )
      elif nextChar == 'x':
         buf.consume( )    # consume the x
         # consume 2 digit hex number
         numDigitsConsumed = buf.consumePast( LispScanner.HEX_DIGIT, maxCharsToConsume=2 )
         if numDigitsConsumed != 2:
            raise ParseError( self, 'Expected exactly two hex digits in escape sequence following \\x.' )
      elif nextChar == 'N':
         buf.consume( )    # consume the N
         # consume unicode character name
         nextChar = buf.peekNextChar( )
         if nextChar != '{':
            raise ParseError( self, 'Expected { following escape sequence \\N.' )
         buf.consumeUpTo( '}' )
         nextChar = buf.peekNextChar( )
         if nextChar != '}':
            raise ParseError( self, 'Escape sequence \\N{ must be terminated by }.' )
         buf.consume( )
      elif nextChar == 'u':
         buf.consume( )    # consume the u
         # consume 4 hex digits
         numDigitsConsumed = buf.consumePast( LispScanner.HEX_DIGIT, maxCharsToConsume=4 )
         if numDigitsConsumed != 4:
            raise ParseError( self, 'Escape sequence expects exactly 4 hex digits following \\u.' )
      elif nextChar == 'U':
         buf.consume( )    # consume the U
         # consume 8 hex digits
         numDigitsConsumed = buf.consumePast( LispScanner.HEX_DIGIT, maxCharsToConsume=8 )
         if numDigitsConsumed != 8:
            raise ParseError( self, 'Escape sequence expects exactly 8 hex digits following \\u.' )
      elif nextChar in '\\\'\"abfnrtv':
         # consume 1 character
         buf.consume( )
      else:
         raise ParseError( self,  'invalid escape sequence.' )

   def _scanNumOrSymbol( self ) -> int:
      '''
      NumberLiteral:  ['+'|'-']('0' .. '9')+                                    # <-- leader
                         ( '/' ('0' .. '9')+                                    # <-- fraction case
                         | 'e' ['+'|'-'] ('0' .. '9')+                          # <-- exponentiation case
                         | '.' ('0' .. '9')+ [ 'e' ['+'|'-'] ('0' .. '9')+ ]    # <-- decimal/exponentiation case
                         )
      '''
      SAVE = self.saveState( )                  # Save the scanner state

      buf = self.buffer
      buf.markStartOfLexeme( )
      nextChar = buf.peekNextChar()
      if nextChar in LispScanner.SIGN:
         buf.consume()
         secondChar = buf.peekNextChar( )
         if (secondChar == '') or (secondChar not in LispScanner.DIGIT):
            self.restoreState( SAVE )         # Restore the scanner state
            return self._scanSymbol( )

      buf.consumePast( LispScanner.DIGIT )
      nextChar = buf.peekNextChar()

      if nextChar == '/':
         # Possibly a Fraction number
         buf.consume( )

         nextChar = buf.peekNextChar( )
         if nextChar not in LispScanner.DIGIT:
            self.restoreState( SAVE )         # Restore the scanner state
            return self._scanSymbol( )

         buf.consumePast( LispScanner.DIGIT )
         return LispScanner.FRAC_TOK

      elif nextChar in ('e', 'E'):
         # Exponentiation case
         buf.consume( )

         nextChar = buf.peekNextChar( )
         if (nextChar not in LispScanner.SIGN) and (nextChar not in LispScanner.DIGIT):
            self.restoreState( SAVE )
            return self._scanSymbol( )

         if nextChar in LispScanner.SIGN:
            buf.consume( )
            nextChar = buf.peekNextChar( )

         if (nextChar not in LispScanner.DIGIT):
            self.restoreState( SAVE )
            return self._scanSymbol( )

         buf.consumePast( LispScanner.DIGIT )
         return LispScanner.FLOAT_TOK

      elif nextChar == '.':
         # Possibly a floating point number
         # '.' ('0' .. '9')+ [ 'e' ['+'|'-'] ('0' .. '9')+ ]    # <-- decimal/exponentiation case
         #self.saveState( SAVE )
         buf.consume()
         nextChar = buf.peekNextChar()
         if nextChar not in LispScanner.DIGIT:
            # Integer
            self.restoreState( SAVE )
            return self._scanSymbol( )

         buf.consumePast( LispScanner.DIGIT )
         nextChar = buf.peekNextChar( )

         if nextChar not in ('e', 'E'):
            return LispScanner.FLOAT_TOK

         buf.consume( )
         nextChar = buf.peekNextChar( )

         if (nextChar not in LispScanner.SIGN) and (nextChar not in LispScanner.DIGIT):
            self.restoreState( SAVE )
            return self._scanSymbol( )

         if nextChar in LispScanner.SIGN:
            buf.consume( )
            nextChar = buf.peekNextChar( )

         if (nextChar not in LispScanner.DIGIT):
            self.restoreState( SAVE )
            return self._scanSymbol( )

         buf.consumePast( LispScanner.DIGIT )
         return LispScanner.FLOAT_TOK

      return LispScanner.INTEGER_TOK

   def _scanSymbol( self ) -> int:
      buf = self.buffer

      buf.markStartOfLexeme( )
      nextChar = buf.peekNextChar()
      if nextChar not in LispScanner.SYMBOL_FIRST:
         raise ParseError( self, 'Invalid symbol character' )
      buf.consume( )

      buf.consumePast( LispScanner.SYMBOL_REST )

      return LispScanner.SYMBOL_TOK

   def _skipWhitespaceAndComments( self ) -> None:
      buf = self.buffer

      nextChar = buf.peekNextChar()
      while (nextChar in '; \t\n\r') and (nextChar != ''):
         if nextChar == ';':
            buf.consumeUpTo( '\n\r' )
         buf.consume( )
         nextChar = buf.peekNextChar()


class LispParser( Parser ):
   def __init__( self ) -> None:
      self._scanner    = LispScanner( )
      self._env = None

   def parse( self, source: str ) -> Any:  # Returns an AST of inputString
      self._scanner.reset( source )
      try:
         return self._parse( )
      finally:
         self._env = None
   
   def parseFile( self, filename: str ) -> Any:
      self._scanner.resetFromFile( filename )
      try:
         return self._parse( )
      finally:
         self._env = None

   def _parse( self ) -> Any:
      # Parse all the sexpressions and insert them into a lisp progn function
      exprAst = self._parseObject( )
      bodyExpr = None
      if exprAst is not None:
         bodyExpr = LList( LSymbol('progn'), exprAst )
         while self._scanner.peekToken() != LispScanner.EOF_TOK:
            exprAst = self._parseObject( )
            bodyExpr.append( exprAst )

      # EOF
      if self._scanner.peekToken( ) != LispScanner.EOF_TOK:
         raise ParseError( self._scanner, 'EOF Expected.' )

      return bodyExpr

   def _parseObject( self ) -> Any: # Returns an AST or None if eof
      lex: str = ''           # Holds the lexeme string
      ast: Any = None         # Holds the parsed AST

      nextToken = self._scanner.peekToken( )
      if nextToken == LispScanner.INTEGER_TOK:
         lex = self._scanner.getLexeme( )
         ast = int(lex)
         self._scanner.consume( )
      elif nextToken== LispScanner.FLOAT_TOK:
         lex = self._scanner.getLexeme( )
         ast = float(lex)
         self._scanner.consume( )
      elif nextToken== LispScanner.FRAC_TOK:
         lex = self._scanner.getLexeme( )
         lex_num,lex_denom = lex.split('/')
         ast = Fraction( int(lex_num), int(lex_denom) )
         self._scanner.consume( )
      elif nextToken == LispScanner.STRING_TOK:
         lex = self._scanner.getLexeme( )
         ast = lex[1:-1]
         self._scanner.consume( )
      elif nextToken == LispScanner.SYMBOL_TOK:
         lex = self._scanner.getLexeme( )   # Make symbols case insensative
         ast = LSymbol(lex)
         self._scanner.consume( )
      elif nextToken == LispScanner.OPEN_PAREN_TOK:
         lex = '()'
         ast = self._parseList( )
      elif nextToken == LispScanner.SINGLE_QUOTE_TOK:
         lex = self._scanner.getLexeme( )
         self._scanner.consume( )
         subordinate = self._parseObject( )
         ast = LList( LSymbol('QUOTE'), subordinate )
      elif nextToken == LispScanner.BACK_QUOTE_TOK:
         lex = self._scanner.getLexeme( )
         self._scanner.consume( )
         subordinate = self._parseObject( )
         ast = LList( LSymbol('BACKQUOTE'), subordinate )
      elif nextToken == LispScanner.COMMA_TOK:
         lex = self._scanner.getLexeme( )
         self._scanner.consume( )
         subordinate = self._parseObject( )
         ast = LList( LSymbol('COMMA'), subordinate )
      elif nextToken == LispScanner.COMMA_AT_TOK:
         lex = self._scanner.getLexeme( )
         self._scanner.consume( )
         subordinate = self._parseObject( )
         ast = LList( LSymbol('COMMA-AT'), subordinate )
      elif nextToken in ( LispScanner.OPEN_BRACKET_TOK, LispScanner.CLOSE_BRACKET_TOK,
                          LispScanner.POUND_SIGN_TOK, LispScanner.PIPE_TOK, LispScanner.COLON_TOK ):
         lex = self._scanner.getLexeme( )
         ast = lex
         self._scanner.consume( )
      elif nextToken == LispScanner.EOF_TOK:
         ast = None
      else:
         raise ParseError( self._scanner, 'Object expected.' )

      return ast

   def _parseList( self ) -> LList:
      theList = LList( )

      # Open List
      nextToken = self._scanner.peekToken()
      if nextToken != LispScanner.OPEN_PAREN_TOK:
         raise ParseError( self._scanner, '( expected.' )
      else:
         self._scanner.consume( )

      # List Entries
      nextToken = self._scanner.peekToken( )
      while nextToken not in (LispScanner.CLOSE_PAREN_TOK,
                                               LispScanner.EOF_TOK):
         theList.append( self._parseObject( ) )
         nextToken = self._scanner.peekToken( )

      # Close List
      if nextToken != LispScanner.CLOSE_PAREN_TOK:
         raise ParseError( self._scanner, ') expected.')
      else:
         self._scanner.consume( )

      return theList

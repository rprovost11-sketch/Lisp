from fractions import Fraction
from typing import Any

from pythonslisp.Parser import Lexer, Parser, ParseError
from pythonslisp.LispAST import LList, LSymbol

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
         'nil', 't', 'e', 'pi'

Grammar
   Start:
      Object* EOF

   Object:
      NumberLiteral | StringLiteral | Symbol | List | '#' | '|' | ':' | '[' | ']'
      | "'" Object | "`" Object | "," Object | ",@" Object

   List:
      '(' Object* ')'
"""

class LispLexer( Lexer ):
   WHITESPACE     = ' \t\n\r'
   SIGN           = '+-'
   DIGIT          = '0123456789'
   OCTAL_DIGIT    = '01234567'
   HEX_DIGIT      = DIGIT + 'ABCDEFabcdef'
   SIGN_OR_DIGIT  = SIGN + DIGIT
   ALPHA_LOWER    = 'abcdefghijklmnopqrstuvwxyz'
   ALPHA_UPPER    = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
   ALPHA          = ALPHA_LOWER + ALPHA_UPPER
   SYMBOL_OTHER   = '~!$%^&*_=\\/?<>:#|'
   SYMBOL_FIRST   = ALPHA + SIGN + SYMBOL_OTHER
   SYMBOL_REST    = ALPHA + SIGN + SYMBOL_OTHER + DIGIT

   EOF_TOK            =   0

   SYMBOL_TOK         = 101    # Value tokens
   STRING_TOK         = 111
   INTEGER_TOK        = 121
   FLOAT_TOK          = 122
   FRAC_TOK           = 123

   OPEN_PAREN_TOK     = 201    # Paired tokens
   CLOSE_PAREN_TOK    = 202

   SINGLE_QUOTE_TOK   = 501    # Misc tokens
   COMMA_TOK          = 502
   COMMA_AT_TOK       = 503
   BACK_QUOTE_TOK     = 504

   def __init__( self ) -> None:
      super( ).__init__( )

   def _scanNextToken( self ) -> int:
      buf = self.buffer

      try:
         self._skipWhitespaceAndComments( )

         nextChar = buf.peekNextChar( )
         if nextChar == '':
            return LispLexer.EOF_TOK
         elif nextChar == '(':
            buf.consume( )
            return LispLexer.OPEN_PAREN_TOK
         elif nextChar == ')':
            buf.consume( )
            return LispLexer.CLOSE_PAREN_TOK
         elif nextChar == "'":
            buf.consume( )
            return LispLexer.SINGLE_QUOTE_TOK
         elif nextChar == '`':
            buf.consume( )
            return LispLexer.BACK_QUOTE_TOK
         elif nextChar == ',':
            buf.consume( )
            nextChar = buf.peekNextChar( )
            if nextChar == '@':
               buf.consume( )
               return LispLexer.COMMA_AT_TOK
            return LispLexer.COMMA_TOK
         elif nextChar == '"':
            return self._scanStringLiteral( )
         elif nextChar in LispLexer.SIGN_OR_DIGIT:
            return self._scanNumOrSymbol( )
         elif nextChar in LispLexer.SYMBOL_FIRST:
            return self._scanSymbol( )
         else:
            raise ParseError( self, 'Unknown Token' )

      except ParseError:
         raise

      except:
         return LispLexer.EOF_TOK

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

      return LispLexer.STRING_TOK

   def _consumeStringEscapeSequence( self ) -> None:
      buf = self.buffer

      nextChar = buf.peekNextChar( )
      if nextChar != '\\':
         raise ParseError( self,  '\\ expected.' )
      buf.consume( )             # Consume the \

      nextChar = buf.peekNextChar( )
      if nextChar in LispLexer.OCTAL_DIGIT:
         # consume an octal number up to a 3 digits
         numCharsConsumed = buf.consumePast( LispLexer.OCTAL_DIGIT, maxCharsToConsume=3 )
         if (numCharsConsumed < 0) or (numCharsConsumed > 3):
            raise ParseError( self, '1 to 3 octacl digits expected following escape character \\.' )
      elif nextChar == 'x':
         buf.consume( )    # consume the x
         # consume 2 digit hex number
         numDigitsConsumed = buf.consumePast( LispLexer.HEX_DIGIT, maxCharsToConsume=2 )
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
         numDigitsConsumed = buf.consumePast( LispLexer.HEX_DIGIT, maxCharsToConsume=4 )
         if numDigitsConsumed != 4:
            raise ParseError( self, 'Escape sequence expects exactly 4 hex digits following \\u.' )
      elif nextChar == 'U':
         buf.consume( )    # consume the U
         # consume 8 hex digits
         numDigitsConsumed = buf.consumePast( LispLexer.HEX_DIGIT, maxCharsToConsume=8 )
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
      SAVE = self.saveState( )                  # Save the lexer state

      buf = self.buffer
      buf.markStartOfLexeme( )
      nextChar = buf.peekNextChar()
      if nextChar in LispLexer.SIGN:
         buf.consume()
         secondChar = buf.peekNextChar( )
         if (secondChar == '') or (secondChar not in LispLexer.DIGIT):
            self.restoreState( SAVE )         # Restore the lexer state
            return self._scanSymbol( )

      buf.consumePast( LispLexer.DIGIT )
      nextChar = buf.peekNextChar()

      if nextChar == '/':
         # Possibly a Fraction number
         buf.consume( )

         nextChar = buf.peekNextChar( )
         if nextChar not in LispLexer.DIGIT:
            self.restoreState( SAVE )         # Restore the lexer state
            return self._scanSymbol( )

         buf.consumePast( LispLexer.DIGIT )
         return LispLexer.FRAC_TOK

      elif nextChar in ('e', 'E'):
         # Exponentiation case
         buf.consume( )

         nextChar = buf.peekNextChar( )
         if (nextChar not in LispLexer.SIGN) and (nextChar not in LispLexer.DIGIT):
            self.restoreState( SAVE )
            return self._scanSymbol( )

         if nextChar in LispLexer.SIGN:
            buf.consume( )
            nextChar = buf.peekNextChar( )

         if (nextChar not in LispLexer.DIGIT):
            self.restoreState( SAVE )
            return self._scanSymbol( )

         buf.consumePast( LispLexer.DIGIT )
         return LispLexer.FLOAT_TOK

      elif nextChar == '.':
         # Possibly a floating point number
         # '.' ('0' .. '9')+ [ 'e' ['+'|'-'] ('0' .. '9')+ ]    # <-- decimal/exponentiation case
         #self.saveState( SAVE )
         buf.consume()
         nextChar = buf.peekNextChar()
         if nextChar not in LispLexer.DIGIT:
            # Integer
            self.restoreState( SAVE )
            return self._scanSymbol( )

         buf.consumePast( LispLexer.DIGIT )
         nextChar = buf.peekNextChar( )

         if nextChar not in ('e', 'E'):
            return LispLexer.FLOAT_TOK

         buf.consume( )
         nextChar = buf.peekNextChar( )

         if (nextChar not in LispLexer.SIGN) and (nextChar not in LispLexer.DIGIT):
            self.restoreState( SAVE )
            return self._scanSymbol( )

         if nextChar in LispLexer.SIGN:
            buf.consume( )
            nextChar = buf.peekNextChar( )

         if (nextChar not in LispLexer.DIGIT):
            self.restoreState( SAVE )
            return self._scanSymbol( )

         buf.consumePast( LispLexer.DIGIT )
         return LispLexer.FLOAT_TOK

      return LispLexer.INTEGER_TOK

   def _scanSymbol( self ) -> int:
      buf = self.buffer

      buf.markStartOfLexeme( )
      nextChar = buf.peekNextChar()
      if nextChar not in LispLexer.SYMBOL_FIRST:
         raise ParseError( self, 'Invalid symbol character' )
      buf.consume( )

      buf.consumePast( LispLexer.SYMBOL_REST )

      return LispLexer.SYMBOL_TOK

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
      self._scanner    = LispLexer( )

   def parse( self, source: str ) -> Any:  # Returns an AST of inputString
      self._scanner.reset( source )
      return self._parse( )
   
   def parseFile( self, filename: str ) -> Any:
      self._scanner.resetFromFile( filename )
      return self._parse( )

   def _parse( self ) -> Any:
      # Parse all the sexpressions and insert them into a lisp progn function
      bodyExpr = LList( LSymbol('progn') )
      while self._scanner.peekToken() != LispLexer.EOF_TOK:
         bodyExpr.append( self._parseObject() )

      # EOF
      if self._scanner.peekToken( ) != LispLexer.EOF_TOK:
         raise ParseError( self._scanner, 'EOF Expected.' )

      return bodyExpr

   def _parseObject( self ) -> Any: # Returns an AST or None if eof
      lex: str = ''           # Holds the lexeme string
      ast: Any = None         # Holds the parsed AST

      nextToken = self._scanner.peekToken( )
      if nextToken == LispLexer.SYMBOL_TOK:
         lex = self._scanner.getLexeme( )   # Make symbols case insensative
         ast = LSymbol(lex)
         self._scanner.consume( )
      elif nextToken == LispLexer.OPEN_PAREN_TOK:
         lex = '()'
         ast = self._parseList( )
      elif nextToken == LispLexer.INTEGER_TOK:
         lex = self._scanner.getLexeme( )
         ast = int(lex)
         self._scanner.consume( )
      elif nextToken== LispLexer.FLOAT_TOK:
         lex = self._scanner.getLexeme( )
         ast = float(lex)
         self._scanner.consume( )
      elif nextToken== LispLexer.FRAC_TOK:
         lex = self._scanner.getLexeme( )
         ast = Fraction( lex )
         self._scanner.consume( )
      elif nextToken == LispLexer.STRING_TOK:
         lex = self._scanner.getLexeme( )
         ast = lex[1:-1]
         self._scanner.consume( )
      elif nextToken == LispLexer.SINGLE_QUOTE_TOK:
         lex = self._scanner.getLexeme( )
         self._scanner.consume( )
         subordinate = self._parseObject( )
         ast = LList( LSymbol('QUOTE'), subordinate )
      elif nextToken == LispLexer.BACK_QUOTE_TOK:
         lex = self._scanner.getLexeme( )
         self._scanner.consume( )
         subordinate = self._parseObject( )
         ast = LList( LSymbol('BACKQUOTE'), subordinate )
      elif nextToken == LispLexer.COMMA_TOK:
         lex = self._scanner.getLexeme( )
         self._scanner.consume( )
         subordinate = self._parseObject( )
         ast = LList( LSymbol('COMMA'), subordinate )
      elif nextToken == LispLexer.COMMA_AT_TOK:
         lex = self._scanner.getLexeme( )
         self._scanner.consume( )
         subordinate = self._parseObject( )
         ast = LList( LSymbol('COMMA-AT'), subordinate )
      elif nextToken == LispLexer.EOF_TOK:
         ast = None
      else:
         raise ParseError( self._scanner, 'Object expected.' )

      return ast

   def _parseList( self ) -> LList:
      scn = self._scanner
      
      theList = LList( )

      # Open List
      nextToken = scn.peekToken()
      if nextToken != LispLexer.OPEN_PAREN_TOK:
         raise ParseError( scn, '( expected.' )
      else:
         scn.consume( )

      # List Entries
      nextToken = scn.peekToken( )
      while nextToken not in (LispLexer.CLOSE_PAREN_TOK, LispLexer.EOF_TOK):
         theList.append( self._parseObject( ) )
         nextToken = scn.peekToken( )

      # Close List
      if nextToken != LispLexer.CLOSE_PAREN_TOK:
         raise ParseError( scn, ') expected.')
      else:
         scn.consume( )

      return theList

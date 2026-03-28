from __future__ import annotations
import ast as _ast_module
from fractions import Fraction
from typing import Any

from pythonslisp.ltk.ParserBase import LexerBase, LexerState, ParserBase, ParseError
from pythonslisp.AST import LSymbol


def _parse_string_literal( lex: str ) -> str:
   """Convert a Lisp string lexeme (including surrounding quotes) to its Python string value.
   Handles all Python escape sequences.  Literal newlines/carriage-returns inside the
   string are pre-escaped so that ast.literal_eval can parse them correctly."""
   inner = lex[1:-1]
   inner = inner.replace( '\r\n', '\\r\\n' )
   inner = inner.replace( '\r',   '\\r'   )
   inner = inner.replace( '\n',   '\\n'   )
   return _ast_module.literal_eval( f'"{inner}"' )

"""
The Language
------------
Lexemes
   Character Classes
      SIGN:         '+-'
      DIGIT:        '0123456789'
      SYMBOL_FIRST: 'a..zA..Z+-~!$%^&*_=\/?<>:#|'
      SYMBOL_REST:  'a..zA..Z+-~!$%^&*_=\/?<>:#|0..9'
   
   Comments
      Comments extend from ';' through '\n'.

   Literals
      NumberLiteral:  [SIGN] DIGIT+
                         ( '/' DIGIT+
                         | 'e' [SIGN] DIGIT+
                         | '.' DIGIT+ [ 'e' [SIGN] DIGIT+ ]
                         )   # Supports integers, floats and fractions
      StringLiteral:  '"' { ^["] } '"'   # Supports all python string escape sequences
      Symbol:         SYMBOL_FIRST {SYMBOL_REST}
                    | any token starting with SIGN_OR_DIGIT that is not a valid NumberLiteral,
                      or a valid NumberLiteral immediately followed by SYMBOL_REST chars

Grammar
   Start:
      { Object } EOF

   Object:
      NumberLiteral | StringLiteral | Symbol | List
      | "'" Object | "`" Object | "," Object | ",@" Object

   List:
      '(' { Object } ')'
"""

class Lexer( LexerBase ):
   # Character Classes
   WHITESPACE            = frozenset(' \t\n\r')
   NEWLINE = frozenset('\n\r')
   SIGN                  = frozenset('+-')
   DIGIT                 = frozenset('0123456789')
   OCTAL_DIGIT           = frozenset('01234567')
   HEX_DIGIT             = DIGIT | frozenset('ABCDEFabcdef')
   SIGN_OR_DIGIT         = SIGN | DIGIT
   ALPHA_LOWER           = frozenset('abcdefghijklmnopqrstuvwxyz')
   ALPHA_UPPER           = frozenset('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
   ALPHA                 = ALPHA_LOWER | ALPHA_UPPER
   SYMBOL_OTHER          = frozenset('~!$%^&*_=\\/?<>:#|')
   SYMBOL_FIRST          = ALPHA | SIGN | SYMBOL_OTHER
   SYMBOL_REST           = ALPHA | SIGN | SYMBOL_OTHER | DIGIT
   ESCAPE_SIMPLE         = frozenset('\\\'"abfnrtv')
   STRING_STOP           = frozenset('"\\')

   # Token Definitions
   EOF_TOK            =   0

   SYMBOL_TOK         = 101    # Value tokens
   STRING_TOK         = 111
   INTEGER_TOK        = 121
   FLOAT_TOK          = 122
   FRAC_TOK           = 123

   OPEN_PAREN_TOK     = 201    # Paired tokens
   CLOSE_PAREN_TOK    = 202

   SINGLE_QUOTE_TOK   = 501    # Misc tokens
   UNQUOTE_TOK          = 502
   UNQUOTE_SPLICING_TOK = 503
   QUASIQUOTE_TOK     = 504

   def __init__( self ) -> None:
      super( ).__init__( )
      self._savedState = LexerState( )

   def _scanNextToken( self ) -> int:
      buf = self.buffer

      self._skipWhitespaceAndComments( )

      nextChar = buf.peekNextChar( )
      if nextChar == '':
         return Lexer.EOF_TOK
      elif nextChar == '(':
         buf.consume( )
         return Lexer.OPEN_PAREN_TOK
      elif nextChar == ')':
         buf.consume( )
         return Lexer.CLOSE_PAREN_TOK
      elif nextChar == "'":
         buf.consume( )
         return Lexer.SINGLE_QUOTE_TOK
      elif nextChar == '`':
         buf.consume( )
         return Lexer.QUASIQUOTE_TOK
      elif nextChar == ',':
         buf.consume( )
         nextChar = buf.peekNextChar( )
         if nextChar == '@':
            buf.consume( )
            return Lexer.UNQUOTE_SPLICING_TOK
         return Lexer.UNQUOTE_TOK
      elif nextChar == '"':
         return self._scanStringLiteral( )
      elif nextChar in Lexer.SIGN_OR_DIGIT:
         return self._scanNumOrSymbol( )
      elif nextChar in Lexer.SYMBOL_FIRST:
         return self._scanSymbol( )
      else:
         raise ParseError( self, 'Unknown Token' )

   def _scanStringLiteral( self ) -> int:
      STRING_STOP = Lexer.STRING_STOP
      buf = self.buffer

      # Scan in the initial quote
      nextChar = buf.peekNextChar( )
      if nextChar != '"':
         raise ParseError( self, '\'"\' expected.' )
      buf.markStartOfLexeme( )
      buf.consume( )

      while True:
         buf.consumeUpTo( STRING_STOP )
         nextChar = buf.peekNextChar( )
         if nextChar != '\\':
            break
         self._consumeStringEscapeSequence( )

      # Scan past the final quote
      if nextChar != '"':
         raise ParseError( self, '\'"\' expected.  A string literal may be unterminated.' )
      buf.consume( )

      return Lexer.STRING_TOK

   def _consumeStringEscapeSequence( self ) -> None:
      buf = self.buffer

      nextChar = buf.peekNextChar( )
      if nextChar != '\\':
         raise ParseError( self,  '\\ expected.' )
      buf.consume( )             # Consume the \

      nextChar = buf.peekNextChar( )
      if nextChar in Lexer.OCTAL_DIGIT:
         # consume an octal number up to a 3 digits
         numCharsConsumed = buf.consumePastWithMax( Lexer.OCTAL_DIGIT, maxCharsToConsume=3 )
         if numCharsConsumed > 3:
            raise ParseError( self, '1 to 3 octal digits expected following escape character \\.' )
      elif nextChar == 'x':
         buf.consume( )    # consume the x
         # consume 2 digit hex number
         numDigitsConsumed = buf.consumePastWithMax( Lexer.HEX_DIGIT, maxCharsToConsume=2 )
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
         numDigitsConsumed = buf.consumePastWithMax( Lexer.HEX_DIGIT, maxCharsToConsume=4 )
         if numDigitsConsumed != 4:
            raise ParseError( self, 'Escape sequence expects exactly 4 hex digits following \\u.' )
      elif nextChar == 'U':
         buf.consume( )    # consume the U
         # consume 8 hex digits
         numDigitsConsumed = buf.consumePastWithMax( Lexer.HEX_DIGIT, maxCharsToConsume=8 )
         if numDigitsConsumed != 8:
            raise ParseError( self, 'Escape sequence expects exactly 8 hex digits following \\U.' )
      elif nextChar in Lexer.ESCAPE_SIMPLE:
         # consume 1 character
         buf.consume( )
      else:
         raise ParseError( self,  'invalid escape sequence.' )

   def _scanNumOrSymbol( self ) -> int:
      '''
      NumberLiteral:  [SIGN] DIGIT+                            # <-- leader
                         ( '/' DIGIT+                          # <-- fraction case
                         | 'e' [SIGN] DIGIT+                   # <-- exponentiation case
                         | '.' DIGIT+ [ 'e' [SIGN] DIGIT+ ]    # <-- decimal/exponentiation case
                         )
      If the token does not match a NumberLiteral, or if SYMBOL_REST characters
      trail a valid number, the entire token is returned as SYMBOL_TOK.
      '''
      SIGN  = Lexer.SIGN
      DIGIT = Lexer.DIGIT

      buf = self.buffer
      buf.markStartOfLexeme( )
      nextChar = buf.peekNextChar()
      if nextChar in SIGN:
         buf.consume()
         secondChar = buf.peekNextChar( )
         if (secondChar == '') or (secondChar not in DIGIT):
            buf.consumePast( Lexer.SYMBOL_REST )
            return Lexer.SYMBOL_TOK

      buf.consumePast( DIGIT )
      nextChar = buf.peekNextChar()

      if nextChar == '/':
         # Possibly a Fraction number
         buf.consume( )

         nextChar = buf.peekNextChar( )
         if nextChar not in DIGIT:
            buf.consumePast( Lexer.SYMBOL_REST )
            return Lexer.SYMBOL_TOK

         buf.consumePast( DIGIT )
         nextChar = buf.peekNextChar()
         if nextChar in Lexer.SYMBOL_REST:
            buf.consumePast( Lexer.SYMBOL_REST )
            return Lexer.SYMBOL_TOK
         return Lexer.FRAC_TOK

      elif nextChar in ('e', 'E'):
         # Exponentiation case
         buf.consume( )

         nextChar = buf.peekNextChar( )
         if (nextChar not in SIGN) and (nextChar not in DIGIT):
            buf.consumePast( Lexer.SYMBOL_REST )
            return Lexer.SYMBOL_TOK

         if nextChar in SIGN:
            buf.consume( )
            nextChar = buf.peekNextChar( )

         if nextChar not in DIGIT:
            buf.consumePast( Lexer.SYMBOL_REST )
            return Lexer.SYMBOL_TOK

         buf.consumePast( DIGIT )
         nextChar = buf.peekNextChar()
         if nextChar in Lexer.SYMBOL_REST:
            buf.consumePast( Lexer.SYMBOL_REST )
            return Lexer.SYMBOL_TOK
         return Lexer.FLOAT_TOK

      elif nextChar == '.':
         # Possibly a floating point number
         # '.' ('0' .. '9')+ [ 'e' ['+'|'-'] ('0' .. '9')+ ]    # <-- decimal/exponentiation case
         buf.consume()
         nextChar = buf.peekNextChar()
         if nextChar not in DIGIT:
            buf.consumePast( Lexer.SYMBOL_REST )
            return Lexer.SYMBOL_TOK

         buf.consumePast( DIGIT )
         nextChar = buf.peekNextChar( )

         if nextChar not in ('e', 'E'):
            if nextChar in Lexer.SYMBOL_REST:
               buf.consumePast( Lexer.SYMBOL_REST )
               return Lexer.SYMBOL_TOK
            return Lexer.FLOAT_TOK

         buf.consume( )
         nextChar = buf.peekNextChar( )

         if (nextChar not in SIGN) and (nextChar not in DIGIT):
            buf.consumePast( Lexer.SYMBOL_REST )
            return Lexer.SYMBOL_TOK

         if nextChar in SIGN:
            buf.consume( )
            nextChar = buf.peekNextChar( )

         if nextChar not in DIGIT:
            buf.consumePast( Lexer.SYMBOL_REST )
            return Lexer.SYMBOL_TOK

         buf.consumePast( DIGIT )
         nextChar = buf.peekNextChar()
         if nextChar in Lexer.SYMBOL_REST:
            buf.consumePast( Lexer.SYMBOL_REST )
            return Lexer.SYMBOL_TOK
         return Lexer.FLOAT_TOK

      if nextChar in Lexer.SYMBOL_REST:
         buf.consumePast( Lexer.SYMBOL_REST )
         return Lexer.SYMBOL_TOK
      return Lexer.INTEGER_TOK

   def _scanSymbol( self ) -> int:
      buf = self.buffer

      buf.markStartOfLexeme( )
      nextChar = buf.peekNextChar()
      if nextChar not in Lexer.SYMBOL_FIRST:
         raise ParseError( self, 'Invalid symbol character' )
      buf.consume( )

      buf.consumePast( Lexer.SYMBOL_REST )

      return Lexer.SYMBOL_TOK

   def _skipWhitespaceAndComments( self ) -> None:
      buf = self.buffer
      while True:
         buf.consumePast( Lexer.WHITESPACE )
         if buf.peekNextChar() != ';':
            return
         buf.consumeUpTo( Lexer.NEWLINE )


class Parser( ParserBase ):
   def __init__( self ) -> None:
      self._scanner    = Lexer( )

   def parse( self, source: str ) -> Any:  # Returns an AST of inputString
      self._scanner.reset( source )
      return self._parse( )
   
   def parseFile( self, filename: str ) -> Any:
      self._scanner.resetFromFile( filename )
      return self._parse( )

   def parseOne( self, source: str ):
      """Parse exactly one s-expression from the start of source.
      Returns (ast, chars_consumed) where chars_consumed is the number of
      characters consumed from source, including the expression itself and
      any trailing whitespace/comments up to the next expression.
      Raises ParseError if source contains no parseable expression."""
      self._scanner.reset( source )
      if self._scanner.peekToken( ) == Lexer.EOF_TOK:
         raise ParseError( self._scanner, 'End of file: no expression found.' )
      ast = self._parseObject( )
      tok = self._scanner.peekToken( )
      buf = self._scanner.buffer
      if tok == Lexer.EOF_TOK:
         chars_consumed = buf.point()
      elif tok == Lexer.UNQUOTE_SPLICING_TOK:
         chars_consumed = buf.point() - 2
      elif tok in ( Lexer.OPEN_PAREN_TOK, Lexer.CLOSE_PAREN_TOK,
                    Lexer.SINGLE_QUOTE_TOK, Lexer.QUASIQUOTE_TOK,
                    Lexer.UNQUOTE_TOK ):
         chars_consumed = buf.point() - 1
      else:
         chars_consumed = buf.mark()
      return ast, chars_consumed

   def _parse( self ) -> Any:
      # Parse all the sexpressions and insert them into a lisp progn function
      bodyExpr = [ LSymbol('progn') ]
      while self._scanner.peekToken() != Lexer.EOF_TOK:
         bodyExpr.append( self._parseObject() )

      # EOF
      if self._scanner.peekToken( ) != Lexer.EOF_TOK:
         raise ParseError( self._scanner, 'EOF Expected.' )

      return bodyExpr

   def _parseObject( self ) -> Any: # Returns an AST or None if eof
      lex: str = ''           # Holds the lexeme string
      ast: Any = None         # Holds the parsed AST

      nextToken = self._scanner.peekToken( )
      if nextToken == Lexer.SYMBOL_TOK:
         lex = self._scanner.getLexeme( )
         self._scanner.consume( )
         # Expanded here rather than in the Expander so that quoted forms like
         # 'foo:bar correctly yield the list (: FOO BAR) as data, since the
         # Expander does not recurse into QUOTE/QUASIQUOTE bodies.
         if ':' in lex and not lex.startswith(':') and all(lex.split(':')):
            parts = lex.split(':')
            ast   = [LSymbol(':')]
            for part in parts:
               ast.append( LSymbol(part) )
         else:
            ast = LSymbol(lex)
      elif nextToken == Lexer.OPEN_PAREN_TOK:
         lex = '()'
         ast = self._parseList( )
      elif nextToken == Lexer.INTEGER_TOK:
         lex = self._scanner.getLexeme( )
         ast = int(lex)
         self._scanner.consume( )
      elif nextToken== Lexer.FLOAT_TOK:
         lex = self._scanner.getLexeme( )
         ast = float(lex)
         self._scanner.consume( )
      elif nextToken== Lexer.FRAC_TOK:
         lex = self._scanner.getLexeme( )
         ast = Fraction( lex )
         self._scanner.consume( )
      elif nextToken == Lexer.STRING_TOK:
         lex = self._scanner.getLexeme( )
         ast = _parse_string_literal( lex )
         self._scanner.consume( )
      elif nextToken == Lexer.SINGLE_QUOTE_TOK:
         self._scanner.consume( )
         subordinate = self._parseObject( )
         if subordinate is None:
            raise ParseError( self._scanner, 'Unexpected end of input after quote.' )
         ast = [ LSymbol('QUOTE'), subordinate ]
      elif nextToken == Lexer.QUASIQUOTE_TOK:
         self._scanner.consume( )
         subordinate = self._parseObject( )
         if subordinate is None:
            raise ParseError( self._scanner, 'Unexpected end of input after quasiquote.' )
         ast = [ LSymbol('QUASIQUOTE'), subordinate ]
      elif nextToken == Lexer.UNQUOTE_TOK:
         self._scanner.consume( )
         subordinate = self._parseObject( )
         if subordinate is None:
            raise ParseError( self._scanner, 'Unexpected end of input after unquote.' )
         ast = [ LSymbol('UNQUOTE'), subordinate ]
      elif nextToken == Lexer.UNQUOTE_SPLICING_TOK:
         self._scanner.consume( )
         subordinate = self._parseObject( )
         if subordinate is None:
            raise ParseError( self._scanner, 'Unexpected end of input after unquote-splicing.' )
         ast = [ LSymbol('UNQUOTE-SPLICING'), subordinate ]
      elif nextToken == Lexer.EOF_TOK:
         ast = None
      else:
         raise ParseError( self._scanner, 'Object expected.' )

      return ast

   def _parseList( self ) -> list:
      scn = self._scanner
      
      theList = list( )

      # Open List
      nextToken = scn.peekToken()
      if nextToken != Lexer.OPEN_PAREN_TOK:
         raise ParseError( scn, '( expected.' )
      else:
         scn.consume( )

      # List Entries
      nextToken = scn.peekToken( )
      while nextToken not in (Lexer.CLOSE_PAREN_TOK, Lexer.EOF_TOK):
         theList.append( self._parseObject( ) )
         nextToken = scn.peekToken( )

      # Close List
      if nextToken != Lexer.CLOSE_PAREN_TOK:
         raise ParseError( scn, ') expected.')
      else:
         scn.consume( )

      return theList

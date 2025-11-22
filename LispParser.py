import ltk.Parser as Parser
from LispAST import LList, LSymbol

import fractions
from typing import Any

"""
The Language
------------
Lexemes
   Comments
      Comments extend from ';;' through '\n'.
      All text between and including these two delimiters is ignored.

   Delimiters:
      ';', '#', '(', ')', '|', '[', ']', ';;', '\n'

   Literals
      NumberLiteral:  ['+'|'-'] ('0' .. '9')+
                         ( '/' ('0' .. '9' )+
                         | 'e' ['+'|'-'] ('0' .. '9')+
                         | '.' ('0' .. '9')+ [ 'e' ['+'|'-'] ('0' .. '9')+ ]
                         )
      StringLiteral:  '"' (^('"'))* '"'
      Symbol:         'a..zA..Z+-~!$%^&*_=\\:/?<>'
                      { 'a..zA..Z+-~!$%^&*_=\\:/?<>0..9' }

   Reserved Symbols
         'nil', 'e', 'pi', 'inf', 'nan'

Grammar
   Start:
      Object EOF

   Object:
      NumberLiteral | StringLiteral | Symbol | List | '#' | '|' | ':' | '[' | ']'
      | "'" Object | "`" Object | "," Object | ",@" Object

   List:
      '(' Object* ')'
"""

class LispScanner( Parser.Scanner ):
   WHITESPACE     = ' \t\n\r'
   SIGN           = '+-'
   DIGIT          = '0123456789'
   SIGN_OR_DIGIT  = SIGN + DIGIT
   ALPHA_LOWER    = 'abcdefghijklmnopqrstuvwxyz'
   ALPHA_UPPER    = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
   ALPHA          = ALPHA_LOWER + ALPHA_UPPER
   SYMBOL_OTHER   = '~!$%^&*_=\\/?<>'
   SYMBOL_FIRST   = ALPHA + SIGN + SYMBOL_OTHER
   SYMBOL_REST    = ALPHA + SIGN + SYMBOL_OTHER + DIGIT + ':'

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

   SEMI_COLON_TOK     = 500    # Other Symbols
   POUND_SIGN_TOK     = 501
   PIPE_TOK           = 502
   COLON_TOK          = 503
   SINGLE_QUOTE_TOK   = 504
   COMMA_TOK = 505
   COMMA_AT_TOK = 506
   BACK_QUOTE_TOK = 507

   def __init__( self, ) -> None:
      super( ).__init__( )

   def _scanNextToken( self ) -> int:
      buf = self.buffer

      try:
         self._skipWhitespaceAndComments( )

         nextChar = buf.peek( )
         if nextChar == '':
            return LispScanner.EOF_TOK
         elif nextChar == '[':
            buf.markStartOfLexeme( )
            buf.consume( )
            return LispScanner.OPEN_BRACKET_TOK
         elif nextChar == ']':
            buf.markStartOfLexeme( )
            buf.consume( )
            return LispScanner.CLOSE_BRACKET_TOK
         elif nextChar == '(':
            buf.markStartOfLexeme( )
            buf.consume( )
            return LispScanner.OPEN_PAREN_TOK
         elif nextChar == ')':
            buf.markStartOfLexeme( )
            buf.consume( )
            return LispScanner.CLOSE_PAREN_TOK
         elif nextChar == ';':
            buf.markStartOfLexeme( )
            buf.consume( )
            return LispScanner.SEMI_COLON_TOK
         elif nextChar == '#':
            buf.markStartOfLexeme( )
            buf.consume( )
            return LispScanner.POUND_SIGN_TOK
         elif nextChar == '|':
            buf.markStartOfLexeme( )
            buf.consume( )
            return LispScanner.PIPE_TOK
         elif nextChar == ':':
            buf.markStartOfLexeme( )
            buf.consume( )
            nextChar = buf.peek( )
            return LispScanner.COLON_TOK
         elif nextChar == "'":
            buf.markStartOfLexeme( )
            buf.consume( )
            nextChar = buf.peek( )
            return LispScanner.SINGLE_QUOTE_TOK
         elif nextChar == '`':
            buf.markStartOfLexeme( )
            buf.consume( )
            nextChar = buf.peek( )
            return LispScanner.BACK_QUOTE_TOK
         elif nextChar == ',':
            buf.markStartOfLexeme( )
            buf.consume( )
            nextChar = buf.peek( )
            if nextChar == '@':
               buf.consume( )
               nextChar = buf.peek( )
               return LispScanner.COMMA_AT_TOK
            else:
               return LispScanner.COMMA_TOK
         elif nextChar == '"':
            return self._scanStringLiteral( )
         elif nextChar in LispScanner.SIGN_OR_DIGIT:
            return self._scanNumOrSymbol( )
         elif nextChar in LispScanner.SYMBOL_FIRST:
            return self._scanSymbol( )
         else:
            raise Parser.ParseError( self, 'Unknown Token' )

      except Parser.ParseError:
         raise

      except:
         return LispScanner.EOF_TOK

   def _scanStringLiteral( self ) -> int:
      buf = self.buffer

      nextChar = buf.peek( )
      if nextChar != '"':
         raise Parser.ParseError( self, '\'"\' expected.' )
      buf.markStartOfLexeme( )
      buf.consume( )
      buf.consumeUpTo( '"' )
      buf.consume( )

      return LispScanner.STRING_TOK

   def _scanNumOrSymbol( self ) -> int:
      '''
      NumberLiteral:  ['+'|'-']('0' .. '9')+                                    # <-- leader
                         ( '/' ('0' .. '9')+                                    # <-- fraction case
                         | 'e' ['+'|'-'] ('0' .. '9')+                          # <-- exponentiation case
                         | '.' ('0' .. '9')+ [ 'e' ['+'|'-'] ('0' .. '9')+ ]    # <-- decimal/exponentiation case
                         )
      '''
      buf = self.buffer

      SAVE = Parser.ScannerState( )
      nextChar = buf.peek( )

      buf.markStartOfLexeme( )
      self.saveState( SAVE )                  # Save the scanner state

      buf.consume( )

      if nextChar in LispScanner.SIGN:
         secondChar = buf.peek( )
         if secondChar not in LispScanner.DIGIT:
            self.restoreState( SAVE )         # Restore the scanner state
            return self._scanSymbol( )

      buf.consumePast( LispScanner.DIGIT )
      nextChar = buf.peek()

      if nextChar == '/':
         # Possibly a Fraction number
         buf.consume( )

         nextChar = buf.peek( )
         if nextChar not in LispScanner.DIGIT:
            self.restoreState( SAVE )         # Restore the scanner state
            return self._scanSymbol( )

         buf.consumePast( LispScanner.DIGIT )
         return LispScanner.FRAC_TOK

      elif nextChar in ('e', 'E'):
         # Exponentiation case
         buf.consume( )

         nextChar = buf.peek( )
         if (nextChar not in LispScanner.SIGN) and (nextChar not in LispScanner.DIGIT):
            self.restoreState( SAVE )
            return self._scanSymbol( )

         if nextChar in LispScanner.SIGN:
            buf.consume( )
            nextChar = buf.peek( )

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
         nextChar = buf.peek()
         if nextChar not in LispScanner.DIGIT:
            # Integer
            self.restoreState( SAVE )
            return self._scanSymbol( )

         buf.consumePast( LispScanner.DIGIT )
         nextChar = buf.peek( )

         if nextChar not in ('e', 'E'):
            return LispScanner.FLOAT_TOK

         buf.consume( )
         nextChar = buf.peek( )

         if (nextChar not in LispScanner.SIGN) and (nextChar not in LispScanner.DIGIT):
            self.restoreState( SAVE )
            return self._scanSymbol( )

         if nextChar in LispScanner.SIGN:
            buf.consume( )
            nextChar = buf.peek( )

         if (nextChar not in LispScanner.DIGIT):
            self.restoreState( SAVE )
            return self._scanSymbol( )

         buf.consumePast( LispScanner.DIGIT )
         return LispScanner.FLOAT_TOK

      return LispScanner.INTEGER_TOK

   def _scanSymbol( self ) -> int:
      buf = self.buffer

      buf.markStartOfLexeme( )
      nextChar = buf.peek()
      if nextChar not in LispScanner.SYMBOL_FIRST:
         raise Parser.ParseError( self, 'Invalid symbol character' )
      buf.consume( )

      buf.consumePast( LispScanner.SYMBOL_REST )

      return LispScanner.SYMBOL_TOK

   def _skipWhitespaceAndComments( self ) -> None:
      buf = self.buffer

      SAVE = Parser.ScannerState( )

      nextChar = buf.peek()
      while (nextChar in '; \t\n\r') and (nextChar != ''):
         buf.consumePast( ' \t\n\r' )

         if buf.peek() == ';':
            self.saveState( SAVE )
            buf.consume()
            if buf.peek() == ';':
               buf.consumeUpTo( '\n\r' )
            else:
               self.restoreState( SAVE )
               return
         nextChar = buf.peek()


class LispParser( Parser.Parser ):
   def __init__( self ) -> None:
      self._scanner    = LispScanner( )

   def parse( self, inputString: str ) -> Any:  # Returns an AST of inputString
      self._scanner.reset( inputString )

      syntaxTree = self._parseObject( )

      # EOF
      if self._scanner.peekToken( ) != LispScanner.EOF_TOK:
         raise Parser.ParseError( self._scanner, 'EOF Expected.' )

      self._scanner.consume( )

      return syntaxTree

   def _parseObject( self ) -> Any: # Returns an AST or None if eof
      nextToken = self._scanner.peekToken( )
      lex: str = ''           # Holds the lexeme string
      ast: Any = None      # Holds the parsed AST

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
         ast    = fractions.Fraction( int(lex_num),
                                         int(lex_denom) )
         self._scanner.consume( )
      elif nextToken == LispScanner.STRING_TOK:
         lex = self._scanner.getLexeme( )
         ast = lex[1:-1]
         self._scanner.consume( )
      elif nextToken == LispScanner.SYMBOL_TOK:
         lex = self._scanner.getLexeme( ).upper( )   # Make symbols case insensative
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
         raise Parser.ParseError( self._scanner, 'Object expected.' )

      return ast

   def _parseList( self ) -> LList:
      theList = [ ]

      # Open List
      if self._scanner.peekToken( ) != LispScanner.OPEN_PAREN_TOK:
         raise Parser.ParseError( self._scanner, '( expected.' )
      else:
         self._scanner.consume( )

      # List Entries
      while self._scanner.peekToken( ) not in (LispScanner.CLOSE_PAREN_TOK,
                                               LispScanner.EOF_TOK):
         theList.append( self._parseObject( ) )

      # Close List
      if self._scanner.peekToken( ) != LispScanner.CLOSE_PAREN_TOK:
         raise Parser.ParseError( self._scanner, ') expected.')
      else:
         self._scanner.consume( )

      return LList( *theList )


def testLispScanner( ) -> None:
   scanner = LispScanner( )
   tokenList = scanner.tokenize("""(defmacro!! defun! (fnName argList &body body) `(def! ',fnName (lambda (,@argList) ,@body)))""")
   for token, lexeme in tokenList:
      print( f'{token:5}  {lexeme}' )

def testLispParser( ) -> None:
   xy = LispParser( )

   def test( anExpr ):
      print( '\n>>> ', anExpr )
      expr = xy.parse( anExpr )
      print( expr )

   test( '(one two three)' )
   test( '(one (two three) four)' )
   test( '((one) two)' )

if __name__ == '__main__':
   testLispScanner( )

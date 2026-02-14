from abc import ABC, abstractmethod
from typing import Any

class LexerState( object ):
   def __init__( self, filename: str = '', source: str = '', point: int = 0, mark: int = 0, lineNum: int = 0 ) -> None:
      self.lexer_tok: int             = 0
      self.buffer_filename:str = filename
      self.buffer_source: str   = source
      self.buffer_point: int    = point
      self.buffer_mark: int     = mark
      self.buffer_lineNum: int  = lineNum

class LexerBuffer( object ):
   def __init__( self ) -> None:
      '''Initialize a scanner buffer instance.'''
      self._filename:str = ''  # the source filename
      self._source:str  = ''   # the string to be analyzed lexically
      self._point:int   = 0    # the current scanner position
      self._mark:int    = 0    # the first character of the lexeme currently being scanned
      self._lineNum:int = 1    # the current line number

   def reset( self, source: str ) -> None:
      '''Re-initialize the instance over a new or the current string.'''
      self._filename = ''
      self._source = source
      self._point    = 0
      self._mark     = 0
      self._lineNum  = 1

   def resetFromFile( self, filename: str ) -> None:
      self._filename = filename
      with open(self._filename, 'r', encoding='utf-8') as file:
         self._source = file.read( )
      self._point    = 0
      self._mark     = 0
      self._lineNum  = 1
      
   def peekNextChar( self ) -> str:
      '''Return the next character in the buffer or an empty string if eof.'''
      try:
         return self._source[ self._point ]        # raises IndexError on EOF
      except IndexError:
         return ''

   def consume( self ) -> None:
      '''Advance the point by one character in the buffer.'''
      try:
         if self._source[ self._point ] == '\n':   # raises on EOF
            self._lineNum += 1
         self._point += 1
      except IndexError:
         pass          # Scanned past eof.  Return gracefully.

   def consumePast( self, aCharSet: str ) -> None:
      '''Consume up to the first character NOT in aCharSet.  Returns the number
      of characters consumed.'''
      try:
         while self._source[self._point] in aCharSet:   # raises on EOF
            self.consume( )
      except IndexError:
         pass    # scanned past eof.  Return gracefully.  Eof will be reported by

   def consumeUpTo( self, aCharSet: str ) -> int:
      '''Consume up to the first character in aCharSet.  Returns the number
      of characters consumed.'''
      try:
         while self._source[self._point] not in aCharSet:   # raises on EOF
            self.consume( )
      except IndexError:
         pass                # scanned past eof.  Return gracefully.

   def consumePastWithMax( self, aCharSet: str, maxCharsToConsume: int ) -> int:
      '''Consume up to the first character NOT in aCharSet.  Returns the number
      of characters consumed.'''
      numCharsConsumed = 0
      try:
         while (self._source[self._point] in aCharSet) and (numCharsConsumed < maxCharsToConsume):   # raises on EOF
            self.consume( )
            numCharsConsumed += 1
      except IndexError:
         pass    # scanned past eof.  Return gracefully.  Eof will be reported by
      return numCharsConsumed

   def consumeUpToWithMax( self, aCharSet: str, maxCharsToConsume: int ) -> int:
      '''Consume up to the first character in aCharSet.  Returns the number
      of characters consumed.'''
      numCharsConsumed = 0
      try:
         while (self._source[self._point] not in aCharSet) and (numCharsConsumed < maxCharsToConsume):   # raises on EOF
            self.consume( )
            numCharsConsumed += 1
      except IndexError:
         pass                # scanned past eof.  Return gracefully.
      return numCharsConsumed

   def saveState( self ) -> LexerState:
      return LexerState( filename=self._filename, source=self._source,
                           point=self._point, mark=self._mark,
                           lineNum=self._lineNum )

   def restoreState( self, stateInst: LexerState ) -> None:
      self._filename = stateInst.buffer_filename
      self._source   = stateInst.buffer_source
      self._point    = stateInst.buffer_point
      self._mark     = stateInst.buffer_mark
      self._lineNum  = stateInst.buffer_lineNum

   def markStartOfLexeme( self ) -> None:
      '''Set mark to the current vlaue of point to record the start of a lex.'''
      self._mark = self._point

   def getLexeme( self ) -> str:
      '''Returns the substring spanning from mark to point.'''
      return self._source[ self._mark : self._point ]

   def scanFilename( self ) -> str:
      return self._filename

   def scanLineNum( self ) -> int:
      '''Return the line num (first line is 1) of point.'''
      return self._lineNum

   def scanColNum( self ) -> int:
      '''Return the column numm (first column is 1) of point.'''
      return self._point - self._linePos( )

   def scanLineTxt( self ) -> str:
      '''Return the complete text of the line currently pointed to by point.'''
      fromIdx = self._linePos( )
      toIdx   = self._source.find( '\n', fromIdx )
      if toIdx == -1:
         return self._source[ fromIdx : ]
      else:
         return self._source[ fromIdx : toIdx ]

   def _linePos( self ) -> int:
      '''Return the index into the buffer string of the first character of the current line.'''
      if self._point >= len(self._source):
         theLinePos = self._source.rfind( '\n', 0, len(self._source) - 1 ) + 1
      else:
         theLinePos = self._source.rfind( '\n', 0, self._point ) + 1
      return 0 if theLinePos < 0 else theLinePos

class Lexer( ABC ):
   def __init__( self ) -> None:
      '''Initialize a Scanner instance.'''
      self.buffer:LexerBuffer  = LexerBuffer( )
      self._tok:int = -1               # The next token

   def reset( self, source: str ) -> None:
      '''Re-initialize the instance over a new string.'''
      self.buffer.reset( source )
      self.consume( )                           # prime the scanner.

   def resetFromFile( self, filename: str ) -> None:
      '''Re-initialize the instance over the contents of a source file.'''
      self.buffer.resetFromFile( filename )
      self.consume( )
   
   def peekToken( self ) -> int:
      '''Return the next (look ahead) token, but do not consume it.'''
      return self._tok

   def consume( self ) -> None:
      '''Advance the scanner to the next token/lexeme in the ScannerBuffer.'''
      self._tok = self._scanNextToken( )

   def getLexeme( self ) -> str:
      '''Return the next (look ahead) lexeme, but do not consume it.
      This should be called before consume.'''
      return self.buffer.getLexeme( )

   def saveState( self ) -> LexerState:
      '''Create a restore point (for backtracking).  The current
      state of the scanner is preserved under aStateName.'''
      stateInst = self.buffer.saveState( )
      stateInst.lexer_tok = self._tok
      return stateInst 

   def restoreState( self, stateInst: LexerState ) -> None:
      '''Restore a saved state (backtrack to the point where the restore point was made).'''
      self._tok = stateInst.lexer_tok
      self.buffer.restoreState( stateInst )

   @abstractmethod
   def _scanNextToken( self ) -> int:
      """Consume the next token (i.e. scan past it).  At the end of this method
      the scanner should be in the following state:
      ScannerBuffer._point, set one char past the last char of the lexeme.
      ScannerBuffer._mark,  set to the first character of the lexeme.
      return value,         the int value of the next token in the buffer
      """
      pass
 
class ParseError( Exception ):
   def __init__( self, aScanner: Lexer, errorMessage: str ) -> None:
      super().__init__( self._generateVerboseErrorString(srcfilename=aScanner.buffer.scanFilename(),
                                                         lineNum=aScanner.buffer.scanLineNum(),
                                                         colNum=aScanner.buffer.scanColNum(),
                                                         sourceLine=aScanner.buffer.scanLineTxt(),
                                                         errorMsg=errorMessage) )

   def _generateVerboseErrorString( self, srcfilename: str, lineNum: int, colNum: int, sourceLine: str, errorMsg: str ):
      indentStr = ' ' * ( colNum - 1 )
      return f'Syntax Error: "{srcfilename}" ({lineNum},{colNum})\n{sourceLine}\n{indentStr}^ {errorMsg}'


class Parser( ABC ):
   @abstractmethod
   def parse( self, source: str ) -> Any:  # Returns an AST of inputString
      pass

   @abstractmethod
   def parseFile( self, filename: str ) -> Any:  # Returns an AST of inputString
      pass


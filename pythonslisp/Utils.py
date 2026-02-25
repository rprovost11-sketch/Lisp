import os
import sys


def retrieveFileList( dirname ) -> list[str]:
   "Returns a list of all the filenames in the specified directory."
   testFileList = os.listdir( dirname )
   testFileList.sort()
   testFileList = [ f'{dirname}/{testFileName}' for testFileName in testFileList
                    if os.path.isfile( f'{dirname}/{testFileName}' ) ]
   return testFileList


def columnize( lst: list[str], displayWidth: int=80, file=None, itemColor=None ) -> None:
   """Display a list of strings as a compact set of columns.

   Each column is only as wide as necessary.
   Columns are separated by two spaces.
   If itemColor is an ANSI escape string, each item is wrapped in that color.
   """
   RESET = '\033[0m'
   size = len(lst)
   if size == 1:
      item = lst[0]
      print( f'{itemColor}{item}{RESET}' if itemColor else item, file=file )
      return
   # Try every row count from 1 upwards
   for nrows in range(1, len(lst)):
      ncols = (size+nrows-1) // nrows
      colwidths = []
      totwidth = -2
      for col in range(ncols):
         colwidth = 0
         for row in range(nrows):
            i = row + nrows*col
            if i >= size:
               break
            x = lst[i]
            colwidth = max(colwidth, len(x))
         colwidths.append(colwidth)
         totwidth += colwidth + 2
         if totwidth > displayWidth:
            break
      if totwidth <= displayWidth:
         break
   else:
      nrows = len(lst)
      ncols = 1
      colwidths = [0]
   for row in range(nrows):
      texts = []
      for col in range(ncols):
         i = row + nrows*col
         if i >= size:
            x = ""
         else:
            x = lst[i]
         texts.append(x)
      while texts and not texts[-1]:
         del texts[-1]
      for col in range(len(texts)):
         content = texts[col]
         padded  = content.ljust(colwidths[col])
         if itemColor and content:
            padding     = padded[len(content):]
            texts[col]  = f'{itemColor}{content}{RESET}{padding}'
         else:
            texts[col]  = padded
      print(str("  ".join(texts)), file=file )


def write_multiFile( outputString: str, *fileList ):
   """Write output to multiple output streams using print.
   fileList is a python list containing output file objects.
   An output file object of None prints directly to the screen."""
   for fileStream in fileList:
      print( outputString, end='\n', flush=True, file=fileStream )

class MultiWriter:
   # ANSI color constants — pass the attribute name as the color argument
   RESET          = '\033[0m'
   BOLD           = '\033[1m'
   DIM            = '\033[2m'
   BLACK          = '\033[30m'
   RED            = '\033[31m'
   GREEN          = '\033[32m'
   YELLOW         = '\033[33m'
   BLUE           = '\033[34m'
   MAGENTA        = '\033[35m'
   CYAN           = '\033[36m'
   WHITE          = '\033[37m'
   BRIGHT_BLACK   = '\033[90m'
   BRIGHT_RED     = '\033[91m'
   BRIGHT_GREEN   = '\033[92m'
   BRIGHT_YELLOW  = '\033[93m'
   BRIGHT_BLUE    = '\033[94m'
   BRIGHT_MAGENTA = '\033[95m'
   BRIGHT_CYAN    = '\033[96m'
   BRIGHT_WHITE   = '\033[97m'
   BOLD_WHITE     = '\033[1;97m'
   BOLD_GREEN     = '\033[1;92m'
   BOLD_RED       = '\033[1;91m'

   @staticmethod
   def _ansi( color: str | None ) -> str:
      "Resolve a color constant name to its ANSI escape sequence, or '' if None/unknown."
      if not color:
         return ''
      return getattr( MultiWriter, color.upper(), '' )

   def __init__( self, fileList: list ):
      self._fileList = fileList

   def write( self, *msgs: list[str], sep=' ', color=None ):
      ansi = MultiWriter._ansi( color )
      for fileStream in self._fileList:
         if ansi and fileStream is None:
            colored = [ f'{ansi}{m}{MultiWriter.RESET}' for m in msgs ]
            print( *colored, sep=sep, end='', file=fileStream )
         else:
            print( *msgs, sep=sep, end='', file=fileStream )

   def writeln( self, *msgs: list[str], sep=' ', color=None ):
      ansi = MultiWriter._ansi( color )
      for fileStream in self._fileList:
         if ansi and fileStream is None:
            colored = [ f'{ansi}{m}{MultiWriter.RESET}' for m in msgs ]
            print( *colored, sep=sep, end='\n', file=fileStream )
         else:
            print( *msgs, sep=sep, end='\n', file=fileStream )

   def flushAll( self ):
      for fileStream in self._fileList:
         if fileStream is None:
            sys.stdout.flush()
         else:
            fileStream.flush()

   def columnize( self, lst: list[str], displaywidth: int = 80, color=None ):
      for fileStream in self._fileList:
         if fileStream is None:
            MultiWriter.columnize_singleFile( lst, displaywidth, color=color )
         else:
            MultiWriter.columnize_singleFile( lst, displaywidth, file=fileStream )

   @staticmethod
   def write_singleFile( *msgParts: list[str], sep=' ', file=None, color=None ):
      ansi = MultiWriter._ansi( color )
      if file is None:
         if ansi:
            colored = [ f'{ansi}{m}{MultiWriter.RESET}' for m in msgParts ]
            print( *colored, sep=sep, end='', flush=True )
         else:
            print( *msgParts, sep=sep, end='', flush=True )
      else:
         print( *msgParts, sep=sep, end='', flush=True, file=file )

   @staticmethod
   def writeln_singleFile( *msgParts: list[str], sep=' ', file=None, color=None ):
      ansi = MultiWriter._ansi( color )
      if file is None:
         if ansi:
            colored = [ f'{ansi}{m}{MultiWriter.RESET}' for m in msgParts ]
            print( *colored, sep=sep, end='\n', flush=True )
         else:
            print( *msgParts, sep=sep, end='\n', flush=True )
      else:
         print( *msgParts, sep=sep, end='\n', flush=True, file=file )

   @staticmethod
   def columnize_singleFile( lst: list[str], displayWidth: int = 80, file=None, color=None ) -> None:
      """Display a list of strings as a compact set of columns.

      Each column is only as wide as necessary.
      Columns are separated by two spaces.
      If color is a MultiWriter color constant name, each item is wrapped in that color.
      """
      ansi  = MultiWriter._ansi( color ) if file is None else ''
      RESET = MultiWriter.RESET
      size  = len(lst)
      if size == 1:
         item = lst[0]
         print( f'{ansi}{item}{RESET}' if ansi else item, file=file )
         return
      # Try every row count from 1 upwards
      for nrows in range(1, len(lst)):
         ncols = (size+nrows-1) // nrows
         colwidths = []
         totwidth = -2
         for col in range(ncols):
            colwidth = 0
            for row in range(nrows):
               i = row + nrows*col
               if i >= size:
                  break
               x = lst[i]
               colwidth = max(colwidth, len(x))
            colwidths.append(colwidth)
            totwidth += colwidth + 2
            if totwidth > displayWidth:
               break
         if totwidth <= displayWidth:
            break
      else:
         nrows = len(lst)
         ncols = 1
         colwidths = [0]
      for row in range(nrows):
         texts = []
         for col in range(ncols):
            i = row + nrows*col
            if i >= size:
               x = ""
            else:
               x = lst[i]
            texts.append(x)
         while texts and not texts[-1]:
            del texts[-1]
         for col in range(len(texts)):
            content = texts[col]
            padded  = content.ljust(colwidths[col])
            if ansi and content:
               padding     = padded[len(content):]
               texts[col]  = f'{ansi}{content}{RESET}{padding}'
            else:
               texts[col]  = padded
         print(str("  ".join(texts)), file=file )


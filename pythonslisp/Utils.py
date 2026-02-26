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

def writeln_multiFile( outputString: str, *fileList ):
   """Write output to multiple output streams using print.
   fileList is a python list containing output file objects.
   An output file object of None prints directly to the screen."""
   for fileStream in fileList:
      print( outputString, end='\n', flush=True, file=fileStream )


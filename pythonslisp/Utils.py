from __future__ import annotations
import os
import sys


def retrieveFileList( dirname ) -> list[str]:
   "Returns a sorted list of .log filenames in the specified directory."
   testFileList = os.listdir( dirname )
   testFileList.sort()
   testFileList = [ f'{dirname}/{testFileName}' for testFileName in testFileList
                    if os.path.isfile( f'{dirname}/{testFileName}' )
                    and testFileName.endswith('.log') ]
   return testFileList

def columnize( lst: list[str], displayWidth: int=80, file=None,
               itemColor=None, itemColors: list[str] | None=None ) -> None:
   """Display a list of strings as a compact set of columns.

   Each column is only as wide as necessary.
   Columns are separated by two spaces.
   itemColors: per-item ANSI color list (one entry per item in lst).
   itemColor:  single ANSI color applied to all items (used when itemColors is None).
   Column widths are computed from uncolored string lengths so color escapes do
   not affect alignment.
   """
   RESET = '\033[0m'
   size  = len(lst)
   if size == 0:
      return
   # Build per-item color list
   if itemColors is not None:
      colors = itemColors
   elif itemColor:
      colors = [itemColor] * size
   else:
      colors = [None] * size
   if size == 1:
      c = colors[0]
      print( f'{c}{lst[0]}{RESET}' if c else lst[0], file=file )
      return
   # Try every row count from 1 upwards until the layout fits displayWidth
   for nrows in range(1, len(lst)):
      ncols    = (size + nrows - 1) // nrows
      colwidths = []
      totwidth  = -2
      for col in range(ncols):
         colwidth = 0
         for row in range(nrows):
            i = row + nrows * col
            if i >= size:
               break
            colwidth = max(colwidth, len(lst[i]))
         colwidths.append(colwidth)
         totwidth += colwidth + 2
         if totwidth > displayWidth:
            break
      if totwidth <= displayWidth:
         break
   else:
      nrows     = len(lst)
      ncols     = 1
      colwidths = [0]
   for row in range(nrows):
      cells = []
      for col in range(ncols):
         i = row + nrows * col
         cells.append( (lst[i], colors[i]) if i < size else ('', None) )
      while cells and not cells[-1][0]:
         del cells[-1]
      rendered = []
      for col, (content, c) in enumerate(cells):
         padded = content.ljust(colwidths[col])
         if c and content:
            padding = padded[len(content):]
            rendered.append( f'{c}{content}{RESET}{padding}' )
         else:
            rendered.append( padded )
      print( '  '.join(rendered), file=file )

def paren_state( text: str ) -> tuple[int, bool]:
   """Count net open parentheses in text, ignoring string contents and ; comments.
   Returns (depth, in_string) where in_string is True if the scan ends inside a string."""
   depth     = 0
   in_string = False
   escape    = False
   i         = 0
   while i < len(text):
      ch = text[i]
      if escape:
         escape = False
      elif in_string:
         if ch == '\\':
            escape = True
         elif ch == '"':
            in_string = False
      else:
         if ch == '"':
            in_string = True
         elif ch == ';':
            while i < len(text) and text[i] != '\n':
               i += 1
         elif ch == '(':
            depth += 1
         elif ch == ')':
            depth -= 1
      i += 1
   return depth, in_string


def writeln_multiFile( outputString: str, *fileList, flush=False ):
   """Write output to multiple output streams using print.
   fileList is a python list containing output file objects.
   An output file object of None prints directly to the screen."""
   for fileStream in fileList:
      print( outputString, end='\n', flush=flush, file=fileStream )


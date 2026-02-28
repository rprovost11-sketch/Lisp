import os
import sys

from pythonslisp.Listener import Listener
from pythonslisp.LispInterpreter import LispInterpreter

LANGUAGE     = 'Python\'s Lisp'
VERSION      = '0.30.30'
AUTHOR       = 'Ronald Provost/Longo'
EMAIL        = 'ronLongo9@outlook.com'
PROJECT = 'https://github.com/rprovost11-sketch/Lisp'
_PACKAGE_DIR = os.path.dirname( os.path.abspath( __file__ ) )
TEST_DIR     = os.path.join( _PACKAGE_DIR, 'testing' )

USAGE = '''   USAGE:  python3.14 -m pythonslisp [options] [lispSourceFile]

Options:
  -h, --help           Display this help message and exit
  -v, --version        Display version number and exit

Arguments:
  lispSourceFile       Lisp source file to execute

- If no argument is specified, the Listener\'s REPL will execute.
- If a lisp source file name is provided, Python\'s Lisp will execute it.
'''

def main( ) -> None:
   argv = sys.argv        # argument values

   # Parse options
   source_file = None

   for arg in argv[1:]:
      if arg in ('-h', '--help'):
         print( f'{LANGUAGE} v{VERSION} by {AUTHOR}' )
         print( )
         print( USAGE )
         return
      elif arg in ('-v', '--version'):
         print( f'{LANGUAGE} v{VERSION} by {AUTHOR}' )
         return
      elif arg.startswith('-'):
         print( f'Error: Unknown option: {arg}\n\n{USAGE}', file=sys.stderr )
         sys.exit(1)
      else:
         # It's a source file
         if source_file is not None:
            print( f'Error: Multiple source files specified.\n\n{USAGE}', file=sys.stderr )
            sys.exit(1)
         source_file = arg

   interp = LispInterpreter()

   if source_file is None:
      # Enter the repl
      try:
         theListener = Listener( interp, language=LANGUAGE,
                                         version=VERSION,
                                         author=AUTHOR,
                                         email=EMAIL,
                                         project=PROJECT,
                                         testdir=TEST_DIR
                                         )
      except FileNotFoundError as ex:
         print( f'Runtime library directory not found: "{LispInterpreter.DEFAULT_LIB_DIR}"' )
         sys.exit(1)

      theListener.readEvalPrintLoop( )
   else:
      # Execute a lisp source file
      try:
         interp.reboot( )
         interp.evalFile( source_file )
      except FileNotFoundError as ex:
         print( f'File not found: "{ex.filename}"', file=sys.stderr )
         sys.exit(1)

if __name__ == '__main__':
   main( )

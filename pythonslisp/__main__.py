import os
import sys

from pythonslisp import __version__
from pythonslisp.ltk.Listener import Listener
from pythonslisp.Interpreter import Interpreter
from pythonslisp.Exceptions import LRuntimeError

LANGUAGE     = 'Python\'s Lisp'
VERSION      = __version__
AUTHOR       = 'Ronald Provost/Longo'
EMAIL        = 'ronLongo9@outlook.com'
PROJECT = 'https://github.com/rprovost11-sketch/Lisp'
_PACKAGE_DIR = os.path.dirname( os.path.abspath( __file__ ) )
TEST_DIR     = os.path.join( _PACKAGE_DIR, 'testing' )

USAGE = '''   USAGE:  python3.14 -m pythonslisp [options] [lispSourceFile]

Options:
  -h, --help           Display this help message and exit
  -v, --version        Display version number and exit
  -e, --eval EXPR      Evaluate EXPR (a quoted string) and print the result, then exit

Arguments:
  lispSourceFile       Lisp source file to execute

- If no argument is specified, the Listener\'s REPL will execute.
- If a lisp source file name is provided, Python\'s Lisp will execute it.
- -e and lispSourceFile are mutually exclusive.
'''

def main( ) -> None:
   argv = sys.argv        # argument values

   # Parse options
   source_file = None
   eval_expr   = None

   i = 1
   while i < len(argv):
      arg = argv[i]
      if arg in ('-h', '--help'):
         print( f'{LANGUAGE} v{VERSION} by {AUTHOR}' )
         print( )
         print( USAGE )
         return
      elif arg in ('-v', '--version'):
         print( f'{LANGUAGE} v{VERSION} by {AUTHOR}' )
         return
      elif arg in ('-e', '--eval'):
         i += 1
         if i >= len(argv):
            print( f'Error: {arg} requires an expression argument.\n\n{USAGE}', file=sys.stderr )
            sys.exit(1)
         eval_expr = argv[i]
      elif arg.startswith('-'):
         print( f'Error: Unknown option: {arg}\n\n{USAGE}', file=sys.stderr )
         sys.exit(1)
      else:
         # It's a source file
         if source_file is not None:
            print( f'Error: Multiple source files specified.\n\n{USAGE}', file=sys.stderr )
            sys.exit(1)
         source_file = arg
      i += 1

   if eval_expr is not None and source_file is not None:
      print( f'Error: -e/--eval and a source file are mutually exclusive.\n\n{USAGE}', file=sys.stderr )
      sys.exit(1)

   interp = Interpreter()

   if eval_expr is not None:
      # Evaluate expression and print result
      try:
         result = interp.eval( eval_expr )
         print( result )
      except LRuntimeError as ex:
         print( str(ex), file=sys.stderr )
         sys.exit(1)
      except Exception as ex:
         print( str(ex), file=sys.stderr )
         sys.exit(1)
   elif source_file is None:
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
         print( f'Runtime library directory not found: "{Interpreter.DEFAULT_LIB_DIR}"' )
         sys.exit(1)

      theListener.readEvalPrintLoop( )
   else:
      # Execute a lisp source file
      try:
         interp.evalFile( source_file )
      except FileNotFoundError as ex:
         print( f'File not found: "{ex.filename}"', file=sys.stderr )
         sys.exit(1)
      except LRuntimeError as ex:
         print( str(ex), file=sys.stderr )
         sys.exit(1)
      except Exception as ex:
         print( str(ex), file=sys.stderr )
         sys.exit(1)

if __name__ == '__main__':
   main( )

import sys

from pythonslisp.Listener import Listener, ListenerCommandError
from pythonslisp.LispInterpreter import LispInterpreter

LANGUAGE     = 'Python\'s Lisp'
VERSION      = '0.24.7'
AUTHOR       = 'Ronald Provost/Longo'
EMAIL        = 'ronLongo9@outlook.com'
TEST_DIR     = 'pythonslisp/testing'
LIBRARY_DIR  = 'pythonslisp/lib'
USAGE = '''python3.14 -m pythonslisp [lispSourceFile|-h|--help|-v|--version]

This command takes one optional argument.

- If no arguments are specified the Listener\'s repl will execute.
- If a lisp source file name is provided as the only argument, Python\'s Lisp
- will execute the source file.
- If -h or --help is the argument, then lisp displays this help message then exits.
- If -v or --version is the argument, then lisp displays the version number
then exits.
'''

def main( ) -> None:
   interp = LispInterpreter( runtimeLibraryDir=LIBRARY_DIR )

   argv = sys.argv        # argument values
   argc = len(argv)       # argument count
   if argc == 1:
      if argv[0] in ('-h', '--help'):
         print( f'{LANGUAGE} v{VERSION} by {AUTHOR}' )
         print( )
         print( USAGE )
         return
      elif argv[0] in ('-v', '--version'):
         print( f'{LANGUAGE} v{VERSION} by {AUTHOR}' )
         return
      
      # Enter the repl
      try:
         theListener = Listener( interp, language=LANGUAGE,
                                         version=VERSION,
                                         author=AUTHOR,
                                         email=EMAIL,
                                         testdir=TEST_DIR
                                         )
      except ListenerCommandError as ex:
         print( ex.args[-1] )
         sys.exit(2)
      
      theListener.readEvalPrintLoop( )
   elif argc == 2:
      # Execute a lisp source file
      sourceFilename = argv[1]
      interp.evalFile( sourceFilename )
   else:
      print( f'Error: Invalid number of arguments.\n{USAGE}', file=sys.stderr )
      sys.exit(1)

if __name__ == '__main__':
   main( )

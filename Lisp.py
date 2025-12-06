from ltk.Listener import Listener
from LispInterpreter import LispInterpreter

import sys

LANGUAGE    = 'Python\'s Lisp'
VERSION     = '0.5.0'
AUTHOR      = 'Ronald Provost/Longo'
EMAIL       = 'ronLongo9@outlook.com'
TEST_DIR    = 'testing'
LIBRARY_DIR = 'lib'

def main( ) -> None:
   interp = LispInterpreter( runtimeLibraryDir=LIBRARY_DIR )

   argv = sys.argv        # argument values
   argc = len(argv)       # argument count
   if argc == 1:
      theListener = Listener( interp, language=LANGUAGE,
                                      version=VERSION,
                                      author=AUTHOR,
                                      email=EMAIL,
                                      testdir=TEST_DIR
                                      )
      theListener.readEvalPrintLoop( )
   elif argc == 2:
      # Execute a lisp source file
      sourceFilename = argv[1]
      interp.evalFile( sourceFilename )
   else:
      print( 'Error: Invalid number of arguments.' )

if __name__ == '__main__':
   main( )

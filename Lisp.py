from ltk.Listener import Listener
from LispInterpreter import LispInterpreter

import sys

def main( ) -> None:
   interp = LispInterpreter( )
   theListener = Listener( interp, language='Python\'s Lisp',
                                   version='0.4.0',
                                   author='Ronald Provost/Longo',
                                   email='ronLongo9@outlook.com',
                                   libdir='lib',
                                   testdir='testing'
                                   )

   argv = sys.argv        # argument values
   argc = len(argv)       # argument count
   if argc == 1:
      theListener.readEvalPrintLoop( )
   elif argc == 2:
      # Execute a lisp source file
      sourceFilename = argv[1]
      interp.eval2( sourceFilename )
   else:
      print( 'Error: Invalid number of arguments.' )

if __name__ == '__main__':
   main( )

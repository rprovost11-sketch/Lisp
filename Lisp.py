from ltk.Listener import Listener
from LispInterpreter import LispInterpreter

import sys

def main( ) -> None:
   interp = LispInterpreter( )
   theListener = Listener( interp, language='Python\'s Lisp',
                                   version='0.2.2',
                                   author='Ronald Provost/Longo',
                                   email='ronLongo9@outlook.com',
                                   libdir='lib',
                                   testdir='testing'
                                   )

   argv = sys.argv        # argument values
   argc = len(argv)       # argument count
   if argc == 2:
      # Execute a lisp source file
      sourceFilename = argv[1]
      theListener.sourceFile_exec( sourceFilename )
      return
   elif argc == 3:
      # Execute a listener session log file
      if argv[1] != '-l':
         print( "Error: Invalid command line switch." )
         return
      logFilename = argv[2]
      theListener.sessionLog_restore( logFilename )
   elif argc > 3:
      print( 'Error: Invalid number of arguments.' )
      return

   theListener.readEvalPrintLoop( )

if __name__ == '__main__':
   main( )

from ltk.Listener import Listener
from LispInterpreter import LispInterpreter

import os
import sys

def main( ) -> None:
   interp = LispInterpreter( )
   theListener = Listener( interp, language='Python\'s Lisp',
                                   version='0.1.6',
                                   author='Ronald Provost/Longo',
                                   email='ronLongo9@outlook.com',
                                   libdir='lib',
                                   testdir='testing'
                                   )

   cli_argv = sys.argv
   cli_argc = len(cli_argv)
   if cli_argc == 1:
      theListener.readEvalPrintLoop( )
   elif cli_argc == 2:
      # Execute a lisp source file
      sourceFilename = cli_argv[1]
      theListener.sourceFile_exec( sourceFilename )
   elif cli_argc == 3:
      # Execute a listener session log file
      if cli_argv[1].upper() != '-L':
         print( "Error: Invalid command line switch." )
         return
      logFilename = cli_argv[2]
      theListener.sessionLog_restore( logFilename )

if __name__ == '__main__':
   main( )

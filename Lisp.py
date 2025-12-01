from ltk.Listener import Listener
from LispInterpreter import LispInterpreter

def main( ) -> None:
   interp = LispInterpreter( )
   theListener = Listener( interp, language='Python\'s Lisp',
                                   version='0.1.5',
                                   author='Ronald Provost/Longo',
                                   libdir='lib',
                                   testdir='testing'
                                   )
   theListener.readEvalPrintLoop( )

if __name__ == '__main__':
   main( )

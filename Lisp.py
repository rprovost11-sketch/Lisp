from ltk.Listener import Listener
from LispInterpreter import LispInterpreter

def main( ) -> None:
   interp = LispInterpreter( )
   theListener = Listener( interp, language='Python\'s Lisp',
                                   version='0.1.6',
                                   author='Ronald Provost/Longo',
                                   email='ronLongo9@outlook.com',
                                   libdir='lib',
                                   testdir='testing'
                                   )
   theListener.readEvalPrintLoop( )

if __name__ == '__main__':
   main( )

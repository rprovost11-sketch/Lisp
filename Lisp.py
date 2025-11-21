import ltk.Listener as Listener
from LispInterpreter import LispInterpreter

def main( ) -> None:
   interp = LispInterpreter( )
   theListener = Listener.Listener( interp, language='Lisp',
                                            version='0.1',
                                            author='Ronald Provost' )
   theListener.readEvalPrintLoop( )

if __name__ == '__main__':
   main( )

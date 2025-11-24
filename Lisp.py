import ltk.Listener as Listener
from LispInterpreter import LispInterpreter

def main( ) -> None:
   interp = LispInterpreter( )
   theListener = Listener.Listener( interp, language='Python\'s Lisp',
                                            version='0.1.5',
                                            author='Ronald Provost/Longo',
                                            testdir='testing',
                                            libdir='lib' )
   theListener.readEvalPrintLoop( )

if __name__ == '__main__':
   main( )

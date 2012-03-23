import clr
clr.AddReference("Calico")

import sys
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
from pylogo.interpreter import Logo
from pylogo.reader import StringTokenizer
from pylogo.common import EOF, LogoError, LogoOutput

import System
import Calico

class LogoEngine(Calico.Engine):
    def PostSetup(self, calico):
        pass

    def Execute(self, text):
        Logo.REP(StringTokenizer(text))
        print("Ok")
        return True

    def ExecuteOld(self, text):
        Logo.push_tokenizer(StringTokenizer(text))
        # FIXME: stay in loop until EOF, or error occurs
        try:
            while True:
                try:
                    v = Logo.expr_top()
                except LogoError, e:
                    if str(e) != str(e.description):
                        System.Console.Error.WriteLine("%s: %s" % (e.description, e))
                    else:
                        System.Console.Error.WriteLine(e)
                    break
                except LogoOutput, e:
                    ##System.Console.Error.WriteLine(e)
                    v = e.message
                except KeyboardInterrupt:
                    if tokenizer.context:
                        tokenizer.context = []
                        System.Console.Error.WriteLine('Aborted')
                    else:
                        System.Console.Error.WriteLine("Bye")
                        return False
                except SystemExit:
                    return False
                except Exception, e:
                    import traceback
                    traceback.print_exc()
                    break
                if v is EOF:
                    break
                if v is not None:
                    print("%s" % repr(v))
        finally:
            Logo.pop_tokenizer()
        print("Ok")
        return True

    def ExecuteFile(self, filename):
        print("Run filename '%s'!" % filename)
        try:
            Logo.import_logo(filename)
        except LogoOutput, e:
            ##System.Console.Error.WriteLine(e)
            print(e.message)
            return True
        except Exception, e:
            import traceback
            traceback.print_exc()
            return False
        return True

    def ReadyToExecute(self, text):
        ## Return True if expression parses ok.
        return True

class LogoDocument(Calico.TextDocument):
    def GetAuthors(self):
        return System.Array[str]([
            "Ian Bicking <ianb@colorstudy.com>",
            "Doug Blank <dblank@cs.brynmawr.edu>"
        ])

class LogoLanguage(Calico.Language):
    def __init__(self):
        self.name = "logo"
        self.proper_name = "Logo"
        self.extensions = System.Array[str](["logo"])
        self.mimetype = "text/plain"
        self.LineComment = ";;"

    def MakeEngine(self, manager):
        self.engine = LogoEngine(manager)

    def MakeDocument(self, calico, filename):
        return LogoDocument(calico, filename, self.name, self.mimetype)

    def GetUseLibraryString(self, fullname):
        pass

def MakeLanguage():
    return LogoLanguage()


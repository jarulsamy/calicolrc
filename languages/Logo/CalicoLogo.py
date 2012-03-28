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
    Logo = Logo

    def PostSetup(self, calico):
        pass

    def Execute(self, text):
        try:
            Logo.REP(StringTokenizer(text))
        except Exception, e:
            print("Caught exception: '%s'" % e.message)
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
    Logo = Logo
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


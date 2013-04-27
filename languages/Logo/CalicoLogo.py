import clr
clr.AddReference("Calico")

import sys
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
sys.path.append(
    os.path.join(os.path.dirname(os.path.abspath(__file__)),
                 "pylogo"))
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

    def PostSetup(self, calico):
        dir = System.IO.DirectoryInfo(System.IO.Path.Combine(calico.path, "..", "modules"))
        for f in dir.GetFiles("*.dll"):
            assembly_name = f.FullName
            #print("Loading", assembly_name)
            dllname = f.Name.split(".")[0]
            try:
                clr.AddReference(f.Name)
                __import__(dllname)
            except:
                #print("Error loading", dllname, assembly_name)
                pass
##             assembly = System.Reflection.Assembly.LoadFile(assembly_name)
##             if assembly:
##                 ## initialize_module if possible
##                 try:
##                     for type in assembly.GetTypes():
##                         try:
##                             method = type.GetMethod("initialize_module")
##                             if method:
##                                 method.Invoke(type, Array[object]([calico.path, calico.OS]))
##                         except:
##                             try:
##                                 method = type.GetMethod("set_gui_thread_id")
##                                 if method:
##                                     method.Invoke(type, Array[object]([MainWindow.gui_thread_id]));
##                             except:
##                                 pass
##                 except:
##                  continue

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
        self.mimetype = "text/x-logo"
        self.LineComment = ";;"

    def MakeEngine(self, manager):
        self.engine = LogoEngine(manager)

    def MakeDocument(self, calico, filename):
        return LogoDocument(calico, filename, self.name, self.mimetype)

    def GetUseLibraryString(self, fullname):
        basename = System.IO.Path.GetFileNameWithoutExtension(fullname)
        return "import \"%s\n" % basename

def MakeLanguage():
    return LogoLanguage()


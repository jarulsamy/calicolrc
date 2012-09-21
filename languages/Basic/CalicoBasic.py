# Sample Language File

import clr
clr.AddReference("Calico")
import Calico
import System
clr.AddReference("gtk-sharp")
import Gtk
clr.AddReference("Mono.TextEditor") 
import Mono.TextEditor

import sys, os
sys.path.append(os.path.dirname(__file__))

import basic

class MyEngine(Calico.Engine):

    def Execute(self, text):
        self.interpreter.Execute(text)
        return True

    def ExecuteFile(self, filename):
        fp = file(filename)
        text = "".join(fp.readlines())
        fp.close()
        self.Execute(text + "\nRUN") ## add a "RUN" on the end
        return True

    def ReadyToExecute(self, text):
        return True

    def PostSetup(self, calico):
        self.interpreter = basic.BasicInterpreter(calico)

class MyDocument(Calico.TextDocument):
    def GetAuthors(self):
        return System.Array[str]([
            "Andrea Griffini <agriff@tin.it>",
            "Douglas S. Blank <dblank@cs.brynmawr.edu>",
        ])

class MyLanguage(Calico.Language):
    def __init__(self):
        self.name = "basic"
        self.proper_name = "Basic"
        self.extensions = System.Array[str](["bas"])
        self.mimetype = "text/x-basic"
        self.LineComment = "REM "
        
    def getExamples(self, menu):
    	pass

    def MakeEngine(self, manager):
        self.engine = MyEngine(manager)

    def MakeDocument(self, calico, filename):
        Mono.TextEditor.Highlighting.SyntaxModeService.LoadStylesAndModes(
            os.path.join(os.path.dirname(__file__), "SyntaxModes"))
        return MyDocument(calico, filename, self.name, self.mimetype)

    def GetUseLibraryString(self, fullname):
        return ""
        
    def getExamplesPath(self, root_path):
    	return os.path.join(os.path.dirname(__file__), "examples")

def MakeLanguage():
    return MyLanguage()

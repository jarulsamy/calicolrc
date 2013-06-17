# This file was constructed automatically by the Calico Project
# See http://calicoproject.org/ for more details.

import sys
import os
import clr                 # Allows connection to CLR/DLR bits
clr.AddReference("Calico") # Add reference to the main Calico.exe
import System
import Calico
clr.AddReference("Mono.TextEditor") 
import Mono.TextEditor

sys.path.append(os.path.dirname(__file__))

# Now, define the Document, Engine, and Language classes:
class MyLanguageEngine(Calico.Engine):
    def SetTraceOn(self, calico):
        import cash
        cash.setTrace(True)

    def SetTraceOff(self):
        import cash
        cash.setTrace(False)

    def RequestPause(self):
        import cash
        cash.setPause(True)

    def ConfigureTrace(self):
        pass

    def PostSetup(self, calico):
        """
        Do things here that you want to do once (initializations).
        """
        self.calico = calico

    def Execute(self, text, feedback=True):
        """
        This is where you do something for the text (code). This is
        the interpreter.
        """
        import cash
        return cash.executeLines(self.calico, text, [["<module>", 1, "'Calico Shell'"]])

    def ExecuteFile(self, filename):
        """
        This is the code that will interprete a file.
        """
        import cash
        fp = open(filename)
        text = "".join(fp.readlines())
        fp.close()
        return cash.executeLines(self.calico, text, [[filename, 1, "'Calico Run file'"]])

    def ReadyToExecute(self, text):
        """
        Return True if expression parses ok and you are ready
        to execute. If you return False, then the user can still
        interactively type text into the Calico Shell.
        """
        return True # because every line is a valid command

class MyLanguageDocument(Calico.TextDocument):
    """
    The Text Editor. Change to turn into a GUI of some other kind.
    """
    def GetAuthors(self):
        return System.Array[str]([
            "Name1 <name1@place.edu>",
            "Name2 <name2@place.edu>"
        ])

class MyLanguage(Calico.Language):
    """
    The Language class holds the Document and the Engine classes, and
    the languages properties.
    """
    def __init__(self):
        self.name = "console"
        self.proper_name = "Console"
        self.extensions = System.Array[str](["sh"])
        self.mimetype = "text/x-text"
        self.LineComment = "##"

    def MakeEngine(self, manager):
        """
        Make and hold the language singleton.
        """
        self.engine = MyLanguageEngine(manager)

    def MakeDocument(self, calico, filename):
        """
        Make a document and return it.
        """
        return MyLanguageDocument(calico, filename, self.name, self.mimetype)

    def GetUseLibraryString(self, fullname):
        """
        Given a path to a a library DLL, return the string to add to
        script.
        """
        pass

    def getExamplesPath(self, root_path):
        """
        Given the root_path to calico, return the path to the examples
        folder.
        """
        return os.path.join(os.path.dirname(__file__), "examples")

# And finally define a method of loading it:
def MakeLanguage():
    """
    Make an instance of the Language, and return it. Usually you do this
    just once, even for non-visible languages.
    """
    Mono.TextEditor.Highlighting.SyntaxModeService.LoadStylesAndModes(
        os.path.join(os.path.dirname(__file__), "SyntaxModes"))
    return MyLanguage()

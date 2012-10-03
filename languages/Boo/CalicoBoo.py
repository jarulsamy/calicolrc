#
# Calico - Scripting Environment
#
# Copyright (c) 2011, Doug Blank <dblank@cs.brynmawr.edu>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# $Id: $

from __future__ import division

# This file was constructed automatically by the Calico Project
# See http://calicoproject.org/ for more details.

import sys
import os
sys.path.append(os.path.dirname(__file__))
sys.path.append(os.path.dirname(__file__) + "/../../bin")
import clr                 # Allows connection to CLR/DLR bits
clr.AddReference("Calico") # Add reference to the main Calico.exe
import System
import Calico
clr.AddReference("Microsoft.Scripting.dll")
clr.AddReference("gtk-sharp")
clr.AddReference("gdk-sharp")
clr.AddReference("Boo.Lang.Interpreter.dll")
clr.AddReference("Boo.Lang.Compiler.dll")

import Boo.Lang.Interpreter
import Boo.Lang.Compiler

class InteractiveInterpreter(Boo.Lang.Interpreter.InteractiveInterpreter):
    """
    Subclassed to gain access to privates.
    """

# Now, define the Document, Engine, and Language classes:
class BooEngine(Calico.Engine):
    def PostSetup(self, calico):
        """
        Do things here that you want to do once (initializations).
        """
        self.calico = calico
        self.interpreter = InteractiveInterpreter()
        self.interpreter.RememberLastValue = True
        self.interpreter.SetValue("calico", calico)

    def Execute(self, text, feedback=True):
        """
        This is where you do something for the text (code). This is
        the interpreter.
        """
        compiler_context = self.interpreter.Eval(text)
        error = False
        for e in compiler_context.Errors:
            if e.Code != 'BCE0034': # side-effects, don't report
                System.Console.Error.WriteLine(e)
                error = True
        if error:
            return
        result = self.interpreter._lastValue
        if feedback and result:
            System.Console.WriteLine(result)
        return True

    def ExecuteFile(self, filename):
        """
        This is the code that will interprete a file.
        """
        System.Console.WriteLine("Run filename '%s'!" % filename)
        self.interpreter.EvalCompilerInput(Boo.Lang.Compiler.IO.FileInput(filename))
        return True

    def ReadyToExecute(self, text):
        """
        Return True if expression parses ok and you are ready
        to execute. If you return False, then the user can still
        interactively type text into the Calico Shell.
        """
        lines = text.split("\n")
        if lines[-1].strip() == "":
            return True 
        elif lines[-1].rstrip().endswith(":"):
            return False
        elif lines[-1].rstrip().endswith("\\"):
            return False
        elif lines[-1].startswith(" "):
            return False
        return True

class BooDocument(Calico.TextDocument):
    """
    The Text Editor. Change to turn into a GUI of some other kind.
    """
    def GetAuthors(self):
        return System.Array[str]([
            "Douglas Blank <dblank@cs.brynmawr.edu>"
        ])

class BooLanguage(Calico.Language):
    """
    The Language class holds the Document and the Engine classes, and
    the languages properties.
    """
    def __init__(self):
        self.name = "boo"
        self.proper_name = "Boo"
        self.extensions = System.Array[str](["boo"])
        self.mimetype = "text/x-boo"
        self.LineComment = "##"

    def MakeEngine(self, manager):
        """
        Make and hold the language singleton.
        """
        self.engine = BooEngine(manager)

    def MakeDocument(self, calico, filename):
        """
        Make a document and return it.
        """
        return BooDocument(calico, filename, self.name, self.mimetype)

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
    return BooLanguage()

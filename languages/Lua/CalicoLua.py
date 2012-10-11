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

# This file was constructed automatically by the Calico Project
# See http://calicoproject.org/ for more details.

from __future__ import print_function
import sys
import os
import clr                 # Allows connection to CLR/DLR bits
clr.AddReference("Calico") # Add reference to the main Calico.exe
import System
import Calico
clr.AddReference("Mono.TextEditor") 
import Mono.TextEditor
sys.path.append(os.path.dirname(__file__))
clr.AddReference('LuaSharp.dll')
clr.AddReference('LuaEnv.dll')
import LuaSharp
import LuaEnv
import traceback
import System

# Now, define the Document, Engine, and Language classes:
class LuaEngine(Calico.Engine):
    def PostSetup(self, calico):
        """
        Do things here that you want to do once (initializations).
        """
        try:
            self.state = LuaSharp.Lua()
        except:
            print("Lua low-level .DLL not installed; Lua will not work...")
        self.env_init = False

    def ExecuteFile(self, text):
        """
        This is where you do something for the text (code). This is
        the interpreter.
        """
        if not self.env_init:
            if self.handleEnvironment():
                LuaEnv.setEnvironment(self.state)
            self.env_init = True
        try:
            if self.handleEnvironment():
                LuaEnv.resetEnvironment(self.state)
            self.state.DoFile(filename)
        except:
            traceback.print_exc()
        return True

    def Execute(self, text, feedback=True):
        if not self.env_init:
            if self.handleEnvironment():
                LuaEnv.setEnvironment(self.state)
            self.env_init = True
        try:
            if self.handleEnvironment():
                LuaEnv.resetEnvironment(self.state)
            if text.strip().startswith("="):
                text = text.strip()
                text = text[1:]
                if text[-1] == ";":
                    text = text[:-1]
                text = "print(%s);" % text
            self.state.DoString(text)
        except:
            traceback.print_exc()
        if feedback:
            print("Done!")

    def ReadyToExecute(self, text):
        """
        Return True if expression parses ok and you are ready
        to execute. If you return False, then the user can still
        interactively type text into the Calico Shell.
        """
        return text.strip().startswith("=") or (text.split("\n")[-1].strip() == "" and len(text.split("\n")) > 0)


    def readit(self, what):
        # FIXME: can't call from the clib?
        # Same problem when trying to exit and ask to save?
        import Myro
        return Myro.ask(what)

    def handleEnvironment(self):
        return System.Environment.OSVersion.Platform != System.PlatformID.Win32NT

class LuaDocument(Calico.TextDocument):
    """
    The Text Editor. Change to turn into a GUI of some other kind.
    """
    def GetAuthors(self):
        return System.Array[str]([
            "Name1 <name1@place.edu>",
            "Name2 <name2@place.edu>"
        ])

class LuaLanguage(Calico.Language):
    """
    The Language class holds the Document and the Engine classes, and
    the languages properties.
    """
    def __init__(self):
        self.name = "lua"
        self.proper_name = "Lua"
        self.extensions = System.Array[str](["lua"])
        self.mimetype = "text/x-lua"
        self.LineComment = "##"

    def MakeEngine(self, manager):
        """
        Make and hold the language singleton.
        """
        self.engine = LuaEngine(manager)

    def MakeDocument(self, calico, filename):
        """
        Make a document and return it.
        """
        return LuaDocument(calico, filename, self.name, self.mimetype)

    def GetUseLibraryString(self, fullname):
        """
        Given a path to a a library DLL, return the string to add to
        script.
        """
        pass

# And finally define a method of loading it:
def MakeLanguage():
    """
    Make an instance of the Language, and return it. Usually you do this
    just once, even for non-visible languages.
    """
    #Mono.TextEditor.Highlighting.SyntaxModeService.LoadStylesAndModes(
    #    os.path.join(os.path.dirname(__file__), "SyntaxModes"))
    return LuaLanguage()


    # def getVariables(self):
    #     """
    #     Get top-level variables.
    #     """
    #     #self.state.DoString("_ = {}; for i in pairs(_G) do _[i] = true; end");
    #     #return LuaEnv.ToList(self.state)
    #     return ["string", "xpcall", "package", "tostring", "gcinfo", "os",
    #             "table", "require", "getfenv", "debug", "next", "assert", "tonumber", "io",
    #             "rawequal", "collectgarbage", "getmetatable", "module", "rawset",
    #             "setmetatable", "math", "error", "pcall", "load", "setfenv", "type", "unpack",
    #             "_VERSION", "select", "ipairs", "print", "rawget", "loadstring", "pairs",
    #             "newproxy","dofile", "_G", "coroutine", "loadfile"]


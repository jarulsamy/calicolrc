#
# Pyjama - Scripting Environment
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

import sys
import clr
clr.AddReference("Mono.CSharp")

import Mono.CSharp
import System

from engine import Engine
from utils import Language, ConsoleStream

class CSharpEngine(Engine):
    def __init__(self, manager):
        super(CSharpEngine, self).__init__(manager, "csharp")
        # FIXME: set console outputs and errors for Evaluate
        self.engine = Mono.CSharp.Evaluator
        for assembly in System.AppDomain.CurrentDomain.GetAssemblies():
            try:
                self.engine.ReferenceAssembly(assembly)
            except:
                print "unable to load assembly"
                return
        self.engine.Init(System.Array[System.String]([]))
        # FIXME: make pyjama available in some manner
        #self.engine.Evaluate("pyjama", manager.pyjama)

    def execute(self, text):
        result = None
        # First, do the using lines:
        program = []
        for line in text.split("\n"):
            if line.strip() == "": continue
            if line.startswith("using "):
                self.engine.Run(line)
            else:
                program.append(line)
        # Next, everything else:
        if program:
            try:
                result = self.engine.Evaluate(line)
            except ValueError, exp:
                pass
        if result:
            self.stdout.write("%s\n" % result)

    def execute_file(self, filename):
        self.stdout.write("Run filename '%s'!\n" % filename)
        program = []
        # First, do the using lines:
        for line in file(filename):
            if line.startswith("using "):
                self.engine.Run(line);
            else:
                program += line
        # Next, everything else:
        if program:
            self.engine.Run("".join(program))
        return

    def ready_for_execute(self, text):
        """
        Return True if expression parses ok.
        """
        lines = text.split("\n")
        if lines[-1].strip() == "":
            return True
        elif lines[-1].startswith(" "):
            return False
        elif not lines[-1].rstrip().endswith(";"):
            return False
        return True

class CSharpLanguage(Language):
    def get_engine_class(self):
        return CSharpEngine

def register_language():
    return CSharpLanguage("csharp", "cs")

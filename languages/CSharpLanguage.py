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
        self.engine = Mono.CSharp.Evaluator
        for assembly in System.AppDomain.CurrentDomain.GetAssemblies():
            self.engine.ReferenceAssembly(assembly)
        self.engine.Init(System.Array[System.String]([]))
        #self.engine.DescribeTypeExpressions = True
        # FIXME: make pyjama available in some manner
        #self.engine.Evaluate("pyjama", manager.pyjama)

    def execute(self, text):
        # FIXME: set console outputs and errors for Evaluate
        result = None
        for line in text.split("\n"):
            if line.strip() == "": continue
            try:
                result = self.engine.Evaluate(line)
            except ValueError, exp:
                pass
        if result:
            self.stdout.write("%s\n" % result)

    def execute_file(self, filename):
        self.stdout.write("Run filename '%s'!\n" % filename)
        for line in file(filename):
            self.engine.Run(line);
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

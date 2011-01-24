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

import clr
import sys
from System.IO import StringReader
from sympl import sympl
from document import MakeDocument
from engine import Engine
from utils import Language

class SymplEngine(Engine):
    def __init__(self, manager):
        super(SymplEngine, self).__init__(manager, "sympl")
        self.engine = sympl.Sympl()
        self.scope =  self.engine.CreateScope()

    def execute(self, text):
        try:
            sympl.parser.ParseExpr(StringReader(text))
            result = self.engine.ExecuteExpr(text, self.scope)
            self.stdout.write("%s\n" % result)
        except Exception, e:
            System.Console.WriteLine(e)

    def execute_file(self, filename):
        self.engine.ExecuteFileInScope(filename, self.scope)

    def ready_for_execute(self, text):
        """
        Return True if expression parses ok.
        """
        lines = text.split("\n")
        if lines[-1] == "":
            return True # force it
        # else, only if valid parse
        try:
            sympl.parser.ParseExpr(StringReader(text))
            return True
        except Exception, e:
            return False

class Sympl(Language):
    def get_engine_class(self):
        return SymplEngine

    def get_document_class(self):
        return MakeDocument

def register_language():
    return Sympl("sympl", "sympl")



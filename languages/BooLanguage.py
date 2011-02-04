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
clr.AddReference("Boo.Lang.Interpreter.dll")
clr.AddReference("Boo.Lang.Compiler.dll")

import Boo.Lang.Interpreter
import Boo.Lang.Compiler

from engine import Engine
from utils import Language

class InteractiveInterpreter(Boo.Lang.Interpreter.InteractiveInterpreter):
    """
    Subclassed to gain access to privates.
    """

class BooEngine(Engine):
    def __init__(self, manager):
        super(BooEngine, self).__init__(manager, "boo")
        self.engine = InteractiveInterpreter()
        self.engine.RememberLastValue = True
        self.engine.SetValue("pyjama", manager.pyjama)

    def execute(self, text):
        compiler_context = self.engine.Eval(text)
        error = False
        for e in compiler_context.Errors:
            if e.Code != 'BCE0034': # side-effects, don't report
                print >> sys.stderr, e
                error = True
        if error:
            return
        result = self.engine._lastValue
        if result:
            self.stdout.write("%s\n" % result)

    def execute_file(self, filename):
        self.stdout.write("Run filename '%s'!\n" % filename)
        self.engine.EvalCompilerInput(Boo.Lang.Compiler.IO.FileInput(filename))

    def ready_for_execute(self, text):
        """
        Return True if expression parses ok.
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

class BooLanguage(Language):
    def get_engine_class(self):
        return BooEngine

def register_language():
    return BooLanguage("boo", "boo")

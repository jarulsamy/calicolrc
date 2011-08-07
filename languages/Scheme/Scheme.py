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

from __future__ import print_function
import clr
import System
import sys
import os

print(os.path.abspath("./languages/Scheme/Scheme"))
sys.path.append(os.path.abspath("./languages/Scheme/Scheme")) # lower dir, with PJScheme:
clr.AddReference("PJScheme.dll")
import PJScheme
from engine import Engine
from utils import Language

class SchemeEngine(Engine):
    def __init__(self, manager):
        super(SchemeEngine, self).__init__(manager, "scheme")
        self.engine = PJScheme

    def execute(self, text):
        result = self.engine.execute(text)
        # FIXME: when exceptions have a better format in Scheme:
        result = "%s" % result
        if result.startswith("(exception "):
            System.Console.Error.WriteLine(result)
        else:
            self.stdout.write("%s\n" % result)

    def execute_file(self, filename):
        self.stdout.write("Run filename '%s'!\n" % filename)
        result = self.engine.execute_file(filename)

    def setup(self):
        super(SchemeEngine, self).setup()
        self.engine.set_dlr(self.manager.scope, self.manager.runtime)

    def ready_for_execute(self, text):
        """
        Return True if expression parses ok.
        """
        lines = text.split("\n")
        if lines[-1] == "":
            return True # force it
        # else, only if valid parse
        retval = str(self.engine.try_parse_string(text))
        # FIXME: when exceptions have a better format in Scheme:
        return not retval.startswith("(exception ")

class Scheme(Language):
    def get_engine_class(self):
        return SchemeEngine

def register_language():
    return Scheme("scheme", "ss")


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
clr.AddReference("IronRuby")
clr.AddReference("IronRuby.Libraries")
clr.AddReference("Microsoft.Scripting")
import IronRuby
import Microsoft.Scripting
import System
from engine import DLREngine
from utils import Language
import os

class RubyEngine(DLREngine):
    def __init__(self, manager):
        super(RubyEngine, self).__init__(manager, "ruby")
        self.dlr_name = "rb"
        self.manager.scriptRuntimeSetup.LanguageSetups.Add(
             Microsoft.Scripting.Hosting.LanguageSetup(
                "IronRuby.Runtime.RubyContext, IronRuby",
                "IronRuby",
                ["IronRuby", "Ruby", "ruby", "rb"],
                [".rb"]))

    def setup(self):
        super(RubyEngine, self).setup()
        # FIXME: IronRuby bug: returns Array, doesn't take list
        paths = list(self.engine.GetSearchPaths())
        paths.Add(os.path.abspath("modules"))
        self.engine.SetSearchPaths(System.Array[str](paths))

    def start(self):
        # Load Languages so that Host System can find DLLs:
        self.engine.Runtime.LoadAssembly(
            System.Type.GetType(IronRuby.Hosting.RubyCommandLine).Assembly)
        self.engine.Runtime.LoadAssembly(
            System.Type.GetType(
             IronRuby.StandardLibrary.BigDecimal.Fraction).Assembly)


class Ruby(Language):
    def get_engine_class(self):
        return RubyEngine

def register_language():
    return Ruby("ruby", "rb")

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
clr.AddReference('LuaSharp.dll')
clr.AddReference('LuaEnv.dll')
import LuaSharp
import LuaEnv

from utils import Language
from engine import Engine
import traceback

class LuaEngine(Engine):
    def __init__(self, manager):
        super(LuaEngine, self).__init__(manager, "lua")
        self.engine = LuaSharp.Lua()
        self.env_init = False

    def readit(self, what):
        # FIXME: can't call from the clib?
        # Same problem when trying to exit and ask to save?
        import Myro
        return Myro.ask(what)

    def printit(self, *args):
        if len(args) == 0:
            self.manager.calico.shell.message("")
        elif len(args) == 1:
            value = args[0]
            if isinstance(value, float) and value == int(value):
                value = int(value)
            self.manager.calico.shell.message(value)
        else:
            for arg in args:
                value = arg
                if isinstance(value, float) and value == int(value):
                    value = int(value)
                self.manager.calico.shell.message(value, end="\t")
            self.manager.calico.shell.message("")            

    def execute(self, text):
        if not self.env_init:
            def printit(*args):
                self.printit(*args)
            def readit(format):
                self.readit(format)
            LuaEnv.setEnvironment(self.engine, printit, readit)
            self.env_init = True
        try:
            self.engine.DoString(text)
        except:
            traceback.print_exc()

    def execute_file(self, filename):
        if not self.env_init:
            def printit(*args):
                self.printit(*args)
            def readit(format):
                self.readit(format)
            LuaEnv.setEnvironment(self.engine, printit, readit)
            self.env_init = True
        try:
            self.engine.DoFile(filename)
        except:
            traceback.print_exc()

class Lua(Language):
    def get_engine_class(self):
        return LuaEngine

def register_language():
    return Lua("lua", "lua")

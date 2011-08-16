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
import LuaSharp

from utils import Language
from engine import Engine
import traceback

class LuaEngine(Engine):
    def __init__(self, manager):
        super(LuaEngine, self).__init__(manager, "lua")
        self.engine = LuaSharp.Lua()

    def execute(self, text):
        try:
            self.engine.DoString(text)
        except:
            traceback.print_exc()

    def execute_file(self, filename):
        try:
            self.engine.DoFile(filename)
        except:
            traceback.print_exc()

class Lua(Language):
    def get_engine_class(self):
        return LuaEngine

def register_language():
    return Lua("lua", "lua")

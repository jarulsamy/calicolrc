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

import System

class LuaEngine(Engine):
    def __init__(self, manager):
        super(LuaEngine, self).__init__(manager, "lua")

    def setup(self):
        self.state = LuaSharp.Lua()
        self.env_init = False

    def ready_for_execute(self, text):
        if super(LuaEngine, self).ready_for_execute(text):
            return True
        else:
            return text.strip().startswith("=")

    def readit(self, what):
        # FIXME: can't call from the clib?
        # Same problem when trying to exit and ask to save?
        import Myro
        return Myro.ask(what)

    def handleEnvironment(self):
        return System.Environment.OSVersion.Platform != System.PlatformID.Win32NT

    def execute(self, text):
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
            self.manager.calico.shell.message("Ok")
        except:
            traceback.print_exc()

    def execute_file(self, filename):
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

    def getVariables(self):
        """
        Get top-level variables.
        """
        #self.state.DoString("_ = {}; for i in pairs(_G) do _[i] = true; end");
        #return LuaEnv.ToList(self.state)
        return ["string", "xpcall", "package", "tostring", "gcinfo", "os",
                "table", "require", "getfenv", "debug", "next", "assert", "tonumber", "io",
                "rawequal", "collectgarbage", "getmetatable", "module", "rawset",
                "setmetatable", "math", "error", "pcall", "load", "setfenv", "type", "unpack",
                "_VERSION", "select", "ipairs", "print", "rawget", "loadstring", "pairs",
                "newproxy","dofile", "_G", "coroutine", "loadfile"]

    def tryGetVariable(self, variable):
        return (False, None)

class Lua(Language):
    def get_engine_class(self):
        return LuaEngine

def register_language():
    return Lua("lua", "lua")

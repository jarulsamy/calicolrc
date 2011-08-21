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
# $Id$

from __future__ import print_function
import clr
import traceback
import glob
import os

import Microsoft.Scripting
import Gtk
import System

from utils import CustomStream

class EngineManager(object):
    def __init__(self, calico):
        self.calico = calico
        self.scriptRuntimeSetup = Microsoft.Scripting.Hosting.ScriptRuntimeSetup()
        #self.scriptRuntimeSetup.DebugMode = True
        self.engine = {}

    def __getitem__(self, name):
        return self.engine[name]

    def get_languages(self):
        return sorted(self.engine.keys())

    def register(self, EngineClass):
        try:
            engine = EngineClass(self)
            self.engine[engine.language] = engine
        except:
            print("Skipping language %s" % EngineClass)

    def setup(self): 
        self.runtime = Microsoft.Scripting.Hosting.ScriptRuntime(
            self.scriptRuntimeSetup)
        self.scope = self.runtime.CreateScope()
        # Create calico as a module:
        self.scope.SetVariable("calico", self.calico)
        # set up other items which can be imported:
        #self.runtime.Globals.SetVariable("goodname", badname)
        #[x for x in self.runtime.Globals.GetVariableNames()]
        # Set up language engines:
        for engine in self.engine:
            self.engine[engine].setup()

    def set_redirects(self, stdout, stderr, stdin): # textviews
        self.stderr, self.stdout, self.stdin = stderr, stdout, stdin
        for engine in self.engine:
            self.engine[engine].set_redirects(self.stdout, self.stderr, self.stdin)

    def start(self):
        for engine in self.engine:
            self.engine[engine].start()

    def reset(self):
        self.setup()
        self.start()
        self.set_redirects(self.stdout, self.stderr, self.stdin)

class Engine(object):
    def __init__(self, manager, language):
        self.manager = manager
        self.language = language
        self.text_based = True

    def execute(self, text):
        raise NotImplemented

    def execute_file(self, filename):
        raise NotImplemented

    def ready_for_execute(self, text):
        """
        Is the text ready for executing?
        """
        # If more than one line in DLR, wait for a blank line
        lines = text.split("\n")
        line_count = len(lines)
        if line_count == 1:
            text = text.strip()
            return (text and text[-1] == ";") # if there is text, and last char is ;
        return lines[-1].strip() == "" # ok, if nothing

    def setup(self):
        pass

    def set_redirects(self, stdout, stderr, stdin): # textviews
        self.sterr = stderr
        self.stdout = stdout

    def start(self):
        pass

    def stop(self):
        pass

    def getVariables(self):
        """
        Get top-level variables.
        """
        return []

    def tryGetVariable(self, variable):
        return (False, None)

class DLREngine(Engine):
    def setup(self):
        self.last_retval = None
        self.engine = self.manager.runtime.GetEngine(self.dlr_name)
        #Python.SetTrace(engine, OnTraceBack); 
        # Load mscorlib.dll:
        self.engine.Runtime.LoadAssembly(
            System.Type.GetType(System.String).Assembly)
        # ---------------------------------------
        for file in glob.glob("modules/*.dll"):
            full_path = os.path.abspath(file)
            clr.AddReference(full_path)
        for assembly in clr.References:
            self.engine.Runtime.LoadAssembly(assembly)
        # ---------------------------------------
        self.engine.Runtime.LoadAssembly(System.Type.GetType(
                System.Diagnostics.Debug).Assembly)

    def set_redirects(self, stdout, stderr, stdin): # textviews
        super(DLREngine, self).set_redirects(stdout, stderr, stdin)
        if not self.manager.calico.debug:
            if stdout:
                self.engine.Runtime.IO.SetOutput(stdout, 
                                                 System.Text.Encoding.UTF8)
            if stderr:
                self.engine.Runtime.IO.SetErrorOutput(stderr,
                                                      System.Text.Encoding.UTF8)
            System.Console.SetOut(self.engine.Runtime.IO.OutputWriter)
            System.Console.SetError(self.engine.Runtime.IO.ErrorWriter)
            System.Console.Out.AutoFlush = True
            System.Console.Error.AutoFlush = True

    def ready_for_execute(self, text):
        """
        Is the text ready for executing?
        """
        # If more than one line in DLR, wait for a blank line
        lines = text.split("\n")
        line_count = len(lines)
        if line_count == 1:
            sctype = Microsoft.Scripting.SourceCodeKind.InteractiveCode
            source = self.engine.CreateScriptSourceFromString(text, sctype)
            return (source.GetCodeProperties() == 
                    Microsoft.Scripting.ScriptCodeParseResult.Complete)
        return lines[-1].strip() == ""

    def execute(self, text):
        self.manager.calico.last_error = ""
        sctype = Microsoft.Scripting.SourceCodeKind.InteractiveCode
        source = self.engine.CreateScriptSourceFromString(text, sctype)
        try:
            source.Compile()
        except:
            sctype = Microsoft.Scripting.SourceCodeKind.Statements
            source = self.engine.CreateScriptSourceFromString(text, sctype)
            try:
                source.Compile()
            except:
                traceback.print_exc()
                return False
        try:
            source.Execute(self.manager.scope)
        except Exception, e:
            if "Thread was being aborted" in e.message:
                self.manager.calico.shell.message("[Script stopped----------]")
            else:
                traceback.print_exc()
            return False
        # What was last thing printed?
        try:
            retval = self.engine.Execute("_")
        except:
            retval = None
        if retval != self.last_retval:
            if (isinstance(retval, Gtk.Widget) and 
                retval.Parent == None and 
                not retval.IsTopLevel):
                # errors here are terminal:
                #self.manager.calico.shell.show_widget(retval)
                #self.manager.calico.shell.message("") # newline
                pass # too many issues: displaying, Invoke
            self.last_retval = retval
        self.manager.calico.shell.message("Ok")
        return True

    def execute_file(self, filename):
        self.manager.calico.last_error = ""
        source = self.engine.CreateScriptSourceFromFile(filename)
        try:
            source.Compile()
        except:
            traceback.print_exc()
            return False
        try:
            source.Execute(self.manager.scope)
        except Exception, e:
            if "Thread was being aborted" in e.message:
                self.manager.calico.shell.message("[Script stopped----------]")
            else:
                traceback.print_exc()

    def getVariables(self):
        """
        Get top-level variables.
        """
        return self.manager.scope.GetVariableNames()

    def tryGetVariable(self, variable):
        return self.manager.scope.TryGetVariable(variable)

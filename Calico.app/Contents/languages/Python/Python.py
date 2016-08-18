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
clr.AddReference("IronPython")
clr.AddReference("IronPython.Modules")
clr.AddReference("Microsoft.Scripting")
import IronPython
import System
import Microsoft.Scripting
from engine import DLREngine
from utils import Language
import os

class PythonEngine(DLREngine):
    def __init__(self, manager): 
        super(PythonEngine, self).__init__(manager, "python")
        self.dlr_name = "py"
        self.language_setup = IronPython.Hosting.Python.CreateLanguageSetup(None)
        self.language_setup.Options["FullFrames"] = True
        # BUG? Options was read-only:
        #self.language_setup = Microsoft.Scripting.Hosting.LanguageSetup(
        #        "IronPython.Runtime.PythonContext, IronPython",
        #        "IronPython",
        #        ["IronPython", "Python", "python", "py"],
        #        [".py"])
        self.manager.scriptRuntimeSetup.LanguageSetups.Add(self.language_setup)

    def setup(self):
        super(PythonEngine, self).setup()
        self.engine.Runtime.LoadAssembly(
            System.Type.GetType(IronPython.Hosting.Python).Assembly)
        # Execute startup script in Python
        text = ("from __future__ import division, with_statement, print_function;" +
                "from Myro import ask;" + 
                "__builtins__['input'] = ask;" +
                "__builtins__['print'] = calico.Print;" +
                "del division, with_statement, ask, print_function;")
        sctype = Microsoft.Scripting.SourceCodeKind.Statements
        source = self.engine.CreateScriptSourceFromString(text, sctype)
        options = self.engine.GetCompilerOptions()
        options.PrintFunction = True
        options.AllowWithStatement = True
        #options.FullFrames = True
        self.compiler_options = options
        source.Compile()
        source.Execute(self.manager.scope)

        # Other possible options:
        #self.compiler_options.AllowWithStatement = True 
        #self.compiler_options.TrueDivision = True
        #('AbsoluteImports', False), 
        #('DontImplyDedent', False), 
        #('InitialIndent', None), 
        #('Interpreted', False), 
        #('Module', IronPython.Runtime.ModuleOptions.None), 
        #('ModuleName', None), 
        #('Optimized', False), 
        #('PrintFunction', False), 
        #('SkipFirstLine', False), 
        #('UnicodeLiterals', False), 
        #('Verbatim', False), 
        #setup = self.engine.Setup
        #setup.ExceptionDetail = True

    def start(self):
        paths = self.engine.GetSearchPaths()
        ## Let users find Calico modules:
        for folder in ["modules", "src"]:
            paths.Add(os.path.abspath(folder))
        self.engine.SetSearchPaths(paths)
        # Now that search paths are set:
        text = ("from debugger import Debugger;" + 
                "debug = Debugger(calico, True, True);" +
                "del Debugger;")
        self.engine.Execute(text, self.manager.scope)

class Python(Language):
    def get_engine_class(self):
        return PythonEngine

def register_language():
    return Python("python", ["py", "pyw"])


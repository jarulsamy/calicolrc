import clr
clr.AddReference("IronPython")
clr.AddReference("IronPython.Modules")
clr.AddReference("Microsoft.Scripting")
import IronPython
import System
import Microsoft.Scripting
from document import MakeDocument
from engine import DLREngine
from utils import Language
import os

class PythonEngine(DLREngine):
    def __init__(self, manager): 
        super(PythonEngine, self).__init__(manager, "python")
        self.dlr_name = "py"
        self.manager.scriptRuntimeSetup.LanguageSetups.Add(
            Microsoft.Scripting.Hosting.LanguageSetup(
                "IronPython.Runtime.PythonContext, IronPython",
                "IronPython",
                ["IronPython", "Python", "python", "py"],
                [".py"]))

    def setup(self):
        super(PythonEngine, self).setup()
        self.engine.Runtime.LoadAssembly(
            System.Type.GetType(IronPython.Hosting.Python).Assembly)
        # Execute startup script in Python
        text = ("from __future__ import division, with_statement;" +
                "del division, with_statement")
        sctype = Microsoft.Scripting.SourceCodeKind.Statements
        source = self.engine.CreateScriptSourceFromString(text, sctype)
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
        ## Let users find Pyjama modules:
        for folder in ["modules", "src"]:
            paths.Add(os.path.abspath(folder))
        self.engine.SetSearchPaths(paths)

class Python(Language):
    def get_engine_class(self):
        return PythonEngine

    def get_document_class(self):
        return MakeDocument

def register_language():
    return Python("python", "py")


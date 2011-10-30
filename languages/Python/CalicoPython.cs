//
//  CalicoPython.cs
//  
//  Author:
//       Douglas S. Blank <dblank@cs.brynmawr.edu>
// 
//  Copyright (c) 2011 The Calico Project
// 
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
// 
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
// 
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

using System;
using System.Collections.Generic;
using System.IO;
using Calico;

public class CalicoPythonEngine : DLREngine {

    public CalicoPythonEngine(EngineManager manager) : base(manager) {
        dlr_name = "py";
        scriptRuntimeSetup = new Microsoft.Scripting.Hosting.ScriptRuntimeSetup();
        Microsoft.Scripting.Hosting.LanguageSetup language = IronPython.Hosting.Python.CreateLanguageSetup(null);
        // Set LanguageSetup options here:
        language.Options["FullFrames"] = true; // for debugging
        scriptRuntimeSetup.LanguageSetups.Add(language);
    }

    public override void setup() {
        Console.WriteLine("setup!");
        runtime = new Microsoft.Scripting.Hosting.ScriptRuntime(scriptRuntimeSetup);
        Console.WriteLine("runtime: {0}", runtime);
        engine = runtime.GetEngine(dlr_name);
        // Set the compiler options here:
        compiler_options = engine.GetCompilerOptions();
        IronPython.Compiler.PythonCompilerOptions options = (IronPython.Compiler.PythonCompilerOptions)compiler_options;
        options.PrintFunction = true;
        options.AllowWithStatement = true;
        options.TrueDivision = true;
        Console.WriteLine("engine: {0}", engine);
        // If the manager.scope environment is not set yet, set it here:
        scope = runtime.CreateScope();
        // Otherwise, we can use one created by another language
    }

    public override void start() {
        ICollection<string> paths = engine.GetSearchPaths();
        // Let users find Calico modules:
        foreach (string folder in new string[] { "modules", "src" }) {
            paths.Add(Path.GetFullPath(folder));
        }
        engine.SetSearchPaths(paths);
    }
    /*
    // Now that search paths are set:
    string text = "from debugger import Debugger;" +
      "debug = Debugger(calico, True, True);" +
      "del Debugger;";
    engine.Execute(text, scope);
  }
  */

    public override bool execute(string text) {
        // This is called by RunInBackground() in the MainWindow
        //manager.calico.last_error = ""
        Microsoft.Scripting.SourceCodeKind sctype = Microsoft.Scripting.SourceCodeKind.InteractiveCode;
        Microsoft.Scripting.Hosting.ScriptSource source = engine.CreateScriptSourceFromString(text, sctype);
        try {
            if (compiler_options != null) {
                source.Compile(compiler_options);
            } else {
                source.Compile();
            }
        } catch {
            sctype = Microsoft.Scripting.SourceCodeKind.Statements;
            source = engine.CreateScriptSourceFromString(text, sctype);
            try {
                if (compiler_options != null) {
                    source.Compile(compiler_options);
                } else {
                    source.Compile();
                }
            } catch {
                //traceback.print_exc()
                return false;
            }
        }
        //try {
            source.Execute(scope);
        //} catch {
        //}
        return true;
    }
    /*
            except Exception, e:
            if "Thread was being aborted" in str(e.message):
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
     */
}

public class CalicoPythonLanguage : Language {

    public CalicoPythonLanguage(string name, string proper, 
				string [] extensions) : 
    base(name, proper, extensions) {
    }

    public override Engine make_engine(EngineManager manager) {
        return new CalicoPythonEngine(manager);
    }

    public static new Language RegisterLanguage() {
        return new CalicoPythonLanguage("python", 
					"Python",
					new string[] { "py", "pyw" });
    }
}

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

    public CalicoPythonEngine(LanguageManager manager) : base(manager) {
        dlr_name = "py";
        scriptRuntimeSetup = new Microsoft.Scripting.Hosting.ScriptRuntimeSetup();
        languageSetup = IronPython.Hosting.Python.CreateLanguageSetup(null);
        // Set LanguageSetup options here:
        languageSetup.Options["FullFrames"] = true; // for debugging
        scriptRuntimeSetup.LanguageSetups.Add(languageSetup); // add to local
    }

    public override void setup() {
        manager.scriptRuntimeSetup.LanguageSetups.Add(languageSetup);
    }

    public override void start() {
        engine = manager.scriptRuntime.GetEngine(dlr_name);  
        // Set the compiler options here:
        compiler_options = engine.GetCompilerOptions();
        IronPython.Compiler.PythonCompilerOptions options = (IronPython.Compiler.PythonCompilerOptions)compiler_options;
        options.PrintFunction = true;
        options.AllowWithStatement = true;
        options.TrueDivision = true;
        // Create a Python-only scope:
		scriptRuntime = new Microsoft.Scripting.Hosting.ScriptRuntime(scriptRuntimeSetup);
        scope = scriptRuntime.CreateScope();
		// Set paths:
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

    public override void set_redirects(CustomStream stdout, 
				       CustomStream stderr) {
      engine.Runtime.IO.SetOutput(stdout, System.Text.Encoding.UTF8);
      engine.Runtime.IO.SetErrorOutput(stderr,System.Text.Encoding.UTF8);
    }

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
            } catch (Exception e) {
                Microsoft.Scripting.Hosting.ExceptionOperations eo = engine.GetService<Microsoft.Scripting.Hosting.ExceptionOperations>();
                manager.stderr.PrintLine(eo.FormatException(e));
                return false;
            }
        }
        try {
            if (manager.UseSharedScope)
                source.Execute(manager.scope);
            else
                source.Execute(scope);
        } catch (Exception e) {
    	  if (e.Message.Contains("Thread was being aborted")) {
    	    manager.stderr.Print("[Script stopped----------]\n");
    	  } else {
            Microsoft.Scripting.Hosting.ExceptionOperations eo = engine.GetService<Microsoft.Scripting.Hosting.ExceptionOperations>();
            manager.stderr.PrintLine(eo.FormatException(e));
    	  }
    	  return false;
	    }
        manager.stderr.PrintLine(Tag.Info, "Ok");
	    return true;
    }
}

public class CalicoPythonLanguage : Language {

    public CalicoPythonLanguage(string name, string proper, 
				string [] extensions) : 
        base(name, proper, extensions) {
    }

    public override void MakeEngine(LanguageManager manager) {
        engine = new CalicoPythonEngine(manager);
    }

    public override Document MakeDocument(string filename) {
        return new TextDocument(filename, name);
    }

    public static new Language RegisterLanguage() {
        return new CalicoPythonLanguage("python", 
					"Python",
					new string[] { "py", "pyw" });
    }
}

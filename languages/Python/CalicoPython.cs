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
        // Create a Python-only scope:
	scriptRuntime = new Microsoft.Scripting.Hosting.ScriptRuntime(scriptRuntimeSetup);
        scope = scriptRuntime.CreateScope();
    }

    public override void Start() {
        // Get engine from manager:
        engine = manager.scriptRuntime.GetEngine(dlr_name);  
        // Set the compiler options here:
        compiler_options = engine.GetCompilerOptions();
        IronPython.Compiler.PythonCompilerOptions options = (IronPython.Compiler.PythonCompilerOptions)compiler_options;
        options.PrintFunction = true;
        options.AllowWithStatement = true;
        options.TrueDivision = true;
	// Set paths:
	ICollection<string> paths = engine.GetSearchPaths();
        // Let users find Calico modules:
        foreach (string folder in new string[] { "modules", "src" }) {
	  paths.Add(Path.GetFullPath(folder));
        }
        engine.SetSearchPaths(paths);
    }
}

public class CalicoPythonLanguage : Language {
	string mimetype;

    public CalicoPythonLanguage(string name, string proper, 
				string [] extensions, string mimetype) : 
        base(name, proper, extensions) {
		this.mimetype = mimetype;
    }

    public override void MakeEngine(LanguageManager manager) {
        engine = new CalicoPythonEngine(manager);
    }

    public override Document MakeDocument(MainWindow calico, string filename) {
      return new TextDocument(calico, filename, name, mimetype);
    }

    public static new Language MakeLanguage() {
        return new CalicoPythonLanguage("python", 
					"Python",
					new string[] { "py", "pyw" },
					"text/x-python");
    }
}

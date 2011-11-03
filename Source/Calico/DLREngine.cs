//
//  DLRLanguage.cs
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

namespace Calico {

    public class DLREngine : Engine {
        public string dlr_name;
        public Microsoft.Scripting.Hosting.ScriptRuntimeSetup scriptRuntimeSetup;
        public Microsoft.Scripting.Hosting.ScriptRuntime scriptRuntime;
        public Microsoft.Scripting.Hosting.LanguageSetup languageSetup;
        public Microsoft.Scripting.CompilerOptions compiler_options;
        public Microsoft.Scripting.Hosting.ScriptEngine engine;
        public Microsoft.Scripting.Hosting.ScriptScope scope;

        public DLREngine(LanguageManager manager) : base(manager) {
        }

        public override void Setup() {
	        manager.scriptRuntimeSetup.LanguageSetups.Add(languageSetup);
	    }

        public override void SetRedirects(CustomStream stdout, 
					  CustomStream stderr) {
	        engine.Runtime.IO.SetOutput(stdout, System.Text.Encoding.UTF8);
	        engine.Runtime.IO.SetErrorOutput(stderr,System.Text.Encoding.UTF8);
	    }

        public override bool Execute(string text) {
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
}

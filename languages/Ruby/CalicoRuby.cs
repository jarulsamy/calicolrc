//
//  CalicoRuby.cs
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

public class CalicoRubyEngine : DLREngine {

    public CalicoRubyEngine(LanguageManager manager) : base(manager) {
        dlr_name = "rb";
        scriptRuntimeSetup = new Microsoft.Scripting.Hosting.ScriptRuntimeSetup();
        languageSetup = IronRuby.Ruby.CreateRubySetup();
        // Set LanguageSetup options here:
        // languageSetup.Options["FullFrames"] = true; // for debugging Python only?
        scriptRuntimeSetup.LanguageSetups.Add(languageSetup);
    }

    public override void setup() {
        manager.scriptRuntimeSetup.LanguageSetups.Add(languageSetup);
    }

    public override void start() {
		engine = manager.scriptRuntime.GetEngine(dlr_name);
        // Set the compiler options here:
        compiler_options = engine.GetCompilerOptions();
        //IronRuby.Compiler.RubyCompilerOptions options = (IronRuby.Compiler.RubyCompilerOptions)compiler_options;
        // set some ruby options
        // Ruby-only scope:
        scriptRuntime = new Microsoft.Scripting.Hosting.ScriptRuntime(scriptRuntimeSetup);
        scope = scriptRuntime.CreateScope();
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
            Microsoft.Scripting.Hosting.ExceptionOperations eo = engine.GetService<Microsoft.Scripting.Hosting.ExceptionOperations>();
            manager.stderr.PrintLine(eo.FormatException(e));
            return false;
        }
        return true;
    }

    public override void set_redirects(CustomStream stdout, 
				       CustomStream stderr) {
      engine.Runtime.IO.SetOutput(stdout, System.Text.Encoding.UTF8);
      engine.Runtime.IO.SetErrorOutput(stderr,System.Text.Encoding.UTF8);
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

public class CalicoRubyLanguage : Language {
	string mimetype;

    public CalicoRubyLanguage(string name, string proper, 
				string [] extensions, string mimetype) : 
    base(name, proper, extensions) {
		this.mimetype = mimetype;
    }

    public override void MakeEngine(LanguageManager manager) {
        engine = new CalicoRubyEngine(manager);
    }

    public override Document MakeDocument(string filename) {
        return new TextDocument(filename, name, mimetype);
		
    }

    public static new Language MakeLanguage() {
        return new CalicoRubyLanguage("ruby", 
					"Ruby",
					new string[] { "rb"},
					"text/x-ruby");
    }
}

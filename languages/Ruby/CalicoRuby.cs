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
    // Ruby-only scope:
    scriptRuntime = new Microsoft.Scripting.Hosting.ScriptRuntime(scriptRuntimeSetup);
    scope = scriptRuntime.CreateScope();
  }

  public override void Start(string path) {
    // Get engine from manager:
    try {
      engine = manager.scriptRuntime.GetEngine(dlr_name);
    } catch {
      System.Console.Error.WriteLine("ERROR: Ruby failed to start");
      engine = null;
    }
    // Set the compiler options here:
    if (engine != null)
      compiler_options = engine.GetCompilerOptions();
    //onRuby.Compiler.RubyCompilerOptions options = (IronRuby.Compiler.RubyCompilerOptions)compiler_options;
    // set some ruby options
  }


  public override void PostSetup(MainWindow calico) {
    base.PostSetup(calico);
    // Set up input
    // "require 'stringio'; ostdout = $stdout; $stdout = fake = StringIO.new; $stdout = ostdout", 
    DirectoryInfo dir = new System.IO.DirectoryInfo(System.IO.Path.Combine(calico.path, 
									   System.IO.Path.Combine("..", "modules")));
    Execute(String.Format("$:.unshift File.join('{0}'); 'Ruby Loaded'", dir),
	    false);
  }

  public override bool Execute(string text, bool ok) {
    // This is called by RunInBackground() in the MainWindow
    //manager.calico.last_error = ""
    if (engine == null) {
      PrintLine("Please restart Calico to use this language.");
      return false;
    }
    Microsoft.Scripting.SourceCodeKind sctype = Microsoft.Scripting.SourceCodeKind.InteractiveCode;
    Microsoft.Scripting.Hosting.ScriptSource source = engine.CreateScriptSourceFromString(text, sctype);

    try {
      if (manager != null && manager.UseSharedScope)			  
	source.Execute(manager.scope);			  
      else
	source.Execute(scope);
    } catch (System.Threading.ThreadAbortException) {
      PrintLine("[Script stopped----------]");
      System.Threading.Thread.Sleep(100);
      System.Threading.Thread.ResetAbort();
    } catch (Exception e) {
      if (e.Message.Contains("Thread was being aborted")) {
	PrintLine("[Script stopped----------]");
      } else {
	Microsoft.Scripting.Hosting.ExceptionOperations eo = engine.GetService<Microsoft.Scripting.Hosting.ExceptionOperations>();
	PrintLine(eo.FormatException(e));
      }
      return false;
    }
    if (ok) {
      PrintLine(Tag.Info, "Ok");
    }
    return true;
  }

  public override bool ExecuteFile(string filename) {
    //manager.calico.last_error = ""
    //IronPython.Hosting.Python.GetSysModule(self.engine).settrace(self.trace)
    Microsoft.Scripting.Hosting.ScriptSource source = engine.CreateScriptSourceFromFile(filename);

    ConfigureTrace();
    try {
      if (manager != null && manager.UseSharedScope)
	source.Execute(manager.scope);
      else
	source.Execute(scope);
    } catch (System.Threading.ThreadAbortException) {
      PrintLine("[Script stopped----------]");
      System.Threading.Thread.Sleep(100);
      System.Threading.Thread.ResetAbort();
    } catch (Exception e) {
      if (e.Message.ToString().Contains("Thread was being aborted")) {
	PrintLine("[Script stopped----------]");
      } else {
	Microsoft.Scripting.Hosting.ExceptionOperations eo = engine.GetService<Microsoft.Scripting.Hosting.ExceptionOperations>();
	PrintLine(eo.FormatException(e));
      }
      return false;
    }
    PrintLine(Tag.Info, "Done");
    return true;
  }
  
}

public class CalicoRubyDocument : TextDocument {

  public CalicoRubyDocument(MainWindow calico, string filename, string language, string mimetype) :
  base(calico, filename, language, mimetype) {
  }
	
  public override string [] GetAuthors() 
  {
    return new string[] {
      "Microsoft Corporation",
      "The IronRuby Team"
    };
  }
}
	
public class CalicoRubyLanguage : Language {

  public CalicoRubyLanguage() :
  base("ruby", "Ruby", new string[] { "rb"}, "text/x-ruby") {
    LineComment = "##";
  }

  public override void MakeEngine(LanguageManager manager) {
    engine = new CalicoRubyEngine(manager);
  }

  public override Document MakeDocument(MainWindow calico, string filename) {
    return new CalicoRubyDocument(calico, filename, name, mimetype);
  }

  public static new Language MakeLanguage() {
    return new CalicoRubyLanguage();
  }

  public override string GetUseLibraryString(string fullname) {
    string bname = System.IO.Path.GetFileNameWithoutExtension(fullname);
    return String.Format("require \"{0}\"\n", bname);
  }
}

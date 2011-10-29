using System;
using System.Collections.Generic;
using System.IO;

public class CalicoPythonEngine : Calico.DLREngine {

  public CalicoPythonEngine() {
  }

  public override void set_manager(Calico.EngineManager manager) {
    this.manager = manager;
    dlr_name = "py";
    scriptRuntimeSetup = new Microsoft.Scripting.Hosting.ScriptRuntimeSetup();
    Microsoft.Scripting.Hosting.LanguageSetup language = IronPython.Hosting.Python.CreateLanguageSetup(null);
    language.Options["FullFrames"] = true;
	scriptRuntimeSetup.LanguageSetups.Add(language);
  }
	
	
	public override bool execute(string text) {
		//manager.calico.last_error = ""
        Microsoft.Scripting.SourceCodeKind sctype = Microsoft.Scripting.SourceCodeKind.InteractiveCode;
        Microsoft.Scripting.Hosting.ScriptSource source = engine.CreateScriptSourceFromString(text, sctype);
        try {
            //if self.compiler_options:
            //    source.Compile(self.compiler_options)
            //else:
            source.Compile();
		} catch {
            sctype = Microsoft.Scripting.SourceCodeKind.Statements;
            source = engine.CreateScriptSourceFromString(text, sctype);
            try {
                //if self.compiler_options:
                //    source.Compile(self.compiler_options)
                //else:
                source.Compile();
			} catch {
                //traceback.print_exc()
                return false;
			}
		}
        try {
            source.Execute(scope);
		} catch {
		}
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
	

  public override void setup() {
    Console.WriteLine("setup!");
    runtime = new Microsoft.Scripting.Hosting.ScriptRuntime(scriptRuntimeSetup);
    Console.WriteLine("runtime: {0}", runtime);
    engine = runtime.GetEngine(dlr_name);
    Console.WriteLine("engine: {0}", engine);
    scope = runtime.CreateScope();
    // Execute startup script in Python
    string text = ("from __future__ import division, with_statement, print_function;"
//		   "from Myro import ask;" + 
//		   "__builtins__['input'] = ask;" +
//		   "__builtins__['print'] = calico.Print;" +
//		   "del division, with_statement, ask, print_function;"
			);
    Microsoft.Scripting.SourceCodeKind sctype = Microsoft.Scripting.SourceCodeKind.Statements;
    Microsoft.Scripting.Hosting.ScriptSource source = engine.CreateScriptSourceFromString(text, sctype);
    Microsoft.Scripting.CompilerOptions options = engine.GetCompilerOptions();
    //options.PrintFunction = true;
    //options.AllowWithStatement = true;
    //options.FullFrames = true;
    //compiler_options = options;
    source.Compile();
    source.Execute(scope);

    // Other possible options:
    //self.compiler_options.AllowWithStatement = True 
    //self.compiler_options.TrueDivision = True
    //('AbsoluteImports', False), 
    //('DontImplyDedent', False), 
    //('InitialIndent', None), 
    //('Interpreted', False), 
    //('Module', IronPython.Runtime.ModuleOptions.None), 
    //('ModuleName', None), 
    //('Optimized', False), 
    //('PrintFunction', False), 
    //('SkipFirstLine', False), 
    //('UnicodeLiterals', False), 
    //('Verbatim', False), 
    //setup = self.engine.Setup
    //setup.ExceptionDetail = True
  }

  public override void start() {
    ICollection<string> paths = engine.GetSearchPaths();
    // Let users find Calico modules:
    foreach (string folder in new string [] {"modules", "src"}) {
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
}

public class CalicoPythonLanguage : Calico.Language {

  public CalicoPythonLanguage(string language, string [] extensions) : 
       base(language, extensions) {
  }

  public override Calico.Engine make_engine() {
    return new CalicoPythonEngine();
  }

  public new static Calico.Language RegisterLanguage() {
    return new CalicoPythonLanguage("python", 
				    new string[] {"py", "pyw"});
  }

}


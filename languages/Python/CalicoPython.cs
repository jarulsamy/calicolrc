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


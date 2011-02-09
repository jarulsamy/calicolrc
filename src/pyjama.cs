using System;
using IronPython.Hosting;
using Microsoft.Scripting;
using Microsoft.Scripting.Hosting;
using Mono.Unix;

public class Pyjama {
  static void Main(string[] args) {
  	Catalog.Init("pyjama","./locale");
	ScriptRuntimeSetup scriptRuntimeSetup = new ScriptRuntimeSetup();
        LanguageSetup language = Python.CreateLanguageSetup(null);
        language.Options["FullFrames"] = true;
	scriptRuntimeSetup.LanguageSetups.Add(language);
	ScriptRuntime runtime = new Microsoft.Scripting.Hosting.ScriptRuntime(scriptRuntimeSetup);
	ScriptScope scope = runtime.CreateScope();
	ScriptEngine engine = runtime.GetEngine("python");
	ScriptSource source = engine.CreateScriptSourceFromFile("src/pyjama.py");
	source.Compile();
	try {
	  source.Execute(scope);
	} catch (IronPython.Runtime.Exceptions.SystemExitException e) {
	  // Nothing to do but exit
	}
  }
}
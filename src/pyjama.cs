using System;
using IronPython.Hosting;
using Microsoft.Scripting;
using Microsoft.Scripting.Hosting;

public class Pyjama {
  static void Main(string[] args) {
	ScriptRuntimeSetup scriptRuntimeSetup = new ScriptRuntimeSetup();
	scriptRuntimeSetup.LanguageSetups.Add(Python.CreateLanguageSetup(null));
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
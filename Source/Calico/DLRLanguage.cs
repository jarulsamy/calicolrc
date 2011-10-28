namespace Calico {

	public class DLREngine : Engine {
	  public string dlr_name;
	  public Microsoft.Scripting.Hosting.ScriptRuntimeSetup scriptRuntimeSetup;
      public Microsoft.Scripting.Hosting.ScriptRuntime runtime;
      public Microsoft.Scripting.Hosting.ScriptScope scope;
	  public Microsoft.Scripting.Hosting.LanguageSetup language_setup;
	  public Microsoft.Scripting.CompilerOptions compiler_options;
	  public Microsoft.Scripting.Hosting.ScriptEngine engine;
	  public EngineManager manager;
	}
}
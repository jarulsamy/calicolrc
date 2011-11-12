using System;
using System.Collections.Generic;

namespace Jigsaw
{
	public class ExpressionEngine
	{
        public string dlr_name;
        public Microsoft.Scripting.Hosting.ScriptRuntimeSetup scriptRuntimeSetup;
        public Microsoft.Scripting.Hosting.ScriptRuntime scriptRuntime;
        public Microsoft.Scripting.Hosting.LanguageSetup languageSetup;
        public Microsoft.Scripting.CompilerOptions compiler_options;
        public Microsoft.Scripting.Hosting.ScriptEngine engine;
        public Microsoft.Scripting.Hosting.ScriptScope scope;
		
		public ExpressionEngine ()
		{
	        dlr_name = "py";
	        scriptRuntimeSetup = new Microsoft.Scripting.Hosting.ScriptRuntimeSetup();
	        languageSetup = IronPython.Hosting.Python.CreateLanguageSetup(null);
	        // Set LanguageSetup options here:
	        languageSetup.Options["FullFrames"] = true; // for debugging
	        scriptRuntimeSetup.LanguageSetups.Add(languageSetup); // add to local
	        // Create a Python-only scope:
			scriptRuntime = new Microsoft.Scripting.Hosting.ScriptRuntime(scriptRuntimeSetup);
	        scope = scriptRuntime.CreateScope();
			engine = scriptRuntime.GetEngine(dlr_name);  
	        compiler_options = engine.GetCompilerOptions();
	        IronPython.Compiler.PythonCompilerOptions options = (IronPython.Compiler.PythonCompilerOptions)compiler_options;
	        options.PrintFunction = true;
	        options.AllowWithStatement = true;
	        options.TrueDivision = true;
		}

		public object Evaluate(string exp) {
            Microsoft.Scripting.SourceCodeKind sctype = Microsoft.Scripting.SourceCodeKind.Expression;
            Microsoft.Scripting.Hosting.ScriptSource source = engine.CreateScriptSourceFromString(exp, sctype);
            source.Compile(compiler_options);
            return source.Execute(scope);
		}
		
		public Expression makeExpression(string text) {
			return new Expression(this, text);
		}
		
	}
	
	public class Expression {
		ExpressionEngine engine;
		string text;
		
		public Expression(ExpressionEngine engine, string text) {
			this.engine = engine;
			this.text = text;
		}
		
		public bool HasErrors() {
			return false;
		}
		public string ParsedExpression() {
			return text;
		}
		public void SetVariable(string variable, string text) {
			engine.SetVariable(variable, text);
		}
		public object Evaluate(string text) {
			return engine.Evaluate(text);
		}
	}
}

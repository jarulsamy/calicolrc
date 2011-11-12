using System;
using System.Collections.Generic;

namespace Jigsaw
{
	public class ExpressionEngine
	{
		Scope _scope;
		
		public ExpressionEngine() {
		}

		public virtual void SetScope(Scope scope) {
			_scope = scope;
		}

		public virtual void SetVariable(string variable, object value) {
			_scope = scope;
		}

		public virtual object Evaluate(string exp) {
            return null;
		}
		
		public virtual Expression makeExpression(string text) {
			return new Expression(this, text);
		}	
		
		public virtual Expression makeScope() {
			return new Scope(this);
		}	
	}
	
	public class Scope {
		public Scope() {
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

	public class DLRExpressionEngine : ExpressionEngine 
	{
        string dlr_name;
        public Microsoft.Scripting.Hosting.ScriptRuntimeSetup scriptRuntimeSetup;
        Microsoft.Scripting.Hosting.ScriptRuntime scriptRuntime;
        Microsoft.Scripting.Hosting.LanguageSetup languageSetup;
        Microsoft.Scripting.CompilerOptions compiler_options;
        Microsoft.Scripting.Hosting.ScriptEngine engine;
        Microsoft.Scripting.Hosting.ScriptScope dlr_scope;
		
		public ExpressionEngine () : base()
		{
	        dlr_name = "py";
	        scriptRuntimeSetup = new Microsoft.Scripting.Hosting.ScriptRuntimeSetup();
	        languageSetup = IronPython.Hosting.Python.CreateLanguageSetup(null);
	        // Set LanguageSetup options here:
	        languageSetup.Options["FullFrames"] = true; // for debugging
	        scriptRuntimeSetup.LanguageSetups.Add(languageSetup); // add to local
	        // Create a Python-only scope:
			scriptRuntime = new Microsoft.Scripting.Hosting.ScriptRuntime(scriptRuntimeSetup);
	        dlr_scope = scriptRuntime.CreateScope();
			engine = scriptRuntime.GetEngine(dlr_name);  
	        compiler_options = engine.GetCompilerOptions();
	        IronPython.Compiler.PythonCompilerOptions options = (IronPython.Compiler.PythonCompilerOptions)compiler_options;
	        options.PrintFunction = true;
	        options.AllowWithStatement = true;
	        options.TrueDivision = true;
		}
		
		public override void SetScope(DLRScope scope) {
			this.scope = scope;
			dlr_scope = scope.dlr_scope;
		}

		public override object Evaluate(string exp) {
            Microsoft.Scripting.SourceCodeKind sctype = Microsoft.Scripting.SourceCodeKind.Expression;
            Microsoft.Scripting.Hosting.ScriptSource source = engine.CreateScriptSourceFromString(exp, sctype);
            source.Compile(compiler_options);
            return source.Execute(dlr_scope);
		}
		
		public override Expression makeExpression(string text) {
			return new DLRExpression(this, text);
		}	
		public override Expression makeScope() {
			return new DLRScope(this);
		}	
	}
	
	public class DLRScope : Scope {
		public Microsoft.Scripting.Hosting.ScriptScope dlr_scope;
		public DLRExpressionEngine engine;
		public DLRScope(DLRExpressionEngine engine) {
			this.engine = engine;
			dlr_scope = engine.scriptRuntime.CreateScope()
		}
	}
	
	public class DLRExpression {
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

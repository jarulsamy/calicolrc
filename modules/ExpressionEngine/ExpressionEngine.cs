using System;
using System.Collections.Generic;

namespace Expression
{
	
	public class Engine {	
		// Public Static API
		public static ExpressionEngine TheEngine = new DLRExpressionEngine();
		public static Expression makeExpression(string text) {
			return TheEngine.makeExpression(text);
		}
		public static Scope makeScope() {
			return TheEngine.makeScope();
		}
		public static void SetVariable(string variable, object value) {
			TheEngine.SetVariable(variable, value);
		}
	}
	
	public class ExpressionEngine
	{
		// Internal API
		public ExpressionEngine ()
		{
		}
		
		public Scope scope { get; set; }

		public virtual void SetScope (Scope scope)
		{
			this.scope = scope;
		}

		public virtual object GetVariable(string text)
		{
			return null;
		}

		public virtual void SetVariable (string variable, object value)
		{
		}

		public virtual object Evaluate (string exp)
		{
			return null;
		}
		public virtual object Evaluate ()
		{
			return null;
		}
		
		public virtual Expression makeExpression ()
		{
			return new Expression (this, "");
		}
		
		public virtual Expression makeExpression (string text)
		{
			return new Expression (this, text);
		}
		
		public virtual Scope makeScope ()
		{
			return new Scope ();
		}	

		public virtual bool HasErrors (string text)
		{
			return false;
		}


	}
	
	public class Scope
	{
		public Scope ()
		{
		}
		
		public virtual IEnumerable<string> GetVariableNames() 
		{
			return null;
		}
		public virtual object GetVariable(string name)
		{
			return null;
		}
		public virtual void SetVariable(string name, object value)
		{
		}
	}
	
	public class Expression
	{
		ExpressionEngine engine;
		string text;
		
		public Expression (ExpressionEngine engine, string text)
		{
			this.engine = engine;
			this.text = text;
		}
		
		public Expression (string text)
		{
			this.engine = null;
			this.text = text;
		}
		
		public bool HasErrors ()
		{
			return engine.HasErrors(text);
		}

		public virtual string ParsedExpression ()
		{
			return text;
		}

		public virtual object GetVariable(string text)
		{
			return engine.GetVariable(text);
		}

		public virtual void SetVariable (string variable, object value)
		{
			engine.SetVariable (variable, value);
		}

		public virtual object Evaluate (string text)
		{
			return engine.Evaluate (text);
		}
		public virtual object Evaluate ()
		{
			return engine.Evaluate (text);
		}
		public virtual string ToRepr(object o)
		{
			 return text;
		}
		public void SetScope(Scope scope) 
		{
			engine.SetScope(scope);
		}
	}

	public class DLRExpressionEngine : ExpressionEngine
	{
		string dlr_name;
		public Microsoft.Scripting.Hosting.ScriptRuntimeSetup scriptRuntimeSetup;
		public Microsoft.Scripting.Hosting.ScriptRuntime scriptRuntime;
		Microsoft.Scripting.Hosting.LanguageSetup languageSetup;
		Microsoft.Scripting.CompilerOptions compiler_options;
		Microsoft.Scripting.Hosting.ScriptEngine engine;
		Microsoft.Scripting.Hosting.ScriptScope dlr_scope;
		
		public DLRExpressionEngine () : base()
		{
			dlr_name = "py";
			scriptRuntimeSetup = new Microsoft.Scripting.Hosting.ScriptRuntimeSetup ();
			languageSetup = IronPython.Hosting.Python.CreateLanguageSetup (null);
			// Set LanguageSetup options here:
			languageSetup.Options ["FullFrames"] = true; // for debugging
			scriptRuntimeSetup.LanguageSetups.Add (languageSetup); // add to local
			// Create a Python-only scope:
			scriptRuntime = new Microsoft.Scripting.Hosting.ScriptRuntime (scriptRuntimeSetup);
			dlr_scope = scriptRuntime.CreateScope ();
			engine = scriptRuntime.GetEngine (dlr_name);  
			compiler_options = engine.GetCompilerOptions ();
			IronPython.Compiler.PythonCompilerOptions options = (IronPython.Compiler.PythonCompilerOptions)compiler_options;
			options.PrintFunction = true;
			options.AllowWithStatement = true;
			options.TrueDivision = true;
		}
		
		public override void SetScope (Scope scope)
		{
			this.scope = scope;
			dlr_scope = ((DLRScope)scope).dlr_scope;
		}

		public override object Evaluate (string exp)
		{
			Microsoft.Scripting.SourceCodeKind sctype = Microsoft.Scripting.SourceCodeKind.Expression;
			Microsoft.Scripting.Hosting.ScriptSource source = engine.CreateScriptSourceFromString (exp, sctype);
			source.Compile (compiler_options);
			return source.Execute (dlr_scope);
		}
		
		public override Expression makeExpression (string text)
		{
			return new DLRExpression (this, text);
		}

		public override Scope makeScope ()
		{
			return new DLRScope (this);
		}

		public override object GetVariable(string text)
		{
			return dlr_scope.GetVariable(text);
		}

		public override void SetVariable (string variable, object value)
		{
			dlr_scope.SetVariable(variable, value);
		}
		
		public override bool HasErrors (string text)
		{
			Microsoft.Scripting.SourceCodeKind sctype = Microsoft.Scripting.SourceCodeKind.Expression;
			Microsoft.Scripting.Hosting.ScriptSource source = engine.CreateScriptSourceFromString (text, sctype);
			try {
				source.Compile (compiler_options);
				return false;
			} catch {
				return true;
			}
		}

	}
	
	public class DLRScope : Scope
	{
		public Microsoft.Scripting.Hosting.ScriptScope dlr_scope;
		public DLRExpressionEngine engine;

		public DLRScope (DLRExpressionEngine engine) : base()
		{
			this.engine = engine;
			dlr_scope = ((DLRExpressionEngine)this.engine).scriptRuntime.CreateScope ();
		}
		public override IEnumerable<string> GetVariableNames() 
		{
			return dlr_scope.GetVariableNames();
		}
		public override object GetVariable(string name)
		{
			return dlr_scope.GetVariable(name);
		}
		public override void SetVariable(string name, object value)
		{
			dlr_scope.SetVariable(name, value);
		}
	}
	
	public class DLRExpression : Expression
	{
		ExpressionEngine engine;
		string text;
		
		public DLRExpression (DLRExpressionEngine engine, string text) : base(engine, text)
		{
			this.engine = engine;
			this.text = text;
		}
		
		public override string ParsedExpression ()
		{
			return text;
		}

		public override object GetVariable(string text)
		{
			return engine.GetVariable(text);
		}

		public override void SetVariable (string variable, object value)
		{
			engine.SetVariable (variable, value);
		}

		public override object Evaluate (string text)
		{
			return engine.Evaluate (text);
		}

        public static string ArrayToString(object[] args) {
            string retval = "";
            if (args != null) {
              int count = ((Array)args).Length;
              for (int i = 0; i < count; i++) {
                      if (args[i] is object[]) {
                            retval += ArrayToString((object[])args[i]);
                      } else {
                            if (retval != "")
                              retval += ", ";
                            retval += args[i];
                      }
              }
            }
            return "[" + retval + "]";
		}
		
		public static bool HasMethod(object obj, string methodName) {
                Type type = obj.GetType();
                return type.GetMethod(methodName) != null;
        }

        public static string InvokeMethod(object obj, string methodName, params object [] args) {
                var type = obj.GetType();
            try {
                System.Reflection.MethodInfo mi = type.GetMethod(methodName);
                    return (string)mi.Invoke(obj, args);
            } catch {
                return null;
            }
        }

		public override string ToRepr (object obj)
		{
            string repr = null;
			
            if (HasMethod(obj, "__repr__")) {
                repr = InvokeMethod(obj, "__repr__", IronPython.Runtime.DefaultContext.Default);
            } else if (HasMethod(obj, "to_s")) {
                repr = InvokeMethod(obj, "to_s");
            } else if (obj is Array) {
                repr = (string)ArrayToString((object[]) obj);
            }
            if (repr == null) {
                repr = obj.ToString();
            }
            return repr;
        }

		public static void Main() {
			Expression Expr = Engine.makeExpression("1 + 1");
			Expr.SetVariable("X", Expr.Evaluate());
			Console.WriteLine("Set {0} = {1}, {2}", "X", Expr.Evaluate(), 
									Expr.GetVariable("X"));
			/*
			
			Expression exp = Engine.makeExpression("1 + 1");
			Engine.SetVariable("x", exp.Evaluate());
			System.Console.WriteLine(((int)exp.Evaluate()) == 2 ? "works!" : "fail!");
			exp = Engine.makeExpression("[1, 2, 3, x, 5]");
			Engine.SetVariable("x", 100);
			object obj = exp.Evaluate();
			System.Console.WriteLine(exp.ToRepr(obj));
			*/
		}
	}
}

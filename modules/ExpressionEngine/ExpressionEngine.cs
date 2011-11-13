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
	}
	
	public class ExpressionEngine
	{
		// Internal API
		public ExpressionEngine ()
		{
		}
		
		public virtual Expression makeExpression (string text)
		{
			return new Expression (text);
		}
		
		public virtual Scope makeScope ()
		{
			return new Scope ();
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
		public virtual void EvaluateStatement (string statement)
		{	
		}
		public virtual bool HasErrors (string exp)
		{
			return false;
		}
		public virtual object Evaluate (string exp)
		{
			return null;
		}

	}
	
	public class Expression
	{
		string text;
		
		public Expression (string text)
		{
			this.text = text;
		}
		
		public bool HasErrors (Scope scope)
		{
			return scope.HasErrors(text);
		}

		public virtual string ParsedExpression ()
		{
			return text;
		}

		public virtual object Evaluate (Scope scope)
		{
			return scope.Evaluate(text);
		}
		public virtual string ToRepr(object o)
		{
			 return text;
		}
	}

	public class DLRExpressionEngine : ExpressionEngine
	{
		string dlr_name;
		public Microsoft.Scripting.Hosting.ScriptRuntimeSetup scriptRuntimeSetup;
		public Microsoft.Scripting.Hosting.ScriptRuntime scriptRuntime;
		Microsoft.Scripting.Hosting.LanguageSetup languageSetup;
		Microsoft.Scripting.CompilerOptions compiler_options;
		Microsoft.Scripting.Hosting.ScriptEngine dlr_engine;
		
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
			//dlr_scope = scriptRuntime.CreateScope ();
			dlr_engine = scriptRuntime.GetEngine (dlr_name);  
			compiler_options = dlr_engine.GetCompilerOptions ();
			IronPython.Compiler.PythonCompilerOptions options = (IronPython.Compiler.PythonCompilerOptions)compiler_options;
			options.PrintFunction = true;
			options.AllowWithStatement = true;
			options.TrueDivision = true;
		}
		
		public override Expression makeExpression (string text)
		{
			return new DLRExpression (text);
		}

		public override Scope makeScope ()
		{
			return new DLRScope (this);
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
		public override void EvaluateStatement (string exp)
		{
			Microsoft.Scripting.SourceCodeKind sctype = Microsoft.Scripting.SourceCodeKind.SingleStatement;
			Microsoft.Scripting.Hosting.ScriptSource source = engine.dlr_engine.CreateScriptSourceFromString (exp, sctype);
			source.Compile (engine.compiler_options);
			source.Execute (dlr_scope);
		}
	}
	
	public class DLRExpression : Expression
	{
		string text;
		
		public DLRExpression (string text) : base(text)
		{
			this.text = text;
		}
		
		public override string ParsedExpression ()
		{
			return text;
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

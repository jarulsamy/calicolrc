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
		public static bool HasErrors(string text) {
			return TheEngine.HasErrors(text);
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
		public virtual void Assignment(string name, string value)
		{
			EvaluateStatement(String.Format("{0} = {1}", name, value));
		}
		public virtual void EvaluateStatement (string statement)
		{	
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
		
		public bool HasErrors ()
		{
			return Engine.HasErrors(text);
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
		public Microsoft.Scripting.CompilerOptions compiler_options;
		public Microsoft.Scripting.Hosting.ScriptEngine dlr_engine;
		
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
		public override bool HasErrors(string text)
		{
             Microsoft.Scripting.SourceCodeKind sctype = Microsoft.Scripting.SourceCodeKind.Expression;	 
             Microsoft.Scripting.Hosting.ScriptSource source = dlr_engine.CreateScriptSourceFromString (text, sctype);	 
             try {	 
                 source.Compile(compiler_options);	 
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
		public override void EvaluateStatement (string exp)
		{
			Microsoft.Scripting.SourceCodeKind sctype = Microsoft.Scripting.SourceCodeKind.SingleStatement;
			Microsoft.Scripting.Hosting.ScriptSource source = engine.dlr_engine.CreateScriptSourceFromString (exp, sctype);
			source.Compile (engine.compiler_options);
			source.Execute (dlr_scope);
		}
		
		public override object Evaluate (string exp)	 
         {	 
             Microsoft.Scripting.SourceCodeKind sctype = Microsoft.Scripting.SourceCodeKind.Expression;	 
             Microsoft.Scripting.Hosting.ScriptSource source = engine.dlr_engine.CreateScriptSourceFromString (exp, sctype);	 
             source.Compile (engine.compiler_options);	 
             return source.Execute (dlr_scope);	 
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
			Scope scope = Engine.makeScope();
			scope.SetVariable("X", Expr.Evaluate(scope));
			System.Console.WriteLine(((int)Expr.Evaluate(scope)) == 2 ? "works!" : "fail!");
			Console.WriteLine("Set {0} = {1}, {2}", 
				"X", 
				Expr.Evaluate(scope), 
				scope.GetVariable("X"));
			scope.EvaluateStatement("X = 42");
			Console.WriteLine("Set {0} = {1}", 
				"X", 
				scope.GetVariable("X"));
			Expr = Engine.makeExpression("[1, 2, 3, x, 5]");
			scope.SetVariable("x", 100);
			object obj = Expr.Evaluate(scope);
			System.Console.WriteLine(Expr.ToRepr(obj));
		}
	}
}

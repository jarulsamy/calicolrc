//
//  Engine.cs
//  
//  Author:
//       Douglas S. Blank <dblank@cs.brynmawr.edu>
// 
//  Copyright (c) 2011 The Calico Project
// 
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
// 
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
// 
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

using System;
using System.IO;
using System.Collections.Generic;
//using IronPython.Hosting;
//using IronPython.Modules;

namespace Calico {

    public class Engine {
        public LanguageManager manager;
        public MainWindow calico;
        public bool trace = false;

        public Engine(LanguageManager manager) {
            this.manager = manager;
        }

        public virtual bool ReadyToExecute(string text) {
            // If more than one line in DLR, wait for a blank line
            string [] lines = text.Split('\n');
            int line_count = lines.Length;
            if (line_count == 1) {
                text = text.Trim();
                return (text != "" && text.EndsWith(";")); // if there is text, and last char is ;
            }
            return lines[lines.Length - 1].Trim() == ""; // ok, if nothing
        }

        public virtual bool Execute(string text) {
            return true;
        }

        public virtual object Eval(string text) {
            return null;
        }

        public virtual bool Execute(string text, bool ok) {
            return true;
        }

        public virtual object Evaluate(string text) {
            return null;
        }

        public virtual bool ExecuteFile(string filename) {
            return true;
        }

        public virtual void Setup(string path) {
        }

        public virtual void Start(string path) {
        }

        public virtual void SetRedirects(CustomStream stdout, CustomStream stderr) {
        }

        public virtual void PostSetup(MainWindow calico) {
        }

        public virtual bool tryGetVariable(string variable, out object value) {
            value = null;
            return false;
        }

        public string [] getVariableParts(string variable) {
            return variable.Split('.');
        }

        public virtual List<string> getCompletions(string root) {
            return new List<string>();
        }

        public virtual void SetTraceOn(MainWindow calico) {
            this.calico = calico;
            trace = true;
        }

        public virtual void SetTraceOff() {
            trace = false;
        }

        public virtual void RequestPause () {
            // request running program to pause
        }

        public virtual void ConfigureTrace() {
        }

        public virtual object GetDefaultContext() {
            return null;
        }

        public virtual void PrintLine(string message) {
        }

        public virtual void PrintLine(Tag tag, string message) {
        }
    }
    
    public class DLREngine : Engine {
        public string dlr_name;
        public Microsoft.Scripting.Hosting.ScriptRuntimeSetup scriptRuntimeSetup;
        public Microsoft.Scripting.Hosting.ScriptRuntime scriptRuntime;
        public Microsoft.Scripting.Hosting.LanguageSetup languageSetup;
        public Microsoft.Scripting.CompilerOptions compiler_options;
        public Microsoft.Scripting.Hosting.ScriptEngine engine;
        public Microsoft.Scripting.Hosting.ScriptScope scope;
 
        public DLREngine(LanguageManager manager) : base(manager) {
        }

        public override void Setup(string path) {
            if (manager != null) {
                try {
                    manager.scriptRuntimeSetup.LanguageSetups.Add(languageSetup);
                } catch {
		            Console.Error.WriteLine("WARNING: Please restart Calico to use this language");
                    engine = null;
                }
            } else {
                scriptRuntimeSetup = new Microsoft.Scripting.Hosting.ScriptRuntimeSetup();
                scriptRuntime = new Microsoft.Scripting.Hosting.ScriptRuntime(scriptRuntimeSetup);
                scope = scriptRuntime.CreateScope();
            }
        }

        public override void SetRedirects(CustomStream stdout, 
                      CustomStream stderr) {
		  if (engine != null) {
            engine.Runtime.IO.SetOutput(stdout, System.Text.Encoding.UTF8);
            engine.Runtime.IO.SetErrorOutput(stderr,System.Text.Encoding.UTF8);
		  }
        }

        public override void PostSetup(MainWindow calico) {
            if (engine == null) {
                PrintLine("Please restart Calico to use this language.");
                return;
            }
		  System.Reflection.Assembly assembly;
		  // ---------------------------------------
		  foreach (System.Reflection.AssemblyName aname in System.Reflection.Assembly.GetExecutingAssembly().GetReferencedAssemblies()) {
			assembly = System.Reflection.Assembly.Load(aname);
			if (engine != null)
			  engine.Runtime.LoadAssembly(assembly);
		  }
		  // ---------------------------------------
		  DirectoryInfo dir = new DirectoryInfo(Path.Combine(calico.path, "..", "modules"));
		  foreach (FileInfo f in dir.GetFiles("*.dll")) {
			string assembly_name = f.FullName;
			assembly = System.Reflection.Assembly.LoadFile(assembly_name);
			if (assembly != null) {
			  // initialize_module if possible
			  try {
				foreach (Type type in assembly.GetTypes()) {
				  System.Reflection.MethodInfo method;
				  try {
					method = type.GetMethod("initialize_module");
                    if (method != null)
					    method.Invoke(type, new object [] {calico.path, calico.OS});
				  } catch {
				  }
				  try {
					method = type.GetMethod("set_gui_thread_id");
                    if (method != null)
					    method.Invoke(type, new object [] {MainWindow.gui_thread_id});
				  } catch {
				  }
				}
			  } catch {
				continue;
			  }
			  if (engine != null) {
				try {
				  engine.Runtime.LoadAssembly(assembly);
				} catch {
				  Console.WriteLine("{0} failed to load assembly {1}", 
					  dlr_name, assembly);
				}
			  }
			}
		  }
        }
		
        public override void PrintLine(string message) {
            if (manager.stderr != null) {
                manager.stderr.PrintLine(message);
            } else {
                Console.Error.WriteLine(message);
            }
        }

        public override void PrintLine(Calico.Tag tag, string message) {
            if (manager.stderr != null) {
                manager.stderr.PrintLine(tag, message);
            } else {
                Console.Error.WriteLine(message);
            }
        }

        public override bool ReadyToExecute(string text) {
            if (engine == null) {
                PrintLine("Please restart Calico to use this language.");
                return false;
            }
            // If more than one line in DLR, wait for a blank line
            string [] lines = text.Split('\n');
            int line_count = lines.Length;
            if (line_count == 1) {
                // Need this here, as there are no compiler_options used:
                text = "from __future__ import division, with_statement, print_function;" + text;
                var sctype = Microsoft.Scripting.SourceCodeKind.InteractiveCode;
                var source = engine.CreateScriptSourceFromString(text, sctype);
                return (source.GetCodeProperties() ==
                        Microsoft.Scripting.ScriptCodeParseResult.Complete);
            }
            return lines[lines.Length - 1].Trim() == ""; // ok, if nothing
        }

        public override object Evaluate(string text) {
		  Microsoft.Scripting.SourceCodeKind sctype = Microsoft.Scripting.SourceCodeKind.Expression;
		  Microsoft.Scripting.Hosting.ScriptSource source = engine.CreateScriptSourceFromString(text, sctype);
		  try {
    	    if (compiler_options != null) {
			  source.Compile(compiler_options);
            } else {
    	      source.Compile();
            }
		  } catch {
			Console.Error.WriteLine("Unable to compile!");
			return null;
		  }
		  object retval = null;
		  bool aborted = false;
		  try {
			if (manager != null && manager.UseSharedScope)
			  retval = source.Execute(manager.scope);
			else
			  retval = source.Execute(scope);
		  } catch (System.Threading.ThreadAbortException tae) {
		      System.Threading.Thread.Sleep(100);
		      System.Threading.Thread.ResetAbort();
		  }
		  if (aborted) {
			System.Console.Error.WriteLine("Running script aborted!");
		  }
		  return retval;
        }

        public override bool Execute(string text) {
		  return Execute(text, true);
        }
		
        public override bool Execute(string text, bool ok) {
		  // This is called by RunInBackground() in the MainWindow
		  //manager.calico.last_error = ""
		  if (engine == null) {
			PrintLine("Please restart Calico to use this language.");
			return false;
		  }
		  Microsoft.Scripting.SourceCodeKind sctype = Microsoft.Scripting.SourceCodeKind.InteractiveCode;
		  Microsoft.Scripting.Hosting.ScriptSource source = engine.CreateScriptSourceFromString(text, sctype);
		  try {
			if (compiler_options != null) {
			  source.Compile(compiler_options);
			} else {
			  source.Compile();
			}
		  } catch {
			sctype = Microsoft.Scripting.SourceCodeKind.Statements;
			source = engine.CreateScriptSourceFromString(text, sctype);
			try {
			  if (compiler_options != null) {
				source.Compile(compiler_options);
			  } else {
				source.Compile();
			  }
			} catch (Exception e) {
			  Microsoft.Scripting.Hosting.ExceptionOperations eo = engine.GetService<Microsoft.Scripting.Hosting.ExceptionOperations>();
			  PrintLine(eo.FormatException(e));
			  return false;
			}
		  }
		  try {
			if (manager != null && manager.UseSharedScope)
			  source.Execute(manager.scope);
			else
			  source.Execute(scope);
		  } catch (System.Threading.ThreadAbortException tae) {
			PrintLine("[Script stopped----------]");
			System.Threading.Thread.Sleep(100);
			System.Threading.Thread.ResetAbort();
		  } catch (Exception e) {
			if (e.Message.Contains("Thread was being aborted")) {
			  PrintLine("[Script stopped----------]");
			} else {
			  Microsoft.Scripting.Hosting.ExceptionOperations eo = engine.GetService<Microsoft.Scripting.Hosting.ExceptionOperations>();
			  PrintLine(eo.FormatException(e));
			}
			return false;
		  }
		  if (ok) {
		      PrintLine(Tag.Info, "Ok");
		  }
		  return true;
        }
		
        public override bool ExecuteFile(string filename) {
		  //manager.calico.last_error = ""
		  //IronPython.Hosting.Python.GetSysModule(self.engine).settrace(self.trace)
		  Microsoft.Scripting.Hosting.ScriptSource source = engine.CreateScriptSourceFromFile(filename);
		  try {
			if (compiler_options != null) {
			  source.Compile(compiler_options);
			} else {
			  source.Compile();
			}
		  } catch (Exception e) {
			Microsoft.Scripting.Hosting.ExceptionOperations eo = engine.GetService<Microsoft.Scripting.Hosting.ExceptionOperations>();
			PrintLine(eo.FormatException(e));
			return false;
		  }
		  ConfigureTrace();
		  try {
			//if (manager != null && manager.UseSharedScope)
			source.Execute(manager.scope);
			//else
			//source.Execute(scope);
		  } catch (System.Threading.ThreadAbortException tae) {
			PrintLine("[Script stopped----------]");
			System.Threading.Thread.Sleep(100);
			System.Threading.Thread.ResetAbort();
		  } catch (Exception e) {
			if (e.Message.ToString().Contains("Thread was being aborted")) {
			  PrintLine("[Script stopped----------]");
			} else {
			  Microsoft.Scripting.Hosting.ExceptionOperations eo = engine.GetService<Microsoft.Scripting.Hosting.ExceptionOperations>();
			  PrintLine(eo.FormatException(e));
			}
			return false;
		  }
		  PrintLine(Tag.Info, "Done");
		  return true;
        }
		
        public override bool tryGetVariable(string variable, out object value) {
            return manager.scope.TryGetVariable(variable, out value);
        }
        public override List<string> getCompletions(string root) {
            List<string> retval = new List<string>();
            foreach (string x in manager.scope.GetVariableNames()) {
                if (x.StartsWith(root))
                    retval.Add(x);
            }
            return retval;
        }

        /*
        public IronPython.Compiler.Ast.PythonAst Parse() {
            IronPython.Runtime.CodeContext context = new IronPython.Runtime.CodeContext();
            SourceUnit sourceUnit = new SourceUnit();
            string mode = "";
            Parser parser = Parser.CreateParser(
                new CompilerContext(sourceUnit, compiler_options, ThrowingErrorSink.Default),
                (PythonOptions)context.LanguageContext.Options);

            PythonAst ast = parser.ParseFile(true);
        }
        
            Microsoft.Scripting.Runtime.LanguageContext context =
            Microsoft.Scripting.SourceUnit sourceUnit = new Microsoft.Scripting.SourceUnit(context);
             IronPython.Compiler.Parser parser = IronPython.Compiler.Parser.CreateParser(
                         new Microsoft.Scripting.Runtime.CompilerContext(sourceUnit,
                                compiler_options,
                                ThrowingErrorSink.Default),
                                (IronPython.PythonOptions)context.LanguageContext.Options);

             PythonAst ast = parser.ParseFile(true);
        }
         */
    }
}

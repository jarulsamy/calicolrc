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

        public virtual void Close() {
        }

        public virtual string [] GetVariableParts(string variable) {
            return variable.Split('.');
        }

	// -----------------------------------------------------

	public virtual object GetMember(object value, string part) {
	    return null;
	}

	public virtual IList<string> GetMemberNames(object obj) {
	    return new List<string>();
	}

        public virtual Calico.Variable TryGetVariable(string variable) {
	    return new Variable(false, null);
        }

	public virtual TabCompletion GetTabCompletion(string to_match) {
	    return new TabCompletion(this, null, to_match);
	}

	public virtual TabCompletion GetTabCompletion(string to_match, 
					 Mono.TextEditor.TextEditor shell) {
	    return new TabCompletion(this, shell, to_match);
	}

        public virtual IList<string> GetCompletions(string root) {
            return new List<string>();
        }

	public virtual string GetHelpOn(string expression) {
	    return String.Format("No available help on expression '{0}'", expression);
	}

	public virtual IDictionary<string,object> GetHelpOnFunctionCall(string oname) {
	    return new Dictionary<string, object> {
		    {"base_class" ,null},    // <type ...>
		    {"init_definition", null},
		    {"type_name", null},     // function
		    {"name", oname}, 
		    {"definition", null}, // has control codes
		    {"isclass", null},
		    {"docstring", null}, // get this
		    {"isalias", null},
		    {"init_docstring", null},
		    {"argspec", null},        // dictionary {"args":[],"varkw":"kwargs","defaults":null,"varargs":"objs"}
		    {"source", null},
		    {"length", null},
		    {"call_def", null},
		    {"call_docstring", null},
		    {"file", null},           // path/to/filename
		    {"string_form", null},    // <function ...>
		    {"found", false},         // true
		    {"class_docstring", null},
		    {"namespace", null},      // Interactive
		    {"ismagic", null}
		};
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
		            Console.Error.WriteLine("Please activate and restart Calico to use this language");
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
		  string os_specific_dir = null;
		  if (calico.OS == "Windows") {
		      os_specific_dir = "windows";
		  } else if (calico.OS == "Mac") {
		      os_specific_dir = "mac";
		  } else {
		      os_specific_dir = "linux";
		  }
		  foreach (DirectoryInfo dir in new DirectoryInfo[] {new DirectoryInfo(Path.Combine(calico.path, "..", "modules")),
								     new DirectoryInfo(Path.Combine(calico.path, os_specific_dir)),
		      }) {
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
                PrintLine("Please activate and restart Calico to use this language.");
                return false;
            }
            // If more than one line in DLR, wait for a blank line
            string [] lines = text.Split('\n');
            int line_count = lines.Length;
            if (line_count == 1) {
                // Need this here, as there are no compiler_options used:
                //text = "from __future__ import division, with_statement, print_function;" + text;
                var sctype = Microsoft.Scripting.SourceCodeKind.InteractiveCode;
                var source = engine.CreateScriptSourceFromString(text, sctype);
                return (source.GetCodeProperties() ==
                        Microsoft.Scripting.ScriptCodeParseResult.Complete);
            }
            return lines[lines.Length - 1].Trim() == ""; // ok, if nothing
        }

        public virtual Microsoft.Scripting.Hosting.CompiledCode SetDLRSpecificCompilerOptions(
			  Microsoft.Scripting.Hosting.ScriptSource source, 
			  Microsoft.Scripting.CompilerOptions compiler_options) {
            return source.Compile(compiler_options);
        }

        public override object Evaluate(string text) {
            if (engine == null) {
                PrintLine("Please activate and restart Calico to use this language.");
                return null;
            }
	    Microsoft.Scripting.SourceCodeKind sctype = Microsoft.Scripting.SourceCodeKind.AutoDetect;
	    Microsoft.Scripting.Hosting.ScriptSource source;
	    Microsoft.Scripting.Hosting.CompiledCode compiledCode;
	    try {
		source = engine.CreateScriptSourceFromString(text, sctype);
		compiledCode = null;
	    } catch (Exception e) {
		Microsoft.Scripting.Hosting.ExceptionOperations eo = engine.GetService<Microsoft.Scripting.Hosting.ExceptionOperations>();
		PrintLine(eo.FormatException(e));
		return null;
	    }
	    try {
		if (compiler_options != null) {
		    compiledCode = SetDLRSpecificCompilerOptions(source, compiler_options);
		} else {
		    compiledCode = source.Compile();
		}
	    } catch {
		// let's try to Execute, before giving up:
		try { 
		    sctype = Microsoft.Scripting.SourceCodeKind.InteractiveCode;
		    source = engine.CreateScriptSourceFromString(text, sctype);
		    if (compiler_options != null) {
			compiledCode = SetDLRSpecificCompilerOptions(source, compiler_options);
		    } else {
			compiledCode = source.Compile();
		    }		 
		} catch {
		    try {
			sctype = Microsoft.Scripting.SourceCodeKind.Statements;
			source = engine.CreateScriptSourceFromString(text, sctype);
			if (compiler_options != null) {
			    compiledCode = SetDLRSpecificCompilerOptions(source, compiler_options);
			} else {
			    compiledCode = source.Compile();
			}
		    } catch (Exception e) {
			Microsoft.Scripting.Hosting.ExceptionOperations eo = engine.GetService<Microsoft.Scripting.Hosting.ExceptionOperations>();
			PrintLine(eo.FormatException(e));
			return null;
		    }
		}
	    }
	    object retval = null;
	    bool aborted = false;
	    try {
		if (manager != null && manager.UseSharedScope)
		    retval = compiledCode.Execute(manager.scope);
		else
		    retval = compiledCode.Execute(scope);
	    } catch (System.Threading.ThreadAbortException) {
		System.Threading.Thread.Sleep(100);
		try {
		    System.Threading.Thread.ResetAbort();
		} catch {
		    // pass
		}
		aborted = true;
	    } catch (Exception e) {
		Microsoft.Scripting.Hosting.ExceptionOperations eo = engine.GetService<Microsoft.Scripting.Hosting.ExceptionOperations>();
		PrintLine(eo.FormatException(e));
	    }
	    if (aborted) {
		System.Console.Error.WriteLine("Running script aborted!");
	    }
	    return retval;
        }

        public object TryEvaluate(string text) {
            if (engine == null) {
                return null;
            }
	    Microsoft.Scripting.SourceCodeKind sctype = Microsoft.Scripting.SourceCodeKind.Expression;
	    Microsoft.Scripting.Hosting.ScriptSource source;
	    Microsoft.Scripting.Hosting.CompiledCode compiledCode;
	    try {
		source = engine.CreateScriptSourceFromString(text, sctype);
		compiledCode = null;
	    } catch {
		return null;
	    }
	    try {
		if (compiler_options != null) {
		    compiledCode = SetDLRSpecificCompilerOptions(source, compiler_options);
		} else {
		    compiledCode = source.Compile();
		}
	    } catch {
		// pass
	    }
	    object retval = null;
	    try {
		if (manager != null && manager.UseSharedScope)
		    retval = compiledCode.Execute(manager.scope);
		else
		    retval = compiledCode.Execute(scope);
	    } catch {
		// pass
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
			PrintLine("Please activate and restart Calico to use this language.");
			return false;
		  }
		  Microsoft.Scripting.SourceCodeKind sctype = Microsoft.Scripting.SourceCodeKind.InteractiveCode;
		  Microsoft.Scripting.Hosting.ScriptSource source = engine.CreateScriptSourceFromString(text, sctype);
		  Microsoft.Scripting.Hosting.CompiledCode compiledCode = null;
		  try {
		      if (compiler_options != null) {
			  compiledCode = SetDLRSpecificCompilerOptions(source, compiler_options);
		      } else {
			  compiledCode = source.Compile();
		      }
		  } catch {
			sctype = Microsoft.Scripting.SourceCodeKind.Statements;
			source = engine.CreateScriptSourceFromString(text, sctype);
			try {
			  if (compiler_options != null) {
				compiledCode = SetDLRSpecificCompilerOptions(source, compiler_options);
			  } else {
				compiledCode = source.Compile();
			  }
			} catch (Exception e) {
			  Microsoft.Scripting.Hosting.ExceptionOperations eo = engine.GetService<Microsoft.Scripting.Hosting.ExceptionOperations>();
			  PrintLine(eo.FormatException(e));
			  return false;
			}
		  }
		  try {
			if (manager != null && manager.UseSharedScope)
			  compiledCode.Execute(manager.scope);
			else
			  compiledCode.Execute(scope);
		  } catch (System.Threading.ThreadAbortException) {
			PrintLine("[Script stopped----------]");
			System.Threading.Thread.Sleep(100);
			try {
			    System.Threading.Thread.ResetAbort();
			} catch {
			    // pass
			}
		  } catch (Exception e) {
			if (e.Message.Contains("Thread was being aborted")) {
			  PrintLine("[Script stopped----------]");
			} else {
			  Microsoft.Scripting.Hosting.ExceptionOperations eo = engine.GetService<Microsoft.Scripting.Hosting.ExceptionOperations>();
			  PrintLine(eo.FormatException(e));
			}
			return false;
		  }
		  /*
		  if (ok) {
		      PrintLine(Tag.Info, "Ok");
		  }
		  */
		  return true;
        }
		
        public override bool ExecuteFile(string filename) {
		  //manager.calico.last_error = ""
		  //IronPython.Hosting.Python.GetSysModule(self.engine).settrace(self.trace)
		  Microsoft.Scripting.Hosting.ScriptSource source = engine.CreateScriptSourceFromFile(filename);
		  Microsoft.Scripting.Hosting.CompiledCode compiledCode = null;
		  try {
			if (compiler_options != null) {
			  compiledCode = SetDLRSpecificCompilerOptions(source, compiler_options);
			} else {
			  compiledCode = source.Compile();
			}
		  } catch (Exception e) {
			Microsoft.Scripting.Hosting.ExceptionOperations eo = engine.GetService<Microsoft.Scripting.Hosting.ExceptionOperations>();
			PrintLine(eo.FormatException(e));
			return false;
		  }
		  ConfigureTrace();
		  try {
			//if (manager != null && manager.UseSharedScope)
			compiledCode.Execute(manager.scope);
			//else
			//source.Execute(scope);
		  } catch (System.Threading.ThreadAbortException) {
			PrintLine("[Script stopped----------]");
			System.Threading.Thread.Sleep(100);
			try {
			    System.Threading.Thread.ResetAbort();
			} catch {
			    // pass
			}
		  } catch (Exception e) {
			if (e.Message.ToString().Contains("Thread was being aborted")) {
			  PrintLine("[Script stopped----------]");
			} else {
			  Microsoft.Scripting.Hosting.ExceptionOperations eo = engine.GetService<Microsoft.Scripting.Hosting.ExceptionOperations>();
			  PrintLine(eo.FormatException(e));
			}
			return false;
		  }
		  //PrintLine(Tag.Info, "Done");
		  return true;
        }
		
        public override Variable TryGetVariable(string variable) {
	    object value;
            bool found = manager.scope.TryGetVariable(variable, out value);
	    return new Variable(found, value);
        }

        public override IList<string> GetCompletions(string root) {
            List<string> retval = new List<string>();
            foreach (string x in manager.scope.GetVariableNames()) {
                if (x.StartsWith(root))
                    retval.Add(x);
            }
	    retval.Sort();
            return retval;
        }

	public override object GetMember(object value, string part) {
	    return engine.Operations.GetMember(value, part);	
	}
	
	public override IList<string> GetMemberNames(object obj) {
	    List<string> retval = new List<string>();
	    foreach (string name in engine.Operations.GetMemberNames(obj)) {
		retval.Add(name);
	    }
	    retval.Sort();
	    return retval;
	}

	public override string GetHelpOn(string expression) {
	    return String.Format("No available help on expression '{0}'", expression);
	}

	public override IDictionary<string,object> GetHelpOnFunctionCall(string oname) {
	    var retval = new Dictionary<string, object> {
		    {"base_class" ,null},    // <type ...>
		    {"init_definition", null},
		    {"type_name", null},     // function
		    {"name", oname}, 
		    {"definition", null}, // has control codes
		    {"isclass", null},
		    {"docstring", null}, // get this
		    {"isalias", null},
		    {"init_docstring", null},
		    {"argspec", null},        // dictionary {"args":[],"varkw":"kwargs","defaults":null,"varargs":"objs"}
		    {"source", null},
		    {"length", null},
		    {"call_def", null},
		    {"call_docstring", null},
		    {"file", null},           // path/to/filename
		    {"string_form", null},    // <function ...>
		    {"found", false},         // true
		    {"class_docstring", null},
		    {"namespace", null},      // Interactive
		    {"ismagic", null}
		};
	    object obj = TryEvaluate(oname);
	    if (obj != null) {
		retval["found"] = true;
		retval["base_class"] = obj.GetType().ToString();
		retval["string_form"] = obj.ToString();
		retval["namespace"] = "Interactive";
		retval["definition"] = "This is the definition";
		retval["docstring"] = "This is the docstring";
		retval["type_name"] = obj.GetType().ToString();
		retval["file"] = "/home/user/test";
		retval["argspec"] = new Dictionary<string,object> {
		    {"args", new List<string>()},
		    {"varkw", "kwargs"},
		    {"defaults", null},
		    {"varargs", "objs"}
		};
	    }
	    return retval;
	}
    }
}

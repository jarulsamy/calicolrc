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

namespace Calico {

    public class Engine {
        public LanguageManager manager;

        public Engine(LanguageManager manager) {
            this.manager = manager;
        }

        public virtual bool ReadyToExecute(string text) {
            return true;
        }

        public virtual bool Execute(string text) {
            return true;
        }

        public virtual bool ExecuteFile(string filename) {
            return true;
        }

        public virtual void Setup() {
        }

        public virtual void Start() {
        }

        public virtual void SetRedirects(CustomStream stdout, CustomStream stderr) {
        }

        public virtual void PostSetup(MainWindow calico) {
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

        public override void Setup() {
            manager.scriptRuntimeSetup.LanguageSetups.Add(languageSetup);
        }

        public override void SetRedirects(CustomStream stdout, 
                      CustomStream stderr) {
            engine.Runtime.IO.SetOutput(stdout, System.Text.Encoding.UTF8);
            engine.Runtime.IO.SetErrorOutput(stderr,System.Text.Encoding.UTF8);
        }

        public override void PostSetup(MainWindow calico) {
            System.Reflection.Assembly assembly;
            // ---------------------------------------
            //engine.Runtime.LoadAssembly(System.Type.GetType(
            //        "System.Diagnostics.Debug").Assembly);
            //engine.Runtime.LoadAssembly(System.Type.GetType("System.String").Assembly);
            foreach (System.Reflection.AssemblyName aname in System.Reflection.Assembly.GetExecutingAssembly().GetReferencedAssemblies()) {
                assembly = System.Reflection.Assembly.Load(aname);
                    engine.Runtime.LoadAssembly(assembly);
            }
            // ---------------------------------------
            DirectoryInfo dir = new DirectoryInfo(Path.Combine(calico.path, "../modules"));
            foreach (FileInfo f in dir.GetFiles("*.dll")) {
                string assembly_name = f.FullName;
                assembly = System.Reflection.Assembly.LoadFile(assembly_name);
                if (assembly != null) {
                    // initialize_module if possible
                    foreach (Type type in assembly.GetTypes()) {
                        System.Reflection.MethodInfo method;
                        try {
                            method = type.GetMethod("initialize_module");
                            method.Invoke(type, new object [] {calico.path, "nt"}); //FIXME
                            Console.WriteLine("ok! called initialize_module in {0} for {1}", assembly_name, dlr_name);
                            break;
                        } catch {
                        }
                        try {
                            method = type.GetMethod("set_gui_thread_id");
                            method.Invoke(type, new object [] {1}); // FIXME: MainWindow.gui_thread_id hasn't been run in delegate yet
                            Console.WriteLine("ok! called set_gui_thread_id in {0} for {1}, {2}", assembly_name, dlr_name, MainWindow.gui_thread_id);
                            break;
                        } catch {
                        }
                    }
                    engine.Runtime.LoadAssembly(assembly);
                }
            }
        }

        public override bool Execute(string text) {
            // This is called by RunInBackground() in the MainWindow
            //manager.calico.last_error = ""
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
                    manager.stderr.PrintLine(eo.FormatException(e));
                    return false;
                }
            }
            try {
                if (manager.UseSharedScope)
                    source.Execute(manager.scope);
                else
                    source.Execute(scope);
            } catch (Exception e) {
              if (e.Message.Contains("Thread was being aborted")) {
                manager.stderr.Print("[Script stopped----------]\n");
              } else {
                Microsoft.Scripting.Hosting.ExceptionOperations eo = engine.GetService<Microsoft.Scripting.Hosting.ExceptionOperations>();
                manager.stderr.PrintLine(eo.FormatException(e));
              }
              return false;
            }
            manager.stderr.PrintLine(Tag.Info, "Ok");
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
                manager.stderr.PrintLine(eo.FormatException(e));
                return false;
            }
            try {
                source.Execute(manager.scope);
            } catch (Exception e) {
                if (e.Message.ToString().Contains("Thread was being aborted")) {
                    manager.calico.Print("[Script stopped----------]");
                } else {
                    Microsoft.Scripting.Hosting.ExceptionOperations eo = engine.GetService<Microsoft.Scripting.Hosting.ExceptionOperations>();
                    manager.stderr.PrintLine(eo.FormatException(e));
                }
                return false;
            }
            return true;
        }
      }
}


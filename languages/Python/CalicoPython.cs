//
//  CalicoPython.cs
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
using System.Collections.Generic;
using System.IO;
using Calico;

namespace CalicoPython
{
	public class CalicoPythonEngine : DLREngine
	{
	
	        static string trace_filename = null;
	        static bool trace_pause = false;

		public CalicoPythonEngine (LanguageManager manager) : base(manager)
		{
			dlr_name = "py";
			scriptRuntimeSetup = new Microsoft.Scripting.Hosting.ScriptRuntimeSetup ();
			languageSetup = IronPython.Hosting.Python.CreateLanguageSetup (null);
			// Set LanguageSetup options here:
			languageSetup.Options ["FullFrames"] = true; // for debugging
			scriptRuntimeSetup.LanguageSetups.Add (languageSetup); // add to local
			// Create a Python-only scope:
			scriptRuntime = new Microsoft.Scripting.Hosting.ScriptRuntime (scriptRuntimeSetup);
			scope = scriptRuntime.CreateScope ();
		}
	
		public override void Start (string path)
		{
			// Get engine from manager:
			if (manager != null) {
				engine = manager.scriptRuntime.GetEngine (dlr_name);  
			} else {
				engine = scriptRuntime.GetEngine (dlr_name);  
			}		
			// Set the compiler options here:
			compiler_options = engine.GetCompilerOptions ();
			IronPython.Compiler.PythonCompilerOptions options = (IronPython.Compiler.PythonCompilerOptions)compiler_options;
			options.PrintFunction = true;
			options.AllowWithStatement = true;
			options.TrueDivision = true;
			// FIXME: before a executefile, __name__ is "__builtin__";
			//        after it is "<module>"
			// FIXME: this doesn't work:
			//options.ModuleName = "__main__";
			//options.Module |= IronPython.Runtime.ModuleOptions.Initialize;
			// Set paths:
			ICollection<string > paths = engine.GetSearchPaths ();
			// Let users find Calico modules:
			foreach (string folder in new string[] { "modules", "src" }) {
				paths.Add (Path.GetFullPath(String.Format("{0}/{1}/{2}", path, "..", folder)));
			}
			engine.SetSearchPaths (paths);
		}
		
		public override void RequestPause () {
		  trace_pause = true;
		}

		public IronPython.Runtime.Exceptions.TracebackDelegate OnTraceBack (
				  IronPython.Runtime.Exceptions.TraceBackFrame frame, 
				  string ttype, object retval)
		{
		  // If in another file, don't stop:
		  if (ttype == "call") { 
		    if (frame.f_code.co_filename != trace_filename)
		      return null;
		  }
		  // If in correct file, and speed is less than full speed, show line:
		  Calico.MainWindow.Invoke (delegate {
		      if (calico.CurrentDocument != null 
			  && calico.CurrentDocument.filename == frame.f_code.co_filename
			  && calico.ProgramSpeed.Value != 100) {
			calico.CurrentDocument.GotoLine ((int)frame.f_lineno);
		      }
		      calico.UpdateLocal((IDictionary<object,object>)frame.f_locals);
		    });
		  // If a stopping criteria:
		  if ((calico.CurrentDocument != null 
		       && calico.CurrentDocument.filename == frame.f_code.co_filename
		       && calico.CurrentDocument.HasBreakpointSetAtLine ((int)frame.f_lineno)) 
		      || calico.ProgramSpeed.Value == 0 
		      || trace_pause) {
		    calico.PlayButton.Sensitive = true;
		    calico.PauseButton.Sensitive = false;
		    calico.playResetEvent.WaitOne ();
		    if (calico.ProgramSpeed.Value == 0 || trace_pause) {
		      calico.playResetEvent.Reset ();
		    }
		    trace_pause = false;
		  } else { // then we are in a delay:
		    int pause = (int)((100 - calico.ProgramSpeed.Value) / 100.0 * 1000);
		    // Force at least a slight sleep, else no GUI controls
		    System.Threading.Thread.Sleep (Math.Max (pause, 1));
		  }
		  // return the call back for this frame trace
		  return OnTraceBack;
		}
		
		public override object GetDefaultContext ()
		{
			return IronPython.Runtime.DefaultContext.Default;
		}
		
		public override void ConfigureTrace ()
		{
		    try {
			  if (trace) {
			    trace_filename = calico.CurrentDocument.filename;
			    trace_pause = false;
			    IronPython.Hosting.Python.SetTrace (engine, OnTraceBack);
			  }
		    } catch { 
		       Console.Error.WriteLine("Error in setting trace.");
		    }
		}
		
        public override void PostSetup(MainWindow calico) {
			base.PostSetup(calico);
            // Set up input
            Execute(//"import clr;" +
                //"clr.AddReference('Myro');" +
                "from Myro import ask;" +
                "__builtins__['input'] = ask;" +
				"del __builtins__['raw_input'];" +
                "del ask;", false);
		}
	}

	public class CalicoPythonDocument : TextDocument
	{

		public CalicoPythonDocument (MainWindow calico, string filename, string language, string mimetype) :
            	   base(calico, filename, language, mimetype)
		{
		}
		
		public override string [] GetAuthors() 
		{
    	    return new string[] {
				"Microsoft Corporation",
				"The IronPython Team"
			};
    	}
	}
	
	public class CalicoPythonLanguage : Language
	{
		public CalicoPythonLanguage () : 
			base("python",  "Python", new string[] { "py", "pyw" }, "text/x-python")
		{
		  LineComment = "##";
		}
		
		public override void MakeEngine (LanguageManager manager)
		{
			engine = new CalicoPythonEngine (manager);
		}

		public override Document MakeDocument (MainWindow calico, string filename)
		{
			return new CalicoPythonDocument (calico, filename, name, mimetype);
		}

		public static new Language MakeLanguage ()
		{
			return new CalicoPythonLanguage ();
		}

		public override string GetUseLibraryString(string fullname) {
			string bname = System.IO.Path.GetFileNameWithoutExtension (fullname);
			return String.Format ("import {0}\n", bname);
		}		
	}
}

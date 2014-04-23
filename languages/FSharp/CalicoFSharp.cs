//
//  CalicoFSharp.cs
//  
//  Author:
//       Douglas S. Blank <dblank@cs.brynmawr.edu>
//       Keith O'Hara <kohara@bard.edu>
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

#pragma warning disable 612

using System;
using System.IO;
using System.Reflection;
using System.Diagnostics;
using System.Threading;
using System.Collections.Generic;
using Calico;
using System.Text;
using Microsoft.FSharp.Compiler;
using Microsoft.FSharp.Compiler.Interactive;
using Microsoft.FSharp.Core;

public class CalicoFSharpEngine : Engine
{
    Shell.FsiEvaluationSession fsiSession;
    StringBuilder sbOut = new StringBuilder();
    StringBuilder sbErr = new StringBuilder();
    Boolean finishedLoading = false;
    object loadingLock;
    
    public CalicoFSharpEngine (LanguageManager manager) : base(manager)    
    {
        var inStream = new StringReader("");
        var outStream = new StringWriter(sbOut);
        var errStream = new StringWriter(sbErr);
        
        // Build command line arguments & start FSI session
        string[] allArgs = { "--noninteractive"};
        
        var fsiConfig = Shell.FsiEvaluationSession.GetDefaultConfiguration();
        fsiSession = new Shell.FsiEvaluationSession(fsiConfig, allArgs, inStream, 
                                                    outStream, errStream);
	loadingLock = new object();
	lock (loadingLock) {
	    finishedLoading = false;
	}
    }

    private void Report()
    {
	Console.Write(sbOut);
	Console.Error.Write(sbErr);
	sbOut.Clear();
	sbErr.Clear();    
    }

    public IList<Assembly> GetCalicoLibraries() {
	List<Assembly> assemblies = new List<Assembly>();
	// First, get system assemblies:
	foreach (Assembly assembly in AppDomain.CurrentDomain.GetAssemblies()) {
	    assemblies.Add(assembly);
	}
	// Add Calico.exe
	Assembly calico_assembly = System.Reflection.Assembly.LoadFile(Path.Combine(calico.path, "Calico.exe"));
	assemblies.Add(calico_assembly);
	// Now manually get the Calico libraries:
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
		Assembly assembly = System.Reflection.Assembly.LoadFile(assembly_name);
		if (assembly != null) {
		    // if already in, move it to end of line:
		    if (assemblies.Contains(assembly)) {
			assemblies.Remove(assembly);
		    }
		    assemblies.Add(assembly);
		}
	    }
	}
	return assemblies;
    }

    private void loadAssemblies()
    {
        string modules_path = Path.Combine(calico.path, "..", "modules");
        modules_path = String.Format("#I \"\"\"{0}\"\"\";;", modules_path);
        if (calico.Debug) {
            Console.WriteLine(modules_path);
            Report();
        }
        fsiSession.EvalInteraction(modules_path);
        IList<Assembly> assemblies = GetCalicoLibraries();
        var load_thread = new System.Threading.Thread (delegate () {
                foreach (Assembly assemblyName in assemblies) {
                    try {
                        string assembly =  assemblyName.Location;                    
                        string assembly_path = "#r \"\"\"" + assembly + "\"\"\";;";
                        if (assembly != "") {
                            if (calico.Debug) {
                                Console.WriteLine(assembly_path);
                            }
                            fsiSession.EvalInteraction(assembly_path);
                        }
                    } catch {
                        if (calico.Debug) {
                            Console.Error.WriteLine("load failed: \"{0}\"", assemblyName);
                        }
                    } finally {
                        sbOut.Clear();
                        sbErr.Clear();
                    }            
                }
                try {
                    fsiSession.EvalInteraction("let calico = Calico.MainWindow._mainWindow;;");               
                } catch {                
                    calico.stderr.WriteLine("F# error: setting 'calico' variable failed");
                }
                if (calico.Debug) {
                    Console.WriteLine("DEBUG: " + sbOut);
                    Console.Error.WriteLine("DEBUG: " + sbErr);
                }
                sbOut.Clear();
                sbErr.Clear();

		lock(loadingLock) {
		    finishedLoading = true;
		}
            });
        load_thread.Start();
    }
    
     public override object Evaluate(string code) {      
	 while (true) {
	     lock(loadingLock) {
		 if (finishedLoading) {
		     break;
		 }
	     }
	 }
         bool shouldReturn = false;
         
         try
         {
             fsiSession.EvalInteraction(code);
             shouldReturn = sbOut.ToString().Contains("val it :");
         } catch {
         } finally{
             if (!shouldReturn) Console.Write(sbOut);
             Console.Error.Write(sbErr);
             sbOut.Clear();
             sbErr.Clear();
         }

         object returnvalue = null;
         if (shouldReturn){
             try{ 
                 var value = fsiSession.EvalExpression("it");
                 if (FSharpOption<Shell.FsiValue>.get_IsSome(value)) {
                     returnvalue = value.Value.ReflectionValue;
                 }
             } catch {
             } finally {
                 sbOut.Clear();
                 sbErr.Clear();
             }
         }
         if (shouldReturn){
             return returnvalue;
         }
         else {             
             return null;
         }
     }
    
    public override bool Execute(string code) {
	 while (true) {
	     lock(loadingLock) {
		 if (finishedLoading) {
		     break;
		 }
	     }
	 }
        object v = Evaluate(code);
        Console.WriteLine(v);
        return false;
    }
    
    public override bool ExecuteFile(string filename) {
	 while (true) {
	     lock(loadingLock) {
		 if (finishedLoading) {
		     break;
		 }
	     }
	 }
        try {
            fsiSession.EvalScript(filename);
            return true;
        } catch {
        } finally{
            Report(); // FIXME: may want to print out as we go
        }
        return false;
    }
    
    
    public override void Close() {
    }
    
    public override void PostSetup(MainWindow calico) {
        base.PostSetup(calico);
        this.calico = calico;
        loadAssemblies();
    }
    
    public override bool ReadyToExecute(string text) {
	lock(loadingLock) {
	    return finishedLoading;
	}
    }
}

public class CalicoFSharpDocument : TextDocument {
    
    public CalicoFSharpDocument(MainWindow calico, string filename, string language, string mimetype) :
    base(calico, filename, language, mimetype) {
    }
}

public class CalicoFSharpLanguage : Language
{
    public CalicoFSharpLanguage () : 
    base("fsharp",  "F#", new string[] { "fs" }, "text/x-fsharp")
    {
        LineComment = "//";
    }
    
    public override void MakeEngine (LanguageManager manager)
    {
        engine = new CalicoFSharpEngine (manager);
    }
    
    public override Document MakeDocument(MainWindow calico, string filename) {
        return new CalicoFSharpDocument(calico, filename, name, mimetype);
    }
    
    public static new Language MakeLanguage ()
    {
        return new CalicoFSharpLanguage ();
    }
    
    public override string GetUseLibraryString(string fullname) {
        string bname = System.IO.Path.GetFileNameWithoutExtension(fullname);
        return String.Format("open {0}\n", bname);
    }
}

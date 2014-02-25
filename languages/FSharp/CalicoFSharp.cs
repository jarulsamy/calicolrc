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
        
        Console.WriteLine(sbOut);
        Console.Error.WriteLine(sbErr);
        sbOut.Clear();
        sbErr.Clear();
        finishedLoading = false;
    }
    
    // Like for this to occur in PostSetup but that doesnt' seem to be the right thing either
    private void loadAssemblies()
    {
        string modules_path = Path.Combine(calico.path, "..", "modules");
        modules_path = String.Format("#I \"\"\"{0}\"\"\";;", modules_path);
        if (calico.Debug) {
            Console.WriteLine(modules_path);
            Console.WriteLine("DEBUG: " + sbOut);
            Console.Error.WriteLine("DEBUG: " + sbErr);
            sbOut.Clear();
            sbErr.Clear();
        }
        fsiSession.EvalInteraction(modules_path);
        Assembly[] assemblies = AppDomain.CurrentDomain.GetAssemblies();
        var load_thread = new System.Threading.Thread (delegate () {
                Array.Reverse(assemblies);
                foreach (Assembly assemblyName in assemblies) {
                    try {
                        string assembly =  assemblyName.Location;                    
                        string assembly_path = "#r \"\"\"" + assembly + "\"\"\";;";
                        if (assembly != "") {
                            if (calico.Debug) {
                                Console.WriteLine(assembly_path);
                            }
                            fsiSession.EvalInteraction(assembly_path);
                            if (calico.Debug) {
                                Console.WriteLine("DEBUG: " + sbOut);
                                Console.Error.WriteLine("DEBUG: " + sbErr);
                                sbOut.Clear();
                                sbErr.Clear();
                            }
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
                } else {
                    sbOut.Clear();
                    sbErr.Clear();
                }
                finishedLoading = true;
            });
        load_thread.Start();
    }
    
     public override object Evaluate(string code) {      
         if (! finishedLoading) {
             return null;
         }
         bool shouldReturn = false;
         
         try
         {
             fsiSession.EvalInteraction(code);
         } catch {
         } finally{
             shouldReturn = sbOut.ToString().Contains("val it :");
             if (!shouldReturn) Console.Write(sbOut);
             Console.Error.Write(sbErr);
             sbOut.Clear();
             sbErr.Clear();
         }
         
         if (shouldReturn){
             try{ 
                 var value = fsiSession.EvalExpression("it");
                 if (FSharpOption<Shell.FsiValue>.get_IsSome(value)) {
                     return (value.Value.ReflectionValue);
                 }
             } catch {
             } finally {
                 sbOut.Clear();
                 sbErr.Clear();
             }
         }
         return null;
     }
    
    public override bool Execute(string code) {
        if (! finishedLoading) {
            return false;
        }
        object v = Evaluate(code);
        Console.WriteLine(v);
        /*
          try
          { 
          var value = fsiSession.EvalExpression(code);
          if (FSharpOption<Shell.FsiValue>.get_IsSome(value)) 
          System.Console.WriteLine(value.Value.ReflectionValue);
          return true;
          }catch (Exception e) {
          //Console.Error.WriteLine(e.Message);
          //Console.Error.WriteLine(e.InnerException);
          }finally{
          Console.WriteLine(sbOut);
          Console.Error.WriteLine(sbErr);
          sbOut.Clear();
          sbErr.Clear();
          }*/
        return false;
    }
    
    public override bool ExecuteFile(string filename) {
        if (! finishedLoading) {
            return false;
        }
        try {
            fsiSession.EvalScript(filename);
            return true;
        } catch {
        } finally{
            Console.Write(sbOut);
            Console.Error.Write(sbErr);
            sbOut.Clear();
            sbErr.Clear();
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
        return finishedLoading;
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

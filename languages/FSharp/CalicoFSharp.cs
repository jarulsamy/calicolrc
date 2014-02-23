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
    Boolean firstEval = true;

    public CalicoFSharpEngine (LanguageManager manager) : base(manager)    
    {
        var inStream = new StringReader("");
        var outStream = new StringWriter(sbOut);
        var errStream = new StringWriter(sbErr);
            
        // Build command line arguments & start FSI session
        string[] allArgs = { "--noninteractive", "--lib:modules/"};
        
        var fsiConfig = Shell.FsiEvaluationSession.GetDefaultConfiguration();
        fsiSession = new Shell.FsiEvaluationSession(fsiConfig, allArgs, inStream, 
                                                    outStream, errStream);
    }
    
    // Like for this to occur in PostSetup but that doesnt' seem to be the right thing either
    private void loadAssemblies()
    {
        if (firstEval)
        {
            foreach (Assembly assembly in AppDomain.CurrentDomain.GetAssemblies()){
                try{
                    if (assembly.Location != "")   
                        fsiSession.EvalInteraction("#r \"" + assembly.Location.Replace(".dll.dll", ".dll") + "\";;");
                    //Console.WriteLine("#r \"" + assembly.Location.Replace(".dll.dll", ".dll") + "\";;");
                }catch {
                    //System.Console.WriteLine(e);
                    //System.Console.WriteLine(assembly);
                }
            }                
            firstEval = false;
            //Console.WriteLine(sbOut);
            //Console.Error.WriteLine(sbErr);
            sbOut.Clear();
            sbErr.Clear();
        }
    }   

    
    public override object Evaluate(string code) {      
        try
        {
            loadAssemblies();
            var results = fsiSession.ParseAndCheckInteraction(code);
            //Console.WriteLine(results.Item2.Errors);
            //var value = fsiSession.EvalExpression(code);
            fsiSession.EvalInteraction(code);
            //return value.Value.ReflectionValue;
            
            /*let  errors3, exitCode3, dynAssembly3 = 
              scs.CompileToDynamicAssembly([| "-o"; fn3; "-a"; fn2 |], Some(stdout,stderr))*/
            
        }catch (Exception e) {
            //Console.Error.WriteLine(e.Message);
            //Console.Error.WriteLine(e.InnerException);
        }finally{
            Console.WriteLine(sbOut);
            Console.Error.WriteLine(sbErr);
            sbOut.Clear();
            sbErr.Clear();
        }
        return null;
    }

    public override bool Execute(string code) {
        Evaluate(code);
        /*
            try
        { 
            loadAssemblies();
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
        try
        {
            loadAssemblies();
            fsiSession.EvalScript(filename);
            return true;
        }catch (Exception e) {
            //Console.Error.WriteLine(e.Message);
            //Console.Error.WriteLine(e.InnerException);
        }finally{
            Console.WriteLine(sbOut);
            Console.Error.WriteLine(sbErr);
            sbOut.Clear();
            sbErr.Clear();
        }
        return false;
    }

	 
    public override void Close() {
     }
    
    public override void PostSetup(MainWindow calico) {
        base.PostSetup(calico);
	 }
    	
    public override bool ReadyToExecute(string text) {
        return true;
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

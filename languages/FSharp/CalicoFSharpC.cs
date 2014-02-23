

//
//  CalicoFSharpC.cs - EXTREMELY HACKY - WORK IN PORGRESS
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
using System.CodeDom.Compiler;
using System.Text;
using Microsoft.FSharp.Compiler.CodeDom;

public class CalicoFSharpEngine : Engine
{
    FSharpCodeProvider compiler; 
    System.CodeDom.Compiler.CompilerParameters cp;
    int x = 0;
    public static object res;
    string filename;

    public CalicoFSharpEngine (LanguageManager manager) : base(manager)    
    {
        x = CalicoFSharpMod.testvaluez;
    }
    
    public static string combine(params string [] items) {
        string retval = "";
        foreach (string item in items) {
            if (retval == "")
                retval = item;
            else
                retval = System.IO.Path.Combine(retval, item);
        }
        return retval;
     }
	 
	 public override object Evaluate(string code) {
         compiler = new FSharpCodeProvider();
         cp = new System.CodeDom.Compiler.CompilerParameters();

         filename = System.IO.Path.GetTempPath() + Guid.NewGuid().ToString() + ".exe";        
          
         foreach (Assembly assembly in AppDomain.CurrentDomain.GetAssemblies()){
             try{
                 if (assembly.Location != "") cp.ReferencedAssemblies.Add(assembly.Location);
                 //System.Console.WriteLine(assembly);
             }catch {
                 //System.Console.WriteLine(e);
                 //System.Console.WriteLine(assembly);
             }
         }


         cp.GenerateInMemory = false;
         cp.OutputAssembly = filename;
         cp.WarningLevel = 4;
         cp.GenerateExecutable = true;
         cp.TreatWarningsAsErrors = false;             
         res = null;

         /*         try{             
             FileInfo currentFile = new FileInfo(filename);
             currentFile.Delete();
         }
         catch (Exception ex){             
             Console.Error.WriteLine("Error on file: {0} {1}", filename, ex.Message);
         }*/
             
         if (!(code.StartsWith("let") && !code.StartsWith("open"))) {
             code = "module CalicoFSharpMod\nopen CalicoFSharpMod\n"  + "CalicoFSharpEngine.res <- " + code + "\n";
             code += "System.Console.WriteLine(CalicoFSharpEngine.res)\n";
         }
         else {
             code = "module CalicoFSharpMod\nopen CalicoFSharpMod\n" + code + "\n";
         }
         
         var cr = compiler.CompileAssemblyFromSource(cp, code);
         //var cr = compiler.CompileAssemblyFromSource(cp, code + " |> System.Console.WriteLine");
         foreach (object s in cr.Output){            
             System.Console.WriteLine(s);
         }
         foreach (object s in cr.Errors){
             System.Console.Error.WriteLine(s);
         }

         try{
             cr.CompiledAssembly.EntryPoint.Invoke(null, null);
         }catch (Exception e){
             System.Console.Error.WriteLine(e);
         }
         
         return res;
	 }


	 public override bool Execute(string code) {

         compiler = new FSharpCodeProvider();
         cp = new System.CodeDom.Compiler.CompilerParameters();
          
         foreach (Assembly assembly in AppDomain.CurrentDomain.GetAssemblies()){
             try{
                 cp.ReferencedAssemblies.Add(assembly.Location);                 
                 //System.Console.WriteLine(assembly);
             }catch{
                 //System.Console.WriteLine(e);
                 //System.Console.WriteLine(assembly);
             }
         }
         
         cp.GenerateInMemory = false;
         string filename = System.IO.Path.GetTempPath() + Guid.NewGuid().ToString() + ".exe";
         cp.OutputAssembly = filename;
         cp.WarningLevel = 4;
         cp.GenerateExecutable = true;
         cp.TreatWarningsAsErrors = false;    

         var cr = compiler.CompileAssemblyFromSource(cp, "module CalicoFSharpMod\nopen CalicoFSharpMod\n" + code);
         //var cr = compiler.CompileAssemblyFromSource(cp, code + " |> System.Console.WriteLine");
         foreach (object s in cr.Output){            
             System.Console.WriteLine(s);
         }
         foreach (object s in cr.Errors){
             System.Console.Error.WriteLine(s);             
         }
         try{
             cr.CompiledAssembly.EntryPoint.Invoke(null, null);
         }catch{
         }
         return true;
	 }
	 
     public override void Close() {
     }
	 	 
	 public override void PostSetup(MainWindow calico) {
	   base.PostSetup(calico);
	 }
	 
    public override bool ExecuteFile(string filename) {
        StringBuilder sb = new StringBuilder();
        String line;
        using (StreamReader sr = new StreamReader(filename))
        {            
            while ((line = sr.ReadLine()) != null) sb.AppendLine(line);           
        }

        compiler = new FSharpCodeProvider();
        cp = new System.CodeDom.Compiler.CompilerParameters();
        
        foreach (Assembly assembly in AppDomain.CurrentDomain.GetAssemblies()){
            try{
                cp.ReferencedAssemblies.Add(assembly.Location);                 
                //System.Console.WriteLine(assembly);
            }catch {
                //System.Console.WriteLine(e);
                //System.Console.WriteLine(assembly);
            }
        }
        
        cp.GenerateInMemory = false;
        string filenamez = System.IO.Path.GetTempPath() + Guid.NewGuid().ToString() + ".exe";
        cp.OutputAssembly = filenamez;
        cp.WarningLevel = 4;
        cp.GenerateExecutable = true;
        cp.TreatWarningsAsErrors = false;    
        
        
        var cr = compiler.CompileAssemblyFromSource(cp, "module CalicoFSharpMod\nopen CalicoFSharpMod\n" + sb.ToString());
        //var cr = compiler.CompileAssemblyFromSource(cp, code + " |> System.Console.WriteLine");
        foreach (object s in cr.Output){            
            System.Console.WriteLine(s);
        }
        foreach (object s in cr.Errors){            
            System.Console.WriteLine(s);             
        }
        try{             
            cr.CompiledAssembly.EntryPoint.Invoke(null, null);
        }catch{
        }
        return true;
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

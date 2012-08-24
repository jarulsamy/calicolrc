//
//  CalicoScheme.cs
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

#pragma warning disable 612

using System;
using System.IO;
using System.Diagnostics;
using System.Threading;
using System.Collections.Generic;
using Calico;

public class CalicoFSharpEngine : Engine
{
     public Process process;

	 public CalicoFSharpEngine (LanguageManager manager) : base(manager)
	 {
	   string path = make_path("");
	   string gtk = make_path("gtk-sharp");
	   string gdk = make_path("gdk-sharp");
	   string glib = make_path("glib-sharp");
	   string atk = make_path("atk-sharp");
	   string fsi = combine(path, "fsi.exe");
	   ProcessStartInfo startInfo = new ProcessStartInfo();
	   // FIXME: on windows, run directly; on others need mono in path
	   string arguments = "";
	   if (System.Environment.OSVersion.Platform.ToString().StartsWith("Win")) {
	     startInfo.FileName = fsi;
	   } else {
	     startInfo.FileName = "mono";
	     arguments = "\"" + fsi + "\" ";
	   }
	   startInfo.Arguments = (
				  arguments + 
				  "--readline- " + 
				  "--lib:\"" + combine(path, "..", "..", "modules") +"\" " +
				  "--lib:\"" + combine(path, "..", "..", "bin") + "\" "
				  //"--lib:\"" + gtk + "\" " +
				  //"--lib:\"" + gdk + "\" " +
				  //"--lib:\"" + glib + "\" " +
				  //"--lib:\"" + atk + "\" " +
				  //"-r:atk-sharp.dll " +
				  //"-r:gdk-sharp.dll " +
				  //"-r:glib-sharp.dll " +
				  //"-r:Myro.dll " + 
				  //"-r:gtk-sharp.dll " +
				  //"-r:Mono.Cairo.dll " + 
				  //"-r:Myro.dll " + 
				  //"-r:Graphics.dll"
				  );
	   startInfo.UseShellExecute = false;
	   startInfo.RedirectStandardError = true;
	   startInfo.CreateNoWindow = true;
	   startInfo.RedirectStandardOutput = true;
	   startInfo.RedirectStandardInput = true;

	   //System.Console.WriteLine(startInfo.FileName);
	   //System.Console.WriteLine(startInfo.Arguments);

	   
	   process = Process.Start(startInfo);
	   process.EnableRaisingEvents = true;
	   process.OutputDataReceived += new DataReceivedEventHandler(outputHandler);
	   process.ErrorDataReceived += new DataReceivedEventHandler(errorHandler);
	   process.BeginOutputReadLine();
	   process.BeginErrorReadLine();
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
	 
     public static string make_path(string assembly) {
	   string retval = "";
	   if (assembly == "") {
		 retval = System.IO.Path.GetDirectoryName(
			 System.Reflection.Assembly.GetExecutingAssembly().GetName().CodeBase).Substring(5);
	   } else {
		 retval = System.IO.Path.GetDirectoryName(
             System.Reflection.Assembly.LoadWithPartialName(assembly).CodeBase).Substring(5);
	   }
	   if (retval.StartsWith("\\")) {
		 retval = retval.Substring(1);
	   }
	   return retval;
     }
	 
	 public override bool Execute(string text) {
	   process.StandardInput.WriteLine(text);
	   return true;
	 }
	 
     public void Close() {
	   process.StandardInput.Close();
	   process.Close();
     }
	 
     private static void outputHandler(object sendingProcess,
		 DataReceivedEventArgs outLine) {
	   if (!String.IsNullOrEmpty(outLine.Data)) {
		 System.Console.WriteLine(outLine.Data);
	   }
     }
	 
     private static void errorHandler(object sendingProcess,
		 DataReceivedEventArgs outLine) {
	   if (!String.IsNullOrEmpty(outLine.Data)) {
		 System.Console.Error.WriteLine(outLine.Data);
	   }
     }
	 
	 public override void PostSetup(MainWindow calico) {
	   base.PostSetup(calico);
	 }
	 
	 public override bool ExecuteFile(string filename) {
	   Evaluate(String.Format("#load \"{0}\";;", filename));
	   return true;
	 }
	 
	 public override bool ReadyToExecute(string text) {
	   string [] lines = text.Split('\n');
	   int last = lines.Length - 1;
	   if (lines[last].Trim() == "") {
		 return true;
	   } else if (lines[last].StartsWith(" ")) {
		 return false;
	   } else if (! lines[last].Trim().EndsWith(";;")) {
		 return false;
	   }
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

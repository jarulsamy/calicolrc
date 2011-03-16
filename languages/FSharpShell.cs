using System;
using System.IO;
using System.Diagnostics;
using System.Threading;

public class FSharpShell {
     public Process process;
     public FSharpShell() {
	     string path = System.IO.Path.GetDirectoryName(
			 System.Reflection.Assembly.GetExecutingAssembly().GetName().CodeBase).Substring(5);
	     if (path.StartsWith("\\")) {
		   path = path.Substring(1);
	     }
#pragma warning disable 612
		 string gtk = System.IO.Path.GetDirectoryName(
			 System.Reflection.Assembly.LoadWithPartialName("gtk-sharp").CodeBase).Substring(5);
		 string gdk = System.IO.Path.GetDirectoryName(
			 System.Reflection.Assembly.LoadWithPartialName("gdk-sharp").CodeBase).Substring(5);
		 string glib = System.IO.Path.GetDirectoryName(
			 System.Reflection.Assembly.LoadWithPartialName("glib-sharp").CodeBase).Substring(5);
		 string atk = System.IO.Path.GetDirectoryName(
			 System.Reflection.Assembly.LoadWithPartialName("atk-sharp").CodeBase).Substring(5);
		 string fsi = System.IO.Path.Combine(path, "fsi.exe");
         ProcessStartInfo startInfo = new ProcessStartInfo();
         startInfo.FileName = "mono";
         startInfo.Arguments = (fsi + " " +
			 "--readline- " + 
			 "--lib:" + path + "/../modules " + 
			 "--lib:" + path + "/../bin " + 
			 "--lib:" + gtk + " " + 
			 "--lib:" + gdk + " " + 
			 "--lib:" + glib + " " + 
			 "--lib:" + atk + " " + 
			 "-r:atk-sharp.dll " +
			 "-r:gdk-sharp.dll " +
			 "-r:glib-sharp.dll " +
			 "-r:Myro.dll " + 
			 "-r:gtk-sharp.dll " +
			 "-r:Mono.Cairo.dll " + 
			 "-r:Myro.dll " + 
			 "-r:Graphics.dll");
         startInfo.UseShellExecute = false;
         startInfo.RedirectStandardError = true;
         startInfo.CreateNoWindow = true;
         startInfo.RedirectStandardOutput = true;
         startInfo.RedirectStandardInput = true;

         process = Process.Start(startInfo);
         process.EnableRaisingEvents = true;
         process.OutputDataReceived += new DataReceivedEventHandler(outputHandler);
         process.ErrorDataReceived += new DataReceivedEventHandler(errorHandler);
         process.BeginOutputReadLine();
         process.BeginErrorReadLine();
     }

     public void Evaluate(string command) {
         process.StandardInput.WriteLine(command);
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
}

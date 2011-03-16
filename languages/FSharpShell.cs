using System;
using System.IO;
using System.Diagnostics;
using System.Threading;

public class FSharpShell {
     public Process process;
     public FSharpShell() {
         ProcessStartInfo startInfo = new ProcessStartInfo();
         startInfo.FileName = "mono";
         startInfo.Arguments = ("/home/dblank/Pyjama/languages/fsi.exe " + 
	    		        "--readline- " + 
			        "--lib:/home/dblank/Pyjama/modules " + 
			        "--lib:/home/dblank/Pyjama/bin " + 
			        "--lib:/usr/lib/cli/gtk-sharp-2.0 " + 
				"--lib:/usr/lib/cli/glib-sharp-2.0 " + 
				"--lib:/usr/lib/cli/gdk-sharp-2.0 " + 
				"--lib:/usr/lib/cli/atk-sharp-2.0 " + 
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

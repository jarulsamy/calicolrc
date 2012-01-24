
namespace Graphviz4Net
{
    using System;
    using System.Diagnostics;
    using System.IO;

    public class DotExeRunner : IDotRunner
    {
        public StreamReader RunDot(string os_name, string startup_path, Action<StreamWriter> writeGraph)
        {
            using (var process = new Process())
            {
                process.StartInfo = new ProcessStartInfo
                                        {
                                            CreateNoWindow = true,
                                            UseShellExecute = false,
                                            Arguments = "-Tdot",
                                            //FileName = "dot.exe",
                                            RedirectStandardOutput = true,
                                            RedirectStandardInput = true
                                        };
				
				if (os_name == "Windows") {
					string file = startup_path;
					file = Path.Combine (file, "bin");
					file = Path.Combine (file, "windows");
					file = Path.Combine (file, "graphviz");
					process.StartInfo.FileName = Path.Combine (file, "dot.exe");
				} else {
					if (File.Exists ("/usr/bin/dot")) {
						// assumes espeak is in /usr/bin/ on macs
						process.StartInfo.FileName = "dot";
					} else if (File.Exists ("/usr/local/bin/dot")) {
						// or look for espeak is in /usr/local/bin/ on macs
						process.StartInfo.FileName = "dot";
					} else {
						// assumes in path
						process.StartInfo.FileName = "dot";
					}
				}
                process.Start();
                process.StandardInput.AutoFlush = true;
                writeGraph(process.StandardInput);
                process.StandardInput.Close();
                return process.StandardOutput;
            }
        }
    }
}
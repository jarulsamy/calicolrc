
namespace Graphviz4Net
{
    using System;
    using System.Diagnostics;
    using System.IO;

    public class DotExeRunner : IDotRunner
    {
        public StreamReader RunDot(Action<StreamWriter> writeGraph)
        {
            using (var process = new Process())
            {
                process.StartInfo = new ProcessStartInfo
                                        {
                                            CreateNoWindow = true,
                                            UseShellExecute = false,
                                            Arguments = "-Tdot",
                                            FileName = "dot.exe",
                                            RedirectStandardOutput = true,
                                            RedirectStandardInput = true
                                        };
                process.Start();
                process.StandardInput.AutoFlush = true;
                writeGraph(process.StandardInput);
                process.StandardInput.Close();
                return process.StandardOutput;
            }
        }
    }
}
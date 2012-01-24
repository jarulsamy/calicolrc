
namespace Graphviz4Net
{
    using System;
    using System.IO;
    using System.Text;

    public class DotRunnerLogDecorator : IDotRunner
    {
        private readonly IDotRunner runner;

        private readonly string filename;

        public DotRunnerLogDecorator(IDotRunner runner, string filename = "tmp")
        {
            this.runner = runner;
            this.filename = filename;
        }

        public StreamReader RunDot(string os, string path, Action<StreamWriter> writeGraph)
        {
            using (var memoryStream = new MemoryStream())
            {
                // we write the graph into the file on disk
                using (var writer = new StreamWriter(memoryStream))
                {
                    writeGraph(writer);
                }

                memoryStream.Close();
                string graph = Encoding.UTF8.GetString(memoryStream.GetBuffer());
                File.WriteAllText(this.filename + ".dot", graph);

                // now we read the file and write it to the real process input.
                using (var reader = this.runner.RunDot(os, path, w => w.Write(graph)))
                {
                    // we read all output, save it into another file, and return it as a memory stream
                    var text = reader.ReadToEnd();
                    File.WriteAllBytes(this.filename + ".layout.dot", Encoding.UTF8.GetBytes(text));
                    return new StreamReader(new MemoryStream(Encoding.UTF8.GetBytes(text)));
                }
            }
        }
    }
}
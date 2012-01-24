
namespace Graphviz4Net
{
    using System;
    using System.IO;

    public interface IDotRunner
    {
        StreamReader RunDot(string os, string startdir, Action<StreamWriter> writeGraph);
    }
}

namespace Graphviz4Net.Dot
{
    using System.IO;

    public interface IDotParser
    {
        DotGraph Parse(StreamReader reader);
    }
}

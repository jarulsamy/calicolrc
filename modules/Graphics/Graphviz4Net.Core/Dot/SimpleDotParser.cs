
namespace Graphviz4Net.Dot
{
    using System;
    using System.Collections.Generic;
    using System.Text.RegularExpressions;
    using System.IO;

    /// <summary>
    /// This simple parser needs the dot source to have specific format. 
    /// The parser uses regular expressions to parse the source.
    /// (In the future we should use generated parser using ANTLR)
    /// </summary>
    public class SimpleDotParser : IDotParser
    {
        public DotGraph Parse(StreamReader reader)
        {
            string line;
            var vertices = new List<DotVertex>();
            line = reader.ReadLine();  // this should be "diagraph g {"

            var result = new DotGraph();
            var lineSplitter = new Regex(@"(?<preamble>[^\[]*)\[(?<attributes>[^\]]*)\]");
            var edgeSplitter = new Regex(@"(?<first>[0-9]*)[^0-9]*(?<second>[0-9]*)");
            Match match;
            while ((line = reader.ReadLine()) != null)
            {
                line = line.Trim();
                if (line.Trim().StartsWith("graph"))
                {
                    match = Regex.Match(line, "\\[bb=\"(?<data>[^\"]*)\"\\]");
                    if (match == null || match.Success == false)
                    {
                        continue;
                    }

                    result.Attributes.Add("bb", match.Result("${data}"));
                    continue;
                }

                if (line.StartsWith("node") || line.Trim() == "}")
                {
                    continue;
                }

                match = lineSplitter.Match(line);
                var preamble = match.Result("${preamble}");
                var attributes = match.Result("${attributes}");

                if (preamble.Contains("-"))
                {
                    var edgeData = edgeSplitter.Match(preamble);
                    var source = edgeData.Result("${first}");
                    var dest = edgeData.Result("${second}");

                    int sourceId, destId;
                    if (int.TryParse(source, out sourceId) == false || int.TryParse(dest, out destId) == false)
                    {
                        continue;
                    }

                    var edge = new DotEdge(vertices[sourceId], vertices[destId]);
                    AddAttributes(attributes, edge.Attributes);
                    result.AddEdge(edge);
                }
                else
                {
                    int id;
                    if (int.TryParse(preamble.Trim(), out id) == false)
                    {
                        continue;
                    }

                    var vertex = new DotVertex(id);
                    AddAttributes(attributes, vertex.Attributes);
                    result.AddVertex(vertex);
                    vertices.InsertAt(id, vertex);
                }
            }

            return result;
        }

        private static void AddAttributes(string data, IDictionary<string, string> attributes)
        {
            var pairs = data.Split(new[] {", "}, StringSplitOptions.RemoveEmptyEntries);
            foreach (var pair in pairs)
            {
                var items = pair.Split(new[] {"="}, 2, StringSplitOptions.None);
                if (items.Length < 2)
                {
                    continue;
                }

                if (items[1].StartsWith("\""))
                {
                    // get rid of "value" in quotes
                    items[1] = items[1].Substring(1, items[1].Length - 1).Substring(0, items[1].Length - 2);
                }

                attributes.Add(items[0], items[1]);
            }
        }
    }
}

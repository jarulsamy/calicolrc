
namespace Graphviz4Net.Dot.AntlrParser
{
    using System.Collections.Generic;

    /// <summary>
    /// Code behind file for generated parser class.
    /// </summary>
    public partial class DotGrammarParser
    {
	    private readonly DotGraph dotGraph = new DotGraph();

        private readonly IList<DotVertex> vertices = new List<DotVertex>();

        private DotSubGraph subGraph;

	    public DotGraph Graph
	    {
		    get { return this.dotGraph; }
	    }

        public void EnterSubGraph(string name)
        {
            this.subGraph = new DotSubGraph {Name = name};
            this.dotGraph.AddSubGraph(this.subGraph);
        }

        public void LeaveSubGraph()
        {
            this.subGraph = null;
        }

        public void AddGraphAttributes(IDictionary<string, string> attributes)
        {
            if (attributes == null)
            {
                return;
            }

            foreach (var attribute in attributes)
            {
                if (this.subGraph == null)
                {
                    this.Graph.Attributes.Add(attribute);
                }
                else
                {
                    this.subGraph.Attributes.Add(attribute);
                }
            }
        }

        public string Unquote(string str)
        {
            return str.Substring(1, str.Length - 1).Substring(0, str.Length - 2).Replace(@"\", string.Empty);
        }

        public void AddEdge(string sourceStr, string targetStr, IDictionary<string, string> attributes)
        {
            int target, source;
            if (int.TryParse(targetStr, out target) == false ||
                int.TryParse(sourceStr, out source) == false)
            {
                throw new ParserException();
            }

            if (target < 0 || source < 0 ||
                target > this.vertices.Count || source > vertices.Count ||
                this.vertices[target] == null || this.vertices[source] == null)
            {
                throw new ParserException();
            }

            this.Graph.AddEdge(new DotEdge(this.vertices[source], this.vertices[target], attributes));
        }

        public void AddVertex(string idStr, IDictionary<string, string> attributes)
        {
            int id;
            if (int.TryParse(idStr, out id) == false)
            {
                throw new ParserException();
            }

            var vertex = new DotVertex(id, attributes);
            this.vertices.InsertAt(id, vertex);

            if (this.subGraph == null)
            {
                this.dotGraph.AddVertex(vertex);
            }
            else
            {
                this.subGraph.AddVertex(vertex);
            }
        }
    }
}

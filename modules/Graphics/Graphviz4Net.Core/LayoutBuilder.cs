
namespace Graphviz4Net
{
    using System.Collections.Generic;
    using System.Linq;
    using System.Windows;
    using Dot;
    using Graphs;

    /// <summary>
    /// Abstract class providers basic implementation of some of the <see cref="ILayoutBuilder"/> 
    /// methods. Methods that are necessary to override in order to implement correct builder are abstract.
    /// </summary>
    public abstract class LayoutBuilder : ILayoutBuilder
    {
        public virtual void Start(IGraph originalGraph)
        {
        }

        public virtual void BuildGraph(double width, double height, IGraph originalGraph, DotGraph dotGraph)
        {
        }

        public abstract void BuildVertex(Point position, double width, double height, object originalVertex, DotVertex dotVertex);

        public abstract void BuildSubGraph(
            double leftX, 
            double upperY, 
            double rightX, 
            double lowerY, 
            ISubGraph originalSubGraph,
            DotSubGraph subGraph);

        public abstract void BuildEdge(Point[] path, IEdge originalEdge, DotEdge edge);

        public virtual void Finish()
        {
        }

        public abstract Size GetSize(object vertex);

        public IEnumerable<KeyValuePair<string, string>> GetAdditionalAttributes(object vertex)
        {
            return Enumerable.Empty<KeyValuePair<string, string>>();
        }
    }
}
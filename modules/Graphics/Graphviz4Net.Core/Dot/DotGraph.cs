
namespace Graphviz4Net.Dot
{
    using Graphs;

    public class DotGraph : Graph<DotVertex, DotSubGraph, DotEdge, Edge<DotSubGraph>>
    {
        private BoundingBox bondingBox = null;

		private static DotGraph Parse(string content)
        {
            var antlrStream = new Antlr.Runtime.ANTLRStringStream(content);
            var lexer = new Graphviz4Net.Dot.AntlrParser.DotGrammarLexer(antlrStream);
            var tokenStream = new Antlr.Runtime.CommonTokenStream(lexer);
            var parser = new Graphviz4Net.Dot.AntlrParser.DotGrammarParser(tokenStream);
            parser.dot();
            return parser.Graph;
        }				
		
		private static DotGraph ParseFile(string filename)
        {
			var antlrStream = new Antlr.Runtime.ANTLRFileStream(filename);
            var lexer = new Graphviz4Net.Dot.AntlrParser.DotGrammarLexer(antlrStream);
            var tokenStream = new Antlr.Runtime.CommonTokenStream(lexer);
            var parser = new Graphviz4Net.Dot.AntlrParser.DotGrammarParser(tokenStream);
            parser.dot();
            return parser.Graph;
        }				
		
        public double? Width
        {
            get { return this.BoundingBox.RightX - this.BoundingBox.LeftX; }
        }

        public double? Height
        {
            get { return this.BoundingBox.UpperY - this.bondingBox.LowerY; }
        }

        public BoundingBox BoundingBox
        {
            get
            {
                string newBb;
                this.Attributes.TryGetValue("bb", out newBb);
                if (this.bondingBox == null || this.bondingBox.Equals(newBb) == false)
                {
                    this.bondingBox = new BoundingBox(newBb);
                }

                return this.bondingBox;
            }
        }       
    }
}


namespace Graphviz4Net.Dot.AntlrParser
{
    using Antlr.Runtime;
    using System.IO;

    public class AntlrParserAdapter : IDotParser
    {
        public DotGraph Parse(StreamReader reader)
        {
            var antlrStream = new ANTLRReaderStream(reader);
            var lexer = new DotGrammarLexer(antlrStream);
            var tokenStream = new CommonTokenStream(lexer);
            var parser = new DotGrammarParser(tokenStream);
            parser.dot();
            return parser.Graph;
        }
    }
}

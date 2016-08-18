
namespace Graphviz4Net.Dot.AntlrParser
{
    using System;

    public class ParserException : ApplicationException
    {
        public ParserException()
            : base("The Graphviz4Net parser cannot parse the file.")
        {            
        }

        public ParserException(string message) : base(message)
        {
        }
    }
}

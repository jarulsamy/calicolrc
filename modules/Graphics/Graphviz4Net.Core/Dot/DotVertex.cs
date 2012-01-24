
using System.Globalization;
using System.Windows;

namespace Graphviz4Net.Dot
{
    using System.Collections.Generic;
    using Graphs;

    public class DotVertex : IAttributed
    {
        private readonly IDictionary<string, string> attributes;

        public DotVertex(int id)
            : this(id, new Dictionary<string, string>())
        {
        }

        public DotVertex(int id, IDictionary<string, string> attributes)
        {
            this.Id = id;
            this.attributes = attributes;
        }

        public int Id { get; private set; }

        public double? Width
        {
            get { return Utils.ParseInvariantNullableDouble(this.Attributes.GetValue("width")); }
        }

        public double? Height
        {
            get { return Utils.ParseInvariantNullableDouble(this.Attributes.GetValue("height")); }
        }

        public Point? Position
        {
            get
            {
                var posStr = this.Attributes.GetValue("pos");
                if (string.IsNullOrEmpty(posStr))
                {
                    return null;
                }

                var posParts = posStr.Split(',');
                if (posParts.Length < 2)
                {
                    return null;
                }

                double x, y;
                if (Utils.TryParseInvariantDouble(posParts[0], out x) == false ||
                    Utils.TryParseInvariantDouble(posParts[1], out y) == false)
                {
                    return null;
                }

                return new Point(x, y);
            }
        }

        public IDictionary<string, string> Attributes
        {
            get { return this.attributes; }
        }

        public override string ToString()
        {
            return this.Id.ToString(CultureInfo.InvariantCulture);
        }
    }
}

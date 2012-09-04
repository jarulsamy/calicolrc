using System;
using System.ComponentModel;

namespace Graphviz4Net
{
	[SerializableAttribute]
	[TypeConverterAttribute(typeof(PointConverter))]
	public struct Point : IFormattable
	{
		public double X;
		public double Y;

		public Point(double x, double y) {
			X = x;
			Y = y;
		}

		public override string ToString ()
		{
			return string.Format ("<{0}, {1}>", X, Y);
		}
		public string ToString (string s, System.IFormatProvider provider)
		{
			return string.Format (s);
		}
	}
}


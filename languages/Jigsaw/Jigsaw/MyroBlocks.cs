using System;
using System.Collections;
using System.Collections.Generic;
using System.Xml;
using Cairo;

namespace Jigsaw
{
	// -----------------------------------------------------------------------
    public class CRobot : CBlock
    {	// Robot block shape class
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CRobot(Double X, Double Y, bool isFactory) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + 175, Y + 20)}),
				isFactory) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkRed;
			this.FillColor = Diagram.Colors.LightPink;
			this.Sizable = false;
		}
		
		public CRobot(Double X, Double Y) : this(X, Y, false) { }
    }
}

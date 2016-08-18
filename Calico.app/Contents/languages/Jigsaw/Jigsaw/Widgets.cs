//
//  Widgets.cs
//  
//  Author:
//       Mark F. Russo <russomf@gmail.com>
// 
//  Copyright (c) 2013 The Calico Project
// 
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
// 
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
// 
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

using System;
using Cairo;
using System.Collections.Generic;

// This Namespace contains various user-drawn widgets based on shapes in the Diagram namespace.

namespace Widgets
{
	public class Colors {
	
		public static readonly Dictionary<string,Color> colors = 
		new Dictionary<string,Color>() {
		   {"teal", newColor(0x00, 0x80, 0x80)}, 
		   {"deepskyblue", newColor(0x00, 0xBF, 0xFF)}, 
		   {"mediumspringgreen", newColor(0x00, 0xFA, 0x9A)}, 
		   {"lime", newColor(0x00, 0xFF, 0x00)}, 
		   {"springgreen", newColor(0x00, 0xFF, 0x7F)}, 
		   {"aqua", newColor(0x00, 0xFF, 0xFF)}, 
		   {"cyan", newColor(0x00, 0xFF, 0xFF)}, 
		   {"midnightblue", newColor(0x19, 0x19, 0x70)}, 
		   {"dodgerblue", newColor(0x1E, 0x90, 0xFF)}, 
		   {"lightseagreen", newColor(0x20, 0xB2, 0xAA)}, 
		   {"forestgreen", newColor(0x22, 0x8B, 0x22)}, 
		   {"seagreen", newColor(0x2E, 0x8B, 0x57)}, 
		   {"limegreen", newColor(0x32, 0xCD, 0x32)}, 
		   {"mediumseagreen", newColor(0x3C, 0xB3, 0x71)}, 
		   {"turquoise", newColor(0x40, 0xE0, 0xD0)}, 
		   {"royalblue", newColor(0x41, 0x69, 0xE1)}, 
		   {"steelblue", newColor(0x46, 0x82, 0xB4)}, 
		   {"mediumturquoise", newColor(0x48, 0xD1, 0xCC)}, 
		   {"indigo", newColor(0x4B, 0x00, 0x82)}, 
		   {"cadetblue", newColor(0x5F, 0x9E, 0xA0)}, 
		   {"cornflowerblue", newColor(0x64, 0x95, 0xED)}, 
		   {"mediumaquamarine", newColor(0x66, 0xCD, 0xAA)}, 
		   {"dimgray", newColor(0x69, 0x69, 0x69)}, 
		   {"dimgrey", newColor(0x69, 0x69, 0x69)}, 
		   {"slateblue", newColor(0x6A, 0x5A, 0xCD)}, 
		   {"olivedrab", newColor(0x6B, 0x8E, 0x23)}, 
		   {"slategray", newColor(0x70, 0x80, 0x90)}, 
		   {"slategrey", newColor(0x70, 0x80, 0x90)}, 
		   {"lightslategray", newColor(0x77, 0x88, 0x99)}, 
		   {"lightslategrey", newColor(0x77, 0x88, 0x99)}, 
		   {"mediumslateblue", newColor(0x7B, 0x68, 0xEE)}, 
		   {"lawngreen", newColor(0x7C, 0xFC, 0x00)}, 
		   {"chartreuse", newColor(0x7F, 0xFF, 0x00)}, 
		   {"aquamarine", newColor(0x7F, 0xFF, 0xD4)}, 
		   {"maroon", newColor(0x80, 0x00, 0x00)}, 
		   {"purple", newColor(0x80, 0x00, 0x80)}, 
		   {"olive", newColor(0x80, 0x80, 0x00)}, 
		   {"gray", newColor(0x80, 0x80, 0x80)}, 
		   {"grey", newColor(0x80, 0x80, 0x80)}, 
		   {"skyblue", newColor(0x87, 0xCE, 0xEB)}, 
		   {"lightskyblue", newColor(0x87, 0xCE, 0xFA)}, 
		   {"blueviolet", newColor(0x8A, 0x2B, 0xE2)}, 
		   {"saddlebrown", newColor(0x8B, 0x45, 0x13)}, 
		   {"darkseagreen", newColor(0x8F, 0xBC, 0x8F)}, 
		   {"lightgreen", newColor(0x90, 0xEE, 0x90)}, 
		   {"mediumpurple", newColor(0x93, 0x70, 0xD8)}, 
		   {"palegreen", newColor(0x98, 0xFB, 0x98)}, 
		   {"yellowgreen", newColor(0x9A, 0xCD, 0x32)}, 
		   {"sienna", newColor(0xA0, 0x52, 0x2D)}, 
		   {"brown", newColor(0xA5, 0x2A, 0x2A)}, 
		   {"lightblue", newColor(0xAD, 0xD8, 0xE6)}, 
		   {"greenyellow", newColor(0xAD, 0xFF, 0x2F)}, 
		   {"paleturquoise", newColor(0xAF, 0xEE, 0xEE)}, 
		   {"lightsteelblue", newColor(0xB0, 0xC4, 0xDE)}, 
		   {"powderblue", newColor(0xB0, 0xE0, 0xE6)}, 
		   {"firebrick", newColor(0xB2, 0x22, 0x22)}, 
		   {"mediumorchid", newColor(0xBA, 0x55, 0xD3)}, 
		   {"rosybrown", newColor(0xBC, 0x8F, 0x8F)}, 
		   {"silver", newColor(0xC0, 0xC0, 0xC0)}, 
		   {"mediumvioletred", newColor(0xC7, 0x15, 0x85)}, 
		   {"indianred", newColor(0xCD, 0x5C, 0x5C)}, 
		   {"peru", newColor(0xCD, 0x85, 0x3F)}, 
		   {"chocolate", newColor(0xD2, 0x69, 0x1E)}, 
		   {"tan", newColor(0xD2, 0xB4, 0x8C)}, 
		   {"lightgray", newColor(0xD3, 0xD3, 0xD3)}, 
		   {"lightgrey", newColor(0xD3, 0xD3, 0xD3)}, 
		   {"palevioletred", newColor(0xD8, 0x70, 0x93)}, 
		   {"thistle", newColor(0xD8, 0xBF, 0xD8)}, 
		   {"orchid", newColor(0xDA, 0x70, 0xD6)}, 
		   {"goldenrod", newColor(0xDA, 0xA5, 0x20)}, 
		   {"crimson", newColor(0xDC, 0x14, 0x3C)}, 
		   {"gainsboro", newColor(0xDC, 0xDC, 0xDC)}, 
		   {"plum", newColor(0xDD, 0xA0, 0xDD)}, 
		   {"burlywood", newColor(0xDE, 0xB8, 0x87)}, 
		   {"lightcyan", newColor(0xE0, 0xFF, 0xFF)}, 
		   {"lavender", newColor(0xE6, 0xE6, 0xFA)}, 
		   {"violet", newColor(0xEE, 0x82, 0xEE)}, 
		   {"palegoldenrod", newColor(0xEE, 0xE8, 0xAA)}, 
		   {"lightcoral", newColor(0xF0, 0x80, 0x80)}, 
		   {"khaki", newColor(0xF0, 0xE6, 0x8C)}, 
		   {"aliceblue", newColor(0xF0, 0xF8, 0xFF)}, 
		   {"honeydew", newColor(0xF0, 0xFF, 0xF0)}, 
		   {"azure", newColor(0xF0, 0xFF, 0xFF)}, 
		   {"sandybrown", newColor(0xF4, 0xA4, 0x60)}, 
		   {"wheat", newColor(0xF5, 0xDE, 0xB3)}, 
		   {"beige", newColor(0xF5, 0xF5, 0xDC)}, 
		   {"salmon", newColor(0xFA, 0x80, 0x72)}, 
		   {"antiquewhite", newColor(0xFA, 0xEB, 0xD7)}, 
		   {"lightgoldenrodyellow", newColor(0xFA, 0xFA, 0xD2)}, 
		   {"fuchsia", newColor(0xFF, 0x00, 0xFF)}, 
		   {"magenta", newColor(0xFF, 0x00, 0xFF)}, 
		   {"deeppink", newColor(0xFF, 0x14, 0x93)}, 
		   {"orangered", newColor(0xFF, 0x45, 0x00)}, 
		   {"tomato", newColor(0xFF, 0x63, 0x47)}, 
		   {"hotpink", newColor(0xFF, 0x69, 0xB4)}, 
		   {"coral", newColor(0xFF, 0x7F, 0x50)}, 
		   {"lightsalmon", newColor(0xFF, 0xA0, 0x7A)}, 
		   {"lightpink", newColor(0xFF, 0xB6, 0xC1)}, 
		   {"pink", newColor(0xFF, 0xC0, 0xCB)}, 
		   {"gold", newColor(0xFF, 0xD7, 0x00)}, 
		   {"peachpuff", newColor(0xFF, 0xDA, 0xB9)}, 
		   {"navajowhite", newColor(0xFF, 0xDE, 0xAD)}, 
		   {"moccasin", newColor(0xFF, 0xE4, 0xB5)}, 
		   {"bisque", newColor(0xFF, 0xE4, 0xC4)}, 
		   {"mistyrose", newColor(0xFF, 0xE4, 0xE1)}, 
		   {"blanchedalmond", newColor(0xFF, 0xEB, 0xCD)}, 
		   {"papayawhip", newColor(0xFF, 0xEF, 0xD5)}, 
		   {"lavenderblush", newColor(0xFF, 0xF0, 0xF5)}, 
		   {"seashell", newColor(0xFF, 0xF5, 0xEE)}, 
		   {"cornsilk", newColor(0xFF, 0xF8, 0xDC)}, 
		   {"lemonchiffon", newColor(0xFF, 0xFA, 0xCD)}, 
		   {"floralwhite", newColor(0xFF, 0xFA, 0xF0)}, 
		   {"snow", newColor(0xFF, 0xFA, 0xFA)}, 
		   {"lightyellow", newColor(0xFF, 0xFF, 0xE0)}, 
		};
		
		public static Color newColor(int r, int g, int b) {
			return new Color(r/255.0, g/255.0, b/255.0);
		}
	}	

	// -----------------------------------------------------------------------
	// Custom event delegate declarations
	public delegate void ScrollEventHandler(Object o, ScrollEventArgs e);

	// -----------------------------------------------------------------------
	public class CRoundedButton : Diagram.CRoundedRectangle
	{
		private bool _Enabled = true;
		
		private Color _FillColor = Diagram.Colors.LightGray;
		private Color _DisabledFillColor = Diagram.Colors.LightGray;
		private Color _LineColor = Diagram.Colors.DarkSlateGray;
		private Color _DisabledLineColor = Diagram.Colors.SlateGray;
		private Color _TextColor = Diagram.Colors.Black;
		private Color _DisabledTextColor = Diagram.Colors.Gray;
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CRoundedButton (double x, double y, double w, double h, string label)
			: base(x, y, w, h)
		{
			this.Text = label;
			this.Radius = 5;
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkSlateGray;
			this.FillColor = Diagram.Colors.LightGray;
			this.TextColor = Diagram.Colors.Black;
			this.Selectable = false;
			this.Enabled = true;
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void OnMouseDown(Diagram.Canvas cvs, Diagram.MouseEventArgs e)
        {	// Handle mouse down event
			
			if (this.Enabled == false) return;
			
			// If within shape, change fill color
			if (this.ContainsPoint(e.X, e.Y, cvs)) {
				this.FillColor = Diagram.Colors.SlateGray;
				cvs.Invalidate();
				this.RaiseMouseDown(cvs);
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void OnMouseMove(Diagram.Canvas cvs, Diagram.MouseEventArgs e)
        {	// Handle mouse move event
			if (this.Enabled == false) return;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnMouseUp(Diagram.Canvas cvs, Diagram.MouseEventArgs e)
        {	// Handle mouse up event
			if (this.Enabled == false) return;
			
			this.FillColor = Diagram.Colors.LightGray;
			cvs.Invalidate();
			if (this.ContainsPoint(e.X, e.Y, cvs)) this.RaiseMouseUp(cvs);
			cvs.handler = cvs;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnDoubleClick(Diagram.Canvas cvs, Diagram.MouseEventArgs e )
		{
			// Must eat double-click events for button to work properly
			//Console.WriteLine("CRoundedButton.OnDoubleClick");
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public bool Enabled 
		{	// Get/set enabled state of button
			get { return _Enabled; }
			set { 
				_Enabled = value;
				
				if (value == true) {
					this.LineColor = _LineColor;
					this.FillColor = _FillColor;
					this.TextColor = _TextColor;
				} else {
					this.LineColor = _DisabledLineColor;
					this.FillColor = _DisabledFillColor;
					this.TextColor = _DisabledTextColor;
				}
			}
		}
	}

	// -----------------------------------------------------------------------
	public class ScrollEventArgs : EventArgs
	{
		public Diagram.Canvas cvs;
		public double dY;

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public ScrollEventArgs(Diagram.Canvas cvs, double dY)
		{
			this.cvs = cvs;
			this.dY = dY;;
		}
	}

	// -----------------------------------------------------------------------
	public class CYScrollBar : Diagram.CRectangle
	{
		// Keep track of previous mouse Y position to enable scrolling of factories
		private double? _prevMouseY = null;

		// The bar that is movable
		private Diagram.CRoundedRectangle bar;
		private double barYLoc = 0.0;			// Location of the top of the bar as a fraction of scrollbar height [0, 1]
		private double barYFrac = 0.5;			// Bar height as a fraction of total scrollbar height 
		private double barYSpan = 1.0;			// Max span as a fraction of total scrollbar height

		// Event that is raised when the scroll bar is moved
		public event ScrollEventHandler Scroll;

		public CYScrollBar(double x, double y, double w, double h) : base(x, y, w, h)
		{
			this.positionAbsolute = true;
			this.absoluteX = x;
			this.absoluteY = y;
			this.LineWidth = 0;
			this.FillColor = Diagram.Colors.Transparent;
			this.Selectable = false;
			this.TopMost = true;

			// Init the bar
			bar = new Diagram.CRoundedRectangle(x, y+barYLoc*h, w, barYFrac*h);
			bar.Radius = 4;
			bar.LineWidth = 2;
			//bar.positionAbsolute = true;
			//bar.absoluteX = barXOffset;
			//bar.absoluteY = barYOffset;
			bar.LineColor = Diagram.Colors.DarkGray;
			bar.FillColor = Diagram.Colors.SemiWhite;
			bar.TextColor = Diagram.Colors.Transparent;
			bar.Visible = true;
			bar.Sizable = false;
			bar.Selectable = false;
			bar.Draggable = false;
			bar.Connectable = false;
			bar.TopMost = true;
		}

		public CYScrollBar(double x, double y) : base(x, y) {
			this.positionAbsolute = true;
			this.absoluteX = x;
			this.absoluteY = y;
			this.TopMost = true;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override Diagram.CShape Clone(double x, double y) {
			CYScrollBar clone = new CYScrollBar(x, y, this.Width, this.Height);
			return (Diagram.CShape)clone;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void Draw(Cairo.Context g) {
			base.Draw (g);
			bar.Draw (g);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Override Top and Left so that the bar can also be adjusted
		public override double Top
		{
			get { return base.Top; }
			set
			{
				base.Top = value;
				bar.Top = value + barYLoc*this.Height;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override double Left
		{
			get { return base.Left; }
			set
			{
				bar.Left = value;
				base.Left = value;
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override double Width
		{
			get { return base.Width; }
			set
			{
				bar.Width = value;
				base.Width = value;
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override double Height
		{
			get { return base.Height; }
			set
			{
				bar.Height = barYFrac*value;
				base.Height = value;

				// Because Top also depends on Height, reset Top when Height is changed. See Top.
				this.Top = this.Top;
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void BringToFront(Diagram.Canvas cvs) 
		{
			base.BringToFront(cvs);
			bar.BringToFront(cvs);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override Boolean ContainsPoint(double X, double Y, Diagram.Canvas cvs)
		{
			return base.ContainsPoint (X, Y, cvs) || bar.ContainsPoint (X, Y, cvs);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Raise the OnScroll event
		public virtual void RaiseScrollEvent(Diagram.Canvas cvs, double dY)
		{
			if (Scroll != null) {
				ScrollEventArgs evargs = new ScrollEventArgs(cvs, dY);
				Scroll(this, evargs);
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnMouseDown( Diagram.Canvas cvs, Diagram.MouseEventArgs e) 
		{
			if (this.ContainsPoint(e.X, e.Y, cvs) ) 
			{
				_prevMouseY = e.Y;
			}
			//base.OnMouseDown(cvs, e);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnMouseMove( Diagram.Canvas cvs, Diagram.MouseEventArgs e){
			if (_prevMouseY != null) 
			{
				//double dY = e.Y - (double)_prevMouseY;
				//double dY = ((double)_prevMouseY - e.Y)*(barYSpan/this.Height);
				double dY = ((double)_prevMouseY - e.Y)*barYSpan;
				RaiseScrollEvent(cvs, dY);
				_prevMouseY = e.Y;
				cvs.Invalidate();
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnMouseUp( Diagram.Canvas cvs, Diagram.MouseEventArgs e)
		{
			_prevMouseY = null;
			base.OnMouseUp (cvs, e);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Update( double x, double y, double w, double h, double frac, double loc, double span ) 
		{
			// This method resizes and repositions the bar part of the widget.
			// The frac parameter is the fraction of the total canvas height that is taken up be the bar part.
			// The loc parameter is the location of the bar over the canvas area

			barYLoc = loc;
			barYFrac = frac;
			barYSpan = span;

			this.Top = y;
			this.Left = x;
			this.Width = w;
			this.Height = h;

//			// Update the total span that the scrollbar represents
//			double ySpanMin = minTop - maxTop;
//			double ySpanMax = cvsHeight + (maxBottom - minBottom);
//			yScrollSpan = ySpanMax - ySpanMin;
//			
//			// Total height of all blocks
//			double stackHeight = maxBottom - minTop;
//			
//			// barFrac is the fraction of the scrollbar area that should be covered be the bar itself [0, 1]
//			double barFrac = stackHeight/yScrollSpan;
//			bar.Height = barFrac*cvsHeight;
//			
//			// barLoc is the fraction of the total scrollbar area at which the scrollbar should be located [0, 1]
//			//double barLoc = (spanMax - maxBottom - cvs.offsetY)/span;
//			double barLoc = (ySpanMax - maxBottom)/yScrollSpan;		// Offset from the bottom
//			barYOffset = barLoc*cvsHeight;							// Used on this.Top
//			
//			// The scrollbar is offset from the top of the block panel by the given fraction of the canvas
//			// ... and it straddles the right edge of the block panel
//			bar.Top = this.Top + barLoc*cvsHeight;
//			//bar.Left = this.Left + (this.Width - 0.5*bar.Width);

		}
	}

	// -----------------------------------------------------------------------
	public class CBlockPalette : Diagram.CRectangle
	{	// The background palette for currently available block factories
		
		// Keep a ref to the currently selected tab so the proper factory blocks are scrolled
		internal CRoundedTab _currTab = null;
		
		// Keep track of previous mouse Y position to enable scrolling of factories
		private double? _prevMouseY = null;

		private CYScrollBar bar;
		private double scrollBarWidth = 11;

//		// Depiction of current scroll position
//		private Diagram.CRoundedRectangle bar;
//		private double barYOffset = 0.0;
//		private double barWidth = 12;
//		private double yScrollSpan = 1.0;		// This parameter is used to communicate the total y-distance that the y-scrollbar represents
//		private double? _prevBarMouseY = null;

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CBlockPalette(double x, double y, double w, double h) : base(x, y, w, h)
		{
			this.LineWidth = 1;
			this.positionAbsolute = true;
			this.absoluteX = x;
			this.absoluteY = y;
			this.LineColor = Diagram.Colors.Honeydew;
			this.FillColor = Diagram.Colors.Honeydew;
			this.TextColor = Diagram.Colors.Transparent;
			this.Visible = true;
			this.Sizable = false;
			this.Selectable = false;
			this.Draggable = false;
			this.Connectable = false;
			this.TopMost = true;

			bar = new CYScrollBar(x + w-0.5*scrollBarWidth, y, scrollBarWidth, h);
			bar.Scroll += OnScrollBarScroll;

//			// Add scrollbar depiction rectangle
//			bar = new Diagram.CRoundedRectangle(x+w-0.5*barWidth, y, barWidth, 20);
//			bar.Radius = 4;
//			bar.LineWidth = 2;
//			//bar.positionAbsolute = true;
//			//bar.absoluteX = barXOffset;
//			//bar.absoluteY = barYOffset;
//			bar.LineColor = Diagram.Colors.DarkGray;
//			bar.FillColor = Diagram.Colors.SemiWhite;
//			bar.TextColor = Diagram.Colors.Transparent;
//			bar.Visible = true;
//			bar.Sizable = false;
//			bar.Selectable = false;
//			bar.Draggable = false;
//			bar.Connectable = false;
//			bar.TopMost = true;
		}

		void OnScrollBarScroll (object sender, ScrollEventArgs e)
		{
			DoScroll(e.cvs, e.dY);
		}
		
		public CBlockPalette(double x, double y) : base(x, y) {
			this.positionAbsolute = true;
			this.absoluteX = x;
			this.absoluteY = y;
			this.TopMost = true;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override Diagram.CShape Clone(double x, double y) {
			CBlockPalette clone = new CBlockPalette(x, y, this.Width, this.Height);
			return (Diagram.CShape)clone;
		}

		public CYScrollBar YScrollbar {
			get {
				return bar;
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Override Top and Left so that the bar can also be adjusted
		public override double Top
		{
			get { return base.Top; }
			set
			{
				base.Top = value;
				bar.Top = value; // + barYOffset;
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override double Left
		{
			get { return base.Left; }
			set
			{
				bar.Left = value; // + this.Width - 0.5*bar.Width;
				base.Left = value;
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void BringToFront(Diagram.Canvas cvs) 
		{
			base.BringToFront(cvs);
			bar.BringToFront(cvs);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void DoConfigure(Diagram.Canvas cvs) {
			// Reset the scroll bar when the canvas is reconfigured
			this.DoScroll (cvs, 0.0);
			return;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void DoZoom(double factor, Diagram.Canvas cvs) {
			// Reset the scroll bar when the canvas scale changes
			this.DoScroll (cvs, 0.0);
			return;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void DoResetZoom(Diagram.Canvas cvs) {
			// Reset the scroll bar when the canvas scale changes
			this.DoScroll (cvs, 0.0);
			return;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void DoScroll(Diagram.Canvas cvs, double dY)
		{	// Test if a scroll is acceptible based on window bounds and block positions
			// and perform if acceptible.
			
			// The top of at least one shape must be positive
			// If not, prevent scroll
			int cvsHeight = Convert.ToInt32(cvs.Allocation.Height/cvs.scale);
			int countShps = 0;
			int countAbove = 0;
			int countBelow = 0;
			double minTop, maxTop;
			double minBottom, maxBottom;

			// Loop over all factory blocks and find the top and bottom of the top and bottom blocks on the stack
			// Account for scroll of the main block canvas, which actually shifts the absolute-positioned block palette

			// Init block bounds
			if (_currTab._shapes.Count > 0) {
				Diagram.CShape shp = _currTab._shapes[0].Target as Diagram.CShape;
				minTop = maxTop = shp.Top + cvs.offsetY;
				minBottom = maxBottom = shp.Top + cvs.offsetY + shp.Height;
			} 
			else
			{
				return;
			}

			// Find block bounds
			foreach (WeakReference wrshp in _currTab._shapes)
			{
				Diagram.CShape shp = wrshp.Target as Diagram.CShape;
				if (shp != null) {
					countShps++;
					double top = shp.Top + cvs.offsetY;
					double bottom = shp.Top + cvs.offsetY + shp.Height;

					if (bottom < 0.0) {
						countAbove++;
					} else if (top > cvsHeight) {
						countBelow++;
					}
					
					if (top < minTop) minTop = top;
					if (top > maxTop) maxTop = top;
					if (bottom < minBottom) minBottom = bottom;
					if (bottom > maxBottom) maxBottom = bottom;
				}
			}
			
			// Perform the scroll of blocks, if conditions permit
			
			// Clip movement so that at least one block is always visible
			if (dY < 0.0 && maxTop <= 0.0) {
				dY = -maxTop;
			} else if (dY > 0.0 && minBottom >= cvsHeight) {
				dY = cvsHeight - minBottom;
			}
			
			// Scroll all factory blocks
			foreach (WeakReference wrshp in _currTab._shapes)
			{
				Diagram.CShape shp = wrshp.Target as Diagram.CShape;
				if (shp != null) {
					shp.Top += dY;
					shp.absoluteY += dY;
				}
			}

			// Now reposition the scrollbar

			// Update the total span that the scrollbar represents
			double ySpanMin = minTop - maxTop;
			double ySpanMax = cvsHeight + (maxBottom - minBottom);
			double ySpan = ySpanMax - ySpanMin;

			// Total height of all blocks
			double stackHeight = maxBottom - minTop;

			// barFrac is the fraction of the scrollbar area that should be covered be the bar itself [0, 1]
			// barLoc is the fraction of the total scrollbar area at which the scrollbar should be located [0, 1]
			double barFrac = stackHeight/ySpan;
			double barLoc = (ySpanMax - maxBottom)/ySpan;		// Offset from the bottom
			double barSpan = ySpan/cvsHeight;

			bar.Update(this.Left + (this.Width - 0.5*bar.Width), this.Top, bar.Width, cvsHeight, barFrac, barLoc, barSpan);

//			bar.Height = barFrac*cvsHeight;
//			bar.Top = this.Top + barLoc*cvsHeight;
//			//bar.Left = this.Left + (this.Width - 0.5*bar.Width);

//			// Init bounds
//			if (_currTab._shapes.Count > 0) {
//				Diagram.CShape shp = _currTab._shapes[0].Target as Diagram.CShape;
//				minTop = maxTop = shp.Top;
//				minBottom = maxBottom = shp.Top + shp.Height;
//			} 
//			else
//			{
//				return;
//			}
//
//			foreach (WeakReference wrshp in _currTab._shapes)
//			{
//				Diagram.CShape shp = wrshp.Target as Diagram.CShape;
//				if (shp != null) {
//					countShps++;
//					double top = shp.Top;
//					double bottom = top + shp.Height;
//
//					//if (shp.Top + shp.Height < 0.0) {
//					if (bottom < -cvs.offsetY) {
//						countAbove++;
//					//} else if (shp.Top > cvsHeight) {
//					} else if (top > cvsHeight-cvs.offsetY) {
//						countBelow++;
//					}
//
//					if (shp.Top < minTop) minTop = shp.Top;
//					if (shp.Top > maxTop) maxTop = shp.Top;
//					if (bottom < minBottom) minBottom = bottom;
//					if (bottom > maxBottom) maxBottom = bottom;
//				}
//			}
//
//			// Perform the scroll if conditions permit
//
//			// Clip movement to max extents
//			if (dY < 0.0 && maxTop <= -cvs.offsetY) {
//				dY = -maxTop-cvs.offsetY;
//			} else if (dY > 0.0 && minBottom >= cvsHeight-cvs.offsetY) {
//				dY = cvsHeight-minBottom-cvs.offsetY;
//			}
//			
//			// If movement is in order, perform it
//			foreach (WeakReference wrshp in _currTab._shapes)
//			{
//				Diagram.CShape shp = wrshp.Target as Diagram.CShape;
//				if (shp != null) {
//					shp.Top += dY;
//					shp.absoluteY += dY;
//				}
//			}
//
//			// barFrac is the fraction of the scrollbar area that should be covered bar itself [0, 1]
//			// barLoc is the fraction of the total span that the scrollbar should be located [0, 1]
//
//			// Total height the scrollbar represents
//			double spanMin = minTop - maxTop;
//			double spanMax = cvsHeight + (maxBottom - minBottom);
//			double span = spanMax - spanMin;
//
//			// This parameter is required for properly manipulating the scrollbar directly. See mouse events.
//			barRatio = cvsHeight/span;
//
//			// Total height of all blocks
//			double stackHeight = maxBottom - minTop;
//			// The height the bar should be to represent the stack of blocks
//			//bar.Height = stackHeight * barRatio;
//			double barFrac = (stackHeight/span);
//			bar.Height = barFrac * cvsHeight;
//
//			// The top of the scroll bar to represent the current scroll location
//			barXOffset = this.Width - 0.5*bar.Width;
//
//			//barYOffset = (spanMax - maxBottom - cvs.offsetY)*barRatio;
//			//barYOffset = (spanMax - maxBottom)*(cvsHeight/span);
//			//double barLoc = (spanMax - maxBottom - cvs.offsetY)/span;
//			//double barLoc = (spanMax - maxBottom)/span;
//
//			double barLoc = (spanMax - maxBottom - cvs.offsetY)/span;
//			barYOffset = barLoc*cvsHeight;
//			bar.Top = this.Top + barLoc*cvsHeight;
//
//			//double barLoc = minTop/span;
//			//barYOffset = cvs.offsetY + barLoc*cvsHeight;
//			//bar.Top = this.Top + cvs.offsetY + barLoc*cvsHeight;
//
//			bar.Left = this.Left + (this.Width - 0.5*bar.Width);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnScroll( Diagram.Canvas cvs, Gdk.EventScroll e)
		{
			if (this.ContainsPoint(e.X, e.Y, cvs) )
			{
				double dY = 0.0;
				if (e.Direction == Gdk.ScrollDirection.Up) {
					dY = 20.0;
				} else if (e.Direction == Gdk.ScrollDirection.Down) {
					dY = -20.0;
				}
				
				DoScroll(cvs, dY);
				
				cvs.Invalidate();
			} else {
				base.OnScroll(cvs, e);
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override Boolean ContainsPoint(double X, double Y, Diagram.Canvas cvs)
		{
			return base.ContainsPoint (X, Y, cvs) || bar.ContainsPoint (X, Y, cvs);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnMouseDown( Diagram.Canvas cvs, Diagram.MouseEventArgs e) 
		{
			if (this.ContainsPoint(e.X, e.Y, cvs) ) 
			{
				_prevMouseY = e.Y;
			}
			//base.OnMouseDown(cvs, e);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnMouseMove( Diagram.Canvas cvs, Diagram.MouseEventArgs e)
		{
			if (_prevMouseY != null && _currTab != null) 
			{
				double dY = e.Y - (double)_prevMouseY;
				
				DoScroll (cvs, dY);
				_prevMouseY = e.Y;
				cvs.Invalidate();
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnMouseUp( Diagram.Canvas cvs, Diagram.MouseEventArgs e)
		{
			_prevMouseY = null;
			base.OnMouseUp (cvs, e);
		}

//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public void MoveBlocksToTop(Diagram.Canvas cvs) {
//			double move = 0;
//			foreach (WeakReference wrshp in _currTab._shapes)
//			{
//				Diagram.CShape shp = wrshp.Target as Diagram.CShape;
//				if (shp != null) {
//					move = shp.Top;
//					cvs.DeselectAll();		// Deselect everything. Cannot select multiple blocks.
//					shp.Select(cvs);		// Select this shape
//					break;
//				}
//			}
//			foreach (WeakReference wrshp in _currTab._shapes)
//			{
//				Diagram.CShape shp = wrshp.Target as Diagram.CShape;
//				if (shp != null) {
//					shp.Top -= move - 5;
//				}
//			}
//			cvs.Invalidate();
//		}

//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public void MoveBlocksToBottom(Diagram.Canvas cvs) {
//			double move = 0;
//			foreach (WeakReference wrshp in _currTab._shapes)
//			{
//				Diagram.CShape shp = wrshp.Target as Diagram.CShape;
//				if (shp != null) {
//					move = shp.Top;
//					cvs.DeselectAll();		// Deselect everything. Cannot select multiple blocks.
//					shp.Select(cvs);		// Select this shape
//				}
//			}
//			foreach (WeakReference wrshp in _currTab._shapes)
//			{
//				Diagram.CShape shp = wrshp.Target as Diagram.CShape;
//				if (shp != null) {
//					shp.Top -= move - 5;
//				}
//			}
//			cvs.Invalidate();
//		}

//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public bool SearchMore(Diagram.Canvas cvs, string s) {
//			///Find with more detail in s; might be a match here
//			/// Return True if found
//			bool found = false;
//			double move = 0;
//			// FIXME: use position in Window, not in Tab
//			foreach (WeakReference wrshp in _currTab._shapes)
//			{
//				Diagram.CShape shp = wrshp.Target as Diagram.CShape;
//				if (shp != null) {
//					if (shp.Text.ToLower().Contains(s.ToLower()) && shp.Top > 0) {
//						found = true;
//						move = shp.Top; 
//						cvs.DeselectAll();		// Deselect everything. Cannot select multiple blocks.
//						shp.Select(cvs);		// Select this shape
//						cvs.Invalidate();
//						break;
//					}
//				}
//			}
//			if (found) {
//				foreach (WeakReference wrshp in _currTab._shapes)
//				{
//					Diagram.CShape shp = wrshp.Target as Diagram.CShape;
//					if (shp != null) {
//						shp.Top -= move - 5; // move down from top slightly
//					}
//				}
//				cvs.Invalidate();
//				_currTab.searchEnd = false;
//			} else if (_currTab.searchEnd) {
//				// we are failing, again; go to top and search
//				_currTab.searchEnd = false;
//				MoveBlocksToTop(cvs);
//				found = SearchNext(cvs, s);
//			}
//			if (! found) {
//				_currTab.searchEnd = true;
//			}
//			return found;
//		}
//
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public bool SearchNext(Diagram.Canvas cvs, string s) {
//			///Find the next occurance of s
//			/// Return True if found
//			bool found = false;
//			double move = 0;
//			// FIXME: use position in Window, not in Tab
//			foreach (WeakReference wrshp in _currTab._shapes)
//			{
//				Diagram.CShape shp = wrshp.Target as Diagram.CShape;
//				if (shp != null) {
//					if (shp.Text.ToLower().Contains(s.ToLower()) && shp.Top > 5) {
//						found = true;
//						move = shp.Top; 
//						cvs.DeselectAll();		// Deselect everything. Cannot select multiple blocks.
//						shp.Select(cvs);		// Select this shape
//						cvs.Invalidate();
//						break;
//					}
//				}
//			}
//			if (found) {
//				foreach (WeakReference wrshp in _currTab._shapes)
//				{
//					Diagram.CShape shp = wrshp.Target as Diagram.CShape;
//					if (shp != null) {
//						shp.Top -= move - 5; // move down from top slightly
//					}
//				}
//				cvs.Invalidate();
//				_currTab.searchEnd = false;
//			} else if (_currTab.searchEnd) {
//				// we are failing, again; go to top and search
//				_currTab.searchEnd = false;
//				MoveBlocksToTop(cvs);
//				found = SearchNext(cvs, s);
//			}
//			if (! found)
//				_currTab.searchEnd = true;
//			return found;
//		}
//
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public bool SearchPrevious(Diagram.Canvas cvs, string s) {
//			/// Find a previous occurance of s from where we are
//			/// Return true if found
//			bool found = false;
//			double move = 0;
//			// in reverse order
//			// FIXME: use position in Window, not in Tab
//			for (int i = _currTab._shapes.Count - 1; i > 0; i--)
//			{
//				WeakReference wrshp = _currTab._shapes[i];
//				Diagram.CShape shp = wrshp.Target as Diagram.CShape;
//				if (shp != null && shp.Top < 0) {
//					if (shp.Text.ToLower().Contains(s.ToLower())) {
//						found = true;
//						move = shp.Top; 
//						cvs.DeselectAll();		// Deselect everything. Cannot select multiple blocks.
//						shp.Select(cvs);		// Select this shape
//						cvs.Invalidate();
//						break;
//					}
//				}
//			}
//			if (found) {
//				foreach (WeakReference wrshp in _currTab._shapes)
//				{
//					Diagram.CShape shp = wrshp.Target as Diagram.CShape;
//					if (shp != null) {
//						shp.Top -= move - 5; // move down from top slightly
//					}
//				}
//				cvs.Invalidate();
//				_currTab.searchTop = false;
//			} else if (_currTab.searchTop) {
//				// we are failing, again; go to bottom and search
//				_currTab.searchTop = false;
//				MoveBlocksToBottom(cvs);
//				found = SearchPrevious(cvs, s);
//			}
//			if (! found)
//				_currTab.searchTop = true;
//			return found;
//		}
		


//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//        public override void Draw(Cairo.Context g)
//        {
////			// @@@
////			// Get current x and y offset from the current transformation matrix
////			Matrix m = g.Matrix;
////			double x = m.X0;
////			double y = m.Y0;
////			g.InverseTransformDistance(ref x, ref y);
////
////			g.Save ();
////			g.Translate(-x, -y);
//
//			// Start with the base rounded rectangle
//			base.Draw(g);
//
////			g.Restore ();
//		}
	}

	// -----------------------------------------------------------------------
	public class CTabPalette : Diagram.CRectangle
	{	// The background palette for currently loaded tabs
		
		// Keep a reference to the list of all tabs
		public List<Widgets.CRoundedTab> allTabs = null;
		
		// Keep track of previous mouse Y position to enable scrolling of factories
		private double? _prevMouseY = null;

		private CYScrollBar bar;
		private double scrollBarWidth = 11;

		// Depiction of current scroll position
		// !!! This widget is managed entirely by the CBlockPalette.
		// !!! Therefore, all important events must be overridden and delegated to widget as needed
//		public Diagram.CRoundedRectangle bar;
//		private double barXOffset;
//		private double barYOffset;
//		private double barWidth = 25; //12;
//		private double barRatio = 0.0;
//		private double? _prevBarMouseY = null;

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CTabPalette(double x, double y, double w, double h) : base(x, y, w, h)
		{
			this.LineWidth = 1;
			this.positionAbsolute = true;
			this.absoluteX = x;
			this.absoluteY = y;
			this.TopMost = true;
			this.LineColor = Diagram.Colors.Honeydew;
			this.FillColor = Diagram.Colors.DarkSlateGray; //LightSlateGray;
			this.TextColor = Diagram.Colors.Transparent;
			this.Visible = true;
			this.Sizable = false;
			this.Selectable = false;
			this.Draggable = false;
			this.Connectable = false;

			bar = new CYScrollBar(x + w-scrollBarWidth, y, scrollBarWidth, h);
			bar.Scroll += HandleScroll;

//			// Add scrollbar depiction rectangle
//			barXOffset = x+w-0.5*barWidth;
//			barYOffset = y;
//			
//			bar = new Diagram.CRoundedRectangle(barXOffset, barYOffset, barWidth, 20);
//			bar.Radius = 4;
//			bar.LineWidth = 2;
//			bar.positionAbsolute = true;
//			bar.absoluteX = barXOffset;
//			bar.absoluteY = barYOffset;
//			bar.LineColor = Diagram.Colors.DarkGray;
//			bar.FillColor = Diagram.Colors.SemiWhite;
//			bar.TextColor = Diagram.Colors.Transparent;
//			bar.Visible = true;
//			bar.Sizable = false;
//			bar.Selectable = false;
//			bar.Draggable = false;
//			bar.Connectable = false;
//			bar.TopMost = true;
		}

		public CTabPalette(double x, double y) : base(x, y) {
			this.positionAbsolute = true;
			this.absoluteX = x;
			this.absoluteY = y;
			this.TopMost = true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override Diagram.CShape Clone(double x, double y) {
			CTabPalette clone = new CTabPalette(x, y, this.Width, this.Height);
			return (Diagram.CShape)clone;
		}

		public CYScrollBar YScrollbar {
			get {
				return bar;
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		void HandleScroll (object sender, ScrollEventArgs e)
		{
			DoScroll(e.cvs, e.dY);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Override Top and Left so that the bar can also be adjusted
		public override double Top
		{
			get { return base.Top; }
			set
			{
				base.Top = value;
				bar.Top = value;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override double Left
		{
			get { return base.Left; }
			set
			{
				bar.Left = value;
				base.Left = value;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void BringToFront(Diagram.Canvas cvs) 
		{
			base.BringToFront(cvs);
			foreach (CRoundedTab tab in allTabs) tab.BringToFront(cvs);
			bar.BringToFront(cvs);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void DoConfigure(Diagram.Canvas cvs) {
			// Reset the scroll bar when the canvas is reconfigured
			this.DoScroll (cvs, 0.0);
			return;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void DoZoom(double factor, Diagram.Canvas cvs) {
			// Reset the scroll bar when the canvas scale changes
			this.DoScroll (cvs, 0.0);
			return;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void DoResetZoom(Diagram.Canvas cvs) {
			// Reset the scroll bar when the canvas scale changes
			this.DoScroll (cvs, 0.0);
			return;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void DoScroll(Diagram.Canvas cvs, double dY)
		{	// Test if a scroll is acceptible based on window bounds and block positions
			// and perform is acceptible.
			
			// The top of at least one shape must be positive
			// If not, prevent scroll
			
			int cvsHeight = Convert.ToInt32(cvs.Allocation.Height/cvs.scale);
			int countShps = 0;
			int countAbove = 0;
			int countBelow = 0;
			double minTop = 0;
			double maxTop = 0;
			double minBottom = 0;
			double maxBottom = 0;

			// Init minTop and maxBottom
			if (allTabs.Count > 0) {
				minTop = maxTop = allTabs[0].Top + cvs.offsetY;
				minBottom = maxBottom = allTabs[0].Top + cvs.offsetY + allTabs[0].Height;
			}
			else
			{
				return;
			}

			foreach (CRoundedTab tab in allTabs)
			{
				countShps++;
				double top = tab.Top + cvs.offsetY;
				double bottom = tab.Top + tab.Height + cvs.offsetY;

				if (bottom < 0.0) {
					countAbove++;
				} else if (top > cvsHeight) {
					countBelow++;
				}
				
				if (top < minTop) minTop = top;
				if (top > maxTop) maxTop = top;
				if (bottom < minBottom) minBottom = bottom;
				if (bottom > maxBottom) maxBottom = bottom;
			}
			
			// Perform the scroll if conditions permit

			// Clip movement so that at least one block is always visible
			if (dY < 0.0 && maxTop <= 0.0) {
				dY = -maxTop;
			} else if (dY > 0.0 && minBottom >= cvsHeight) {
				dY = cvsHeight - minBottom;
			}

//			// Scroll up
//			if (dY < 0.0) {
//				if (countAbove == countShps) {
//					dY = maxTop;
//				} else if (countAbove == countShps - 1) {
//					dY = 0.0;
//				} else if (countBelow == countShps) {
//					dY = cvsHeight-minBottom;
//				}
//
//			// Scroll down
//			} else if (dY > 0.0) {
//				if (countBelow == countShps) {
//					dY = cvsHeight-minBottom;
//				} else if (countBelow == countShps - 1) {
//					dY = 0.0;
//				}
//			}
			
			// If movement is in order, perform it
			foreach (CRoundedTab tab in allTabs)
			{
				tab.Top += dY;
				tab.absoluteY += dY;
			}

			// Total height the scrollbar represents
			double ySpanMin = minTop - maxTop;
			double ySpanMax = cvsHeight + (maxBottom - minBottom);
			double ySpan = ySpanMax - ySpanMin;

			double stackHeight = maxBottom - minTop;
			double barFrac = stackHeight/ySpan;
			double barLoc = (ySpanMax - maxBottom)/ySpan;
			double barSpan = ySpan/cvsHeight;
			bar.Update(this.Left + (this.Width - bar.Width), this.Top, bar.Width, cvsHeight, barFrac, barLoc, barSpan);

//			// The height the bar should be to represent the stack of blocks
//			bar.Height = (stackHeight/ySpan) * cvsHeight;
//			
//			// The top of the scroll bar to represent the current scroll location
//			barXOffset = this.Width - 0.5*bar.Width;
//			barYOffset = ((spanMax - maxBottom - cvs.offsetY)/ySpan)*cvsHeight;
//			bar.Left = this.Left + barXOffset;
//			bar.Top = this.Top + barYOffset;
//			
//			// This parameter is required for properly manipulating the scrollbar directly. See mouse events.
//			barRatio = cvsHeight/ySpan;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnScroll( Diagram.Canvas cvs, Gdk.EventScroll e){
			if (this.ContainsPoint(e.X, e.Y, cvs) )
			{
				double dY = 0.0;
				if (e.Direction == Gdk.ScrollDirection.Up) {
					dY = 20.0;
				} else if (e.Direction == Gdk.ScrollDirection.Down) {
					dY = -20.0;
				}
				DoScroll (cvs, dY);

				cvs.Invalidate();
			} else {
				base.OnScroll(cvs, e);
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override Boolean ContainsPoint(double X, double Y, Diagram.Canvas cvs)
		{
			return base.ContainsPoint (X, Y, cvs) || bar.ContainsPoint (X, Y, cvs);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnMouseDown( Diagram.Canvas cvs, Diagram.MouseEventArgs e)
		{
			// First check bar
//			if (bar.ContainsPoint(e.X, e.Y, cvs) ) 
//			{
//				_prevBarMouseY = e.Y;
//				return;
//			}

			if (this.ContainsPoint(e.X, e.Y, cvs) ) {
				_prevMouseY = e.Y;
			}
			//base.OnMouseDown(cvs, e);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnMouseMove( Diagram.Canvas cvs, Diagram.MouseEventArgs e)
		{
//			if (_prevBarMouseY != null) 
//			{
//				double dY = ((double)_prevBarMouseY - e.Y)/barRatio;
//				
//				DoScroll (cvs, dY);
//				_prevBarMouseY = e.Y;
//				cvs.Invalidate();
//				return;
//			}

			if (_prevMouseY != null) {
				double dY = e.Y - (double)_prevMouseY;

				DoScroll (cvs, dY);
				_prevMouseY = e.Y;
				cvs.Invalidate();
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnMouseUp( Diagram.Canvas cvs, Diagram.MouseEventArgs e)
		{
//			_prevBarMouseY = null;
			_prevMouseY = null;
			base.OnMouseUp (cvs, e);
		}
		
		//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		//		public void MoveBlocksToTop(Diagram.Canvas cvs) {
		//			double move = 0;
		//			foreach (WeakReference wrshp in _currTab._shapes)
		//			{
		//				Diagram.CShape shp = wrshp.Target as Diagram.CShape;
		//				if (shp != null) {
		//					move = shp.Top;
		//					cvs.DeselectAll();		// Deselect everything. Cannot select multiple blocks.
		//					shp.Select(cvs);		// Select this shape
		//					break;
		//				}
		//			}
		//			foreach (WeakReference wrshp in _currTab._shapes)
		//			{
		//				Diagram.CShape shp = wrshp.Target as Diagram.CShape;
		//				if (shp != null) {
		//					shp.Top -= move - 5;
		//				}
		//			}
		//			cvs.Invalidate();
		//		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		//		public void MoveBlocksToBottom(Diagram.Canvas cvs) {
		//			double move = 0;
		//			foreach (WeakReference wrshp in _currTab._shapes)
		//			{
		//				Diagram.CShape shp = wrshp.Target as Diagram.CShape;
		//				if (shp != null) {
		//					move = shp.Top;
		//					cvs.DeselectAll();		// Deselect everything. Cannot select multiple blocks.
		//					shp.Select(cvs);		// Select this shape
		//				}
		//			}
		//			foreach (WeakReference wrshp in _currTab._shapes)
		//			{
		//				Diagram.CShape shp = wrshp.Target as Diagram.CShape;
		//				if (shp != null) {
		//					shp.Top -= move - 5;
		//				}
		//			}
		//			cvs.Invalidate();
		//		}
	}

	// -----------------------------------------------------------------------
	public class CTextBox : Diagram.CShape
	{
		private Gtk.Entry _entry = null;
		//private Fixed _fixed = null;
		
		public CTextBox(double x, double y, double w, double h, string text, Gtk.Fixed fixd)
			: base(x, y, w, h)
		{
			this.Selectable = false;
			_entry.SetSizeRequest((int)w, (int)h);
			_entry.SetSizeRequest((int)w, (int)h);
			//_entry.HasFrame = false;
			//_fixed = fixd;
			fixd.Put(_entry, (int)x, (int)y);
			_entry.Hide();
		}
		
		public override bool Visible {
			set {
				if (value == true) {
					_entry.Show();
					visible = true;
				}
				else {
					_entry.Hide();
					visible = false;
				}
			}
			get {
				return visible;
			}
		}
	}
	
	// -----------------------------------------------------------------------
	public class CRoundedTab : Diagram.CRoundedRectangle
	{
		private bool _toggled = false;
		private List<WeakReference> _tabs = new List<WeakReference>(); // weak references to other tabs in tab group
		internal List<WeakReference> _shapes = new List<WeakReference>();

		// Save a reference to the block palette
		internal CBlockPalette _palette = null;
		internal CTabPalette _pnlTab = null;

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CRoundedTab (double x, double y, double w, double h, string label, CBlockPalette palette, CTabPalette pnlTab,
							Cairo.Color fillColor, Cairo.Color lineColor)
			: base(x, y, w, h)
		{
			_palette = palette;
			_pnlTab = pnlTab;
			this.positionAbsolute = true;
			this.absoluteX = x;
			this.absoluteY = y;
			this.TopMost = true;
			this.Text = label;
			this.Radius = 5;
			this.LineWidth = 0;
			this.LineColor = lineColor; //Diagram.Colors.Silver;
			this.FillColor = fillColor; //Diagram.Colors.Silver;
			this.OriginalFillColor = fillColor;
			this.fontFace = "Arial"; 
			//this.FontStyle = FontStyle.Bold;
			this.Selectable = false;
			this.SetToggle(null, false);
		}


		public List<Diagram.CShape> AllShapes () 
		{
		    List<Diagram.CShape> retval = new List<Diagram.CShape>();
		    foreach (WeakReference wrshp in _shapes) {
			Diagram.CShape shp = wrshp.Target as Diagram.CShape;
			if (shp != null) {
			    retval.Add(shp);
			}
		    }
		    return retval;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnScroll(Diagram.Canvas cvs, Gdk.EventScroll e)
		{
			// delegate event to tab palette
			if (_pnlTab != null) _pnlTab.OnScroll(cvs, e);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void OnMouseDown(Diagram.Canvas cvs, Diagram.MouseEventArgs e)
        {	// Override CShape OnMouseDown method 
		
			// If within shape, change fill color
			if (this.ContainsPoint(e.X, e.Y, cvs)) 
			{	
				this.SetToggle(cvs, true);
				_palette.DoConfigure(cvs);
				cvs.Invalidate();
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnMouseUp(Diagram.Canvas cvs, Diagram.MouseEventArgs e)
        {	// Override OnMouseUp event
			cvs.handler = cvs;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void Draw(Cairo.Context g)
		{	
		    // Draw the tab widget
		    
		    // Start with the base rounded rectangle
		    base.Draw(g);
		    // Next, draw the specifics for a CRoundedTab
		    double x = this.left;
		    double y = this.top;
		    
		    double endWidth = 10;
		    double w = endWidth; 
		    double h = this.height;
		    double hh = 0.5*h;
		    double hw = 0.5*w;
		    double cx = x + hw;
		    double cy = y + hh;
		    double r = this.Radius;
		    double hpi = 0.5*Math.PI;
		    if ( r > hh || r > hw ) r = Math.Min( hh, hw );
		    
		    // Draw the color tab indicator, toggled
		    if (_toggled) {
			// Draw the tab end 
			g.Save();
			g.MoveTo( x, y+r );
			g.Arc(    x+r, y+r, r, Math.PI, -hpi );
			g.LineTo( x+w, y );
			g.LineTo( x+w, y+h );
			g.LineTo( x+r, y+h );
			g.Arc(    x+r, y+h-r, r, hpi, Math.PI );
			g.ClosePath();
			
			// Fill
			g.Color = this.OriginalFillColor;
			g.FillPreserve();

			// Stroke
			g.Color = new Color(this.LineColor.R, this.LineColor.G, this.LineColor.B, 0.4); 
			//g.DashStyle = this.LineStyle;
			g.LineWidth = 2;
			g.Stroke();


			// Full size:
			w = this.width;
			hw = 0.5*w;
			cx = x + hw;
			if ( r > hh || r > hw ) r = Math.Min( hh, hw );

			/*  Right-hand side:
			g.MoveTo( x+w, y );
			g.LineTo( x+w, y+h );
			g.LineTo( x+w-endWidth, y+h );
			g.LineTo( x+w-endWidth, y );
			g.ClosePath();

			g.Color = Diagram.Colors.Honeydew;
			g.FillPreserve();
			g.Color = new Color(this.LineColor.R, this.LineColor.G, this.LineColor.B, 0.4); 
			g.Stroke();
			*/

			g.MoveTo( x, y+r );
			g.Arc(    x+r, y+r, r, Math.PI, -hpi );
			g.LineTo( x+w-r, y );
			g.Arc(    x+w-r, y+r, r, -hpi, 0.0 );
			g.LineTo( x+w, y+h-r );
			g.Arc(    x+w-r, y+h-r, r, 0.0, hpi);
			g.LineTo( x+r, y+h );
			g.Arc(    x+r, y+h-r, r, hpi, Math.PI );
			g.ClosePath();
			
			// Stroke
			g.Color = Diagram.Colors.Black;
			//g.DashStyle = this.LineStyle;
			g.LineWidth = 2;
			g.Stroke();

			g.Restore();
		    }
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void AddTab(CRoundedTab rt)
		{	// Add another CRoundToggle to the group to act in concert 
			_tabs.Add( new WeakReference(rt) );
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void AddTabs(List<CRoundedTab> rts)
		{	// Add a list of associated rounded tabs to this tab
			foreach (CRoundedTab rt in rts) 
				if (this != rt)
					_tabs.Add( new WeakReference(rt) );
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void AddShape(Diagram.CShape shp) 
		{	// Add a CShape object to the shapes with visibility controlled by the tab 
			_shapes.Add( new WeakReference(shp) );
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void AddShapes(List<Diagram.CShape> shps) 
		{
			foreach (Diagram.CShape shp in shps) 
				_shapes.Add( new WeakReference(shp) );
		}
		
		// - - - Return selected state - - - - - - - - - - - - - - - -
		public bool Toggled 
		{
			get
			{
				return _toggled;
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Toggle(Diagram.Canvas cvs) 
		{	// Toggle button state 
			this.SetToggle(cvs, !_toggled);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void SetToggle(Diagram.Canvas cvs, bool val) 
		{	// Set the toggle state of a toggle button 
			// Set this tab state
			_toggled = val;
			
			if (_toggled) {
			    // Change the tab to indicate it is selected
			    this.FillColor = Diagram.Colors.Honeydew;
			} else {
			    this.FillColor = this.OriginalFillColor;
			}
			
			// If turned on, turn off all other tabs in group
			// and show all referenced shapes
			if (_toggled == true) 
			{
				if (_palette != null) _palette._currTab= this;

				foreach (WeakReference wr in _tabs) {
					CRoundedTab rt = wr.Target as CRoundedTab;
					if (rt != null) rt.SetToggle(cvs, false);
				}
				foreach (WeakReference wrshp in _shapes) {
					Diagram.CShape shp = wrshp.Target as Diagram.CShape;
					if (shp != null) {
						shp.Visible = true;
						shp.BringToFront(cvs);
					}
				}
			}
			// If turned off, hide all referenced shapes
			else {
				foreach (WeakReference wrshp in _shapes) {
					Diagram.CShape shp = wrshp.Target as Diagram.CShape;
					if (shp != null) shp.Visible = false;
				}
			}
			
			// Redraw
			if (cvs != null) cvs.DeselectAll();
		}
	}
	
//	// -----------------------------------------------------------------------
//	public class CPanel : CRectangle
//	{
//		// A list holding shape weak references to be shown/hidden when panel is shown/hidden
//		private List<WeakReference> _shapes = new List<WeakReference>();
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public CPanel(double X, double Y, double W, double H) 
//		: base(X, Y, W, H){
//			
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public override bool Visible {
//			set {
//				if (value == true) {
//					foreach (WeakReference wr in _shapes) {
//						if (wr.Target is Widget) {
//							.Show();
//					visible = true;
//				}
//				else {
//					_entry.Hide();
//					visible = false;
//				}
//			}
//			get {
//				return visible;
//			}
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public override void OnMouseDown(Canvas cvs, MouseEventArgs e)
//        {	// Handle mouse down event
//			
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//        public override void OnMouseMove(Canvas cvs, MouseEventArgs e)
//        {	// Handle mouse move event
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public override void OnMouseUp(Canvas cvs, MouseEventArgs e)
//        {	// Handle mouse up event
//			cvs.handler = cvs;
//		}
//	}
}

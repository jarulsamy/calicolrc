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
	public class CBlockPalette : Diagram.CRectangle
	{	// The background palette for currently available block factories
		
		// Keep a ref to the currently selected tab so the proper factory blocks are scrolled
		internal CRoundedTab _currTab = null;
		
		// Keep track of previous mouse Y position to enable scrolling of factories
		private double? _prevMouseY = null;
		
		//private CSlider slider;
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CBlockPalette(double x, double y, double w, double h) : base(x, y, w, h)
		{
			this.LineWidth = 1;
			this.LineColor = Diagram.Colors.Honeydew;
			this.FillColor = Diagram.Colors.Honeydew;
			this.TextColor = Diagram.Colors.Transparent;
			this.Visible = true;
			this.Sizable = false;
			this.Selectable = false;
			this.Draggable = false;
			this.Connectable = false;
			
			// Add slider to widget - 20 pixels wide
			//this.slider = new CSlider(x+w-20-5, y+5, 20, h-10, 0.0);
		}
		
		public CBlockPalette(double x, double y) : base(x, y) {}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override Diagram.CShape Clone(double x, double y) {
			CBlockPalette clone = new CBlockPalette(x, y, this.Width, this.Height);
			return (Diagram.CShape)clone;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void MoveBlocksToTop(Diagram.Canvas cvs) {
			double move = 0;
			foreach (WeakReference wrshp in _currTab._shapes)
			{
				Diagram.CShape shp = wrshp.Target as Diagram.CShape;
				if (shp != null) {
					move = shp.Top;
					cvs.DeselectAll();		// Deselect everything. Cannot select multiple blocks.
					shp.Select(cvs);		// Select this shape
					break;
				}
			}
			foreach (WeakReference wrshp in _currTab._shapes)
			{
				Diagram.CShape shp = wrshp.Target as Diagram.CShape;
				if (shp != null) {
					shp.Top -= move - 5;
				}
			}
			cvs.Invalidate();
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void MoveBlocksToBottom(Diagram.Canvas cvs) {
			double move = 0;
			foreach (WeakReference wrshp in _currTab._shapes)
			{
				Diagram.CShape shp = wrshp.Target as Diagram.CShape;
				if (shp != null) {
					move = shp.Top;
					cvs.DeselectAll();		// Deselect everything. Cannot select multiple blocks.
					shp.Select(cvs);		// Select this shape
				}
			}
			foreach (WeakReference wrshp in _currTab._shapes)
			{
				Diagram.CShape shp = wrshp.Target as Diagram.CShape;
				if (shp != null) {
					shp.Top -= move - 5;
				}
			}
			cvs.Invalidate();
		}

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
				
				foreach (WeakReference wrshp in _currTab._shapes)
				{
					Diagram.CShape shp = wrshp.Target as Diagram.CShape;
					if (shp != null) {
						shp.Top += dY;
					}
				}
				cvs.Invalidate();
			} else {
				base.OnScroll(cvs, e);
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnMouseDown( Diagram.Canvas cvs, Diagram.MouseEventArgs e){
			if (this.ContainsPoint(e.X, e.Y, cvs) ) {
				_prevMouseY = e.Y;
			}
			//base.OnMouseDown(cvs, e);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnMouseMove( Diagram.Canvas cvs, Diagram.MouseEventArgs e){
			if (_prevMouseY != null && _currTab != null) {
				double dY = e.Y - (double)_prevMouseY;
				foreach (WeakReference wrshp in _currTab._shapes)
				{
					Diagram.CShape shp = wrshp.Target as Diagram.CShape;
					if (shp != null) {
						shp.Top += dY;
					}
				}
				_prevMouseY = e.Y;
				cvs.Invalidate();
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnMouseUp( Diagram.Canvas cvs, Diagram.MouseEventArgs e){
			_prevMouseY = null;
			base.OnMouseUp (cvs, e);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void Draw(Cairo.Context g)
        {
			base.Draw(g);
//			this.slider.Top = this.Top+5;
//			this.slider.Left = this.Left + this.Width - this.slider.Width-5;
//			this.slider.Height = this.Height - 10;
//			this.slider.Draw(g);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//        public override Boolean ContainsPoint(Diagram.CPoint pnt, Diagram.Canvas cvs)
//        {
//			return slider.ContainsPoint(pnt, cvs);
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
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CRoundedTab (double x, double y, double w, double h, string label, CBlockPalette palette,
							Cairo.Color fillColor, Cairo.Color lineColor)
			: base(x, y, w, h)
		{
			_palette = palette;
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
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void OnMouseDown(Diagram.Canvas cvs, Diagram.MouseEventArgs e)
        {	// Override CShape OnMouseDown method 
		
			// If within shape, change fill color
			if (this.ContainsPoint(e.X, e.Y, cvs)) 
			{	
				this.SetToggle(cvs, true);
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
        {	// Draw the tab widget
			
			// Start with the base rounded rectangle
			base.Draw(g);
			
			// Add two smaller rectangle for scrolling up and down
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
			
			if (_toggled) this.FillColor = Diagram.Colors.Honeydew;
			else		  this.FillColor = this.OriginalFillColor;
			
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

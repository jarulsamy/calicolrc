using System;
using Cairo;
using System.Collections.Generic;

// This Namespace contains various user-drawn widgets based on shapes in the Diagram namespace.

namespace Widgets
{
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
			base.OnMouseDown(cvs, e);
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
		
//		// Search data for this set of blocks:
//		internal bool searchEnd = false;
//		internal bool searchTop = false;
		
		// Save a reference to the block palette
		internal CBlockPalette _palette = null;
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CRoundedTab (double x, double y, double w, double h, string label, CBlockPalette palette)
			: base(x, y, w, h)
		{
			_palette = palette;
			this.Text = label;
			this.Radius = 5;
			this.LineWidth = 0;
			this.LineColor = Diagram.Colors.Silver;
			this.FillColor = Diagram.Colors.Silver;
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
			else		  this.FillColor = Diagram.Colors.Silver;
			
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

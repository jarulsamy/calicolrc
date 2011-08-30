using System;
using System.Drawing;
using System.Collections.Generic;

// This Namespace contains various user-drawn widgets based on shapes in the Diagram namespace.
// Most notably, this contains common GUI widgets.

namespace Widgets
{
	// -----------------------------------------------------------------------
	public class CRoundedButton : Diagram.CRoundedRectangle
	{
		private bool _Enabled = true;
		
		private Color _FillColor = Color.LightGray;
		private Color _DisabledFillColor = Color.LightGray;
		private Color _LineColor = Color.DarkSlateGray;
		private Color _DisabledLineColor = Color.SlateGray;
		private Color _TextColor = Color.Black;
		private Color _DisabledTextColor = Color.Gray;
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CRoundedButton (double x, double y, double w, double h, string label)
			: base(x, y, w, h)
		{
			this.Text = label;
			this.Radius = 5;
			this.LineWidth = 5;
//			this.LineColor = Color.DarkSlateGray;
//			this.FillColor = Color.LightGray;
//			this.TextColor = Color.Black;
			this.FontStyle = FontStyle.Bold;
			this.Selectable = false;
			this.Enabled = true;
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void OnMouseDown(Diagram.Canvas cvs, Diagram.MouseEventArgs e)
        {	// Handle mouse down event
			
			if (this.Enabled == false) return;
			
			// If within shape, change fill color
			if (this.ContainsPoint(e.X, e.Y, cvs)) {
				//Console.WriteLine("CRoundedButton.OnMouseDown");
				this.FillColor = Color.SlateGray;
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
			
			this.FillColor = Color.LightGray;
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
		public bool Enabled {
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
		private List<WeakReference> _shapes = new List<WeakReference>(); 
		
		/// <summary>
		/// Constructor 
		/// </summary>
		/// <param name="x">
		/// A <see cref="System.Double"/>
		/// </param>
		/// <param name="y">
		/// A <see cref="System.Double"/>
		/// </param>
		/// <param name="w">
		/// A <see cref="System.Double"/>
		/// </param>
		/// <param name="h">
		/// A <see cref="System.Double"/>
		/// </param>
		/// <param name="label">
		/// A <see cref="System.String"/>
		/// </param>
		/// <param name="toggled">
		/// A <see cref="System.Boolean"/>
		/// </param>
		public CRoundedTab (double x, double y, double w, double h, string label)
			: base(x, y, w, h)
		{
			this.Text = label;
			this.Radius = 5;
			this.LineWidth = 0;
			this.LineColor = Color.Silver;
			this.FillColor = Color.Silver;
			this.FontStyle = FontStyle.Bold;
			this.Selectable = false;
			this.SetToggle(null, false);
		}
		
		/// <summary>
		/// Add another CRoundToggle to the group to act in concert 
		/// </summary>
		/// <param name="rt">
		/// A <see cref="CRoundedToggle"/>
		/// </param>
		public void AddTab(CRoundedTab rt){
			_tabs.Add( new WeakReference(rt) );
		}
		
		/// <summary>
		/// Add a list of associated rounded tabs to this tab
		/// </summary>
		/// <param name="rts">
		/// A <see cref="List<CRoundedTab>"/>
		/// </param>
		public void AddTabs(List<CRoundedTab> rts){
			foreach (CRoundedTab rt in rts) _tabs.Add( new WeakReference(rt) );
		}
		
		/// <summary>
		/// Add a CShape object to the shapes with visibility controlled by the tab 
		/// </summary>
		/// <param name="shp">
		/// A <see cref="CShape"/>
		/// </param>
		public void AddShape(Diagram.CShape shp) {
			_shapes.Add( new WeakReference(shp) );
		}
		
		public void AddShapes(List<Diagram.CShape> shps) {
			foreach (Diagram.CShape shp in shps) _shapes.Add( new WeakReference(shp) );
		}
		
        /// <summary>
        /// Override CShape OnMouseDown method 
        /// </summary>
        /// <param name="cvs">
        /// A <see cref="Canvas"/>
        /// </param>
        /// <param name="e">
        /// A <see cref="MouseEventArgs"/>
        /// </param>
        public override void OnMouseDown(Diagram.Canvas cvs, Diagram.MouseEventArgs e)
        {	// Handle mouse down event
			
			// If within shape, change fill color
			if (this.ContainsPoint(e.X, e.Y, cvs)) {
				this.SetToggle(cvs, true);
				cvs.Invalidate();
			}
		}
		
		/// <summary>
		/// Toggle button state 
		/// </summary>
		/// <param name="val">
		/// A <see cref="System.Boolean"/>
		/// </param>
		public void Toggle(Diagram.Canvas cvs) {
			this.SetToggle(cvs, !_toggled);
		}
		
		/// <summary>
		/// Set the toggle state of a toggle button 
		/// </summary>
		/// <param name="val">
		/// A <see cref="System.Boolean"/>
		/// </param>
		public void SetToggle(Diagram.Canvas cvs, bool val) {
			// Set this tab state
			_toggled = val;
			if (_toggled) this.FillColor = Color.Honeydew;
			else		  this.FillColor = Color.Silver;
			
			// If turned on, turn off all other tabs in group
			// and show all referenced shapes
			if (_toggled == true) {
				foreach (WeakReference wr in _tabs) {
					CRoundedTab rt = wr.Target as CRoundedTab;
					if (rt != null) rt.SetToggle(cvs, false);
				}
				foreach (WeakReference wrshp in _shapes) {
					Diagram.CShape shp = wrshp.Target as Diagram.CShape;
					if (shp != null) shp.Visible = true;
				}
			}
			// If turned off, hide all referenced shapes
			else {
				foreach (WeakReference wrshp in _shapes) {
					Diagram.CShape shp = wrshp.Target as Diagram.CShape;
					if (shp != null) shp.Visible = false;
				}
			}
			
			if (cvs != null) cvs.DeselectAll();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void OnMouseMove(Diagram.Canvas cvs, Diagram.MouseEventArgs e)
        {	// Handle mouse move event
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnMouseUp(Diagram.Canvas cvs, Diagram.MouseEventArgs e)
        {	// Handle mouse up event
			cvs.handler = cvs;
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

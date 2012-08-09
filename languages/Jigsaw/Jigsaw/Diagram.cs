using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Text;
using System.Xml;
using System.IO;
using System.Reflection;
using Cairo;

// Notes:

// Be able to add/remove intermediate points on Splines and Connectors
// Modify Decorators - set Anchor and Offset (Anchor: TopLeft, MiddleLeft, BottomLeft, TopCenter, MiddleCenter, BottomCenter, TopRight, MiddleRight, BottomRight)
// There is a problem with creating splines or connectors.
// It is possible to create them with only one point.
// (Start drawing and then hit Esc or right-mouse)
// This causes a runtime error when attempting to draw with only one point.

namespace Diagram
{
	// -----------------------------------------------------------------------
	public static class Colors {
		public static readonly Color Transparent    = new Color(0.0,    0.0,    0.0,    0.0);
		public static readonly Color White          = new Color(1.0,    1.0,    1.0);
		public static readonly Color SemiWhite      = new Color(1.0,    1.0,    1.0,    0.7);
		public static readonly Color Silver         = new Color(0.75,   0.75,   0.75);
		public static readonly Color Gray           = new Color(0.5,    0.5,    0.5);
		public static readonly Color LightGray      = new Color(0.8242, 0.8242, 0.8242);
		public static readonly Color DarkGray       = new Color(0.6601, 0.6601, 0.6601);
		public static readonly Color SlateGray      = new Color(0.4375, 0.5,    0.5625);
		public static readonly Color DarkSlateGray  = new Color(0.1562, 0.3086, 0.3086);
		public static readonly Color LightSlateGray = new Color(0.4648, 0.5312, 0.5977);
		public static readonly Color WhiteSmoke     = new Color(0.9570, 0.9570, 0.9570);
		public static readonly Color Black          = new Color(0.0,    0.0,    0.0);

		public static readonly Color Yellow         = new Color(1.0,    1.0,    0.0);
		public static readonly Color LightYellow    = new Color(1.0,    1.0,    0.875);
		public static readonly Color DarkGoldenrod  = new Color(0.7187, 0.5234, 0.0430);
		public static readonly Color PaleGoldenrod  = new Color(0.9297, 0.9062, 0.6641);
		public static readonly Color Honeydew       = new Color(0.9375, 1.0,    0.9375);
		
		public static readonly Color LightBlue      = new Color(0.6758, 0.8437, 0.8984);
		public static readonly Color DarkBlue       = new Color(0.0,    0.0,    0.5430);

		public static readonly Color Red            = new Color(1.0,    0.0,    0.0);
		public static readonly Color DarkRed        = new Color(0.5430, 0.0,    0.0);
		public static readonly Color LightPink      = new Color(1.0,    0.7109, 0.7539);
		
		public static readonly Color DarkGreen      = new Color(0.0,    0.3910, 0.0);
		public static readonly Color LightGreen     = new Color(0.5625, 0.9297, 0.5625);
		
		public static readonly Color Purple         = new Color(0.56,   0.0,    0.5);
		public static readonly Color Plum           = new Color(0.867,  0.625,  0.867);
		public static readonly Color Thistle        = new Color(0.844,  0.746,  0.844);
	}
	
    // -----------------------------------------------------------------------
    public enum EMode
    {	// Current operational modes for the Canvas object. Starts out in Editing mode.
        Editing,  Drawing, 
		
		// Following modes are reserved for EditMode and DrawMode. Both start as Idle.
		Idle,
        DragLeftStart,    DragLeft,
        DrawLeftStart,    DrawLeft,
        SizeLeftStart,    SizeLeft,
        PEditLeftStart,   PEditLeft, 
		TranslatingStart, Translating,
        LassoLeftStart,   LassoLeft
    }
	
	// -----------------------------------------------------------------------
	public enum DockSide
	{	// Define the side of the diagram to which the shape should be docked
		Left, Right, Top, Bottom, None
	}
	
	// -----------------------------------------------------------------------
	public enum MouseButtons
	{
		Left = 1,
		Middle = 2,
		Right = 3
	}
	
	// -----------------------------------------------------------------------
	public class MouseEventArgs {
		public double X;
		public double Y;
		public MouseButtons Button;
		public bool ShiftKey = false;
		public bool ControlKey = false;
		
		public MouseEventArgs(double X, double Y) {
			this.X = X;
			this.Y = Y;
		}

		public MouseEventArgs(double X, double Y, MouseButtons Button) {
			this.X = X;
			this.Y = Y;
			this.Button = Button;
		}
		
		public MouseEventArgs(double X, double Y, Gtk.ButtonPressEventArgs e) {
			this.X = X;
			this.Y = Y;
			this.Button = (MouseButtons)e.Event.Button;
			this.ShiftKey = (e.Event.State & Gdk.ModifierType.ShiftMask) != 0;
			this.ControlKey = (e.Event.State & Gdk.ModifierType.ControlMask) != 0;
		}

		public MouseEventArgs(double X, double Y, Gtk.ButtonReleaseEventArgs e) {
			this.X = X;
			this.Y = Y;
			this.Button = (MouseButtons)e.Event.Button;
			this.ShiftKey = (e.Event.State & Gdk.ModifierType.ShiftMask) != 0;
			this.ControlKey = (e.Event.State & Gdk.ModifierType.ControlMask) != 0;
		}
		
		public MouseEventArgs(double X, double Y, Gtk.MotionNotifyEventArgs e) {
			this.X = X;
			this.Y = Y;
			//this.Button = (MouseButtons)e.Event.Button;
			this.ShiftKey = (e.Event.State & Gdk.ModifierType.ShiftMask) != 0;
			this.ControlKey = (e.Event.State & Gdk.ModifierType.ControlMask) != 0;
		}
	}
	
	// -----------------------------------------------------------------------
	public static class DashStyle {
		public static double[] Solid = null;
		public static double[] Dot = new double[] { 4.0, 4.0 };
		public static double[] Dash = new double[] { 8.0, 8.0 };
	}
	
    // -----------------------------------------------------------------------
    public interface ICanvasEventHandler
    {	// This interface is to be implemented by all Canvas shapes that will handle mouse events.
        void OnMouseDown(Canvas cvs, MouseEventArgs e);
        void OnMouseMove(Canvas cvs, MouseEventArgs e);
        void OnMouseUp(Canvas cvs, MouseEventArgs e);
        void OnDoubleClick(Canvas cvs, MouseEventArgs e);
		void OnScroll(Canvas cvs, Gdk.EventScroll e);
        void CancelProcess(Canvas cvs);                 // Called when the current process should be cancelled
    }

    // -----------------------------------------------------------------------
    // Custom event delegate declarations
    public delegate void CanvasEventHandler(Canvas cvs, ShapeListEventArgs e);
    public delegate void ShapeEventHandler(CShape shp, ShapeEventArgs e);
	public delegate void MouseEventHandler(Canvas cvs, MouseEventArgs e);

    // -----------------------------------------------------------------------
    public class Canvas : Gtk.DrawingArea, ICanvasEventHandler
    {
		// Public properties
        public  List<CShape> shapes;                            // List of shapes on shape layer
        public  List<CShape> connectors;                        // List of shapes on the connector layer
        private List<CShape> annotation;                        // List of shapes on annotation layer
		
		public Color BackColor = Colors.LightSlateGray;			// Back color
        internal CShape lasso;                                  // Shapes used as lasso

        internal CPoint mouseDownExact;                         // Exact location of last mouse down event
        internal CPoint mouseDownGrid;                          // Grid-snapped of last mouse down event
        internal CPoint mouseExact;                             // Exact location of last mouse move event
        internal CPoint mouseGrid;                              // Grid-snapped of last mouse move event
        internal CPoint mouseUpExact;                           // Exact location of last mouse up event
        internal CPoint mouseUpGrid;                            // Grid-snapped of last mouse up event

        internal EMode Mode     = EMode.Editing;                // Init in editing mode
        internal EMode EditMode = EMode.Idle;                	// Edit mode starts idle
        internal EMode DrawMode = EMode.Idle;                	// Draw mode starts idle
		
		internal double worldWidth = 3000.0;					// Size of underlying canvas world
		internal double worldHeight = 2000.0;
        internal double scale = 1.0;                            // Drawing scale (zoom) factor
		internal double scaleCenterX = 0.0;						// The point about which scaling is performed (screen coordinates)
		internal double scaleCenterY = 0.0;
		internal double offsetX = 0.0;							// The current translation amount
		internal double offsetY = 0.0;
		internal double maxOffsetX = 0.0;						// Range of valid offset values
		internal double minOffsetX = 0.0;
		internal double maxOffsetY = 0.0;
		internal double minOffsetY = 0.0;
		
		private double gridsize = 5.0;                          // Size of the drawing grid in pixels
        private bool modified = false;                          // True if canvas has been modified and should be saved
		
		internal Gdk.ModifierType ModifierKeys = 0;				// State of modifier keys
		
        internal ICanvasEventHandler handler;                   // The object currently handling Canvas events
		
        static private int shapeIdx = 0;                        // Helps with automatic shape name creation
        static private int connectorIdx = 0;                    // Helps with automatic connector name creation

        // Custom events
        public event CanvasEventHandler SelectionChanged;       // Raised when shape selection changed
		public event EventHandler CanvasChanged;
        public event CanvasEventHandler ShapesCreated;          // Raised when a new shape is created
        public event CanvasEventHandler ShapesDeleted;          // Raised when a shape is deleted
        public event CanvasEventHandler ShapesMoved;            // Raised when one or more shape are moved
        public event CanvasEventHandler ShapesSized;            // Raised when one or more shape are resized
        public event CanvasEventHandler CanvasError;            // Raised when a canvas error occurs

        // These events are raised for benefit of script
        public event MouseEventHandler CanvasMouseDown;         // Raised when mouse down on canvas
        public event MouseEventHandler CanvasMouseMove;         // Raised when mouse move on canvas
        public event MouseEventHandler CanvasMouseUp;           // Raised when mouse up on canvas
        public event MouseEventHandler CanvasClick;             // Raised when click on canvas
        public event MouseEventHandler CanvasDoubleClick;       // Raised when double-click on canvas
		
		// Reference to single PropertiesWindow object
		private Jigsaw.PropertyWindow _propertyWin = null;
		
		// Boolean indicating whether or not to draw inset
		internal bool _showInset = false;
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public Canvas(int width, int height, double worldWidth, double worldHeight)
        {
			this.SetSizeRequest(width, height);
			this.worldWidth = worldWidth;
			this.worldHeight = worldHeight;
			
			// Set up events
			this.AddEvents ((int)Gdk.EventMask.AllEventsMask );
			this.CanFocus = true;		// Required to receive KeyPressEvents
			
			this.ButtonPressEvent   += this.OnMouseDown; //new Gtk.ButtonPressEventHandler ( this.OnMouseDown );
			this.ButtonReleaseEvent += this.OnMouseUp; //new Gtk.ButtonReleaseEventHandler ( this.OnMouseUp );
			this.MotionNotifyEvent  += this.OnMouseMove; //new Gtk.MotionNotifyEventHandler ( this.OnMouseMove );
			this.ScrollEvent        += this.OnScroll; //new Gtk.ScrollEventHandler( this.OnScroll );
			this.KeyPressEvent      += this.OnKeyPress; //new Gtk.KeyPressEventHandler( this.OnKeyPress );
			
			// Snoop on all key events
			//Gtk.Key.SnooperInstall(this.SnoopKey);
			
            // The Canvas object maintains two layers (collections):
            // - The shapes layer holds all managed blocks on the canvas
            // - The annotation layer holds all handles and other objects
            //   that are used to manipulate shapes
            this.shapes     = new List<CShape>();
            this.connectors = new List<CShape>();
            this.annotation = new List<CShape>();
			
			// Init scrolling offset limits
			this.UpdateOffsetLimits(width, height);
			
            // Init editing states
            this.BeginEditing();

            // Init the lasso
            List<CPoint> lassopts = new List<CPoint>();
            lassopts.Add(new CPoint(0.0,0.0));
            lassopts.Add(new CPoint(1.0,1.0));
            this.lasso = new CRectangle(lassopts, "", Colors.Black, Colors.Gray, 1, Colors.Transparent,
                                        false, false, false, false, false);
            this.annotation.Add(this.lasso);
			
			// Properties window shared by all blocks
			_propertyWin = new Jigsaw.PropertyWindow(this);
			
            // The object currently handling events.
            // Canvas directs native UserControl events to this object's methods.
            this.handler = this;        // Init to the Canvas itself
        }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		internal void PrintDump() {
			Console.WriteLine("Shapes:");
			foreach (CShape s in this.shapes) Console.WriteLine(s);
			Console.WriteLine("Connectors:");
			foreach (CShape s in this.connectors) Console.WriteLine(s);
			Console.WriteLine("Annotation:");
			foreach (CShape s in this.annotation) Console.WriteLine(s);
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		internal int SnoopKey(Gtk.Widget o, Gdk.EventKey k) {
//			switch(k.Key)
//			{
//				case Gdk.Key.Up:
//					this.DoZoom(1.05);
//					break;
//				case Gdk.Key.Down:
//					this.DoZoom(1.0/1.05);
//					break;
//			}
//			
//			// Return unique id
//			return 1;
//		}
		
		// - - - Clear all delegates that have registed to handle this class's events - - -
        public void ClearEventHandlers()
        {
            this.CanvasMouseDown = null;
            this.CanvasMouseMove = null;
            this.CanvasMouseUp   = null;
            this.CanvasClick     = null;
        }

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public virtual void ShowPropertiesWindow()
//		{
//			_propertyWin.SetPosition(Gtk.WindowPosition.Mouse);
//			_propertyWin.Deiconify();
//			_propertyWin.ShowAll();
//			_propertyWin.KeepAbove = true;	// The Mono 2.6.7 runtime needs this here for the Window to stay above others
//		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public void HidePropertiesWindow()
//		{
//			_propertyWin.Hide();
//		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		internal void Invalidate()
		{	// Indicate that the canvas must be redrawn
			this.QueueDraw();
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public void Draw(Cairo.Context g)
        {
			g.Save();

			// Scale the diagram to the zoom factor at center point
			g.Translate( this.scaleCenterX,  this.scaleCenterY);
			g.Scale(     this.scale,         this.scale);
			g.Translate(-this.scaleCenterX, -this.scaleCenterY);
			g.Translate( this.offsetX,       this.offsetY);
			
			// Clear background
			g.Color = this.BackColor;
			g.Paint();
			
            // Always draw in antialias mode.
            // Caution: this smears single pixels into small blurs on pixel boundaries.
            g.Antialias = Antialias.Subpixel;
			
            // Draw all visible 
			// connectors (bottom layer), then
			// shapes (middle layer), and finally
			// annotations (top layer)
            foreach (CConnector o in this.connectors) if (o.Visible == true) o.Draw(g);
            foreach (CShape     o in this.shapes    ) if (o.Visible == true) o.Draw(g);
            foreach (CShape     o in this.annotation) if (o.Visible == true) o.Draw(g);
			
			// Reset transform
			g.Restore();
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public void DrawInset(Cairo.Context g)
        {	// Draw the inset navigation thumbnail in upper right corner of window
			// Hard-coded to a size of 300x200, with a 10-pixel buffer
			g.Save();
			
			// Get the size of the window
			int w, h;
			this.GdkWindow.GetSize (out w, out h);
			
			// Translate to a point in the upper right corner
			// Leave a border of 10 pixels
			g.Translate( w-310,  10);
			
			// Set up a clip region into which a small version of diagram will be drawn
			g.Rectangle(-1.0, -1.0, 302.0, 202.0);
			g.Clip();
			
			// Draw background in clip region
			g.MoveTo(-1.0, -1.0);
			g.LineTo(301.0, -1.0);
			g.LineTo(301.0, 201.0);
			g.LineTo(-1.0, 201.0);
			g.LineTo(-1.0, -1.0);
			g.ClosePath();
			
			// Fill the rectangle
			g.Color = new Color(1.0, 1.0, 1.0, 0.3);
			g.Fill();
			
			// Save coordinate system
			g.Save();

			// Scale down so small version of diagram will fit
			// Scaling world down to 10% into a box of 300x200
			// Assumes original world size is 3000x2000
			g.Scale(0.1, 0.1);
			
            // Always draw in antialias mode.
            // Caution: this smears single pixels into small blurs on pixel boundaries.
            //g.Antialias = Antialias.Subpixel;
			
            // Draw all visible 
			// connectors (bottom layer), then
			// shapes (middle layer), and finally
			// annotations (top layer)
            foreach (CConnector o in this.connectors) if (o.Visible == true) o.Draw(g);
            foreach (CShape     o in this.shapes    ) if (o.Visible == true) o.Draw(g);
            //foreach (CShape     o in this.annotation) if (o.Visible == true) o.Draw(g);
			
			// Transform from world coordinates to screen coordinates
			g.Translate(-(this.offsetX-this.scaleCenterX), -(this.offsetY-this.scaleCenterY));
			g.Scale(    1.0/this.scale,       1.0/this.scale);
			g.Translate( -this.scaleCenterX,  -this.scaleCenterY);
			
			// Draw the rectangle that frames the current viewport
			g.MoveTo(0.0, 0.0);
			g.LineTo(w, 0.0);
			g.LineTo(w, h);
			g.LineTo(0.0, h);
			g.LineTo(0.0, 0.0);
			g.ClosePath();
			
			// Stroke the viewport rectangle
			g.Color = Colors.Black;
			g.LineWidth = 1;
			g.Stroke();
			
			// Reset transform
			g.Restore();
			
			// Draw a border around the clip region
			g.MoveTo(-1.0, -1.0);
			g.LineTo(301.0, -1.0);
			g.LineTo(301.0, 201.0);
			g.LineTo(-1.0, 201.0);
			g.LineTo(-1.0, -1.0);
			g.ClosePath();
			
			// Stroke the outer rectangle
			g.Color = Colors.Black;
			g.LineWidth = 2;
			g.Stroke();
						
			g.Restore();
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected override bool OnExposeEvent(Gdk.EventExpose args)
        {	// Handle the Paint event by drawing all shapes and annotations
			using (Context g = Gdk.CairoHelper.Create( args.Window )) 
			{
				Draw( g );
				
				if (_showInset) DrawInset(g);
			}
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		internal void UpdateOffsetLimits()
		{
			if (this.GdkWindow == null) return;
			
			// Get the size of the window
			int w, h;
			this.GdkWindow.GetSize(out w, out h);
			
			this.UpdateOffsetLimits(w, h);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		internal void UpdateOffsetLimits(int w, int h)
		{	// Recompute the offset limits when dependent values change, such as scale
			
			// Compute the range of allowable offsets
			this.maxOffsetX = this.scaleCenterX-(this.scaleCenterX/this.scale);
			this.maxOffsetY = this.scaleCenterY-(this.scaleCenterY/this.scale);
			this.minOffsetX = w-this.worldWidth-this.maxOffsetX;
			this.minOffsetY = h-this.worldHeight-this.maxOffsetY;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		internal void ClipOffsets()
		{	// Clip offsets to current limits
			if (this.offsetX > this.maxOffsetX) this.offsetX = this.maxOffsetX;
			if (this.offsetY > this.maxOffsetY) this.offsetY = this.maxOffsetY;
			if (this.offsetX < this.minOffsetX) this.offsetX = this.minOffsetX;
			if (this.offsetY < this.minOffsetY) this.offsetY = this.minOffsetY;	
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void DoTranslate(double deltaX, double deltaY)
		{
			this.offsetX += deltaX;
			this.offsetY += deltaY;
			
			// Update and clip offsets
			this.UpdateOffsetLimits();
			this.ClipOffsets();
			this.Invalidate();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void DoZoom(double factor)
		{	// Perform the zoom
			this.scale *= factor;
			
			// Compute the minimum zoom factor
			int w, h;
			this.GdkWindow.GetSize(out w, out h);
			double minZoom = Math.Max(w/this.worldWidth, h/this.worldHeight);
			
			// Clip zoom factor
			if (this.scale < minZoom) this.scale = minZoom;
			if (this.scale > 20.0) this.scale = 20.0;
			
			//this.scaleCenterX = 0.5*this.Allocation.Width;
			//this.scaleCenterY = 0.5*this.Allocation.Height;

			this.scaleCenterX = 0.0; //0.5*this.Allocation.Width;
			this.scaleCenterY = 0.0; //0.5*this.Allocation.Height
			
			// Update and clip offsets
			this.UpdateOffsetLimits();
			this.ClipOffsets();
			this.Invalidate();	
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void DoResetZoom() {
			this.scale = 1.0;
			this.offsetX = 0.0;
			this.offsetY = 0.0;
			this.scaleCenterX = 0.5*this.Allocation.Width; //scl.X;
			this.scaleCenterY = 0.5*this.Allocation.Height; //scl.Y;
			this.UpdateOffsetLimits();
			this.ClipOffsets ();
			this.Invalidate();	
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnViewZoomIn(object sender, EventArgs e)
		{
			this.DoZoom (1.05);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnViewZoomOut(object sender, EventArgs e)
		{
			this.DoZoom(1.0/1.05);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnViewZoom100(object sender, EventArgs e)
		{
			this.DoResetZoom();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnViewToggleInset(object sender, EventArgs e)
		{
			this.ToggleInset();
			this.Invalidate();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void ToggleInset() {
			this._showInset = !this._showInset;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected void OnKeyPress(object o, Gtk.KeyPressEventArgs e) 
		{
			if (e.Event.Key == Gdk.Key.Up) {
				this.DoTranslate(0.0, 5.0);
				e.RetVal = true;
			} else if (e.Event.Key == Gdk.Key.Down) {
				this.DoTranslate(0.0, -5.0);
				e.RetVal = true;
			} else if (e.Event.Key == Gdk.Key.Left) {
				this.DoTranslate(5.0, 0.0);
				e.RetVal = true;
			} else if (e.Event.Key == Gdk.Key.Right) {
				this.DoTranslate(-5.0, 0.0);
				e.RetVal = true;
			} else if ((e.Event.State & Gdk.ModifierType.ControlMask) != 0) {
				if ( e.Event.Key == Gdk.Key.plus ) {
					this.DoZoom(1.05);
				} else if (e.Event.Key == Gdk.Key.minus ) {
					this.DoZoom(1.0/1.05);
				} else if (e.Event.Key == Gdk.Key.Key_0 ) {
					this.DoResetZoom();
				}
				e.RetVal = true;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		internal void TransformPoint(double sx, double sy, out double wx, out double wy) {;
			wx = (sx - this.scaleCenterX)/this.scale + this.scaleCenterX - this.offsetX;
			wy = (sy - this.scaleCenterY)/this.scale + this.scaleCenterY - this.offsetY;
		}
		
		internal void InverseTransformPoint(double wx, double wy, out double sx, out double sy) {
			sx = this.scale*(wx + this.offsetX - this.scaleCenterX) + this.scaleCenterX;
			sy = this.scale*(wy + this.offsetY - this.scaleCenterY) + this.scaleCenterY;
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        protected void OnMouseDown(object o, Gtk.ButtonPressEventArgs e)
        {	// Handle DrawingArea mouse events
			
			// This is necessary for key and scroll events to work on canvas
			this.GrabFocus();
			
			// Update mouse down location and route event to current handler
			// Scale and translate point coordinates
			double ex = 0.0;
			double ey = 0.0;
			double sx = (double)e.Event.X;
			double sy = (double)e.Event.Y;
			
			this.TransformPoint(sx, sy, out ex, out ey);

            this.mouseDownExact = new CPoint(ex, ey); // Save the exact and snapped mousedown points
            this.mouseDownGrid = this.SnapToGrid(this.mouseDownExact);
			
			// Save special key state
			this.ModifierKeys = e.Event.State;
			
            // If something hit then set as handler.
            // First check if an annotation, shape or connector was hit.
            CShape ho = this.HitAnnotation(this.mouseDownExact);
			if (ho == null) ho = this.HitShape(this.mouseDownExact);
			if (ho == null) ho = this.HitConnector(this.mouseDownExact);
			
			// If something other than canvas was hit, reset handler.
            if (ho != null) this.handler = ho;
			
            // Route event to event handler
			MouseEventArgs mev = new MouseEventArgs(ex, ey, e);
			switch (e.Event.Type) {
			case Gdk.EventType.ButtonPress:
				this.handler.OnMouseDown(this, mev);
				break;
			case Gdk.EventType.TwoButtonPress:
				this.handler.OnDoubleClick(this, mev);
				break;
			default:
				break;
			}
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        protected void OnMouseMove(object o, Gtk.MotionNotifyEventArgs e)
        {	// Handle MouseMove events
			
            // Update mouse location and route event to current handler
			double ex = 0.0;
			double ey = 0.0;
			double sx = (double)e.Event.X;
			double sy = (double)e.Event.Y;
			this.TransformPoint(sx, sy, out ex, out ey);
			
            this.mouseExact = new CPoint(ex, ey); // Save the exact and snapped mouse position
            this.mouseGrid = this.SnapToGrid(this.mouseExact);
			
			// Save special key state
			this.ModifierKeys = e.Event.State;
			
            // Route event to current handler
			MouseEventArgs mev = new MouseEventArgs(ex, ey, e);
            this.handler.OnMouseMove(this, mev);
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        protected void OnMouseUp(object o, Gtk.ButtonReleaseEventArgs e)
        {	// Handle MouseUp events
			
            // Update mouse up location and route event to current handler
			double ex = 0.0;
			double ey = 0.0;
			double sx = (double)e.Event.X;
			double sy = (double)e.Event.Y;
			this.TransformPoint(sx, sy, out ex, out ey);

            this.mouseUpExact = new CPoint(ex, ey);       // Save the exact and snapped mouse up location
            this.mouseUpGrid = this.SnapToGrid(this.mouseExact);
			
			// Save special key state
			this.ModifierKeys = e.Event.State;
			
            // Route event to current handler
            this.handler.OnMouseUp(this, new MouseEventArgs(ex, ey, e));
        }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        protected void OnScroll(object o, Gtk.ScrollEventArgs e)
        {	// Handle DrawingArea wheel scroll events
			
			Gdk.EventScroll scl = (Gdk.EventScroll)e.Event;
			
            // If something hit then set as handler.
            // First check if an annotation, shape or connector was hit.
            //CShape ho = this.HitAnnotation(this.mouseExact);
			//if (ho == null) ho = this.HitShape(this.mouseExact);
			//if (ho == null) ho = this.HitConnector(this.mouseExact);
			
			ICanvasEventHandler ho = this.HitShape(this.mouseExact);
			if (ho == null) ho = this;
			
			ho.OnScroll (this, scl);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnScroll(Canvas cvs, Gdk.EventScroll scl)
        {	// Handle DrawingArea wheel scroll events

			if (scl.Direction == Gdk.ScrollDirection.Up) {
				this.DoTranslate(0.0, 20.0);
			} else if (scl.Direction == Gdk.ScrollDirection.Down) {
				this.DoTranslate(0.0, -20.0);
			} else if (scl.Direction == Gdk.ScrollDirection.Left) {
				this.DoTranslate(-20.0, 0.0);
			} else if (scl.Direction == Gdk.ScrollDirection.Right) {
				this.DoTranslate(20.0, 0.0);
			}
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnMouseDown(Canvas cvs, MouseEventArgs e)
        {	// Canvas mouse down handler
			
            if (this.Mode == EMode.Editing)
			{
	            // Deselect all if click on canvas with no shift key
	            int ndeselected = 0;
				
	            //if (Control.ModifierKeys != Keys.Shift) ndeselected = this.DeselectAll();
				if ((this.ModifierKeys & Gdk.ModifierType.ShiftMask) == 0) ndeselected = this.DeselectAll();
	            
	            // Indicate that the canvas selection has changed
	            if (ndeselected > 0) this.RaiseSelectionChangedEvent();
	
	            // Move the lasso into position
	            this.lasso.MorphTo(this.mouseDownExact, this.mouseDownExact);
	
	            // Update state of canvas so other methods are activated on subsequent mouse events
	            this.EditMode = EMode.LassoLeftStart;
	
	            // Redraw
	            this.Invalidate();
			}
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnMouseMove(Canvas cvs, MouseEventArgs e)
        {	// Canvas mouse move handler
			
            if (this.Mode == EMode.Editing)
            {
                if (this.EditMode == EMode.LassoLeftStart)
                {
                    this.lasso.MorphTo( this.mouseDownExact, this.mouseExact );
                    this.lasso.Visible = true;          // Make sure that this does not mark canvas as being modified
                    this.EditMode = EMode.LassoLeft;
                }
                else if (this.EditMode == EMode.LassoLeft)
                {
                    this.lasso.MorphTo( this.mouseDownExact, this.mouseExact );

                    // Redraw
                    this.Invalidate();
                }
            }
        }
        
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnMouseUp(Canvas cvs, MouseEventArgs e)
        {	// Canvas mouse up handler
			
            if (this.Mode == EMode.Editing)
            {
                if (this.EditMode == EMode.LassoLeft || this.EditMode == EMode.LassoLeftStart)
                {
                    // Select any shape contained within the lasso bounds
                    Boolean sel_changed = false;
                    foreach (CShape s in this.shapes)
                    {
                        // Check the selectable property here to prevent the Contains calculation if not necessary
                        if (s.Selectable == true && s.Visible == true && this.lasso.ContainsShape(s))
                        {
                            s.Select(cvs);
                            sel_changed = true;
                        }
                    }

                    foreach (CShape s in this.connectors)
                    {
                        // Check the selectable property here to prevent the Contains calculation if not necessary
                        if (s.Selectable == true && s.Visible == true && this.lasso.ContainsShape(s))
                        {
                            s.Select(cvs);
                            sel_changed = true;
                        }
                    }

                    // Hide lasso
                    this.lasso.Visible = false;
                    this.lasso.MorphTo(new CPoint(0.0,0.0), new CPoint(1.0,1.0));

                    // Indicate that the canvas selection has changed
                    if (sel_changed == true) this.RaiseSelectionChangedEvent();
                }

                // Move edit state back to idle
                this.EditMode = EMode.Idle;
            }
            
            // Redraw
            this.Invalidate();
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public virtual void OnDoubleClick(Canvas cvs, MouseEventArgs e)
        {	// Handle double-clicke event
			
//            CShape ho = null;
			
            if (this.Mode == EMode.Editing)
            {
                // If any other canvas state
                // If something hit then set as handler and route event to it.
				
//                // First check if an annotation, shape or connector was hit.
//                ho = this.HitAnnotation(this.mouseDownExact);
//				if (ho == null) ho = this.HitShape(this.mouseDownExact);
//				if (ho == null) ho = this.HitConnector(this.mouseDownExact);
//                if (ho != null)
//                {
//                    this.handler = ho;
//                    this.handler.OnDoubleClick(this, e);
//                    return;
//                }
//
//                // Nothing hit.
                
                // Deselect all if click on canvas with no shift key
                int ndeselected = 0;
                //if (Control.ModifierKeys != Keys.Shift) ndeselected = this.DeselectAll();
				if ((this.ModifierKeys & Gdk.ModifierType.ShiftMask) == 0) ndeselected = this.DeselectAll();
                
                // Indicate that the canvas selection has changed
                if (ndeselected > 0) this.RaiseSelectionChangedEvent();

                // Move the lasso into position
                //this.lasso.MorphTo(this.mouseDownExact, this.mouseDownExact);

                // Update state of canvas so other methods are activated on subsequent mouse events
                //this.EditMode = EMode.LassoLeftStart;

                // Redraw
                this.Invalidate();
            }
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		protected void OnPropertiesShow(object sender, EventArgs e)
//		{
//			this.ShowPropertiesWindow();
//		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void CancelProcess(Canvas cvs)
        {
            // There is nothing to cancel when the Canvas is the event handler
        }

//        // - - - - - - - - - - - - - - - - - - - - - - - - - - 
//        //protected override void OnDragEnter(DragEventArgs e)
//		protected void OnDragEnter(object o, DragBeginArgs e)
//        {
//            // React to the enter of a drag
//            e.Effect = DragDropEffects.Move;
//        }
//
//        // - - - - - - - - - - - - - - - - - - - - - - - - - - 
//        //protected override void OnDragDrop(DragEventArgs e)
//		protected void OnDragDrop(object o, DragDropArgs e)
//        {
//            // Create a new shape on the drawing canvas
//
//            // Location of the drop
//            double z = this.zoom;                                  // Get scale factor
//            CPoint tpt1  = new CPoint(this.PointToClient(new Point(e.X, e.Y)));
//            CPoint tpt2 = this.SnapToGrid(tpt1);
//            CPoint pt1 = new CPoint(tpt2.X/z, tpt2.Y/z);
//            
//            // Check if the IDataObject contains a TreeNode
//            if (e.Data.GetDataPresent(typeof(TreeNode)))
//            {
//                // Get the CShape class Type from the TreeNode and create an instance.
//                TreeNode tn = (TreeNode)e.Data.GetData(typeof(TreeNode));
//                if (tn.Tag != null)
//                {
//                    // ------------
//                    // Assume that the string that tags the TreeNode is of the form: AssemblyName;NamespaceName.ClassName
//                    // Get these strings, Look up Assembly and create a Type
//                    String info = (String)tn.Tag;
//                    Type t = this.FindType(info);
//
//                    // -------------
//                    //String[] items = info.Split(new String[] { ";" }, StringSplitOptions.RemoveEmptyEntries);
//                    //if (items.Length != 2) return;
//                    //String assemblyName = items[0].Trim();
//                    //String className = items[1].Trim();
//
//                    //// Look up the assembly. Should already be loaded (see Assembly.Load)
//                    //Assembly assembly = null;
//                    //Assembly[] asmbs = AppDomain.CurrentDomain.GetAssemblies();
//                    //foreach (Assembly a in asmbs)
//                    //{
//                    //    if (a.FullName.StartsWith(assemblyName) == true)
//                    //    {
//                    //        assembly = a;
//                    //        break;
//                    //    }
//                    //}
//                    //if (assembly == null) return;
//
//                    //// Create the type
//                    //Type t = assembly.GetType(className);
//
//                    // -------------
//                    //// Create an instance of the shape class.
//                    //// The shape class must have a constructor that take only a list of CPoint objects.
//                    //String className = (String)tn.Tag;
//                    //Assembly assembly = Assembly.GetExecutingAssembly();
//                    //AssemblyName assemblyName = assembly.GetName();
//                    //Type t = assembly.GetType(assemblyName.Name + "." + className);
//
//                    // -------------
//                    //Type t = (Type)tn.Tag;                  // Get class type to create from TreeNode Tag
//
//                    // -------------
//
//                    // Create parameters
//                    System.Object[] args = { pt1.X, pt1.Y };
//
//                    if (t.IsSubclassOf(typeof(CConnector)) == true)
//                    {   // If a connector, create and add to the connector layer
//                        CConnector s = (CConnector)Activator.CreateInstance(t, args);
//                        this.AddConnector(s);
//
//                        // Update selection state and raise event
//                        //if (Control.ModifierKeys != Keys.Shift) this.DeselectAll();
//						if ((this.ModifierKeys & ModifierType.ShiftMask) == 0) this.DeselectAll();
//                        s.Select(this);
//
//                        // Raise events
//                        this.RaiseShapesCreatedEvent(s);
//                        this.RaiseSelectionChangedEvent();
//                    }
//                    else
//                    {   // If not a connector, create and add to the shape layer
//                        CShape s = (CShape)Activator.CreateInstance(t, args);
//                        this.AddShape(s);
//
//                        // Update selection state and raise event
//                        //if (Control.ModifierKeys != Keys.Shift) this.DeselectAll();
//						if ((this.ModifierKeys & ModifierType.ShiftMask) == 0) this.DeselectAll();
//                        s.Select(this);
//
//                        // Raise events
//                        this.RaiseShapesCreatedEvent(s);
//                        this.RaiseSelectionChangedEvent();
//                    }
//                    
//                    // Update the canvas
//                    this.Invalidate();
//                }
//            }
//        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // This utility looks up or loads an assembly
        // and, if found, tries to create the given NamespaceName.ClassName Type
        // Assumes info is a string of the form "AssemblyName;NamespaceName.ClassName"
        private Type FindType(String info)
        {
            String[] items = info.Split(new String[] { ";" }, StringSplitOptions.RemoveEmptyEntries);
            if (items.Length != 2) return null;
            String assemblyName = items[0].Trim();
            String namespaceClassName = items[1].Trim();

            // Look up the assembly.
            Assembly assembly = null;
            Assembly[] asmbs = AppDomain.CurrentDomain.GetAssemblies();
            foreach (Assembly a in asmbs)
            {
                if (a.GetName().Name == assemblyName)
                {
                    assembly = a;
                    break;
                }
            }

            // If not found, try to load it
            if (assembly == null)
            {
                //System.Security.Policy.Evidence ev = new System.Security.Policy.Evidence();
                //assembly = Assembly.Load(assemblyName, ev);
            	assembly = Assembly.LoadFrom(assemblyName);
			}

            // If still don't have it, then quit
            if (assembly == null) return null;

            // Create the type
            Type t = assembly.GetType(namespaceClassName);

            return t;
        }

        // - - - Allow external class to raise the SelectionChanged event - - - - - - - 
        public virtual void RaiseSelectionChangedEvent()
        {
            if (SelectionChanged != null)
            {
                ShapeListEventArgs evargs = new ShapeListEventArgs(SelectedShapes());
				SelectionChanged(this, evargs);
            }
        }

        // - - - Allow external class to raise the SelectionChanged event - - - - - - - 
        public virtual void RaiseCanvasChangedEvent()
        {
			CanvasChangedEventArgs evargs = new CanvasChangedEventArgs(this.Modified);
			CanvasChanged(this, evargs);
        }

        // - - - Allow external class to raise the ShapesCreated event - - -
        public void RaiseShapesCreatedEvent(CShape s)
        {
            if (ShapesCreated != null)
            {
                this.Modified = true;
                ShapeListEventArgs evargs = new ShapeListEventArgs(s);
                ShapesCreated(this, evargs);
            }
        }

        // - - - Allow external class to raise the ShapesDeleted event - - -
        public void RaiseShapesDeletedEvent(CShape s)
        {
            if (ShapesDeleted != null)
            {
                this.Modified = true;
                ShapeListEventArgs evargs = new ShapeListEventArgs(s);
                ShapesDeleted(this, evargs);
            }
        }

        // - - - Allow external class to raise the ShapesMoved event - - -
        public void RaiseShapesMovedEvent(List<CShape> shps)
        {
            if (ShapesMoved != null)
            {
                this.Modified = true;
                ShapeListEventArgs evargs = new ShapeListEventArgs(shps);
                ShapesMoved(this, evargs);
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - 
        // Allow external class to raise the ShapesSized event
        public void RaiseShapesSizedEvent(List<CShape> shps)
        {
            if (ShapesSized != null)
            {
                this.Modified = true;
                ShapeListEventArgs evargs = new ShapeListEventArgs(shps);
                ShapesSized(this, evargs);
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - 
        // Allow external class to raise the ShapesSized event
        public void RaiseCanvasError(string msg)
        {
            if (CanvasError != null)
            {
                ShapeListEventArgs evargs = new ShapeListEventArgs(msg);
                CanvasError(this, evargs);
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - 
        // Allow external class to raise the MouseDown event
        public void RaiseCanvasMouseDown(MouseEventArgs e)
        {
            if (CanvasMouseDown != null)
            {
                try
                {
                    CanvasMouseDown(this, e);
                }
                catch (Exception ex)
                {
                    this.RaiseCanvasError(ex.Message);
                    Console.Error.WriteLine(ex.Message);
                }
            }
        }

		// - - - - - - - - - - - - - - - - - - - - - - - - - - 
        // Allow external class to raise the DoubleClick event
        public void RaiseCanvasDoubleClick(MouseEventArgs e)
        {
            if (CanvasDoubleClick != null)
            {
                try
                {
                    CanvasDoubleClick(this, e);
                }
                catch (Exception ex)
                {
                    this.RaiseCanvasError(ex.Message);
                    Console.Error.WriteLine(ex.Message);
                }
            }
        }
	
        // - - - - Allow external class to raise the MouseUp event - - - - - - - 
        public void RaiseCanvasMouseUp(MouseEventArgs e)
        {
            if (CanvasMouseUp != null)
            {
                try
                {
                    CanvasMouseUp(this, e);
                }
                catch (Exception ex)
                {
                    this.RaiseCanvasError(ex.Message);
                    Console.Error.WriteLine(ex.Message);
                }
            }
        }

        // - - - Allow external class to raise the MouseMove event - - -
        public void RaiseCanvasMouseMove(MouseEventArgs e)
        {
            if (CanvasMouseMove != null)
            {
                try
                {
                    CanvasMouseMove(this, e);
                }
                catch (Exception ex)
                {
                    this.RaiseCanvasError(ex.Message);
                    Console.Error.WriteLine(ex.Message);
                }
            }
        }

        // - - - Allow external class to raise the Click event - - -
        public void RaiseCanvasClick(MouseEventArgs e)
        {
            if (CanvasClick != null)
            {
                try
                {
                    CanvasClick(this, e);
                }
                catch (Exception ex)
                {
                    this.RaiseCanvasError(ex.Message);
                    Console.Error.WriteLine(ex.Message);
                }
            }
        }

        // - - - This method deletes all shapes on the canvas and sets it up as new - - -
        public void Clear()
        {
            // Make sure everything is deselected
            this.DeselectAll();

            // Now delete all connectors and shapes
            this.connectors.Clear();
            this.shapes.Clear();

            this.Modified = true;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual Boolean Modified
        {
            get { return this.modified; }
            set { 
				if (this.modified != value) {
					this.modified = value; 
					//Console.WriteLine ("Modified {0} at {1}", value, DateTime.Now.ToString ());
					try {
						this.RaiseCanvasChangedEvent();
					} catch {
						// Not ready yet
					}
				}
			}
        }

        // - - - Create a new point with coordintes equal to the closest grid point. - - -
        public CPoint SnapToGrid(CPoint pnt)
        {
			if (pnt == null) return null;
            double gs = this.gridsize;
            return new CPoint((double)Math.Floor((pnt.X / gs) + 0.5) * gs,
                              (double)Math.Floor((pnt.Y / gs) + 0.5) * gs);
        }

        // - - - Return a List<CShape> of selected shapes and connectors - - - - - - -
        public List<CShape> SelectedShapesAndConnectors()
        {
            List<CShape> tmp = new List<CShape>();
            foreach (CShape s in this.shapes)
            {
                if (s.Selected == true) tmp.Add(s);
            }
            foreach (CShape s in this.connectors)
            {
                if (s.Selected == true) tmp.Add(s);
            }
            return tmp;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Return a List<CShape> of selected CShape objects on the shapes list
        public List<CShape> SelectedShapes()
        {
            List<CShape> tmp = new List<CShape>();
            foreach (CShape s in this.shapes)
            {
                if (s.Selected == true) tmp.Add(s);
            }
            return tmp;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Return a List<CShape> of selected CShape objects on the connector list
        public List<CConnector> SelectedConnectors()
        {
            List<CConnector> tmp = new List<CConnector>();
            foreach (CConnector s in this.connectors)
            {
                if (s.Selected == true) tmp.Add(s);
            }
            return tmp;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Return a List<CShape> of all selected CShape and CConnector objects
        public List<CShape> AllSelected()
        {
            List<CShape> tmp = new List<CShape>();
            foreach (CShape s in this.shapes)
            {
                if (s.Selected == true) tmp.Add(s);
            }
            foreach (CShape s in this.connectors)
            {
                if (s.Selected == true) tmp.Add(s);
            }
            return tmp;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Return a List<CShape> of all CShape and CConnector objects
        public List<CShape> AllShapes()
        {
            List<CShape> tmp = new List<CShape>();
            tmp.AddRange(this.shapes);
            tmp.AddRange(this.connectors);
            return tmp;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public void AddShape(CShape shp)
        {	// Add a shape to the shape layer

            // Generate a unique name for this shape, if it does not have one
            if (shp.Name.Length == 0)
            {
                String nm;
                Boolean bUniqueName;

                do
                {
                    Canvas.shapeIdx++;                  // Next shape index and build new name
                    nm = String.Format("Shape{0}", Canvas.shapeIdx);
                    bUniqueName = true;                 // Assume this name is unique

                    foreach (CShape s in this.shapes)   // Check that name does not already exist
                    {
                        if (s.Name == nm)               // It is not unique.
                        {
                            bUniqueName = false;        // Try again.
                            break;
                        }
                    }

                } while (bUniqueName == false);         // keep going until find a unique name

                shp.Name = nm;                          // Assign the name
            }

            // Add the shape to the list
            this.shapes.Add(shp);
            this.Modified = true;
        }

        // - - - Add a shape to the connector layer - - -
        public void AddConnector(CShape shp)
        {
            // Generate a unique name for this connector, if it does not have one
            if (shp.Name.Length == 0)
            {
                String nm;
                Boolean bUniqueName;

                do
                {
                    Canvas.connectorIdx++;              // Next connector index and build new name
                    nm = String.Format("Connector{0}", Canvas.connectorIdx);
                    bUniqueName = true;                 // Assume this name is unique

                    foreach (CShape s in this.connectors)// Check that name does not already exist
                    {
                        if (s.Name == nm)               // It is not unique.
                        {
                            bUniqueName = false;        // Try again.
                            break;
                        }
                    }

                } while (bUniqueName == false);         // keep going until find a unique name

                shp.Name = nm;                          // Assign the name
            }

            // Add to list of connectors on canvas
            this.connectors.Add(shp);
            this.Modified = true;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Add a shape to the annotation layer
        public void AddAnnotation(CShape shp)
        {
            this.annotation.Add(shp);
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Delete a shape from the shape layer
        public void DeleteShape(CShape shp)
        {
            shp.Deselect(this);                     // Eliminate all annotation
            shp.Disconnect();                       // Disconnect any connectors
            this.shapes.Remove(shp);                // Remove from the canvas shape list
            this.Modified = true;                   // Mark as modified
            this.RaiseShapesDeletedEvent(shp);      // Raise event
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Delete a connector from the connector layer
        public void DeleteConnector(CConnector con)
        {
            con.Deselect(this);                     // Eliminate all annotation
            con.Disconnect();                       // Disconnect from any shapes
            this.connectors.Remove(con);            // Remove from the canvas connector list
            this.Modified = true;                   // Mark as modified
            this.RaiseShapesDeletedEvent(con);      // Raise event
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Delete a shape from the annotation layer
        public void DeleteAnnotation(CShape shp)
        {
            this.annotation.Remove(shp);
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Delete all annotation shapes
		public void DeleteAllAnnotation()
        {
            this.annotation.Clear();
        }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Delete all selected shapes and connectors
        public void DeleteAllSelected()
        {
            // First build two lists of selected connecotrs and shapes
            // Need to do this first because deleting connectors causes one or more the shapes
            // to become deselected. 
            // If selected shapes are obtained after connectors are deleted, some of the 
            // originally selected shapes are no longer selected and are therefore not deleted.
            List<CConnector> selcon = this.SelectedConnectors();
            List<CShape> selshp = this.SelectedShapes();

            foreach (CConnector s in selcon)
            {
                this.DeleteConnector(s);
            }
            foreach (CShape s in selshp)
            {
                this.DeleteShape(s);
            }
        }

        // - - - Select all shapes - - - - - - - - - - - - - - - - - - -
        // - - - Return the number of shapes selected.
        public int SelectAll()
        {
            int count = 0;
            foreach (CShape s in this.shapes)
            {
                if (s.Selected == false)
                {
                    s.Select(this);
                    count++;
                }
            }
            foreach (CShape s in this.connectors)
            {
                if (s.Selected == false)
                {
                    s.Select(this);
                    count++;
                }
            }
            return count;
        }

        // - - - Deselect all shapes - - - - - - - - - - - - - - - - - - -
        // - - - Return the number of shapes deselected.
        public int DeselectAll()
        {
            int count = 0;
            foreach (CShape s in this.shapes)
            {
                if (s.Selected == true)
                {
                    s.Deselect(this);
                    count++;
                }
            }
            foreach (CShape s in this.connectors)
            {
                if (s.Selected == true)
                {
                    s.Deselect(this);
                    count++;
                }
            }
            return count;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CShape HitShape(CPoint pt)
        {	/* This method searches through the shapes on the list from top down
			   and returns the first shape that was hit. */
			
            // Check shapes
            for ( int i=this.shapes.Count-1; i>=0; i--)
            {
                CShape shp = this.shapes[i].HitShape(pt, this);
                if (shp != null) return shp;
            }

            return null;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CShape HitShape(CPoint pt, Type shape_type)
        {	// If shape_type is set, return first shape that is an instance of given type.
			
            // Check shapes
            for (int i = this.shapes.Count - 1; i >= 0; i--)
            {
                CShape shp = this.shapes[i].HitShape(pt, this);
                if (shp != null)
                {
                    if (shp.GetType() == shape_type) return shp;
                }
            }

            return null;
        }
		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public void ReDockShapes(double dx, double dy) 
//		{	// Move docked shapes so that they remain docked
//			
//            foreach (CShape s in this.shapes)
//            {
//                if (s.Dock == DockSide.Left)
//                {
//					s.Left -= dx;
//					s.Top  -= dy;
//                }
//            }
//		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CShape HitConnector(CPoint pt)
        {	// This method searches through the connectors on the list from top down
        	// and returns the first connector that was hit.
			
            // Check connectors
            for (int i = this.connectors.Count - 1; i >= 0; i--)
            {
                CShape shp = this.connectors[i].HitShape(pt, this);
                if (shp != null) return shp;
            }

            return null;
        }

        // If shape_type is set, return first shape that is an instance of given type.
        public CShape HitConnector(CPoint pt, Type shape_type)
        {
            // Check connectors
            for (int i = this.connectors.Count - 1; i >= 0; i--)
            {
                CShape shp = this.connectors[i].HitShape(pt, this);
                if (shp != null)
                {
                    if (shp.GetType() == shape_type) return shp;
                }
            }

            return null;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CShape HitAnnotation(CPoint pt)
        {
            // This method searches through the shapes on the annotation layer
            // from top down and returns the first shape that was hit.

            for ( int i=this.annotation.Count-1; i>=0; i--)
            {
                CShape shp = this.annotation[i].HitShape(pt, this);
                if (shp != null) return shp;
            }

            return null;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public double Zoom
        {	// Set drawing zoom factor. A number greater than 0.
            get { return this.scale; }
            set {
				if (value < 0.0) value = 1.0;
				this.scale = value;
			}
        }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public void BringSelectedForward()
        {
            // Bring selected shapes forward in z-order by one position
            Boolean start_swapping = false;

            for (int i = this.shapes.Count - 1; i >= 0; i--)
            {
                CShape s = this.shapes[i];

                // Do not start swapping until found first unselected shape
                if (!s.Selected)
                {
                    start_swapping = true;
                }
                else if (start_swapping == true && s.Selected == true) 
                {
                    // Swap
                    CShape tmp = this.shapes[i+1];
                    this.shapes[i+1] = this.shapes[i];
                    this.shapes[i] = tmp;
                }
            }

            this.Invalidate();
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public void SendSelectedBackward()
        {
            // Send selected shapes backward in z-order by one position

            Boolean start_swapping = false;

            for (int i = 0; i < this.shapes.Count; i++)
            {
                CShape s = this.shapes[i];

                // Do not start swapping until found first unselected shape
                if (!s.Selected)
                {
                    start_swapping = true;
                }
                else if (start_swapping == true && s.Selected == true)
                {
                    // Swap
                    CShape tmp = this.shapes[i - 1];
                    this.shapes[i - 1] = this.shapes[i];
                    this.shapes[i] = tmp;
                }
            }

            this.Invalidate();
        }

        /// <summary>
        ///  Bring selected shapes to front of z-order
        /// </summary>
        public void BringSelectedToFront()
        {
            List<CShape> tmp = new List<CShape>();

            for (int i = this.shapes.Count - 1; i >= 0; i--)
            {
                CShape s = this.shapes[i];

                // Do not start swapping until found first unselected shape
                if (s.Selected)
                {
                    tmp.Insert(0, s);
                    this.shapes.RemoveAt(i);
                }
            }
            this.shapes.AddRange(tmp);
            this.Invalidate();
        }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public void SendSelectedToBack()
        {
            // Send selected shapes to bottom of z-order
            List<CShape> tmp = new List<CShape>();

            for (int i = this.shapes.Count - 1; i >= 0; i--)
            {
                CShape s = this.shapes[i];

                if (s.Selected)
                {
                    tmp.Insert(0, s);
                    this.shapes.RemoveAt(i);
                }
            }
            this.shapes.InsertRange(0, tmp);
            this.Invalidate();
        }

		/// <summary>
		/// Bring given CShape to front of z-order 
		/// </summary>
		public void BringToFront(CShape s) {
			this.shapes.Remove(s);
			this.shapes.Add(s);
		}
		
		/// <summary>
		/// Move given shape to bottom of z-order 
		/// </summary>
		public void SendToBack(CShape s) {
			this.shapes.Remove(s);
			this.shapes.Insert(0,s);
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public void BeginEditing()
        {	// Move the canvas into the editing state
			
            // Tell the current handler to cancel anything that it is doing.
            if (this.handler != null) this.handler.CancelProcess(this);

            // Change state
            this.Mode = EMode.Editing;
            this.DrawMode = EMode.Idle;
            this.EditMode = EMode.Idle;
            this.handler = this;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Begin interactive drawing of a named shape class
        public void BeginDrawing(ICanvasEventHandler handler)
        {
            this.Mode = EMode.Drawing;
            this.DrawMode = EMode.Idle;
            this.handler = handler;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Nudge selected shapes by given dx and dy
        public void NudgeSelected(double dx, double dy)
        {
            foreach (CShape s in this.SelectedShapes()) s.Nudge(dx, dy);
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Write XML representation of this canvas
        public virtual void ToXml(XmlWriter w)
        {
            w.WriteStartElement("canvas");
            //w.WriteAttributeString("backColor", ColorTranslator.ToHtml(this.BackColor));

            // Write all shapes
            int idCount = 0;
            foreach (CShape s in this.shapes)
            {
                idCount++;
                s._id = idCount;        // Assign temp id to shape
                s.ToXml(w);
            }

            // Write all connectors now that shapes have been assigned ids
            foreach (CConnector s in this.connectors)
            {
                s.ToXml(w);
            }
            w.WriteEndElement();

            // Must close by resetting all shape ids to 0 (unassigned)
            // otherwise subsequent save may be incorrect.
            foreach (CShape s in this.shapes)
            {
                s._id = 0;
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Write XML representation of selected object on the canvas
        public void SelectedToXml(XmlWriter w)
        {
            w.WriteStartElement("jigsaw");
            //w.WriteAttributeString("backColor", ColorTranslator.ToHtml(this.BackColor));

            // Write all shapes
            int idCount = 0;
            foreach (CShape s in this.SelectedShapes())
            {
                idCount++;
                s._id = idCount;        // Assign temp id to shape
                s.ToXml(w);
            }
						
            // Write all connectors
            foreach (CConnector s in this.SelectedConnectors())
            {
                s.ToXml(w);
            }
            w.WriteEndElement();

            // Must close by resetting all shape ids to 0 (unassigned)
            // otherwise subsequent save may be incorrect.
            foreach (CShape s in this.shapes)
            {
                s._id = 0;
            }
        }
			
		// - - -
		public void ShowPropertiesWindow() {
			
		}
		public void HidePropertiesWindow() {
			
		}
    }

    /// <summary>
    /// Class for event args of Shape event involving a single shape
    /// </summary>
    public class ShapeEventArgs : EventArgs
    {	
        public CShape Shape = null;
        public String Message = null;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public ShapeEventArgs(CShape shape) : this(shape, "")
        {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public ShapeEventArgs(CShape shape, String msg)
        {
            this.Shape = shape;
            this.Message = msg;
        }
    }

	/// <summary>
	/// Class for event args of CanvasChanged event
	/// </summary>
    public class CanvasChangedEventArgs : EventArgs
    {
        public bool Modified;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CanvasChangedEventArgs(bool value)
        { 
			Modified = value;
		}
	}
	
	/// <summary>
	/// Class for event args of Shape event involving multiple shapes
	/// </summary>
    public class ShapeListEventArgs : EventArgs
    {
        public List<CShape> Shapes = new List<CShape>();
        public String Message = null;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public ShapeListEventArgs()
        { }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public ShapeListEventArgs(List<CShape> shps)
        {
            this.Shapes = shps;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public ShapeListEventArgs(CShape shp)
        {
            this.Shapes.Add(shp) ;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public ShapeListEventArgs(String msg)
        {
            this.Message = msg;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public ShapeListEventArgs(List<CShape> shps, String msg)
        {
            this.Shapes = shps;
            this.Message = msg;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public ShapeListEventArgs(CShape shp, String msg)
        {
            this.Shapes.Add(shp);
            this.Message = msg;
        }
    }

    // -----------------------------------------------------------------------
    public class CPoint
    {	// A simple point object that is a ref type, not a value type.
        public double X;
        public double Y;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CPoint(double X, double Y)
        {
            this.X = X;
            this.Y = Y;
        }
        public CPoint(Point pt)
        {
            this.X = (double)pt.X;
            this.Y = (double)pt.Y;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CPoint Clone()
        {
            return new CPoint(this.X, this.Y);
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public void ToXML(XmlWriter w)
        {
            w.WriteStartElement("point");
            w.WriteAttributeString("x", this.X.ToString());
            w.WriteAttributeString("y", this.Y.ToString());
            w.WriteEndElement();
        }
    }

    // -----------------------------------------------------------------------
    public class CShape : ICanvasEventHandler
    {	// Base class for all shape objects
        internal List<CPoint> points = new List<CPoint>();     	// List of points that defines the shape
        internal Dictionary<String, CHandle> handles = new Dictionary<String, CHandle>();
        internal CShape Outline = null;                         // The shape that outlines this shape
		internal bool _isFactory = false;						// True if the block is a factory
        internal int _id = 0;                                   // This is a temp val assigned during serialization to Xml.
                                                                // It is used as shape refs by connectors when serialized to Xml.
        private String name = "";                               // Code name of this object
        protected String text = "";                             // Text to be drawn in shape
//		protected int textWidth = 0;							// Text metrics
//		protected int textHeight = 0;
//		protected int textXBearing = 0;
//		protected int textYBearing = 0;
        private Cairo.Color textColor = Colors.Black;           // Color of text
        private Cairo.Color lineColor = Colors.Black;
        private int lineWidth = 1;
		private double[] dashStyle = null; //new double[] {1.0};
        private Cairo.Color fillColor = Colors.White;
        internal String fontFace = "Arial"; //"Consolas"; //"courier new"; //
        internal double fontSize = 12.0;
        internal FontSlant fontSlant = FontSlant.Normal;
		internal FontWeight fontWeight = FontWeight.Bold;
        protected Boolean selected = false;
        private System.Object tag = null;                       // An arbitrary object that tags this object
//        private StringAlignment horizontalAlign = StringAlignment.Center;
//        private StringAlignment verticalAlign = StringAlignment.Center;
//        private Pango.Alignment horizontalAlign = Pango.Alignment.Center;
//        private Pango.Alignment verticalAlign = Pango.Alignment.Center;
//        private DashStyle lineStyle = DashStyle.Solid;

        protected Boolean visible = true;
        public Boolean Draggable = true;
        public Boolean Sizable = true;
        public Boolean Selectable = true;
        public Boolean Connectable = true;

        // Bounding Box coordinates
        internal double left;
        internal double top;
        internal double width;
        internal double height;
		
		internal DockSide Dock = DockSide.None;					// Default to no docking
		
        // Maintain a center point
        internal CPoint center = null;

        // Custom events
        public event ShapeEventHandler MouseDown;
        public event ShapeEventHandler MouseUp;
        public event ShapeEventHandler Click;
        public event ShapeEventHandler DoubleClick;
		public event ShapeEventHandler Transformed;

        // A list of connectors connected to this shape. This is maintained by CConnector methods
        public List<CConnector> Connectors = new List<CConnector>();
        
        // A list of shapes that decorate this one
        public Dictionary<String, CDecorator> Decorators = new Dictionary<String, CDecorator>(); 
		
		internal Diagram.Canvas _cvs = null;
		protected int _X;								// Cache
		protected int _Y;
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		
        // Constructors
        public CShape(List<CPoint> points, String text, Color textColor, 
                      Color lineColor, int lineWidth, Color fillColor,
                      Boolean visible, Boolean draggable, Boolean sizable, Boolean selectable, Boolean connectable )
		{
            // Copy the actual points from the given list to the internal list.
            // use the actual points rather than creating new ones in case another
            // object has a reference to these points.
            foreach (CPoint p in points) this.points.Add(p);

            this.text = text;
            this.textColor = textColor;
            this.lineColor = lineColor;
            this.lineWidth = lineWidth;
            this.fillColor = fillColor;
            this.visible = visible;
            this.Draggable = draggable;
            this.Sizable = sizable;
            this.Selectable = selectable;
            this.Connectable = connectable;
			
            this.center = new CPoint(0.0, 0.0);
            this.UpdateBoundingBox();
        }

        public CShape(List<CPoint> points):
            this(points, "", Colors.Black, Colors.Black, 1, Colors.White, 
                 true, true, true, true, true) {}

        public CShape(double X, double Y, double W, double H)
            : this(new List<CPoint>(new CPoint[] { new CPoint(X, Y), new CPoint(X + W, Y + H) })) { }

        // This constructor is primarily for drag and drop creation of shapes.
        public CShape(Double X, Double Y)
            : this(new List<CPoint>(new CPoint[] { new CPoint(X, Y), new CPoint(X + 50.0, Y + 50.0) })) { }

        // This constructor takes a Dictionary that contains all constructor parameters.
        // The lack of a key for a given parameter means use a default value
        public CShape(Dictionary<String, System.Object> parms)
        {
            // Copy the actual points from the given list to the internal list.
            // use the actual points rather than creating new ones in case another
            // object has a reference to these points.
            if (parms.ContainsKey("points") == true)
                foreach (CPoint p in (List<CPoint>)parms["points"]) this.points.Add(p);
            if (parms.ContainsKey("text") == true) this.text = (String)parms["text"];
            if (parms.ContainsKey("textColor") == true) this.textColor = (Color)parms["textColor"];
            if (parms.ContainsKey("lineColor") == true) this.lineColor = (Color)parms["lineColor"];
            if (parms.ContainsKey("fillColor") == true) this.fillColor = (Color)parms["fillColor"];
            if (parms.ContainsKey("lineWidth") == true) this.lineWidth = (int)parms["lineWidth"];
//            if (parms.ContainsKey("lineStyle") == true) this.lineStyle = (DashStyle)parms["lineStyle"];
            if (parms.ContainsKey("fontFace") == true) this.fontFace = (String)parms["fontFace"];
            if (parms.ContainsKey("fontSize") == true) this.fontSize = (float)parms["fontSize"];
            if (parms.ContainsKey("fontSlant") == true) this.fontSlant = (FontSlant)parms["fontSlant"];
            if (parms.ContainsKey("fontWeight") == true) this.fontWeight = (FontWeight)parms["fontWeight"];
            if (parms.ContainsKey("visible") == true) this.Visible = (Boolean)parms["visible"];
            if (parms.ContainsKey("draggable") == true) this.Draggable = (Boolean)parms["draggable"];
            if (parms.ContainsKey("sizable") == true) this.Sizable = (Boolean)parms["sizable"];
            if (parms.ContainsKey("selectable") == true) this.Selectable = (Boolean)parms["selectable"];
            if (parms.ContainsKey("connectable") == true) this.Connectable = (Boolean)parms["connectable"];

            this.center = new CPoint(0.0, 0.0);
            this.UpdateBoundingBox();
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public bool IsFactory
		{	// Returns true if this block is a factory block
			get {
				return _isFactory;
			}
		}
		
        // - - - String representation of a shape is its name - - - - - - -
        public override string ToString()
        {
            return this.name;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public System.Object Tag
        {
            get { return this.tag; }
            set { this.tag = value; }
        }

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual bool Visible
        {
            get { return this.visible; }
            set { this.visible = value; }
        }
		
		// - - - Create the context menu for this shape - - - - - - - - - -
		protected virtual bool ShowContextMenu(Canvas cvs, int X, int Y) 
		{
			// Cache info
			_cvs = cvs;
			_X = X;
			_Y = Y;
			
			// Create and show context menu
			Gtk.Menu mnu = new Gtk.Menu();
			
			Gtk.MenuItem mnuDelShape = new Gtk.MenuItem("Delete");
			mnuDelShape.Activated += OnDeleteShape;

			Gtk.MenuItem mnuToFront = new Gtk.MenuItem("Bring to Front");
			mnuToFront.Activated += OnBringToFront;
			
			Gtk.MenuItem mnuToBack = new Gtk.MenuItem("Send to Back");
			mnuToBack.Activated += OnSendToBack;
			
			Gtk.MenuItem mnuProps = new Gtk.MenuItem("Properties");
//			mnuProps.Activated += OnPropertiesShow;
			
			mnu.Append(mnuDelShape);
			mnu.Append( new Gtk.SeparatorMenuItem() );
			mnu.Append(mnuToFront);
			mnu.Append(mnuToBack);
			mnu.Append( new Gtk.SeparatorMenuItem() );
			mnu.Append(mnuProps);
			
			mnu.ShowAll();
			mnu.Popup();
			
			return true;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		protected virtual void OnPropertiesShow(object sender, EventArgs e)
//		{
//			_cvs.ShowPropertiesWindow();
//			_cvs = null;
//		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected virtual void OnDeleteShape(object sender, EventArgs e)
		{
			_cvs.DeleteShape(this);
			_cvs.Invalidate();
			_cvs = null;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected virtual void OnBringToFront(object sender, EventArgs e)
		{
			this.BringToFront(_cvs);
			_cvs.Invalidate();
			_cvs = null;		
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected virtual void OnSendToBack(object sender, EventArgs e)
		{
			this.SendToBack(_cvs);
			_cvs.Invalidate();
			_cvs = null;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void BringToFront(Canvas cvs) 
		{
			cvs.BringToFront(this);	
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void SendToBack(Canvas cvs) 
		{
			cvs.SendToBack(this);
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Create and return a factory instance for a CShape.
        // Should be overridden by subclasses.
        public static ICanvasEventHandler CreateFactory()
        {
            return null;
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// A utility function that makes a new instance of this CShape object
		// At the given location. Override to copy specific subclass properties.		
		public virtual CShape Clone(double X, double Y) 
		{	// Clone this shape. Optionally clone edges.
			System.Object[] args = new System.Object[] {X, Y};
			CShape clone = (CShape)Activator.CreateInstance(this.GetType(), args);
			
			clone.Text = this.Text;
			clone.TextColor = this.TextColor;
			clone.FillColor = this.FillColor;
			clone.LineWidth = this.LineWidth;
			clone.LineColor = this.LineColor;
			clone.DashStyle = this.DashStyle;
			clone.Width = this.Width;
			clone.Height = this.Height;
			return clone;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public virtual void OnDrop(Canvas cvs)
		{	// Implement drop behavior.
			CShape dropped = null;
			
			// If not a factory shape, just move the shape
			if (!this.IsFactory) {
				this.MatchOutline(cvs);
				dropped = this;
			} else {
				// When a factory object is dropped, create a new instance.
				dropped = this.Clone(this.Outline.Left, this.Outline.Top);
				cvs.AddShape( dropped );
			}
			
			// Deselect all shapes including factory and select dropped
			cvs.DeselectAll();
			dropped.Select(cvs);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public virtual void OnDrag(Canvas cvs)
		{	// Implement drag behavior.
			// Default is to translate the shape outline to the mouse.
			this.TranslateOutline(cvs);
		}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public void ClearEventHandlers()
        {	// Clear all delegates that have registed to handle this class's events
            this.MouseDown = null;
            this.MouseUp = null;
            this.Click = null;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - 
        public void RaiseMouseDown(Canvas cvs)
        {	// Allow external class to raise the MouseDown event
            if (MouseDown != null)
            {
                try
                {
                    ShapeEventArgs evargs = new ShapeEventArgs(this);
                    MouseDown(this, evargs);
                }
                catch (Exception ex)
                {
                    if (cvs != null) cvs.RaiseCanvasError(ex.Message);
                    Console.Error.WriteLine(ex.Message);
                }
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - 
        public void RaiseMouseUp(Canvas cvs)
        {	// Allow external class to raise the MouseUp event
            if (MouseUp != null)
            {
                try
                {
                    ShapeEventArgs evargs = new ShapeEventArgs(this);
                    MouseUp(this, evargs);
                }
                catch (Exception ex)
                {
                    if (cvs != null)
                    {
                        cvs.RaiseCanvasError(ex.Message);
                    }
                    Console.Error.WriteLine(ex.Message);
                }
            }
        }

        // - - -  Allow external class to raise the Click event - - - - - - - - 
        public void RaiseClick(Canvas cvs)
        {
            if (Click != null)
            {
                try
                {
                    ShapeEventArgs evargs = new ShapeEventArgs(this);
                    Click(this, evargs);
                }
                catch (Exception ex)
                {
                    if (cvs != null)
                    {
                        cvs.RaiseCanvasError(ex.Message);
                    }
                    Console.Error.WriteLine(ex.Message);
                }
            }
        }

		// - - - Allow external class to raise the DoubleClick event - - - - - - - - - - - 
        public void RaiseDoubleClick(Canvas cvs)
        {
            if (Click != null)
            {
                try
                {
                    ShapeEventArgs evargs = new ShapeEventArgs(this);
                    DoubleClick(this, evargs);
                }
                catch (Exception ex)
                {
                    if (cvs != null)
                    {
                        cvs.RaiseCanvasError(ex.Message);
                    }
                    Console.Error.WriteLine(ex.Message);
                }
            }
        }

		// - - - Raise Transformed event - - - - - - - - - - - 
        public void RaiseTransformed(Canvas cvs)
        {
            if (Transformed != null)
            {
                try
                {
                    ShapeEventArgs evargs = new ShapeEventArgs(this);
                    Transformed(this, evargs);
                }
                catch (Exception ex)
                {
                    if (cvs != null)
                    {
                        cvs.RaiseCanvasError(ex.Message);
                    }
                    Console.Error.WriteLine(ex.Message);
                }
            }
        }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Center"), CategoryAttribute("Layout"), BrowsableAttribute(true)]
        public virtual Point Center
        {
            get { return new Point((int)this.center.X, (int)this.center.Y); }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Left"), CategoryAttribute("Layout"), BrowsableAttribute(true)]
        public virtual double Left
        {
            get { return this.left; }
            set
            {
                this.MorphTo(value, this.top,
                             value + this.width,
                             this.top + this.height);
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Top"), CategoryAttribute("Layout"), BrowsableAttribute(true)]
        public virtual double Top
        {
            get { return this.top; }
            set
            {
                this.MorphTo(this.left, value,
                             this.left + this.width,
                             value + this.height);
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Width"), CategoryAttribute("Layout"), BrowsableAttribute(true)]
        public virtual double Width
        {
            get { return this.width; }
            set
            {
                this.MorphTo(this.left, this.top,
                             this.left + value,
                             this.top + this.height);
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Height"), CategoryAttribute("Layout"), BrowsableAttribute(true)]
        public virtual double Height
        {
            get { return this.height; }
            set
            {
                this.MorphTo(this.left, this.top,
                             this.left + this.width,
                             this.top + value);
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Location"), CategoryAttribute("Layout"), BrowsableAttribute(true)]
        public virtual Point Location
        {
            get { return new Point((int)this.left, (int)this.top); }
            set
            {
                this.MorphTo(value.X, value.Y, 
                             value.X + this.width, 
                             value.Y + this.height);
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Shape name"), CategoryAttribute("Design"), BrowsableAttribute(true)]
        public virtual String Name
        {
            get { return this.name; }
            set { this.name = value; }
        }

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Font Face"), CategoryAttribute("Appearance"), BrowsableAttribute(true)]
        public virtual string FontFace
        {
            get { return this.fontFace; }
            set { this.fontFace = value; }
        }

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Font Face"), CategoryAttribute("Appearance"), BrowsableAttribute(true)]
        public virtual FontWeight FontWeight
        {
            get { return this.fontWeight; }
            set { this.fontWeight = value; }
        }

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Font Slant"), CategoryAttribute("Appearance"), BrowsableAttribute(true)]
        public virtual FontSlant FontSlant
        {
            get { return this.fontSlant; }
            set { this.fontSlant = value; }
        }

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Font Size"), CategoryAttribute("Appearance"), BrowsableAttribute(true)]
        public virtual double FontSize
        {
            get { return this.fontSize; }
            set { this.fontSize = value; }
        }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Shape text"), CategoryAttribute("Appearance"), BrowsableAttribute(true)]
        public virtual String Text
        {
            get { return this.text; }
            set { this.text = value; }
        }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Shape text color"), CategoryAttribute("Appearance"), BrowsableAttribute(true)]
        public virtual Color TextColor
        {
            get { return this.textColor; }
            set { this.textColor = value; }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Line color"), CategoryAttribute("Appearance"), BrowsableAttribute(true)]
        public Color LineColor
        {
            get { return this.lineColor; }
            set { this.lineColor = value; }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Line width"), CategoryAttribute("Appearance"), BrowsableAttribute(true)]
        public int LineWidth
        {
            get { return this.lineWidth; }
            set { this.lineWidth = value; }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Line style"), CategoryAttribute("Appearance"), BrowsableAttribute(true)]
        public double[] DashStyle
        {
            get { return this.dashStyle; }
            set { this.dashStyle = value; }
        }

//        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//        [DescriptionAttribute("Line style"), CategoryAttribute("Appearance"), BrowsableAttribute(true)]
//        public DashStyle LineStyle
//        {
//            get { return this.lineStyle; }
//            set { this.lineStyle = value; }
//        }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Brush color"), CategoryAttribute("Appearance"), BrowsableAttribute(true)]
        public virtual Color FillColor
        {
            get { return this.fillColor; }
            set { this.fillColor = value; }
        }

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//        [DescriptionAttribute("Horizontal text alignment"), CategoryAttribute("Appearance"), BrowsableAttribute(true)]
//        public Pango.Alignment AlignHorizontal
//        {
//            get { return this.horizontalAlign; }
//            set { this.horizontalAlign = value; }
//        }
//
//        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//        [DescriptionAttribute("Vertical text alignment"), CategoryAttribute("Appearance"), BrowsableAttribute(true)]
//        public Pango.Alignment AlignVertical
//        {
//            get { return this.verticalAlign; }
//            set { this.verticalAlign = value; }
//        }
		
//        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//        [DescriptionAttribute("Horizontal text alignment"), CategoryAttribute("Appearance"), BrowsableAttribute(true)]
//        public StringAlignment AlignHorizontal
//        {
//            get { return this.horizontalAlign; }
//            set { this.horizontalAlign = value; }
//        }
//
//        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//        [DescriptionAttribute("Vertical text alignment"), CategoryAttribute("Appearance"), BrowsableAttribute(true)]
//        public StringAlignment AlignVertical
//        {
//            get { return this.verticalAlign; }
//            set { this.verticalAlign = value; }
//        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void Draw(Cairo.Context g)
        {
            // Draw of this instance must be implemented by subclasses
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void DrawDecorators(Cairo.Context g)
        {	// Draw all CShape Decorators, if any
            foreach (CDecorator s in this.Decorators.Values) s.Draw(g);
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void MorphTo(CShape shp)
        {	// Morph the size and location of this shape to a bounding box as defined by two points.
            this.MorphTo(shp.left, shp.top, shp.left + shp.width, shp.top + shp.height);
        }

        public virtual void MorphTo(CPoint p1, CPoint p2)
        {	// p1 is the upper left and p2 is the lower right of the new bounding box.
            this.MorphTo(p1.X, p1.Y, p2.X, p2.Y);
        }

        public virtual void MorphTo(double x1, double y1, double x2, double y2)
        {
            // Calculate scale and translation factors.
            int npoints = this.points.Count;
            double xs = this.points[0].X;
            double ys = this.points[0].Y;
            double xe = this.points[npoints - 1].X;
            double ye = this.points[npoints - 1].Y;
            
            // Scale factors
            double dx1 = x2 - x1;
            double dx2 = xe - xs;
            double dy1 = y2 - y1;
            double dy2 = ye - ys;
            double sx = 1.0;
            double sy = 1.0;
            if (dx2 != 0.0) { sx = dx1/dx2; }
            if (dy2 != 0.0) { sy = dy1 / dy2; }

            // Translation factors
            double tx = x1 - xs;
            double ty = y1 - ys;

            // Transform all points in this shape
            this.Transform(xs, ys, tx, ty, sx, sy);
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public void Translate(CPoint delta)
        {
            Translate(delta, this);
        }
        
        public virtual void Translate(CPoint delta, CShape wrt)
        {
            // Simple x-y translation wrt (with respect to) the wrt shape
            
            // Get the upper left point of the wrt and this shape
            double x1 = wrt.left;
            double y1 = wrt.top;
            double x2 = this.left;
            double y2 = this.top;

            // Translate by delta.X, delta.Y
            for (int i = 0; i < this.points.Count; i++)
            {
                this.points[i].X = this.points[i].X - x2 + x1 + delta.X;
                this.points[i].Y = this.points[i].Y - y2 + y1 + delta.Y;
            }

            this.UpdateBoundingBox();
            this.UpdateHandles();
            this.UpdateDecorators();
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public void TranslateTo(CPoint pt)
        {	// Translate the upper left corner of this shape to the given point
            this.Translate(new CPoint(pt.X - this.left, pt.Y - this.top));
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public void TranslateCenterTo(CPoint pt)
        {	// Translate the center point of this shape to the given point
			
            // Translates this shape's center point to the given point.
            this.Translate(new CPoint(pt.X - this.center.X, pt.Y - this.center.Y));
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void Scale(double ax, double ay, double sx, double sy)
        {
            Scale(ax, ay, sx, sy, this);
        }

        public virtual void Scale(double ax, double ay, double sx, double sy, CShape wrt)
        {
            // Scale by factor (sx, sy) from anchor point (ax, ay) with respect to shape wrt.
            // Note: It is assumed that the wrt shape has the same number of points as this shape.

            // Translate anchor to origin.
            // Scale shape points.
            // Translate back to anchor.
            for (int i = 0; i < this.points.Count; i++) {
                this.points[i].X = (wrt.points[i].X - ax) * sx + ax;
                this.points[i].Y = (wrt.points[i].Y - ay) * sy + ay;
            }
            this.UpdateBoundingBox();
            this.UpdateHandles();
            this.UpdateDecorators();

            //        # Needs saving after any scaling
            //        self.cvs.Modified = True
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void Transform(double ax, double ay, double dx, double dy, double sx, double sy)
        {
            Transform(ax, ay, dx, dy, sx, sy, this);
        }

        public virtual void Transform(double ax, double ay, double dx, double dy, double sx, double sy, CShape wrt)
        {
            // Transform shape. A combined transform and scale.
            // Translate by dx, dy with respect to wrt
            // Scale by sx, sy with anchor point ax, ay, with respect to wrt.

            // Translate anchor to origin.
            // Scale shape points.
            // Translate back to anchor.
            for (int i=0; i<wrt.points.Count; i++)
            {
                this.points[i].X = (wrt.points[i].X - ax) * sx + ax + dx;
                this.points[i].Y = (wrt.points[i].Y - ay) * sy + ay + dy;
            }

            this.UpdateBoundingBox();
            this.UpdateHandles();
            this.UpdateDecorators();
			
			this.RaiseTransformed(_cvs);
			
            // Needs saving after a transformation
            //cvs.Modified = true;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public void UpdateHandles()
        {
            // Move all shape handles to bounding box corners
            foreach (CHandle h in this.handles.Values) h.ResetLocation();
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void UpdateBoundingBox()
        {	// Recalculate the bounding box for this shape as well as the center point
			
            // If not points ...
            if (this.points.Count == 0)
            {
                this.top = 0.0;
                this.left = 0.0;
                this.width = 0.0;
                this.height = 0.0;
                this.center.X = 0.0;
                this.center.Y = 0.0;
                return;
            }

            // Find the bounding box corners
            double minx, maxx, miny, maxy;

            CPoint p1 = this.points[0];
            minx = p1.X;
            maxx = p1.X;
            miny = p1.Y;
            maxy = p1.Y;

            for (int i = 1; i < points.Count; i++)
            {
                CPoint p = points[i];
                if (p.X > maxx) { maxx = p.X; }
                else if (p.X < minx) { minx = p.X; }
                if (p.Y > maxy) { maxy = p.Y; }
                else if (p.Y < miny) { miny = p.Y; }
            }

            // Save Top, Left, Width and Height
            this.top = miny;
            this.left = minx;
            this.width = maxx - minx;
            this.height = maxy - miny;

            // Save Center X and Y
            this.center.X = 0.5 * (maxx + minx);
            this.center.Y = 0.5 * (maxy + miny);
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public void UpdateDecorators()
        {	// Update the location of all decorator objects
            foreach (CDecorator s in this.Decorators.Values) s.Update(this);
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual CShape HitShape(CPoint pt, Canvas cvs)
        {	// If this shape is hit, return it.
        	// Can also return a subordinate shape that makes up a composite.
			
            // Return the shape hit by given Point.
            // Simple behavior is to use ContainsPoint.
            // More complex overridden bahavior may return a
            // part of a composite shape.
            //if (this.ContainsPoint(pt, cvs) == true) return this;
			if (this.ContainsPoint(pt, cvs) && this.visible) return this;
			return null;
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public virtual void OnScroll(Canvas cvs, Gdk.EventScroll e)
		{	// Default behavior is to hand event back to canvas
			cvs.OnScroll(cvs, e);
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnMouseDown(Canvas cvs, MouseEventArgs e)
        {	// Handle mouse down event
			
            // If the canvas is in the editing state
            if (cvs.Mode == EMode.Editing)
            {
                // If the shape is not selected and the shift key is not down deselect all shapes
                //if (this.Selected == false && Control.ModifierKeys != Keys.Shift) cvs.DeselectAll();
				if (this.Selected == false && (cvs.ModifierKeys & Gdk.ModifierType.ShiftMask) == 0) 
					cvs.DeselectAll();

                // Select this shape
				// Indicate that the canvas selection has changed
                this.Select(cvs);
                cvs.RaiseSelectionChangedEvent();
				
				// Intercept the right-mouse and show inspector window
				if (e.Button == Diagram.MouseButtons.Right && this.IsFactory == false) 
				{
					this.ShowContextMenu(cvs, (int)e.X, (int)e.Y);
					
					// Reset event handler. We are done.
					cvs.handler = cvs;
					
				} else {
	            
	                // Change the canvas state to "drag left start"
	                if (this.Draggable == true) cvs.EditMode = Diagram.EMode.DragLeftStart;
					
					// Do not reset event handler. May be doing more.
				}
				
                // Redraw
                cvs.Invalidate();
            }
        }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnMouseMove(Canvas cvs, MouseEventArgs e)
        {	// Handle mouse move event
			
            // Editing shapes
            if (cvs.Mode == EMode.Editing)
            {
                // Start dragging
                if (cvs.EditMode == EMode.DragLeftStart)
                {
                    // Mouse moved by at least one grid point
                    if (cvs.mouseGrid.X != cvs.mouseDownGrid.X 
                        || cvs.mouseGrid.Y != cvs.mouseDownGrid.Y)
                    {
                        // Begin left dragging
                        //cvs.Capture = true;
                        //Gdk.Pointer.Grab();
						
                        // Draw new outline box for each selected shape and connector
                        foreach (CShape s in cvs.SelectedShapesAndConnectors()) s.StartOutline(cvs);

                        // Change state
                        cvs.EditMode = EMode.DragLeft;
                    }
                }
                // Continue dragging
                else if (cvs.EditMode == EMode.DragLeft)
                {
                    // Move all shape outlines
                    foreach (CShape s in cvs.SelectedShapesAndConnectors())
                    {
                        if (s.Draggable == true) s.OnDrag(cvs); //s.Outline.Translate(delta, s);
                    }
                }
            }

            // Redraw
            cvs.Invalidate();
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnMouseUp(Canvas cvs, MouseEventArgs e)
        {	// Handle mouse up event
            // If editing canvas ...
            if (cvs.Mode == EMode.Editing)
            {
                // If capturing, then stop
                //if (cvs.Capture == true) cvs.Capture = false;
				//if (Gdk.Pointer.IsGrabbed) Gdk.Pointer.Ungrab();

                // If starting to drag this shape ...
                if (cvs.EditMode == EMode.DragLeftStart)
                { // do nothing because mouse didn't actually move a significant distance
                }

                // If currently dragging ...
                else if (cvs.EditMode == EMode.DragLeft)
                {
                    // Get selected, draggable shapes
                    List<CShape> shps = cvs.SelectedShapesAndConnectors();
					
					// Perform DoDrop behavior for selected shapes
                    foreach (CShape s in shps) s.OnDrop(cvs);
					
                    // Move selected shapes to their outline locations
                    //foreach (CShape s in shps) s.MatchOutline(cvs);

                    // Erase outlines
                    foreach (CShape s in shps) s.StopOutline(cvs);

                    // Raise the shapes events
                    cvs.RaiseShapesMovedEvent(shps);
					cvs.RaiseSelectionChangedEvent();
                }

                // Move out of the dragging state back to editing
                cvs.BeginEditing();

                // Redraw
                cvs.Invalidate();
            }
        }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnDoubleClick(Canvas cvs, MouseEventArgs e)
        {	// Handle double click event
			
            // If the canvas is in the editing state
            if (cvs.Mode == EMode.Editing)
            {
                // If the shape is not selected and the shift key is not down deselect all shapes
                //if (this.Selected == false && Control.ModifierKeys != Keys.Shift) cvs.DeselectAll();
				if (this.Selected == false && (cvs.ModifierKeys & Gdk.ModifierType.ShiftMask) == 0) 
					cvs.DeselectAll();

                // Select this shape
                this.Select(cvs);
				
				// If double-clicked on a non-factory block, show View Window
//				if (this.IsFactory == false) cvs.ShowPropertiesWindow();
				
                // Indicate that the canvas selection has changed
                cvs.RaiseSelectionChangedEvent();
				
				// Reset handler
				cvs.handler = cvs;
				
                // Redraw
                cvs.Invalidate();
            }
        }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void CancelProcess(Canvas cvs)
        {
            // Cancel any dragging that has been started.

            // If capturing, then stop
            //if (cvs.Capture == true) cvs.Capture = false;
			//if (Gdk.Pointer.IsGrabbed) Gdk.Pointer.Ungrab();

            // If editing shapes
            if (cvs.Mode == EMode.Editing)
            {
                // and if currently dragging ...
                if (cvs.EditMode == EMode.DragLeft)
                {
                    // Get selected, draggable shapes
                    List<CShape> shps = cvs.SelectedShapesAndConnectors();

                    // Erase outlines
                    foreach (CShape s in shps) s.StopOutline(cvs);
                }
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Read selected property
        public Boolean Selected
        {
            // NOTE: An alternative to the selected property is to look to see if handles is not null
            get { return this.selected; }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void Select(Canvas cvs)
        {
            // Select the current shape.
            // This includes creating handle shapes for the object
            // and placing the handles on the annotation layer.
            
            if (!this.Selectable) return;   // If not selectable, return
            if (!this.Visible) return;      // If not visible, return
            if (this.selected) return;      // If already selected, return
            
            // Create size handles.
            double x1 = this.left;
            double y1 = this.top;
            double x2 = x1 + this.width;
            double y2 = y1 + this.height;
            double xm = this.center.X;
            double ym = this.center.Y;
            
            CSizeHandle h1 = new CSizeHandle(new CPoint(x1, y1), this, "upper-left",   true,  true);
            CSizeHandle h2 = new CSizeHandle(new CPoint(x1, y2), this, "lower-left",   true,  true);
            CSizeHandle h3 = new CSizeHandle(new CPoint(x2, y1), this, "upper-right",  true,  true);
            CSizeHandle h4 = new CSizeHandle(new CPoint(x2, y2), this, "lower-right",  true,  true);
            CSizeHandle h5 = new CSizeHandle(new CPoint(x1, ym), this, "left-middle",  true,  false);
            CSizeHandle h6 = new CSizeHandle(new CPoint(x2, ym), this, "right-middle", true,  false);
            CSizeHandle h7 = new CSizeHandle(new CPoint(xm, y1), this, "upper-center", false, true);
            CSizeHandle h8 = new CSizeHandle(new CPoint(xm, y2), this, "lower-center", false, true);
            
            //# Set opposite anchor handle for each size handle
            //# Note: this creates self references that must be cleared when deselecting
            h1.Anchor = h4;
            h2.Anchor = h3;
            h3.Anchor = h2;
            h4.Anchor = h1;
            h5.Anchor = h6;
            h6.Anchor = h5;
            h7.Anchor = h8;
            h8.Anchor = h7;
            
            //# Update shape handles
            this.handles.Clear();
            this.handles[h1.Name] = h1;
            this.handles[h2.Name] = h2;
            this.handles[h3.Name] = h3;
            this.handles[h4.Name] = h4;
            this.handles[h5.Name] = h5;
            this.handles[h6.Name] = h6;
            this.handles[h7.Name] = h7;
            this.handles[h8.Name] = h8;

            // Add handles to the canvas annotation layer
            foreach (CHandle h in this.handles.Values)
            {
                cvs.AddAnnotation(h);
            }

            //# The shape is now selected
            this.selected = true;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Deselect the shape
        public void Deselect(Canvas cvs)
        {
            //# Deselect the current shape
            foreach (CHandle h in this.handles.Values)
            {
                h.ClearRefs();              // Clear internal object references. (Is this necessary?)
                cvs.DeleteAnnotation(h);    // Remove handle from annotation layer
            }
            this.handles.Clear();           // Clear handles dictionary
            this.selected = false;          // Set shape to unselected
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Disconnect all connectors that are connected to this shape
        public virtual void Disconnect()
        {
            // If no connectors, return
            if (this.Connectors.Count == 0) return;

            // Make a copy so can modify collection in loop
            CConnector[] tmp = new CConnector[this.Connectors.Count];
            this.Connectors.CopyTo(tmp);

            foreach (CConnector c in tmp)
            {
                if (c.begin == this)
                {
                    c.DisconnectBegin();
                }
                else if (c.end == this)
                {
                    c.DisconnectEnd();
                }
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Returns true if given point is within this shape.
        // This generic implementation just looks within bounding box.
        // Can be overridden for improved implementation.
        public virtual Boolean ContainsPoint(CPoint pnt, Canvas cvs)
        {
			return this.ContainsPoint(pnt.X, pnt.Y, cvs);
		}
		
        public virtual Boolean ContainsPoint(double X, double Y, Canvas cvs)
        {
            if (this.Visible == false) return false;
			
			// if absolute positioning, convert point back to absolute coordinates
			//if (this.Dock == DockSide.Left) cvs.InverseTransformPoint(X, Y, out X, out Y);
			
			// If docked, undo translate
//			if (this.Dock == DockSide.Left) {
//				X = X + cvs.offsetX;
//				Y = Y + cvs.offsetY;
//			}
			
            // If in bounding box, then true
            if (this.left <= X && this.top <= Y 
                && (this.left + this.width) >= X 
                && (this.top + this.height) >= Y)
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Return true if this shape contains given shape
        // This implementation uses bounding boxes.
        // Can be overridden for better performance.
        public virtual Boolean ContainsShape(CShape shp)
        {
            // This determines if the current shape contains the one passed.
            // Custom behavior is specific to shape.

            if (this.left <= shp.left && this.top <= shp.top 
                && (this.left + this.width) >= (shp.left + shp.width)
                && (this.top + this.height) >= (shp.top + shp.height))
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void StartOutline(Canvas cvs)
        {
            // Create the outlining shape to be drawn
            // Add the outline to the top of the canvas annotation list
			
			// If an outline is already started, do nothing
			if (this.Outline != null) return;
			
			this.Outline = this.Clone(this.left, this.top);
			this.Outline.TextColor = new Color(this.TextColor.R, this.TextColor.G, this.TextColor.B, 0.4); //  Color.FromArgb(100, this.TextColor);
			this.Outline.LineColor = new Color(this.LineColor.R, this.LineColor.G, this.LineColor.B, 0.4); //Color.FromArgb(100, this.LineColor);
			this.Outline.FillColor = new Color(this.FillColor.R, this.FillColor.G, this.FillColor.B, 0.4); //Color.FromArgb(100, this.FillColor);
			this.Outline.Visible = true;
			this.Outline.Draggable = false;
			this.Outline.Sizable = false;
			this.Outline.Selectable = false;
			this.Outline.Connectable = false;
			cvs.AddAnnotation(this.Outline);
			
			// Following is dotted outline option for dragging
//            List<CPoint> pts = new List<CPoint>() { 
//				new CPoint( this.left, this.top ), 
//				new CPoint( this.left + this.width, this.top + this.height ) 
//			};
//            this.Outline = new CRectangle(pts, "", Color.Black,
//			                              Color.Gray, 1, DashStyle.Dash,
//                                		  Color.FromArgb(100, this.FillColor), 
//			                              true, false, false, false, false);
//			
//			cvs.AddAnnotation(this.Outline);
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void StopOutline(Canvas cvs)
        {
            // Delete the outline, if it exists.
            try
            {
                cvs.DeleteAnnotation(this.Outline);
            }
            catch //(Exception e)
            {
                // do nothing
            }
            this.Outline = null;
        }

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public virtual void TranslateOutline(Canvas cvs)
		{	// Move the shape's outline by the amount dictated by mouse movement
						
	        // Calculate the amount the mouse has moved
            CPoint delta = new CPoint( cvs.mouseGrid.X-cvs.mouseDownGrid.X,
                                       cvs.mouseGrid.Y-cvs.mouseDownGrid.Y );
			this.Outline.Translate(delta, this);
		}

	    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void MatchOutline(Canvas cvs)
        {   // Move and size the shape to its own outline shape.
			
            if ( this.Outline == null) return;
            this.MorphTo(this.Outline);
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void EditPoints(Canvas cvs)
        {
            // If desired, can be overriden by subclasses
        }

        // - - - Nudge location of this shape by dx, dy - - - - - - - - - -
        public void Nudge(double dx, double dy)
        {
            foreach (CPoint p in this.points)
            {
                p.X += dx;
                p.Y += dy;
            }
            this.UpdateBoundingBox();
            this.UpdateHandles();
            this.UpdateDecorators();
        }

		// - - - Outputs shape as XML - - - - - - - - - -
        // CShape subclasses must provide a method that outputs an 
        // XML representation of itself to the given XmlWriter
        // Output XML representation of this shape.
        public virtual void ToXml(XmlWriter w)
        {
            w.WriteStartElement("shape");
            this.WriteXmlAttributes(w); 
            this.WriteXmlPoints(w);
			this.WriteXmlTags(w);
            //this.WriteXmlDecorators(w);
            w.WriteEndElement();
        }

        // - - - Override to write custom Xml content of a shape - - - - - - - - -
        protected virtual void WriteXmlTags(XmlWriter w)
        {
        }

        // - - - Write the base standard attributes shared by all shapes - - - - - - - -
        protected virtual void WriteXmlAttributes(XmlWriter w)
        {
            // Get object assembly and full name
            Type typ = this.GetType();
            String FullAsmName = typ.Assembly.FullName;
            String[] items = FullAsmName.Split(new String[] { "," }, StringSplitOptions.RemoveEmptyEntries);
            String AsmName = items[0];
            String FullName = typ.FullName;

            w.WriteAttributeString("id", this._id.ToString());
            //w.WriteAttributeString("typeName", this.GetType().Name);
            w.WriteAttributeString("typeName", String.Format("{0};{1}", AsmName, FullName));
            w.WriteAttributeString("name", this.Name);
            w.WriteAttributeString("text", this.Text);
            //w.WriteAttributeString("textColor", ColorTranslator.ToHtml(this.TextColor));
            //w.WriteAttributeString("lineColor", ColorTranslator.ToHtml(this.LineColor));
            w.WriteAttributeString("lineWidth", this.LineWidth.ToString());
            //w.WriteAttributeString("lineStyle", this.LineStyle.ToString());
            //w.WriteAttributeString("fillColor", ColorTranslator.ToHtml(this.FillColor));
            w.WriteAttributeString("fontFace", this.fontFace);
            w.WriteAttributeString("fontSize", this.fontSize.ToString());
            w.WriteAttributeString("fontSlant", this.fontSlant.ToString());
            w.WriteAttributeString("visible", this.Visible.ToString());
            w.WriteAttributeString("draggable", this.Draggable.ToString());
            w.WriteAttributeString("sizable", this.Sizable.ToString());
            w.WriteAttributeString("selectable", this.Selectable.ToString());
            w.WriteAttributeString("connectable", this.Connectable.ToString());
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Write the points in the shape to Xml writer
        protected virtual void WriteXmlPoints(XmlWriter w)
        {
            w.WriteStartElement("points");
            foreach (CPoint pnt in this.points) pnt.ToXML(w);
            w.WriteEndElement();
        }

        // - - - Write all decorators in the shape to Xml writer - - - - - - - -
        protected virtual void WriteXmlDecorators(XmlWriter w)
        {
            w.WriteStartElement("decorators");
            foreach (String k in this.Decorators.Keys)
            {
                this.Decorators[k].ToXml(w, k);
            }
            w.WriteEndElement();
        }
    }

    // --- Rectangle shape class -------------------------------------------
    public class CRectangle : CShape
    {
        // Constructors
        public CRectangle(List<CPoint> points) : base(points) { }

        public CRectangle(List<CPoint> points, Color lineColor, Color fillColor) :
            base(points, "", Colors.Black, lineColor, 1, fillColor,
                 true, true, true, true, true) { }

        public CRectangle(List<CPoint> points, String text, Color textColor, 
                            Color lineColor, int lineWidth, Color fillColor) :
            base(points, text, textColor, lineColor, lineWidth, fillColor,
                 true, true, true, true, true) { }

        public CRectangle(List<CPoint> points, String text, Color textColor,
                Color lineColor, int lineWidth, Color fillColor,
                Boolean visible, Boolean draggable, Boolean sizable, Boolean selectable, Boolean connectable) :
            base(points, text, textColor, lineColor, lineWidth, fillColor,
                 visible, draggable, sizable, selectable, connectable)
        { }
		
		public CRectangle(Double X, Double Y, Double W, Double H) : base(X, Y, W, H) { }
		
        // This constructor is primarily for drag and drop creation of shapes.
        public CRectangle(Double X, Double Y) : base(X, Y) { }

        // Constructor that uses a dictionary of initial values
        public CRectangle(Dictionary<String, System.Object> parms) : base(parms) { }

        // - - - Create and return a shape factory instance for a CRectangle - - - -
        public static new ICanvasEventHandler CreateFactory()
        {
            return new CShapeFactory(typeof(CRectangle));
        }

        // - - - Draw the rectangle shape on the canvas - - - - -
        public override void Draw(Cairo.Context g)
        {
            // Cannot draw a rectangle with negative width or height, 
            // so use bounding box points to draw
            double x = this.left;
            double y = this.top;
			
			//if (this.Dock == DockSide.Left) g.InverseTransformPoint(ref x, ref y);
			
            double w = this.width;
            double h = this.height;
			double cx = x + 0.5*w;
			double cy = y + 0.5*h;
			
			g.Save();
			
			// Path
			g.MoveTo(x, y);
			g.LineTo(x+w, y);
			g.LineTo(x+w, y+h);
			g.LineTo(x, y+h);
			g.LineTo(x, y);
			g.ClosePath();

			// Fill
			g.Color = this.FillColor;
			g.FillPreserve();
			
			// Stroke
			g.Color = this.LineColor;
			if (this.DashStyle != null) g.SetDash( this.DashStyle, 0.0 );
			g.LineWidth = this.LineWidth;
			g.Stroke();
			
            // Draw text
            if (this.Text.Length > 0)
            {
				g.Color = this.TextColor;
				g.SelectFontFace(this.fontFace, this.fontSlant, this.fontWeight);
				g.SetFontSize(this.fontSize);
				TextExtents te = g.TextExtents(this.Text);
				g.MoveTo(cx - 0.5*te.Width - te.XBearing, cy - 0.5*te.Height - te.YBearing); 
				g.ShowText(this.Text);
				
//				Pango.Layout layout = Pango.CairoHelper.CreateLayout(g);
//				Pango.FontDescription desc = Pango.FontDescription.FromString(
//						   String.Format("{0} {1} {2}", this.fontFace, this.fontWeight, this.fontSize));
//				layout.FontDescription = desc;
//				layout.SetText(text);
//				layout.Alignment = Pango.Alignment.Center;
//				int layoutWidth, layoutHeight;
//				layout.GetSize(out layoutWidth, out layoutHeight);
//				double teHeight = (double)layoutHeight / Pango.Scale.PangoScale; 
//				double teWidth = (double)layoutWidth / Pango.Scale.PangoScale;
//				g.MoveTo(cx - 0.5*teWidth, cy - 0.5*teHeight); 
//				Pango.CairoHelper.ShowLayout(g, layout);
            }

			// Finally, draw any shape decorator shapes
            this.DrawDecorators(g);
			
			g.Restore();
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void MorphTo(double x1, double y1, double x2, double y2)
        {
            // Get bounding box
            double minx, miny, maxx, maxy;
            if (x1 < x2)
            {
                minx = x1;
                maxx = x2;
            }
            else
            {
                maxx = x1;
                minx = x2;
            }

            if (y1 < y2)
            {
                miny = y1;
                maxy = y2;
            }
            else
            {
                maxy = y1;
                miny = y2;
            }

            // Reassign
            this.points[0].X = minx;
            this.points[0].Y = miny;
            this.points[1].X = maxx;
            this.points[1].Y = maxy;

            this.UpdateBoundingBox();
            this.UpdateHandles();
            this.UpdateDecorators();
        }
    }

    // -----------------------------------------------------------------------
    // Ellipse shape class
    public class CEllipse : CShape
    {
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Constructors
        public CEllipse(List<CPoint> points) : base(points) { }

        public CEllipse(List<CPoint> points, Color lineColor, Color fillColor) :
            base(points, "", Colors.Black, lineColor, 1, fillColor,
                  true, true, true, true, true) { }

        public CEllipse(List<CPoint> points, String text, Color textColor, Color lineColor, int lineWidth, Color fillColor) :
            base(points, text, textColor, lineColor, lineWidth, fillColor,
                 true, true, true, true, true) { }

        public CEllipse(List<CPoint> points, String text, Color textColor,
                Color lineColor, int lineWidth, Color fillColor,
                Boolean visible, Boolean draggable, Boolean sizable, Boolean selectable, Boolean connectable) :
            base(points, text, textColor, lineColor, lineWidth, fillColor,
                  visible, draggable, sizable, selectable, connectable) { }

        public CEllipse(List<CPoint> points, String text, Color textColor,
                Color lineColor, int lineWidth, Color fillColor, String fontDescription,
                Boolean visible, Boolean draggable, Boolean sizable, Boolean selectable, Boolean connectable) :
            base(points, text, textColor, lineColor, lineWidth, fillColor,
                 visible, draggable, sizable, selectable, connectable) { }
		
		public CEllipse(double X, double Y, double W, double H) : base(X, Y, W, H) { }
		
        // This constructor is primarily for drag and drop creation of shapes.
        public CEllipse(Double X, Double Y) : base(X, Y) { }

        // Constructor that uses a dictionary of initial values
        public CEllipse(Dictionary<String, System.Object> parms) : base(parms) { }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Create and return a shape factory instance for a CEllipse.
        public static new ICanvasEventHandler CreateFactory()
        {
            return new CShapeFactory(typeof(CEllipse));
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Draw the ellipse shape on the canvas
        public override void Draw(Cairo.Context g)
        {
            // Note that an ellipse can have negative width or height and draw correctly.
            // The CBoundingBox is not strictly necessary to reorient points.
            double x = this.left;
            double y = this.top;
			
            double w  = this.width;
            double h  = this.height;
			double hw = 0.5*w;
			double hh = 0.5*h;
			double cx = x + hw;
			double cy = y + hh;
			
			g.Save();
			
			// Path
			g.Translate(cx, cy);
			g.Scale(1.0, h/w);
			g.MoveTo(hw, 0.0);
			g.Arc(0.0, 0.0, hw, 0.0, 2.0 * Math.PI);
			g.ClosePath();
			
			// Fill
			g.Color = this.FillColor;
			g.FillPreserve();
			
			// Stroke
			g.Color = this.LineColor;
			//p.DashStyle = this.LineStyle;
			g.LineWidth = this.LineWidth;
			g.Stroke();

			// Draw text
            if (this.Text.Length > 0)
            {
				g.Color = this.TextColor;
				g.SelectFontFace(this.fontFace, this.fontSlant, this.fontWeight);
				g.SetFontSize(this.fontSize);
				TextExtents te = g.TextExtents(this.Text);
				g.MoveTo(cx - 0.5*te.Width - te.XBearing, cy - 0.5*te.Height - te.YBearing); 
				g.ShowText(this.Text);
            }

            // Finally, draw any shape decorator shapes
            this.DrawDecorators(g);
			
			// Must return to uniform device space before stroking in order to prevent 
			// lines from being deformed by scaling.
			g.Restore();
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override Boolean ContainsPoint(CPoint pnt, Canvas cvs)
        {	// Returns true if given point is within this shape.
			
            // The equation of an ellipse is defined as:
            //         2           2
            // (x - x0)    (y - y0)
            // --------  + -------- = 1
            //     2           2
            //    a           b
            //
            // where (x0, y0) is the ellipse center point,
            // 'a' is the half-width of the ellipse
            // and 'b' is the half-height of the ellipse.
            // We can determine if a point is within the ellipse by
            // substituting for the center points (x0, y0),
            // the half-width (a) and half-height (b),
            // and the given mouse location (x, y),
            // and determining if the value of the equation's right-hand side
            // is less than or equal to 1.  If so, the point is within the
            // ellipse.  If not, the point is outside the ellipse.
            //
            // The parametric form of the equation is:
            // x = a*cos(t), y=b*sin(t), where t=0 to 2Pi
            // This is a way to determine where a line starting from the center
            // of an ellipse, intersects the ellipse's boundary.
            // Given a, b, and the angle that the line makes with the x-axis (t/2?),
            // we can calculate x and y.

            // If not visible, return false
            if (this.Visible == false) return false;

            double x = pnt.X;
            double y = pnt.Y;
			
			// if absolute positioning, convert point back to absolute coordinates
			//if (this.Dock == DockSide.Left) cvs.InverseTransformPoint(x, y, out x, out y);
			
            double x0 = this.center.X;
            double y0 = this.center.Y;
            double a = 0.5 * this.width;
            double b = 0.5 * this.height;
            double rhs = (x-x0)*(x-x0)/(a*a) + (y-y0)*(y-y0)/(b*b);
            
            if (rhs <= 1.0) return true;
            else            return false;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void StartOutline(Canvas cvs)
        {
            // Create the outlining shape to be drawn
            // Add the outline to the top of the canvas annotation list
            List<CPoint> pts = new List<CPoint>();
            foreach (CPoint p in this.points) pts.Add( new CPoint(p.X, p.Y));

            this.Outline = new CEllipse(pts, "", Colors.Black, Colors.Gray, 1, Colors.Transparent,
                                         true, false, false, false, false);
            cvs.AddAnnotation(this.Outline);
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void MorphTo(double x1, double y1, double x2, double y2)
        {
            // Get bounding box
            double minx, miny, maxx, maxy;
            if (x1 < x2)
            {
                minx = x1;
                maxx = x2;
            }
            else
            {
                maxx = x1;
                minx = x2;
            }

            if (y1 < y2)
            {
                miny = y1;
                maxy = y2;
            }
            else
            {
                maxy = y1;
                miny = y2;
            }

            // Reassign
            this.points[0].X = minx;
            this.points[0].Y = miny;
            this.points[1].X = maxx;
            this.points[1].Y = maxy;

            this.UpdateBoundingBox();
            this.UpdateHandles();
            this.UpdateDecorators();
        }
    }

    // -----------------------------------------------------------------------
    public class CRoundedRectangle : CShape
    {	// Rounded Rectangle shape class
		
		// radius = (float)(0.2 * Math.Min(w, h));
		private double radius = 1.0; 
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		
        // Constructors
        public CRoundedRectangle(List<CPoint> points) : base(points) { }

        public CRoundedRectangle(List<CPoint> points, Color lineColor, Color fillColor) :
            base(points, "", Colors.Black, lineColor, 1, fillColor,
                 true, true, true, true, true) { }

        public CRoundedRectangle(List<CPoint> points, String text, Color textColor,
                                Color lineColor, int lineWidth, Color fillColor) :
            base(points, text, textColor, lineColor, lineWidth, fillColor,
                 true, true, true, true, true) { }

        public CRoundedRectangle(List<CPoint> points, String text, Color textColor,
                Color lineColor, int lineWidth, Color fillColor,
                Boolean visible, Boolean draggable, Boolean sizable, Boolean selectable, Boolean connectable) :
            base(points, text, textColor, lineColor, lineWidth, fillColor,
                 visible, draggable, sizable, selectable, connectable) { }

        public CRoundedRectangle(List<CPoint> points, String text, Color textColor,
                Color lineColor, int lineWidth, Color fillColor, String fontDescription,
                Boolean visible, Boolean draggable, Boolean sizable, Boolean selectable, Boolean connectable) :
            base(points, text, textColor, lineColor, lineWidth, fillColor,
                 visible, draggable, sizable, selectable, connectable) { }

        public CRoundedRectangle(double X, double Y, double W, double H) : base(X, Y, W, H) { }

        // This constructor is primarily for drag and drop creation of shapes.
        public CRoundedRectangle(Double X, Double Y) : base(X, Y) { }

        // Constructor that uses a dictionary of initial values
        public CRoundedRectangle(Dictionary<String, System.Object> parms) : base(parms) { }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public static new ICanvasEventHandler CreateFactory()
        {	// Create and return a shape factory instance for a CRoundedRectangle.
            return new CShapeFactory(typeof(CRoundedRectangle));
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public double Radius
        {	// Set radius of corners
            get { return this.radius; }
            set { this.radius = value; }
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override CShape Clone(double X, double Y) 
		{	// Override and set radius beyond base class behavior
			CRoundedRectangle clone = (CRoundedRectangle)base.Clone(X, Y);
			clone.Radius = this.Radius;
			return clone;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void Draw(Cairo.Context g)
        {	// Draw the rounded rectangle shape on the canvas
            // Note that an ellipse can have negative width or height and draw correctly.
            // The CBoundingBox is not strictly necessary to reorient points.
			double x = this.left;
			double y = this.top;
			
            double w = this.width;
            double h = this.height;
			double hh = 0.5*h;
			double hw = 0.5*w;
			double cx = x + hw;
			double cy = y + hh;
			double r = this.radius;
			double hpi = 0.5*Math.PI;
			
			if ( r > hh || r > hw ) r = Math.Min( hh, hw );

			g.Save();
			
			//if (this.Dock == DockSide.Left) g.Translate( -_cvs.offsetX, -_cvs.offsetY);
			//g.InverseTransformPoint(ref x, ref y);

			g.MoveTo( x, y+r );
			g.Arc(    x+r, y+r, r, Math.PI, -hpi );
			g.LineTo( x+w-r, y );
			g.Arc(    x+w-r, y+r, r, -hpi, 0.0 );
			g.LineTo( x+w, y+h-r );
			g.Arc(    x+w-r, y+h-r, r, 0.0, hpi);
			g.LineTo( x+r, y+h );
			g.Arc(    x+r, y+h-r, r, hpi, Math.PI );
			g.ClosePath();
			
			// Fill
			g.Color = this.FillColor;
			g.FillPreserve();
			
			// Stroke
			g.Color = this.LineColor;
			//g.DashStyle = this.LineStyle;
			g.LineWidth = this.LineWidth;
			g.Stroke();
			
            // Draw text
            if (this.Text.Length > 0)
            {
				g.Color = this.TextColor;
				g.SelectFontFace(this.fontFace, this.fontSlant, this.fontWeight);
				g.SetFontSize(this.fontSize);
				TextExtents te = g.TextExtents(this.Text);
				g.MoveTo(cx - 0.5*te.Width - te.XBearing, cy - 0.5*te.Height - te.YBearing); 
				g.ShowText(this.Text);
				
//				g.Color = this.TextColor;
//				Pango.Layout layout = Pango.CairoHelper.CreateLayout(g);
//				Pango.FontDescription desc = Pango.FontDescription.FromString(
//						   String.Format("{0} {1} {2}", this.fontFace, this.fontWeight, this.fontSize));
//				layout.FontDescription = desc;
//				layout.SetText(text);
//				layout.Alignment = Pango.Alignment.Center;
//				int layoutWidth, layoutHeight;
//				layout.GetSize(out layoutWidth, out layoutHeight);
//				double teHeight = (double)layoutHeight / Pango.Scale.PangoScale; 
//				double teWidth = (double)layoutWidth / Pango.Scale.PangoScale;
//				g.MoveTo(cx - 0.5*teWidth, cy - 0.5*teHeight); 
//				Pango.CairoHelper.ShowLayout(g, layout);
            }

            // Finally, draw any shape decorator shapes
            this.DrawDecorators(g);

			g.Restore();
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void MorphTo(double x1, double y1, double x2, double y2)
        {
            // Get bounding box
            double minx, miny, maxx, maxy;
            if (x1 < x2)
            {
                minx = x1;
                maxx = x2;
            }
            else
            {
                maxx = x1;
                minx = x2;
            }

            if (y1 < y2)
            {
                miny = y1;
                maxy = y2;
            }
            else
            {
                maxy = y1;
                miny = y2;
            }

            // Reassign
            this.points[0].X = minx;
            this.points[0].Y = miny;
            this.points[1].X = maxx;
            this.points[1].Y = maxy;

            this.UpdateBoundingBox();
            this.UpdateHandles();
            this.UpdateDecorators();
        }
    }
	
    // -----------------------------------------------------------------------
    public class CLine : CShape
    {	// line shape class
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		
        // Constructors
        public CLine(List<CPoint> points) : base(points) { }

        public CLine(List<CPoint> points, Color lineColor) :
            base(points, "", Colors.Black, lineColor, 1, Colors.Transparent,
                 true, true, true, true, true) { }

        public CLine(List<CPoint> points, String text, Color textColor, Color lineColor, int lineWidth) :
            base(points, text, textColor, lineColor, lineWidth, Colors.Transparent,
                 true, true, true, true, true) { }

        public CLine(List<CPoint> points, String text, Color textColor,
                 Color lineColor, int lineWidth,
                 Boolean visible, Boolean draggable, Boolean sizable, Boolean selectable, Boolean connectable) :
            base(points, text, textColor, lineColor, lineWidth, Colors.Transparent,
                 visible, draggable, sizable, selectable, connectable) { }

        // This constructor is primarily for drag and drop creation of shapes.
        public CLine(Double X, Double Y) : base(X, Y) { }

        // Constructor that uses a dictionary of initial values
        public CLine(Dictionary<String, System.Object> parms) : base(parms) { }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("X1"), CategoryAttribute("Layout"), BrowsableAttribute(true)]
        public virtual double X1
        {
            get { return this.points[0].X; }
            set
            { 
                this.points[0].X = value;
                this.UpdateBoundingBox();
                this.UpdateHandles();
                this.UpdateDecorators();
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("X2"), CategoryAttribute("Layout"), BrowsableAttribute(true)]
        public virtual double X2
        {
            get { return this.points[1].X; }
            set
            {
                this.points[1].X = value;
                this.UpdateBoundingBox();
                this.UpdateHandles();
                this.UpdateDecorators();
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Y1"), CategoryAttribute("Layout"), BrowsableAttribute(true)]
        public virtual double Y1
        {
            get { return this.points[0].Y; }
            set
            {
                this.points[0].Y = value;
                this.UpdateBoundingBox();
                this.UpdateHandles();
                this.UpdateDecorators();
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Y2"), CategoryAttribute("Layout"), BrowsableAttribute(true)]
        public virtual double Y2
        {
            get { return this.points[1].Y; }
            set
            {
                this.points[1].Y = value;
                this.UpdateBoundingBox();
                this.UpdateHandles();
                this.UpdateDecorators();
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Create and return a shape factory instance for a CLine.
        public static new ICanvasEventHandler CreateFactory()
        {
            return new CShapeFactory(typeof(CLine));
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Draw the line shape on the canvas
        public override void Draw(Cairo.Context g)
        {
            // Draw the line
            double x1 = this.points[0].X;
            double y1 = this.points[0].Y;
            double x2 = this.points[1].X;
            double y2 = this.points[1].Y;
			
			g.Save();
			
			g.MoveTo(x1, y1);
			g.LineTo(x2, y2);
			
			g.Restore();
			
			// Stroke
			g.Color = this.LineColor;
			//g.DashStyle = this.LineStyle;
			g.LineWidth = this.LineWidth;
			g.Stroke();
			
            // Draw text
            if (this.Text.Length > 0)
            {
				g.Color = this.TextColor;
				g.SelectFontFace(this.fontFace, this.fontSlant, this.fontWeight);
				g.SetFontSize(8.0);
				TextExtents te = g.TextExtents(this.Text);
				g.MoveTo(this.Left - te.Width/2.0 - te.XBearing, this.Top - te.Height - te.YBearing); 
				g.ShowText(this.Text);
            }

            // Finally, draw any shape decorator shapes
            this.DrawDecorators(g);
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void StartOutline(Canvas cvs)
        {
            // Create the outlining shape to be drawn
            // Add the outline to the top of the canvas annotation list
            List<CPoint> pts = new List<CPoint>();
            foreach (CPoint p in this.points) pts.Add( new CPoint(p.X, p.Y) );

            this.Outline = new CLine( pts, "", Colors.Black,
                                      Colors.Gray, 1,
                                      true, false, false, false, false);
            cvs.AddAnnotation(this.Outline);
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void MatchOutline(Canvas cvs)
        {
            // Special implementation of MatchOutline
            // Move line to match its own outline by updating coordinates of its points.
            if (this.Outline == null) return;
            CShape o = this.Outline;
            for (int i=0; i<this.points.Count; i++)
            {
                this.points[i].X = o.points[i].X;
                this.points[i].Y = o.points[i].Y;
            }
            this.UpdateBoundingBox();
            this.UpdateHandles();
            this.UpdateDecorators();
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Mark all points with CEditHandle handles so each point can be edited.
        public override void EditPoints(Canvas cvs)
        {
            // If not selectable, visible or if already selected, just return
            if (this.Selectable == false) return;
            if (this.Visible == false) return;
            if (this.Selected == true) this.Deselect(cvs);

            // Clear handles
            this.handles.Clear();

            // Create edit handles - one for each end point
            // Add to annotation layer and save a reference to underlying object
            int cnt = 0;
            foreach (CPoint p in this.points)
            {
                String name = cnt.ToString();
                CEditHandle h = new CEditHandle(p, this, name);
                this.handles[name] = h;             // Update handles dictionary
                cvs.AddAnnotation(h);               // Add handle to the annotation layer
                cnt++;
            }
            // The line is now selected
            this.selected = true;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override Boolean ContainsPoint(CPoint pt, Canvas cvs)
        {
            //Return True if given point hits this shape

            // Note: this method can be replaced with the more general version
            // implemented as the CSpline.ContainsPoint method
            
            // The distance defined by the points (x1, y1) and (x2, y2),
            // and a point at (x0, y0),
            // The distance from the point to the line is defined by:
            //
            //     |(x2-x1)(y1-y0) - (x1-x0)(y2-y1)|
            // d = -----------------------------------
            //               __________________
            //             /       2        2
            //          \/  (x2-x1) + (y2-y1)
            //
            // A point can be thought of as being sufficiently near a line if the
            // point is both within the line's bounding box,
            // and the distance from the point to a line is < ~2 pixels

            // If not visible can't be hit
            if (this.Visible == false) return false;

            double x = pt.X;
            double y = pt.Y;

			// if absolute positioning, convert point back to absolute coordinates
			//if (this.Dock == DockSide.Left) cvs.InverseTransformPoint(x, y, out x, out y);
			
            // Rule out the point if it is outside the bounding box
            double bx1 = this.left;
            double by1 = this.top;
            double bx2 = this.left + this.width;
            double by2 = this.top + this.height;
            if (x < bx1 || y < by1 || x > bx2 || y > by2) return false;
            
            // Now calculate the distance between the point and the line
            int npts = this.points.Count;
            double x1 = this.points[0].X; 
            double y1 = this.points[0].Y;
            double x2 = this.points[npts-1].X;
            double y2 = this.points[npts-1].Y;
            double dx = x2-x1;
            double dy = y2-y1;
            double denom = dx*dx + dy*dy;
            double d = 0.0;
            if (denom > 0)
                d = Math.Abs(dx * (y1 - y) - (x1 - x) * dy) / Math.Sqrt(denom);
            else
                d = 0.0;
            
            if (d > 2.0) return false;
            return true;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void MorphTo(double x1, double y1, double x2, double y2)
        {
            this.points[0].X = x1;
            this.points[0].Y = y1;
            this.points[1].X = x2;
            this.points[1].Y = y2;

            this.UpdateBoundingBox();
            this.UpdateHandles();
            this.UpdateDecorators();
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Selecting a line is actually the same as editing its points
        public override void Select(Canvas cvs)
        {
            this.EditPoints(cvs);
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Hide the CShape FillColor property because lines have no brush color
        [BrowsableAttribute(false)]
        public override Color FillColor
        {
            get { return Colors.Transparent; }
        }
    }

    // -----------------------------------------------------------------------
    // A spline shape
    public class CSpline : CShape
    {
        private double tension = 0.0;       	// Tension of the spline
		
		//private Cairo.Context _context = null;
		//private Cairo.Path _path = null;		// Local cache of most current path used for hit testing
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Constructors
        public CSpline(List<CPoint> points)
            : this(points, 0.5, Colors.Black) { }

        public CSpline(List<CPoint> points, double tension)
            : this(points, tension, Colors.Black) { }

        public CSpline(List<CPoint> points, double tension, Color lineColor)
            : this(points, tension, "", Colors.Black, lineColor, 1)
        { }

        public CSpline(List<CPoint> points, double tension, String text, 
		               Color textColor, Color lineColor, int lineWidth) :
            this(points, tension, text, textColor, lineColor, lineWidth,
                  true, true, true, true, true) { }

        public CSpline(List<CPoint> points, double tension, String text, 
		               Color textColor, Color lineColor, int lineWidth,
                	   Boolean visible, Boolean draggable, Boolean sizable, Boolean selectable, Boolean connectable) :
            base(points, text, textColor, lineColor, lineWidth, Colors.Transparent,
                 visible, draggable, sizable, selectable, connectable)
        {
            this.tension = tension;
        }

        // This constructor is primarily for drag and drop creation of shapes.
        public CSpline(Double X, Double Y) 
            : this( new List<CPoint>( new CPoint[] {
                new CPoint(X, Y), new CPoint(X+25.0, Y+50.0), 
                new CPoint(X+50.0, Y), new CPoint(X+75.0, Y+50.0)}))
        { }

        // Constructor that uses a dictionary of initial values
        public CSpline(Dictionary<String, System.Object> parms) : base(parms)
        {
            if (parms.ContainsKey("tension") == true)
            {
                this.tension = (double)parms["tension"];
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Create and return a shape factory instance for a CSpline.
        public static new ICanvasEventHandler CreateFactory()
        {
            return new CSplineFactory();
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Draw the line shape on the canvas
        public override void Draw(Cairo.Context g)
        {
			g.Save();
			CPoint pt = this.points[0];
			g.MoveTo(pt.X, pt.Y);
            for (int i=1; i< this.points.Count; i++)
            {
                pt = this.points[i];
				g.LineTo(pt.X, pt.Y);
				// TODO: What to use for control points?
				//http://stackoverflow.com/questions/2534786/drawing-a-clamped-uniform-cubic-b-spline-using-cairo
                // g.RelCurveTo( ...
            }
			g.Restore();
			
			// Copy path for possible later hit testing
			//this._context = g;
			//this._path = g.CopyPathFlat();
			
			g.Color = this.LineColor;
			g.LineWidth = this.LineWidth;
			g.Stroke();

            // Draw text
            if (this.Text.Length > 0)
            {
				g.Color = this.TextColor;
				g.SelectFontFace(this.fontFace, this.fontSlant, this.fontWeight);
				g.SetFontSize(this.fontSize);
				TextExtents te = g.TextExtents(this.Text);
				g.MoveTo(this.Left - 0.5*te.Width - te.XBearing, this.Top - 0.5*te.Height - te.YBearing); 
				g.ShowText(this.Text);
            }

            // Finally, draw any shape decorator shapes
            this.DrawDecorators(g);
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override Boolean ContainsPoint(CPoint pnt, Canvas cvs)
        {
            // If not visible can't be hit
            if (this.Visible == false) return false;
			
			// http://cairographics.org/hittestpython/
//			this._context.AppendPath(this._path);
//			return this._context.InStroke(pnt.X, pnt.Y);

			// if absolute positioning, convert point back to absolute coordinates
			double x = pnt.X;
			double y = pnt.Y;
			//if (this.Dock == DockSide.Left) cvs.InverseTransformPoint(x, y, out x, out y);
			
			using(ImageSurface draw = new ImageSurface(Format.A1, 1000, 1000)) {
				using (Context g = new Context(draw)) {
					
					CPoint pt = this.points[0];
					g.MoveTo(pt.X, pt.Y);
		            for (int i=1; i< this.points.Count; i++) {
		                pt = this.points[i];
						g.LineTo(pt.X, pt.Y);
		            }
					
					g.LineWidth = this.LineWidth+2000;
					g.Color = Colors.Black;
					g.Stroke();
					
					bool rslt = g.InStroke(x, y);
					return rslt;
				}
			}

//            //Return True if given point hits this curve
//            // This seems very odd to me, but apparently it is the standard way to do it.
//            // See http://www.bobpowell.net/hittestlines.htm
//            
//            // If not visible can't be hit
//            if (this.Visible == false) return false;
//
//            Pen p = new Pen(this.LineColor, this.LineWidth+3);
//            p.DashStyle = DashStyle.Solid; // this.LineStyle;   // Always use solid for hit detection
//
//            // Copy and scale all points, and create an Array object
//            double z  = cvs.Zoom;
//            int npts = this.points.Count;
//            PointF[] pts = new PointF[npts];
//            for (int i=0; i<npts; i++)
//            {
//                pts[i] = new PointF( (float)(z*this.points[i].X), (float)(z*this.points[i].Y) );
//            }
//
//            // Create a GraphicsPath
//            GraphicsPath pth = new GraphicsPath();
//            pth.AddCurve(pts, (float)this.tension);
//            pth.Widen(p);
//            p.Dispose();
//
//            // Use the GraphicsPath IsVisible method to see if the point hits the curve.
//            if (pth.IsVisible(new PointF((float)(z * pnt.X), (float)(z * pnt.Y)))) return true;
//            return false;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        [DescriptionAttribute("Tension"), CategoryAttribute("Appearance"), BrowsableAttribute(true)]
        public virtual Double Tension
        {
            get { return this.tension; }
            set { this.tension = value; }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void StartOutline(Canvas cvs)
        {
            // Create the outlining shape to be drawn
            // Add the outline to the top of the canvas shape list
            List<CPoint> pts = new List<CPoint>();          // Copy points
            foreach (CPoint p in this.points) pts.Add( new CPoint(p.X, p.Y));
            
            this.Outline = new CSpline( pts, this.tension, "", Colors.Black,
                                        Colors.Gray, 1, 
                                        true, false, false, false, false);
            cvs.AddAnnotation(this.Outline);
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void MatchOutline(Canvas cvs)
        {
            // Special implementation of MatchOutline
            // Move line to match its own outline by updating coordinates of its points.
            if (this.Outline == null) return;

            CShape o = this.Outline;
            for (int i=0; i<this.points.Count; i++)
            {
                this.points[i].X = o.points[i].X; 
                this.points[i].Y = o.points[i].Y;
            }

            this.UpdateBoundingBox();
            this.UpdateHandles();
            this.UpdateDecorators();
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Mark all points with CEditHandle handles so each point can be edited.
        public override void EditPoints(Canvas cvs)
        {
            // If not selectable, visible or if already selected, just return
            if (this.Selectable == false) return;
            if (this.Visible == false) return;
            if (this.Selected == true) this.Deselect(cvs);

            // Clear handles
            this.handles.Clear();
            
            // Create edit handles - one for each end point
            // Add to annotation layer and save a reference to underlying object
            int cnt = 0;
            foreach (CPoint p in this.points)
            {
                String name = cnt.ToString();
                CEditHandle h = new CEditHandle(p, this, name);
                this.handles[name] = h;             // Update handles dictionary
                cvs.AddAnnotation(h);               // Add handle to the annotation layer
                cnt ++;
            }
            // The line is now selected
            this.selected = true;
        }


        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Hide the CShape FillColor property because splines have no brush color
        [BrowsableAttribute(false)]
        public override Color FillColor
        {
            get { return Colors.Transparent; }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        protected override void WriteXmlAttributes(XmlWriter w)
        {
            w.WriteAttributeString("tension", this.tension.ToString());
        }
    }

    // -----------------------------------------------------------------------
    // A wire class that connects shapes
    public class CConnector : CSpline
    {
        internal CShape begin;       // Reference to beginning shape
        internal CShape end;         // Reference to ending shape

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Constructors with both begin and end connected shapes
        // Use null when no begin and/or end
        public CConnector(CShape begin, CShape end) : this(begin, end, new List<CPoint>()) { }

        public CConnector(CShape begin, CShape end, List<CPoint> points)
            : this( begin, end, points, 0.5, "", Colors.Black, Colors.Black, 1,
                    true, true, true, true)
        { }

        public CConnector(CShape begin, CShape end, List<CPoint> points, double tension, String text, Color textColor,
                        Color lineColor, int lineWidth, 
                        Boolean visible, Boolean draggable, Boolean sizable, Boolean selectable)
            : base(new List<CPoint>(new CPoint[] { new CPoint(0.0,0.0), new CPoint(0.0,0.0) }),
                                    tension, text, textColor, lineColor, lineWidth,
                                    visible, draggable, sizable, selectable, false)
        {   // Add all internal points
            int count = 1;
            foreach (CPoint p in points)
            {
                this.points.Insert(count, p);
                count++;
            }

            // Save ref to begin and end shapes, if provided.
            // If not provided, delete temp point added in base constructor.
            if (begin != null)
            {
                this.ConnectBegin(begin);
            }
            else
            {
                this.points.RemoveAt(0);
            }
            if (end != null)
            {
                this.ConnectEnd(end);
            }
            else
            {
                this.points.RemoveAt(this.points.Count - 1);
            }

            this.UpdateBoundingBox();
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Constructors with begin connected shape and additional points
        public CConnector(CShape begin, CPoint pnt)
            : this(begin, null, new List<CPoint>(new CPoint[] {pnt}),
                   0.5, "", Colors.Black, Colors.Black, 1,
                   true, true, true, true)
        { }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Constructors with no connected shapes, only points
        public CConnector(List<CPoint> points)
            : this(points, 0.5, "", Colors.Black, Colors.Black, 1,
                   true, true, true, true)
        { }

        public CConnector(List<CPoint> points, double tension, String text, Color textColor,
                          Color lineColor, int lineWidth, 
                          Boolean visible, Boolean draggable, Boolean sizable, Boolean selectable)
            : base(points, tension, text, textColor, lineColor, lineWidth, 
                                    visible, draggable, sizable, selectable, false)
        { }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // This constructor is primarily for drag and drop creation of shapes.
        public CConnector(Double X, Double Y) 
            : this( new List<CPoint>( new CPoint[] {
                new CPoint(X, Y), new CPoint(X+25.0, Y), 
                new CPoint(X+25.0, Y+50.0), new CPoint(X+50.0, Y+50.0)}))
        { }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Constructor that uses a dictionary of initial values
        public CConnector(Dictionary<String, System.Object> parms) : base(parms)
        {
            // Save ref to begin and end shapes, if provided.
            // Add temp points to point list before connecting.
            if (parms.ContainsKey("begin") == true)
            {
                CShape begin = (CShape)parms["begin"];
                this.points.Insert(0, new CPoint(0, 0));
                this.ConnectBegin(begin);
            }

            if (parms.ContainsKey("end") == true)
            {
                CShape end = (CShape)parms["end"];
                this.points.Insert(this.points.Count, new CPoint(0, 0));
                this.ConnectEnd(end);
            }

            this.UpdateBoundingBox();

        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Create and return a shape factory instance for a CConnector.
        public static new ICanvasEventHandler CreateFactory()
        {
            return new CConnectorFactory();
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // The only selection of wires is selection for editing points
        public override void Select(Canvas cvs)
        {
            // When a shape is dragged, if a connector is connected to it,
            // the dragging changes one or both endpoints of the connector
            // and therefore changes the connector's bounding box,
            // which breaks other functions.
            // The bounding box of all connectors attached to shapes
            // should be recalculated after a shape moves.
            // At the time of this writing, there is no way to know if a shape
            // has a connector attached, so all connector bounding boxes are updated
            // every time it is selected.
            this.UpdateBoundingBox();
            this.EditPoints(cvs);
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // A special implementation of EditPoints that colors ends if connected
        public override void EditPoints(Canvas cvs)
        {
            base.EditPoints(cvs);

            // Check if end points are connected and modify color
            int lpnt = this.points.Count - 1;
            string spnt = lpnt.ToString();
            if (this.begin != null) this.handles["0"].FillColor = Colors.Yellow;
            if (this.end != null) this.handles[spnt].FillColor = Colors.Yellow;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Connect the beginning of this wire to the given shape
        public virtual void ConnectBegin(CShape shp)
        {
            this.begin = shp;
            shp.Connectors.Add(this);
            this.points[0] = shp.center;
            if (this.Selected == true)
            {
                this.handles["0"].point = shp.center;
                this.handles["0"].FillColor = Colors.Yellow;
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // This disconnects the beginning of the wire from the shape
        // and replaces it with the given new point,
        // or a new point at the same location as the last connection
        public void DisconnectBegin()
        {
            // Not connected. Return.
            if (this.begin == null) return;

            // Replace existing connection point with clone of
            // connected shape's center point
            CPoint npnt = new CPoint(this.begin.center.X, this.begin.center.Y);
            this.DisconnectBegin(npnt);
        }

        public virtual void DisconnectBegin(CPoint npnt)
        {
            // Remove the connector from the shape's list
            if (this.begin != null) this.begin.Connectors.Remove(this);

            // Clear connected shape reference and replace first point of connector
            this.begin = null;
            this.points[0] = npnt;

            if (this.Selected == true)
            {
                this.handles["0"].point = this.points[0];
                this.handles["0"].FillColor = Colors.White;
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Connect the end of this wire to the given shape
        public virtual void ConnectEnd(CShape shp)
        {
            int lpnt = this.points.Count-1;
            this.end = shp;
            shp.Connectors.Add(this);
            this.points[lpnt] = shp.center;

            if (this.selected)
            {
                String spnt = lpnt.ToString();
                this.handles[spnt].point = shp.center;
                this.handles[spnt].FillColor = Colors.Yellow;
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // This disconnects the end of the wire from the shape
        // and replaces it with the given new point,
        // or a new point at the same location as the last connection
        public void DisconnectEnd()
        {
            if (this.end == null) return;

            CPoint npnt = new CPoint(this.end.center.X, this.end.center.Y);
            this.DisconnectEnd(npnt);
        }

        public virtual void DisconnectEnd(CPoint npnt)
        {
            // Remove the connector from the shape's list
            if (this.end != null) this.end.Connectors.Remove(this);

            int lpnt = this.points.Count - 1;
            String spnt = lpnt.ToString();
            this.end = null;
            this.points[lpnt] = npnt;
            if (this.Selected == true)
            {
                this.handles[spnt].point = this.points[lpnt];
                this.handles[spnt].FillColor = Colors.White;
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Disconnect this wire on both ends
        public override void Disconnect()
        {
            this.DisconnectBegin();
            this.DisconnectEnd();
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Disconnect this wire from the given shape
        public virtual void Disconnect(CShape shp)
        {
            if (this.begin == shp) this.DisconnectBegin();
            if (this.end == shp) this.DisconnectEnd();
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void MatchOutline(Canvas cvs)
        {
            // Special implementation of MatchOutline.
            // Move line to match its own outline by updating coordinates of its points.
            // Reconnect endpoints to shapes, when they fall on a shape.
            
            if ( this.Outline == null) return;

            CShape o = this.Outline;
            int npts = this.points.Count;

            // First look at the end points of the wire.
            // If either lands on a connectable shape, then reconnect them.

            // If moved first point to a terminal, modify connection.
            CShape term = cvs.HitShape(o.points[0]);
            if (term != null)
            {
                if (term.Connectable == true)
                {
                    this.ConnectBegin(term);
                }
                else
                {
                    this.DisconnectBegin(this.Outline.points[0].Clone());
                }
            }
            // First point did not hit a terminal. Disconnect.
            // Update the point that the handle references 
            // to a clone of the first point in the outline.
            else
            {
                this.DisconnectBegin(this.Outline.points[0].Clone());
            }

            // If moved last point to a terminal
            term = cvs.HitShape(o.points[npts-1]);
            if (term != null)
            {
                if (term.Connectable == true)
                {
                    this.ConnectEnd(term);
                }
                else
                {
                    this.DisconnectEnd(this.Outline.points[this.Outline.points.Count - 1].Clone());
                }
            }
            // Last point did not hit a terminal. Disconnect.
            // Update the point that the connector references to 
            // a clone of the last point of the outline.
            else
            {
                this.DisconnectEnd(this.Outline.points[this.Outline.points.Count - 1].Clone());
            }

            // Move the internal points
            for (int i=1; i<npts-1; i++)
            {
                this.points[i].X = o.points[i].X;
                this.points[i].Y = o.points[i].Y;
            }
            
            this.UpdateHandles();
            this.UpdateBoundingBox();
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // A custom implementation of method to write the points in the connector to Xml
        protected override void WriteXmlPoints(XmlWriter w)
        {
            int idxStartPt = 1;
            int idxEndPt = this.points.Count - 2;

            // If a connected begin shape ...
            if (this.begin != null)
            {
                // and the ids dictionary contains the references begin shape
                // the write it
                if (this.begin._id > 0)
                {
                    w.WriteStartElement("begin");
                    w.WriteAttributeString("id", this.begin._id.ToString());
                    w.WriteEndElement();
                }
                else
                {   // No connecte begin shae in ids dictionary, move point index to 0
                    idxStartPt = 0;
                }
            }
            else
            {   // No connected start shape. Move point index to 0
                idxStartPt = 0;
            }

            // If a connected end shape ...
            if (this.end != null)
            {
                // and the ids dictionary contains the references end shape
                // then write it
                if (this.end._id > 0)
                {
                    w.WriteStartElement("end");
                    w.WriteAttributeString("id", this.end._id.ToString());
                    w.WriteEndElement();
                }
                else
                {   // No connected end shape in ids dictionary. Update end point index
                    idxEndPt = this.points.Count - 1;
                }
            }
            else
            {   // No connected end shape. Update end point index
                idxEndPt = this.points.Count - 1;
            }

            // Cannot use base.WriteXmlPoints because may exclude first and last
            w.WriteStartElement("points");
            for (int i = idxStartPt; i <= idxEndPt; i++)
            {
                CPoint pnt = this.points[i];
                pnt.ToXML(w);
            }
            w.WriteEndElement();
        }
    }

    // -----------------------------------------------------------------------
    // Base class for all handles
    public class CHandle : CEllipse
    {
        internal CPoint point = null;           // Reference to Point object that locates handle
        protected CShape shape;                 // The shape this handle references
//        public String Name = "";                // Name of the handle

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CHandle(CPoint pnt, CShape shp, String name, Color fillColor)
            : base(new List<CPoint>( new CPoint[] {new CPoint(pnt.X-3, pnt.Y-3), new CPoint(pnt.X+3, pnt.Y+3)} ),
             "", Colors.Black, Colors.Black, 1, fillColor, true, false, false, false, false)
        {
            this.point = pnt;   
            this.shape = shp;
            this.Name = name;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Clear object references
        public virtual void ClearRefs()
        {
            this.point = null;
            this.shape = null;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void ResetLocation()
        {
            //# Reset this handle location with respect to its shape.
            //# All handle subclasses must know how to do this on its own.
        }
    }

    // -----------------------------------------------------------------------
    // Sizer Handle
    public class CSizeHandle : CHandle
    {
        public CHandle Anchor = null;   //# The opposite anchor handle to be used during sizing
        private Boolean sizex = true;   //# True if to size in x direction
        private Boolean sizey = true;   //# True if to size in y direction

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CSizeHandle(CPoint pnt, CShape shp, String name, Boolean sizex, Boolean sizey)
            : base(pnt, shp, name, Colors.White)
        {
            this.sizex = sizex;
            this.sizey = sizey;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Clear object references
        public override void ClearRefs()
        {
            this.Anchor = null;
            this.point = null;
            this.shape = null;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void ResetLocation()
        {
            // Reset this size handle location with respect to its shape.
            // All handles must know how to reset their locations wrt their shape.
            double x1 = this.shape.left;
            double y1 = this.shape.top;
            double x2 = this.shape.left + this.shape.width;
            double y2 = this.shape.top + this.shape.height;
            double cx = this.shape.center.X;
            double cy = this.shape.center.Y;
            
            if (this.Name == "upper-left")
                this.TranslateCenterTo(new CPoint(x1, y1));
            else if (this.Name == "upper-right")
                this.TranslateCenterTo(new CPoint(x2, y1));
            else if (this.Name == "lower-left")
                this.TranslateCenterTo(new CPoint(x1, y2));
            else if (this.Name == "lower-right")
                this.TranslateCenterTo(new CPoint(x2, y2));
            else if (this.Name == "left-middle")
                this.TranslateCenterTo(new CPoint(x1, cy));
            else if (this.Name == "right-middle")
                this.TranslateCenterTo(new CPoint(x2, cy));
            else if (this.Name == "upper-center")
                this.TranslateCenterTo(new CPoint(cx, y1));
            else if (this.Name == "lower-center")
                this.TranslateCenterTo(new CPoint(cx, y2));
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void OnMouseDown(Canvas cvs, MouseEventArgs e)
        {
            if (cvs.Mode == EMode.Editing && cvs.EditMode == EMode.Idle)
            {
                // Start outlines in prep for sizing on all sizable shapes
                foreach (CShape s in cvs.SelectedShapes())
                {
                    if (s.Sizable == true) s.StartOutline(cvs);
                }

                // Change canvas state to start sizing underlying object
                cvs.EditMode = EMode.SizeLeftStart;
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void OnMouseMove(Canvas cvs, MouseEventArgs e)
        {
            if (cvs.Mode == EMode.Editing)
            {
                if (cvs.EditMode == EMode.SizeLeftStart)
                {
                    // Mouse moved by at least one grid point
                    if (cvs.mouseGrid.X != cvs.mouseDownGrid.X || cvs.mouseGrid.Y != cvs.mouseDownGrid.Y)
                    {
                        //cvs.Capture = true;                 // Start capturing mouse
						//Gdk.Pointer.Grab();
                        cvs.EditMode = EMode.SizeLeft;    // Promote state and move and size outline
                    }
                }
                else if (cvs.EditMode == EMode.SizeLeft)
                {
                    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                    // This style of sizing changes the size of all objects using scale factors
                    // that are computed from the relative size change of the object being manipulated.
                    // This is the way PowerPoint does it.

                    double x = cvs.mouseGrid.X;
                    double y = cvs.mouseGrid.Y;

                    double ax = this.Anchor.center.X;
                    double ay = this.Anchor.center.Y;
                    double nx = 1.0;
                    double dx = 1.0;
                    double ny = 1.0;
                    double dy = 1.0;

                    // If sizing in the x direction ...
                    if (this.sizex == true)
                    {
                        nx = x-ax;
                        dx = cvs.mouseDownGrid.X-ax;
                    }
                    // If sizing in the y-direction ...
                    if (this.sizey == true)
                    {
                        ny = y-ay;
                        dy = cvs.mouseDownGrid.Y-ay;
                    }
                    // If any numerator or denominator is zero, skip scale operation.
                    if (nx*dx*ny*dy != 0.0)
                    {
                        double sx = nx/dx;
                        double sy = ny/dy;

                        // If the shift button is down, change scale factor to be max of sx and sy
                        //if (Control.ModifierKeys == Keys.Shift)
						if ((cvs.ModifierKeys & Gdk.ModifierType.ShiftMask) != 0)
                        {
                            double maxval = Math.Max(Math.Abs(sx), Math.Abs(sy));
                            sx = (double)Math.Sign(sx) * maxval;
                            sy = (double)Math.Sign(sy) * maxval;
                        }

                        foreach (CShape s in cvs.SelectedShapes())
                        {
                            CHandle anchor = s.handles[this.Anchor.Name];
                            double sax = anchor.center.X;
                            double say = anchor.center.Y;
                            if (s.Sizable == true) s.Outline.Scale(sax, say, sx, sy, s);
                        }
                    }
                }

                // Redraw
                cvs.Invalidate();
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void OnMouseUp(Canvas cvs, MouseEventArgs e)
        {
            if (cvs.Mode == EMode.Editing)
            {
                // If sizing
                if (cvs.EditMode == EMode.SizeLeftStart || cvs.EditMode == EMode.SizeLeft)
                {
                    // Cancel mouse capturing
                    //if (cvs.Capture == true) cvs.Capture = false;
					//if (Gdk.Pointer.IsGrabbed) Gdk.Pointer.Ungrab();
                    
                    // Update all selected shapes to their outlines
                    List<CShape> shps = new List<CShape>();
                    foreach (CShape s in cvs.SelectedShapes())
                    {
                        if (s.Sizable == true)
                        {
                            s.MatchOutline(cvs);
                            s.StopOutline(cvs);

                            // Build a list of shapes sized for ShapesSized event
                            shps.Add(s);
                        }
                    }
                    
                    // Move out of the dragging state
                    cvs.handler = cvs;
                    cvs.EditMode = EMode.Idle;

                    // Raise events
                    cvs.RaiseShapesSizedEvent(shps);

                    // Redraw
                    cvs.Invalidate();
                }
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void CancelProcess(Canvas cvs)
        {
            // Cancel any sizing that has been started.
            if (cvs.Mode == EMode.Editing)
            {
                if (cvs.EditMode == EMode.SizeLeft)
                {
                    // Cancel mouse capturing
                    //if (cvs.Capture == true) cvs.Capture = false;
					//if (Gdk.Pointer.IsGrabbed) Gdk.Pointer.Ungrab();

                    // Update all selected shapes to their outlines
                    foreach (CShape s in cvs.SelectedShapes())
                        if (s.Sizable == true)
							s.StopOutline(cvs);

                    // Move out of the dragging state
                    cvs.handler = cvs;
                    cvs.EditMode = EMode.Idle;
                }
            }
        }
    }

    // -----------------------------------------------------------------------
    // Shape Point Edit Handle
    public class CEditHandle : CHandle
    {
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CEditHandle(CPoint pnt, CShape shp, String name) 
            : this(pnt, shp, name, Colors.White) { }
        public CEditHandle(CPoint pnt, CShape shp, String name, Color fillColor)
            : base(pnt, shp, name, fillColor)
        {
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void ResetLocation()
        {
            // Move edit handle to its corresponding point location.
            this.TranslateCenterTo(new CPoint(this.point.X, this.point.Y));
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void OnMouseDown(Canvas cvs, MouseEventArgs e)
        {
            // Advance the editing state.
            // It is assumed that this object will not get the event unless it has been hit.
            if (cvs.Mode == EMode.Editing && cvs.EditMode == EMode.Idle)
                cvs.EditMode = EMode.PEditLeftStart;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void OnMouseMove(Canvas cvs, MouseEventArgs e)
        {
            if (cvs.Mode == EMode.Editing)
            {
                if (cvs.EditMode == EMode.PEditLeftStart)
                {
                    // Mouse moved a minimum amount
                    if (cvs.mouseGrid.X != cvs.mouseDownGrid.X 
                        || cvs.mouseGrid.Y != cvs.mouseDownGrid.Y)
                    {
                        //cvs.Capture = true;                                 // Start capturing mouse
						//Gdk.Pointer.Grab();
                        this.shape.StartOutline(cvs);                       // Start outlining the underlying object
                        cvs.EditMode = EMode.PEditLeft;                   // Promote state
                    }
                }
                else if (cvs.EditMode == EMode.PEditLeft)                 // Move point in the shape outline
                {
                    double dx = cvs.mouseExact.X - cvs.mouseDownExact.X;
                    double dy = cvs.mouseExact.Y - cvs.mouseDownExact.Y;

                    int pnum = int.Parse(this.Name);                        // Handle name is underlying point number
                    CPoint shape_pt     = this.shape.points[pnum];          // Get point on line and outline
                    CPoint outline_pt   = this.shape.Outline.points[pnum];
                    outline_pt.X = shape_pt.X + dx;                         // Edit outline shape point location
                    outline_pt.Y = shape_pt.Y + dy;
                }

                // Redraw
                cvs.Invalidate();
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void OnMouseUp(Canvas cvs, MouseEventArgs e)
        {
            if (cvs.Mode == EMode.Editing)
            {
                // Finish editing points
                if (cvs.EditMode == EMode.PEditLeft 
                    || cvs.EditMode == EMode.PEditLeftStart)
                {
                    // Cancel mouse capture
                    //if (cvs.Capture == true) cvs.Capture = false;
					//if (Gdk.Pointer.IsGrabbed) Gdk.Pointer.Ungrab();
                    
                    // Update underlying shape
                    this.shape.MatchOutline(cvs);
                    
                    // Clear outline
                    this.shape.StopOutline(cvs);

                    // Move out of the state and reset event handler
                    cvs.handler = cvs;
                    cvs.EditMode = EMode.Idle;

                    // Raise events
                    cvs.RaiseShapesSizedEvent(new List<CShape>() { this.shape });

                    cvs.Invalidate();
                } 
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void CancelProcess(Canvas cvs)
        {
            // Cancel any editing that has started
            if (cvs.Mode == EMode.Editing)
            {
                // Finish editing points
                if (cvs.EditMode == EMode.PEditLeft 
                    || cvs.EditMode == EMode.PEditLeftStart)
                {
                    // Cancel mouse capture
                    //if (cvs.Capture == true) cvs.Capture = false;
					//if (Gdk.Pointer.IsGrabbed) Gdk.Pointer.Ungrab();
					
                    // Clear outline
                    this.shape.StopOutline(cvs);

                    // Move out of the state and reset event handler
                    cvs.handler = cvs;
                    cvs.EditMode = EMode.Idle;
                }
            }
        }
    }

    // -----------------------------------------------------------------------
    // A factory class that implements the ability to interactively create CShape objects
    public class CShapeFactory : ICanvasEventHandler
    {
        private Type class_type = null;         // Type of class to create
        private CShape cache = null;            // Temporary storage of shape being created
        //private String class_name = null;       // Class name of shape being created

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Create a shape factory given a class type
        public CShapeFactory(Type class_type)
        {
            this.class_type = class_type;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Create a shape factory given a class name
        public CShapeFactory(String class_name)
        {
            // Give a class name, fetch and save the associated Type
            Assembly assembly = Assembly.GetExecutingAssembly();
            AssemblyName assemblyName = assembly.GetName();
            this.class_type = assembly.GetType(assemblyName.Name + "." + class_name);

            //this.class_name = class_name;
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public virtual void OnScroll(Canvas cvs, Gdk.EventScroll e)
		{	// Default behavior is to hand event back to canvas
			cvs.OnScroll(cvs, e);
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnMouseDown(Canvas cvs, MouseEventArgs e)
        {
            // Drawing a new shape
            if (cvs.Mode == EMode.Drawing)
            {
                // If not yet initiated drawing
                if (cvs.DrawMode == EMode.Idle) 
                    cvs.DrawMode = EMode.DrawLeftStart;
            }
        }

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnDoubleClick(Canvas cvs, MouseEventArgs e)
        {
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnMouseMove(Canvas cvs, MouseEventArgs e)
        {
            if (cvs.Mode == EMode.Drawing && cvs.DrawMode != EMode.Idle)
            {
                // Get mouse locations
                double x = cvs.mouseGrid.X;
                double y = cvs.mouseGrid.Y;
                double mdx = cvs.mouseDownGrid.X;
                double mdy = cvs.mouseDownGrid.Y;
                double xdiff, ydiff, maxval;

                // ... and the mouse has moved a minimum amount
                if (x != mdx || y != mdy)
                {
                    // ... and we have already initiated drawing ... 
                    if (cvs.DrawMode == EMode.DrawLeftStart)
                    {
                        // If the shift button is down, change size to be max of x and y
                        //if (Control.ModifierKeys == Keys.Shift)
                        if ((cvs.ModifierKeys & Gdk.ModifierType.ShiftMask) != 0)
						{
                            xdiff = x - mdx;
                            ydiff = y - mdy;
                            maxval = Math.Max(Math.Abs(xdiff), Math.Abs(ydiff));
                            x = mdx + Math.Sign(xdiff)*maxval;
                            y = mdy + Math.Sign(ydiff)*maxval;
                        }

                        //// Create a new shape using the class that was
                        //// saved in class_name and update drawing state
                        //Assembly assembly = Assembly.GetExecutingAssembly();
                        //AssemblyName assemblyName = assembly.GetName();
                        //Type t = assembly.GetType(assemblyName.Name + "." + class_name);
                        
                        List<CPoint> pts = new List<CPoint>();
                        pts.Add(new CPoint(mdx, mdy));
                        pts.Add(new CPoint(x, y));
                        object[] args = {pts};
                        this.cache = (CShape)Activator.CreateInstance(this.class_type, args);

                        cvs.AddShape(this.cache);

                        // ... and update state to full drawing mode
                        cvs.DrawMode = EMode.DrawLeft;
                    }

                    // ... and the shape has already been initiated ...
                    else if (cvs.DrawMode == EMode.DrawLeft)
                    {
                        // If the shift button is down, change size to be max of x and y
                        //if (Control.ModifierKeys == Keys.Shift)
						if ((cvs.ModifierKeys & Gdk.ModifierType.ShiftMask) != 0)
                        {
                            xdiff = x - mdx;
                            ydiff = y - mdy;
                            maxval = Math.Max(Math.Abs(xdiff), Math.Abs(ydiff));
                            x = mdx + Math.Sign(xdiff)*maxval;
                            y = mdy + Math.Sign(ydiff)*maxval;
                        }

                        // ... then resize it to follow the mouse.
                        //Console.WriteLine(String.Format("{0}, {1}, {2}, {3}", mdx, mdy, x, y));
                        this.cache.MorphTo(new CPoint(mdx, mdy), new CPoint(x, y));
                    }
                }

                // Redraw
                cvs.Invalidate();
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnMouseUp(Canvas cvs, MouseEventArgs e)
        {
            if (cvs.DrawMode == EMode.DrawLeft)
            {
                // Deselect all if no shift key
                //if (Control.ModifierKeys != Keys.Shift) cvs.DeselectAll();
                if ((cvs.ModifierKeys & Gdk.ModifierType.ShiftMask) == 0) cvs.DeselectAll();
				
                if (this.cache != null)
                {
                    // Select the shape that has been created.
                    //if (Control.ModifierKeys != Keys.Shift) cvs.DeselectAll();
                    this.cache.Select( cvs );

                    // Move back to idle draw state
                    cvs.DrawMode = EMode.Idle;
                    
                    // Indicate shape created and change in selection.
                    cvs.RaiseShapesCreatedEvent( this.cache );
                    cvs.RaiseSelectionChangedEvent();

                    // Clean up
                    this.cache = null;
                }
            }
            // Redraw
            cvs.Invalidate();
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void CancelProcess(Canvas cvs)
        {
            // Cancel the current shape creation process

            // If drawing a new shape
            if (cvs.Mode == EMode.Drawing)
            {
                // ... and the shape has already been initiated ...
                if (cvs.DrawMode == EMode.DrawLeft)
                {
                    cvs.DeleteShape(this.cache);
                    this.cache = null;
                }
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    }

    // -----------------------------------------------------------------------
    // A factory class that implements the ability to interactively create CSpline objects
    public class CSplineFactory : ICanvasEventHandler
    {
        private CShape cache = null;            // Temporary storage of shape being created
        private bool inCancelProcess = false;   // Flag to help avoid infinite loops in CancelProcess

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnDoubleClick(Canvas cvs, MouseEventArgs e)
        {
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public virtual void OnScroll(Canvas cvs, Gdk.EventScroll e)
		{	// Default behavior is to hand event back to canvas
			cvs.OnScroll(cvs, e);
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnMouseDown(Canvas cvs, MouseEventArgs e)
        {
            // - - - - - - - - - -
            // Drawing a new shape
            if (cvs.Mode == EMode.Drawing)
            {
                // If not yet initiated drawing
                if (cvs.DrawMode == EMode.Idle)
                {
                    // If an existing shape was not hit ... initiate drawing
                    CShape hit_object = cvs.HitShape(cvs.mouseDownExact);
                    if (hit_object == null)
                    {
                        cvs.DrawMode = EMode.DrawLeftStart;
                    }
                }
                // If already created a CSpline, then add another point
                else if (cvs.DrawMode == EMode.DrawLeft)
                {
                    // If the right button, then stop creating CSpline
                    if (e.Button == MouseButtons.Right)
                    {
                        // Go back to editing state
                        cvs.BeginEditing();
                    }
                    // Otherwise, add another point to CSpline and continue
                    else
                    {
                        double x = cvs.mouseGrid.X;
                        double y = cvs.mouseGrid.Y;
                        this.cache.points.Add( new CPoint(x, y) );
                    }
                }
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnMouseMove(Canvas cvs, MouseEventArgs e)
        {
            if (cvs.Mode == EMode.Drawing && cvs.DrawMode != EMode.Idle)
            {
                // Get mouse locations
                double x = cvs.mouseGrid.X;
                double y = cvs.mouseGrid.Y;
                double mdx = cvs.mouseDownGrid.X;
                double mdy = cvs.mouseDownGrid.Y;
                
                // ... and the mouse has moved a minimum amount
                if (x != mdx || y != mdy)
                {
                    // ... and we have already initiated drawing ... 
                    if (cvs.DrawMode == EMode.DrawLeftStart)
                    {
                        // Create a new CSpline with mouse down and current mouse position
                        List<CPoint> pts = new List<CPoint>();
                        pts.Add( new CPoint(mdx, mdy) );
                        pts.Add( new CPoint(x, y) );
                        this.cache = new CSpline(pts);
                        cvs.AddShape(this.cache);

                        // ... and update state to full drawing mode
                        cvs.DrawMode = EMode.DrawLeft;
                    }
                    // ... and the shape has already been created ...
                    else if (cvs.DrawMode == EMode.DrawLeft)
                    {
                        // ... then move the last point only
                        CPoint pt = this.cache.points[this.cache.points.Count - 1];
                        pt.X = x;
                        pt.Y = y;
                        this.cache.UpdateBoundingBox();
                    }
                }
                // Redraw
                cvs.Invalidate();
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnMouseUp(Canvas cvs, MouseEventArgs e)
        {
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void CancelProcess(Canvas cvs)
        {
            // Cancel the drawing process

            // This method has a tendency to get into infinite loops.
            // Prevent that from happening.
            if (this.inCancelProcess == true) return;
            this.inCancelProcess = true;

            // Drawing a new shape
            if (cvs.Mode == EMode.Drawing)
            {
                // If already created a CSpline, then add another point
                if (cvs.DrawMode == EMode.DrawLeft)
                {
                    // Delete the last point
                    if (this.cache.points.Count > 0)
                        this.cache.points.RemoveAt(this.cache.points.Count - 1);

                    // If not enough points left, delete the spline all together
                    if (this.cache.points.Count <= 1)
                    {
                        cvs.DeleteShape(this.cache);
                        this.cache.points.Clear();
                        cvs.RaiseShapesCreatedEvent(null);
                    }
                    else
                    {   // Update bounding box if a spline not deleted
                        this.cache.UpdateBoundingBox();

                        // Select the shape that has been created.
                        //if (Control.ModifierKeys != Keys.Shift) cvs.DeselectAll();
						if ((cvs.ModifierKeys & Gdk.ModifierType.ShiftMask) == 0) cvs.DeselectAll();
							
                        this.cache.Select(cvs);

                        // Indicate shape created and change in selection.
                        cvs.RaiseShapesCreatedEvent(this.cache);
                        cvs.RaiseSelectionChangedEvent();
                    }

                    this.cache = null;
                }
            }
            this.inCancelProcess = false;
        }

        //// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        //public virtual void OnKeyDown(Canvas cvs, KeyEventArgs e)
        //{
        //    // - - - - - - - - - -
        //    // If drawing a new shape
        //    if (cvs.State == EState.Drawing)
        //    {
        //        // ... and if already created a CSpline
        //        if (cvs.DrawState == EState.DrawLeft)
        //        {
        //            // ... and if the key is ESC, then stop creating CSpline
        //            if (e.KeyCode == Keys.Escape)
        //            {
        //                // Remove the last point used for creating CSpline
        //                this.cache.points.RemoveAt(this.cache.points.Count-1);
        //                this.cache.UpdateBoundingBox();
                        
        //                // Select the shape that has been created.
        //                if (Control.ModifierKeys != Keys.Shift) cvs.DeselectAll();
        //                this.cache.Select( cvs );

        //                // Move back to idle draw state
        //                cvs.DrawState = EState.Idle;

        //                // Indicate shape createa and change in selection.
        //                cvs.RaiseShapesCreatedEvent( this.cache );
        //                cvs.RaiseSelectionChangedEvent();

        //                // Clean up
        //                this.cache = null;

        //                cvs.Invalidate();
        //            }
        //        }
        //    }
        //}
    }

    // -----------------------------------------------------------------------
    // A factory class that implements the ability to interactively create CSpline objects
    public class CConnectorFactory : ICanvasEventHandler
    {
        private Type class_type = null;         // Type of CConnector class to create
        private CConnector cache = null;        // Temporary storage of connector being created
        private CShape csrc = null;             // Temporary storage of source shape being connected
        private bool inCancelProcess = false;   // Flag to help avoid infinite loops in CancelProcess

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Create a shape factory given a class type
        public CConnectorFactory() : this(typeof(CConnector))
        { }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Create a shape factory given a class type
        public CConnectorFactory(Type class_type)
        {
            this.class_type = class_type;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnDoubleClick(Canvas cvs, MouseEventArgs e)
        {
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public virtual void OnScroll(Canvas cvs, Gdk.EventScroll e)
		{	// Default behavior is to hand event back to canvas
			cvs.OnScroll(cvs, e);
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnMouseDown(Canvas cvs, MouseEventArgs e)
        {
            // - - - - - - - - - -
            // Drawing a new shape
            if (cvs.Mode == EMode.Drawing)
            {
                // If not yet initiated drawing
                if (cvs.DrawMode == EMode.Idle)
                {
                    // See if first mouse down hit an existing shape
                    CShape hit_object = cvs.HitShape(cvs.mouseDownExact);

                    // If hit a shape ... cache a reference to the source
                    if (hit_object != null && hit_object.Connectable == true)
                    {
                        this.csrc = hit_object;
                    }
                    // Otherwise, make sure source is null
                    else
                    {
                        this.csrc = null;
                    }

                    // Deselect all shapes
                    //if (Control.ModifierKeys != Keys.Shift) cvs.DeselectAll();
					if ((cvs.ModifierKeys & Gdk.ModifierType.ShiftMask) == 0) cvs.DeselectAll();

                    // In any case... initiate drawing
                    cvs.DrawMode = EMode.DrawLeftStart;
                }

                // If already created a CConnector ..
                else if (cvs.DrawMode == EMode.DrawLeft)
                {
                    // If hit a different shape with left mouse, 
                    // then complete the connection
                    if (e.Button == MouseButtons.Left)
                    {
                        CShape hit_object = cvs.HitShape(cvs.mouseDownExact);
                        if (hit_object != null)
                        {
                            if (hit_object != this.csrc && hit_object.Connectable == true)
                            {
                                this.cache.ConnectEnd(hit_object);

                                // Wrap up
                                this.cache.UpdateBoundingBox();

                                // Select the shape that has been created.
                                //if (Control.ModifierKeys != Keys.Shift) cvs.DeselectAll();
								if ((cvs.ModifierKeys & Gdk.ModifierType.ShiftMask) == 0) cvs.DeselectAll();
                                this.cache.Select(cvs);

                                // Move back to idle draw state
                                cvs.DrawMode = EMode.Idle;

                                // Indicate shape created and change in selection.
                                cvs.RaiseShapesCreatedEvent(this.cache);
                                cvs.RaiseSelectionChangedEvent();

                                // Clean up
                                this.cache = null;
                                this.csrc = null;

                                // Go back to editing state
                                cvs.BeginEditing();

                                cvs.Invalidate();
                            }
                        }

                        // Otherwise, add another point to CConnector and continue
                        else
                        {
                            double x = cvs.mouseGrid.X;
                            double y = cvs.mouseGrid.Y;
                            this.cache.points.Add(new CPoint(x, y));
                        }
                    }

                    // If the right button, then stop creating CConnector
                    else if (e.Button == MouseButtons.Right)
                    {
                        // Go back to editing state
                        cvs.BeginEditing();
                    }
                }
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnMouseMove(Canvas cvs, MouseEventArgs e)
        {
            if (cvs.Mode == EMode.Drawing && cvs.DrawMode != EMode.Idle)
            {
                // Get mouse locations
                double x = cvs.mouseGrid.X;
                double y = cvs.mouseGrid.Y;
                double mdx = cvs.mouseDownGrid.X;
                double mdy = cvs.mouseDownGrid.Y;
                
                // ... and we have already initiated drawing ... 
                if (cvs.DrawMode == EMode.DrawLeftStart)
                {
                    // ... and the mouse has moved a minimum amount
                    if (x != mdx || y != mdy)
                    {
                        // If there is a source shape cached,
                        // then create the new connector with the source shape as the beginning
                        if (this.csrc != null && this.csrc.Connectable == true)
                        {
                            //this.cache = new CConnector(this.csrc, new CPoint(x, y));
                            object[] args = { this.csrc, new CPoint(x, y) };
                            this.cache = (CConnector)Activator.CreateInstance(this.class_type, args);
                        }
                        // If no source shape cached, then use the mouse down 
                        // and current point to create the connector
                        else
                        {
                            List<CPoint> pts = new List<CPoint>();
                            pts.Add(new CPoint(mdx, mdy));
                            pts.Add(new CPoint(x, y));

                            object[] args = { pts };
                            this.cache = (CConnector)Activator.CreateInstance(this.class_type, args);
                            //this.cache = new CConnector(pts);
                        }

                        // Add the connector the the connector layer
                        cvs.AddConnector(this.cache);

                        // ... and update state to full drawing mode
                        cvs.DrawMode = EMode.DrawLeft;
                    }
                }

                // ... and the shape has already been created ...
                else if (cvs.DrawMode == EMode.DrawLeft)
                {
                    // ... then move the last point only
                    CPoint pt = this.cache.points[this.cache.points.Count - 1];
                    pt.X = x;
                    pt.Y = y;
                    this.cache.UpdateBoundingBox();
                }

                // Redraw
                cvs.Invalidate();
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void OnMouseUp(Canvas cvs, MouseEventArgs e)
        {
            // - - - - - - - - - -
            // If drawing a new shape
            if (cvs.Mode == EMode.Drawing)
            {
                // If already created a CConnector ..
                if (cvs.DrawMode == EMode.DrawLeft)
                {
                    // If hit a different shape with left mouse, then complete the connection
                    if (e.Button == MouseButtons.Left)
                    {
                        CShape hit_object = cvs.HitShape(cvs.mouseExact);
                        if (hit_object != null && hit_object.Connectable == true)
                        {
                            if (hit_object != this.csrc)
                            {
                                this.cache.ConnectEnd(hit_object);

                                // Wrap up
                                this.cache.UpdateBoundingBox();

                                // Select the shape that has been created.
                                //if (Control.ModifierKeys != Keys.Shift) cvs.DeselectAll();
								if ((cvs.ModifierKeys & Gdk.ModifierType.ShiftMask) == 0) cvs.DeselectAll();
                                this.cache.Select(cvs);

                                // Move back to idle draw state
                                cvs.DrawMode = EMode.Idle;

                                // Indicate shape created and change in selection.
                                cvs.RaiseShapesCreatedEvent(this.cache);
                                cvs.RaiseSelectionChangedEvent();

                                // Clean up
                                this.cache = null;
                                this.csrc = null;

                                cvs.Invalidate();
                            }
                        }
                    }
                }
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void CancelProcess(Canvas cvs)
        {
            // Cancel the current connector creation process

            // This method has a tendency to get into infinite loops.
            // Prevent that from happening.
            if (this.inCancelProcess == true) return;
            this.inCancelProcess = true;

            // If drawing a new shape
            if (cvs.Mode == EMode.Drawing)
            {
                // and already created a CConnector ..
                if (cvs.DrawMode == EMode.DrawLeft)
                {
                    // Delete the last point
                    if (this.cache.points.Count > 0)
                        this.cache.points.RemoveAt(this.cache.points.Count - 1);

                    // If not enough points left, delete the connector all together
                    if (this.cache.points.Count <= 1)
                    {
                        cvs.DeleteConnector(this.cache);
                        this.cache.points.Clear();
                        cvs.RaiseShapesCreatedEvent(null);
                    }
                    else
                    {   // Update bounding box if a connector not deleted
                        this.cache.UpdateBoundingBox();

                        // Select the shape that has been created.
                        //if (Control.ModifierKeys != Keys.Shift) cvs.DeselectAll();
						if ((cvs.ModifierKeys & Gdk.ModifierType.ShiftMask) == 0) cvs.DeselectAll();
                        this.cache.Select(cvs);

                        cvs.RaiseShapesCreatedEvent(this.cache);
                        cvs.RaiseSelectionChangedEvent();
                    }

                    this.cache = null;
                    this.csrc = null;
                }
            }
            this.inCancelProcess = false;

        }

        //// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        //public virtual void OnKeyDown(Canvas cvs, KeyEventArgs e)
        //{
        //    // - - - - - - - - - -
        //    // If drawing a new shape
        //    if (cvs.State == EState.Drawing)
        //    {
        //        // ... and if already created a CSpline
        //        if (cvs.DrawState == EState.DrawLeft)
        //        {
        //            // ... and if the key is ESC, then stop creating CSpline
        //            if (e.KeyCode == Keys.Escape)
        //            {
        //                if (this.cache.points.Count <= 1)
        //                {
        //                    cvs.DeleteConnector(this.cache);
        //                    this.cache.points.Clear();
        //                    //cvs.RaiseShapesCreatedEvent(null);
        //                }
        //                else
        //                {
        //                    // Remove the last point used for creating CSpline
        //                    this.cache.points.RemoveAt(this.cache.points.Count - 1);
        //                    this.cache.UpdateBoundingBox();

        //                    // Select the shape that has been created.
        //                    if (Control.ModifierKeys != Keys.Shift) cvs.DeselectAll();
        //                    this.cache.Select(cvs);

        //                    // Indicate shape createa and change in selection.
        //                    cvs.RaiseShapesCreatedEvent(this.cache);
        //                    cvs.RaiseSelectionChangedEvent();
        //                }

        //                // Move back to idle draw state
        //                //cvs.DrawState = EState.Idle;

        //                // Clean up
        //                this.cache = null;
        //                this.csrc = null;

        //                cvs.BeginEditing();

        //                cvs.Invalidate();
        //            }
        //        }
        //    }
        //}
    }

    // -----------------------------------------------------------------------
    // This class groups a decorator shape with its parent as well as information
    // about how to draw the shape with respect to the parent shape
    public class CDecorator
    {
        private CShape shape = null;
        private CPoint offset = null;
        private System.Object tag = null;      // An arbitrary object that tags this object

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CDecorator(CShape shp, CPoint offset)
        {
            this.shape = shp;
            this.offset = offset;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public void Update(CShape parent)
        {
            double x = parent.Left + this.offset.X;
            double y = parent.Top + this.offset.Y;
            this.shape.TranslateTo(new CPoint(x, y));
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Draw the decorator shape using position information
        public void Draw(Cairo.Context g)
        {
            this.shape.Draw(g);
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public System.Object Tag
        {
            get { return this.tag; }
            set { this.tag = value; }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CShape Shape
        {
            get { return this.shape; }
            set { this.shape = value; }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Generate an XML representation of this decorator
        public virtual void ToXml(XmlWriter w, String key)
        {
            w.WriteStartElement("decorator");
            w.WriteAttributeString("key", key);
            w.WriteAttributeString("offsetx", this.offset.X.ToString());
            w.WriteAttributeString("offsety", this.offset.Y.ToString());
            this.shape.ToXml(w);
            w.WriteEndElement();
        }
    }
}
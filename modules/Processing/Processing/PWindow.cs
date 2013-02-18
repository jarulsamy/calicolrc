/*
Calico - Scripting Environment

Copyright (c) 2012, Mark F. Russo <mfrusso@brynmawr.edu>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

$Id: $
*/

using System;
using System.IO;
using System.Threading;
using System.Collections.Generic;
using Cairo;
using Gtk;

// ------------------ PWindow ----------------------------------------------
internal class PWindow : Gtk.Window
{
	internal Gtk.DrawingArea _cvs;								// Widget on which to display graphics
	internal Cairo.ImageSurface _img;							// Surface upon which all internal drawing is done
	private bool _needsQuit = false;							// True if window needs Application.Quit on destroy

	private double _width;										// Internal cache of window size
	private double _height;
	private long _frameCount;									// Count the number of times the expose event was fired
	private bool _smooth = false;								// True if to perform subpixel smoothing
	private bool _toFill = true;								// True if to fill a shape
	private Color _fillColor = new Color(1.0, 1.0, 1.0);		// Current fill color
	private bool _toStroke = true;								// True if to stroke a shape
	private Color _strokeColor = new Color(0.0, 0.0, 0.0);		// Current stroke color
	private double _strokeWeight = 1.0;							// Thickness of stroked lines
	private LineCap _strokeCap = LineCap.Round;					// Default line cap
	private LineJoin _strokeJoin = LineJoin.Miter;				// Default line join 
	private double _tightness = 0.2;							// Spline smooth factor {0.0, 1.0]
	private double _textSize = 12.0;							// Default text size
	private TextAlign _textAlignX = TextAlign.LEFT;				// Text alignment mode in x-direction
	private TextYAlign _textAlignY = TextYAlign.TOP;			// Text alignment mode in y-direction
	private double _textScaleFactor = 120.0/72.0;				// (screen resolution / dots per inch)
	private static EllipseMode _ellipseMode = EllipseMode.CENTER;
	private static RectMode _rectMode = RectMode.CORNER;
	private static ImageMode _imageMode = ImageMode.CORNER;

	private Matrix _mat = new Matrix();							// Cairo transform matrix. Init to identity.
	private List<Matrix> _stack = new List<Matrix>();

	public event DeleteEventHandler windowClosed;				// Raised when window is closed

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public PWindow (int w, int h, int x, int y, bool needsQuit ) : base(WindowType.Toplevel)
	{
		_needsQuit = needsQuit;
		_frameCount = 0;
		_textScaleFactor = 120.0/72.0;		// (screen resolution / dots per inch)  TODO: How to get screen resolution?

		// Create window with drawing area
		this.size(w, h);
		if (x > -10000) this.Move (x, y);

		this.DeleteEvent += onDeleteEvent;

		_cvs = new Gtk.DrawingArea();
		_cvs.AddEvents ((int)Gdk.EventMask.AllEventsMask );
		_cvs.CanFocus = true;		// Required to receive KeyPressEvents
		_cvs.ExposeEvent += this.onExposeEvent;

		this.Add (_cvs);

		// Create internal image on which all drawing is done
		_img = new Cairo.ImageSurface(Format.Argb32, w, h);

		this.ShowAll ();
	}
	public PWindow (int w, int h, bool needsQuit ): this(w, h, -10000, -10000, needsQuit) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	protected void onDeleteEvent (object o, DeleteEventArgs args)
	{
		raiseWindowClosed(args);				// Let others know that the window closed itself
		if (_needsQuit) Application.Quit();		// This must be executed here if required
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	protected virtual void raiseWindowClosed(DeleteEventArgs e)
    {
        DeleteEventHandler handler = windowClosed;
        if (handler != null)
        {
            handler(this, e);
        }
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	protected void onExposeEvent (object o, Gtk.ExposeEventArgs args)
	{	// Handle the Paint event by drawing all shapes and annotations
		using (Context g = Gdk.CairoHelper.Create( _cvs.GdkWindow )) {
			g.SetSource(_img);
			g.Paint ();
		}
		_frameCount++;		// Count the number of frames 
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private void _stroke(Context g)
	{	// Utility function to stroke the current path
		if (_toStroke) {
			if (_smooth) g.Antialias = Antialias.Subpixel;
			g.Color = _strokeColor;
			g.LineCap = _strokeCap;
			g.LineJoin = _strokeJoin;
			g.LineWidth = _strokeWeight;
			g.Stroke ();
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private void _fill(Context g) 
	{	// Utility function to fill and preserve the current path
		if (_toFill) {
			if (_smooth) g.Antialias = Antialias.Subpixel;
			g.Color = _fillColor;
			g.FillPreserve ();
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private void _fill2(Context g) 
	{	// Utility function to fill and discard the current path
		if (_toFill) {
			if (_smooth) g.Antialias = Antialias.Subpixel;
			g.Color = _fillColor;
			g.Fill ();
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//	internal void _updatePixels(Gdk.Pixbuf _pixbuf, bool _immediateMode)
//	{
//		//Console.WriteLine ("in _p._updatePixels");
//		Context g = null;
//		try {
//			//using (Context g = new Cairo.Context(_p._img)) {
//			g = new Cairo.Context(_img);
//			Gdk.CairoHelper.SetSourcePixbuf(g, _pixbuf, 0.0, 0.0);
//			g.Paint ();
//			//}
//			((IDisposable) g).Dispose();
//			g = null;
//			if (_immediateMode) redraw ();
//		} catch (Exception ex) {
//			if (g != null) ((IDisposable) g).Dispose();
//			g = null;
//			Console.WriteLine ( "PWindow.updatePixels(): " + ex.Message);
//		}
//		//Console.WriteLine ("out _p._updatePixels");
//	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void redraw() 
	{	// Force the window to update
		this.QueueDraw ();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void size(int w = 400, int h = 300) 
	{	// Set window size
		this.SetSizeRequest(w, h);
		Cairo.ImageSurface timg = new Cairo.ImageSurface(Format.Argb32, w, h);

		// If the internal image already exists, copy to the new one
		if (_img != null) {
			using (Context g = new Context(timg)) {
				g.SetSource(_img);
				g.Paint ();
			}
		}
		_img = timg;

		_width = w;
		_height = h;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public double map(double n = 0.0, double min1 = 0.0, double max1 = 1.0, double min2 = 0.0, double max2 = 1.0) 
	{	// Map a number from one range to another
		return ((n - min1)/(max1 - min1)) * (max2 - min2) + min2;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public double constrain(double n = 0.0, double min = 0.0, double max = 1.0)
	{	// Constrain a number to a range
		if (n < min) n = min;
		if (n > max) n = max;
		return n;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void smooth() {
		_smooth = true;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void noSmooth() {
		_smooth = false;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private Color _color(double r, double g, double b, double a) 
	{	// Build and return a Color structure mapping from 0-255 to 0.0-1.0
		r = constrain ( map (r, 0.0, 255.0, 0.0, 1.0), 0.0, 1.0);
		g = constrain ( map (g, 0.0, 255.0, 0.0, 1.0), 0.0, 1.0);
		b = constrain ( map (b, 0.0, 255.0, 0.0, 1.0), 0.0, 1.0);
		a = constrain ( map (a, 0.0, 255.0, 0.0, 1.0), 0.0, 1.0);
		Color c = new Color(r, g, b, a);
		return c;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void fill(double r, double g, double b, double a) 
	{	// Set fill color for all drawing moving forward
		_fillColor = _color(r, g, b, a);
		_toFill = true;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void noFill() 
	{	// Turn off fill color
		_toFill = false;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void stroke(double r, double g, double b, double a) 
	{	// Set stroke color for all drawing moving forward
		_strokeColor = _color(r, g, b, a);
		_toStroke = true;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void noStroke() {
		_toStroke = false;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void translate(double tx, double ty)
	{	// Apply a translate transform
		_mat.Translate (tx, ty);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void scale(double sx, double sy)
	{	// Apply a scale transform
		_mat.Scale (sx, sy);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void rotate(double a)
	{	// Apply a scale transform
		_mat.Rotate (a);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void pushMatrix()
	{	// Save the current transformation matrix
		Matrix tmat = (Matrix)_mat.Clone();
		_stack.Add ( tmat );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void popMatrix()
	{	// Restore the last transformation matrix
		if (_stack.Count == 0) return;
		int last = _stack.Count - 1;
		_mat = _stack[last];
		_stack.RemoveAt(last);
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void resetMatrix()
	{	// Reset the current transformation matrix to the identity matrix
		_mat.InitIdentity();
		_stack.Clear ();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public bool focused
	{	// True if window is focused
		get {
			return this.HasToplevelFocus;
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public long frameCount
	{	// Return the number of times the expose event fired on this window
		get {
			return _frameCount;
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public double strokeWeight {
		get { return _strokeWeight; }
		set { _strokeWeight = value; }
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void strokeCap(Cairo.LineCap style) 
	{
		_strokeCap = style;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void strokeJoin(Cairo.LineJoin style) 
	{
		_strokeJoin = style;
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void rectMode(RectMode mode) 
	{
		_rectMode = mode;
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void ellipseMode(EllipseMode mode) 
	{
		_ellipseMode = mode;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void imageMode(ImageMode mode) 
	{
		_imageMode = mode;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void curveTightness(double tightness)
	{
		_tightness = tightness;
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void textSize(double s)
	{
		_textSize = s;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void textAlign(TextAlign align)
	{
		_textAlignX = align;
		_textAlignY = TextYAlign.TOP;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void textAlign(TextAlign align, TextYAlign yalign)
	{
		_textAlignX = align;
		_textAlignY = yalign;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public double textWidth(string txt)
	{	// Return width of string
		double w = 0.0;
		using (Context g = new Context(_img)) {
			TextExtents te = g.TextExtents(txt);
			w = te.Width * _textScaleFactor;
		};
		return w;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void background(double r, double g, double b, double a) 
	{	// Fill background with a color
		using (Context cx = new Context(_img)) {
			cx.Color = _color(r, g, b, a);
			cx.Paint();
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void point(double x, double y) 
	{	// Draw a point and optionally stroke
		using (Context g = new Context(_img)) 
		{
			g.Matrix = _mat;
			g.Save ();
			g.LineCap = LineCap.Round;
			g.MoveTo (x, y);
			g.LineTo (x, y);
			_stroke (g);
			g.Restore ();
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void line(double x1, double y1, double x2, double y2) 
	{	// Draw a line and optionally stroke
		using (Context g = new Context(_img)) {
			g.Matrix = _mat;
			g.MoveTo (x1, y1);
			g.LineTo (x2, y2);
			_stroke (g);
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void rect(double x, double y, double w, double h) 
	{	// Draw a rectangle and optionally fill and stroke
		using (Context g = new Context(_img)) {

			switch (_rectMode) {
			case RectMode.CENTER:
				x = x - 0.5*w;
				y = y - 0.5*h;
				break;
			case RectMode.RADIUS:
				x = x - w;
				y = y - h;
				w = 2.0*w;
				h = 2.0*h;
				break;
			case RectMode.CORNERS:
				w = w - x;	// x2
				h = h - y;	// y2
				break;
			default: //RectMode.CORNER:
				// Nothing to do
				break;
			}

			g.Matrix = _mat;
			g.Rectangle (x, y, w, h);
			_fill (g);
			_stroke (g);
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void triangle(double x1, double y1, double x2, double y2, double x3, double y3)
	{	// Draw a triangle and optionally fill and stroke
		using (Context g = new Context(_img)) {
			g.Matrix = _mat;
			g.MoveTo (x1, y1);
			g.LineTo (x2, y2);
			g.LineTo (x3, y3);
			g.LineTo (x1, y1);
			g.ClosePath();
			_fill (g);
			_stroke (g);
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void quad(double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4)
	{	// Draw a quadrangle and optionally fill and stroke
		using (Context g = new Context(_img)) {
			g.Matrix = _mat;
			g.MoveTo (x1, y1);
			g.LineTo (x2, y2);
			g.LineTo (x3, y3);
			g.LineTo (x4, y4);
			g.LineTo (x1, y1);
			g.ClosePath();
			_fill (g);
			_stroke (g);
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void ellipse(double x, double y, double w, double h) 
	{	// Draw an ellipse and optionally fill and stroke
		using (Context g = new Context(_img)) {
			double hw = 0.5*w;
			double hh = 0.5*h;

			// Init center x/y as if using CORNER mode
			double cx = x + hw;
			double cy = y + hh;

			switch (_ellipseMode) {
			case EllipseMode.CENTER:
				cx = x;
				cy = y;
				break;
			case EllipseMode.RADIUS:
				cx = x;
				cy = y;
				w = 2.0*w;
				h = 2.0*h;
				break;
			case EllipseMode.CORNERS:
				// TODO
				break;
			default: //EllipseMode.CORNER:
				cx = x + hw;
				cy = y + hh;
				break;
			}

			g.Matrix = _mat;
			g.Save();
			
			// Path
			g.Translate(cx, cy);
			g.Scale(1.0, h/w);
			g.MoveTo(hw, 0.0);
			g.Arc(0.0, 0.0, hw, 0.0, 2.0 * Math.PI);
			g.ClosePath();
			
			// Must restore to uniform device space before stroking in order to prevent lines from being deformed by scaling.
			g.Restore();

			// Fill and Stroke
			_fill (g);
			_stroke (g);
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void arc(double x, double y, double w, double h, double start, double stop) 
	{	// Draw an arc
		double hw = 0.5*w;
		double hh = 0.5*h;

		// Init center x/y as if using CORNER mode
		double cx = x + hw;
		double cy = y + hh;

		switch (_ellipseMode) {
		case EllipseMode.CENTER:
			cx = x;
			cy = y;
			break;
		case EllipseMode.RADIUS:
			cx = x;
			cy = y;
			w = 2.0*w;
			h = 2.0*h;
			break;
		case EllipseMode.CORNERS:
			// TODO:
			break;
		default: //EllipseMode.CORNER:
			cx = x + hw;
			cy = y + hh;
			break;
		}

		using (Context g = new Context(_img)) {
			g.Matrix = _mat;

			// Draw the body of the arc, and fill (if to fill)
			g.Save();

			// Path
			g.Translate(cx, cy);
			g.Scale(1.0, h/w);
			g.MoveTo (0.0, 0.0);
			g.LineTo(Math.Cos (start)*hw, Math.Sin (start)*hw);
			g.Arc(0.0, 0.0, hw, start, stop);
			g.ClosePath();
			
			// Must return to uniform device space before stroking in order to prevent lines from being deformed by scaling.
			g.Restore();

			_fill2 (g);		// Use the non-preserving variant of fill
		}

		using (Context g = new Context(_img)) {
			g.Matrix = _mat;

			// Do it again but only stroke the perimeter of the arc
			g.Save();
			
			// Path
			g.Translate(cx, cy);
			g.Scale(1.0, h/w);
			g.MoveTo(Math.Cos (start)*hw, Math.Sin (start)*hw);
			g.Arc(0.0, 0.0, hw, start, stop);
			
			// Must return to uniform device space before stroking in order to prevent lines from being deformed by scaling.
			g.Restore();

			_stroke (g);
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void spline(List<PKnot> knots, bool toClose) 
	{	// Render a spline given a list of knots
		PKnot ph, pi, pj, pk;

		int n = knots.Count;
		if (n == 0) return;

		double t = _tightness;

		// Remove duplicates
		for (int i=n-1; i>0; i--) {
			if (knots[i].x == knots[i-1].x && knots[i].y == knots[i-1].y)
				knots.RemoveAt(i);
		}
		n = knots.Count;

		// Draw the spline
		using (Context g = new Context(_img)) {
			g.Matrix = _mat;
			g.Save();

			// If less than 3 knots, draw a straight line from first to last point, which may be the same point
			if (n < 3) 
			{
				g.MoveTo (knots[0].x, knots[0].y);
				g.LineTo (knots[1].x, knots[1].y);

			} else {	// 3 or more points

				// Start by calculating all angles and distances, even if never used.
				for (int j=0; j<n; j++) {
					int i = (j-1+n) % n;
					int k = (j+1+n) % n;
					pi = knots[i];
					pj = knots[j];
					pk = knots[k];
					double dx = pk.x-pi.x;
					double dy = pk.y-pi.y;

					// If points are the same, then the angle is perpenticular to the segment joining i and j
					if (dx == 0.0 && dy == 0.0) {
						pj.a = Math.Atan2 (pj.y-pi.y, pj.x-pi.x) + Processing.HALF_PI;
					} else {
						pj.a = Math.Atan2 (dy, dx);
					}
					dx = pj.x-pi.x;
					dy = pj.y-pi.y;
					pj.d = Math.Sqrt (dx*dx + dy*dy);
				}

				// Init the end of the drawing loop
				int end = n+1;

				// If not to close the shape, make some adjustments
				if (!toClose) {
					ph = knots[n-2];
					pi = knots[n-1];
					pj = knots[0];
					pk = knots[1];
					pj.a = Math.Atan2 (pk.y-pj.y, pk.x-pj.x);
					pi.a = Math.Atan2 (pi.y-ph.y, pi.x-ph.x);
					end = n;	// Don't draw the last segment
				}

				// Draw all points
				g.MoveTo (knots[0].x, knots[0].y);
				for (int jj=1; jj<end; jj++)
				{
					int i = (jj-1+n) % n;
					int j = (jj  +n) % n;
					int k = (jj+1+n) % n;
					pi = knots[i];
					pj = knots[j];
					pk = knots[k];

					switch (pj.type) {
					case PKnotType.VERTEX:
						g.LineTo (pj.x, pj.y);
						break;

					case PKnotType.BEZIER:
						g.CurveTo (pj.cx1, pj.cy1, pj.cx2, pj.cy2, pj.x, pj.y);
						break;

					case PKnotType.CURVE:
						// TODO: Move draw method to classes?

						// Calculate control points
						pj.cx1 = t*pj.d * Math.Cos (pi.a) + pi.x;
						pj.cy1 = t*pj.d * Math.Sin (pi.a) + pi.y;
						pj.cx2 = pj.x - t*pj.d * Math.Cos (pj.a);
						pj.cy2 = pj.y - t*pj.d * Math.Sin (pj.a);

						g.CurveTo (pj.cx1, pj.cy1, pj.cx2, pj.cy2, pj.x, pj.y);
						break;
					}
				}
			}

			if (toClose) g.ClosePath();
			g.Restore();

			_fill (g);
			_stroke (g);
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public PImage get(int x, int y, int width, int height)
	{	// Create a new image from a portion of the existing image.
		PImage img = new PImage(width, height);

		using (Context g = new Context(img._img)) {
			_img.Show (g, -x, -y);
		}

		return img;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void image(PImage img, double x, double y, double w, double h) 
	{	// Render an image into the rectangle sepcified

		// Calculate position and scale parameters based on image mode
		double ww = img.width ();		// Default is CORNER
		double hh = img.height ();
		double sx = w/ww;
		double sy = h/hh;

		switch (_imageMode) {
		case ImageMode.CENTER:
			x = x - 0.5*w;
			y = y - 0.5*h;
			//sx = w/ww;		// Same as default
			//sy = h/hh;
			break;
		case ImageMode.CORNERS:
			// In this mode, w is x2, h is y2
			sx = (w - x)/ww;
			sy = (h - y)/hh;
			break;
		default: // ImageMode.CORNER:
			break;
		}

		using (Context g = new Context(_img)) {
			g.Matrix = _mat;
			g.Save ();
			g.Translate (x, y);
			g.Scale ( sx, sy );
			img._img.Show (g, 0, 0);
			g.Restore ();
		}
		this.QueueDraw ();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void text(string txt, double x, double y, double w, double h) 
	{	// Draw text to the context

		using (Context g = new Context(_img)) {
			g.Matrix = _mat;
			g.Save ();
			TextExtents te = g.TextExtents(txt);
			double s = (_textSize/g.FontExtents.Height) * _textScaleFactor;

			// If a box is specified, compute the new location of the text within the box using the _textAlign parameter
			switch (_textAlignX) {
			case TextAlign.LEFT:
				//x = x - te.XBearing;
				break;
			case TextAlign.CENTER:
				x = x + 0.5*w - 0.5*te.Width * s + 1.5*s;			// Last factor is fudge factor
				break;
			case TextAlign.RIGHT:
				x = x + w - te.Width * s + te.XBearing * s + 3.0*s;	// Last factor is fudge factor
				break;
			}

			switch (_textAlignY) {
			case TextYAlign.TOP:
				y = y + te.Height * s;
				break;
			case TextYAlign.CENTER:
				y = y + 0.5*h + 0.5*te.Height * s;
				break;
			case TextYAlign.BOTTOM:
				y = y + h + te.YBearing;
				break; 
			case TextYAlign.BASELINE:
				y = y + h;
				break;
			}

			g.Translate (x, y);
			g.Scale (s, s);
			g.TextPath (txt);
			g.Restore ();
			_fill (g);
			_stroke (g);
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void text(string txt, double x, double y) 
	{	// Draw text to the context
		using (Context g = new Context(_img)) {
			g.Matrix = _mat;
			g.Save ();
			g.Translate (x, y);
			double s = (_textSize/g.FontExtents.Height) * _textScaleFactor;
			g.Scale (s, s);
			g.TextPath (txt);
			g.Restore ();
			_fill (g);
			_stroke (g);
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
}

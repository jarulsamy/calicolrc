 /*
 Calico - Scripting Environment

 Copyright (c) 2012, 2013, Mark F. Russo <russomf@gmail.com>

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
 using System.Collections;
 using System.Net;
 using Cairo;
 using Gtk;

 // ------------------ Shared Constants -----------------------------------
 internal enum EllipseMode
 {
	 CENTER = 0,
	 CORNER = 1,
	 RADIUS = 2,
	 CORNERS = 3
 }

 internal enum RectMode
 {
	 CENTER = 0,
	 CORNER = 1,
	 RADIUS = 2,
	 CORNERS = 3
 }

 internal enum ImageMode
 {
	 CENTER = 0,
	 CORNER = 1,
	 CORNERS = 2
 }

 internal enum TextAlign
 {
	 LEFT = 0,
	 CENTER = 1,
	 RIGHT = 2
 }

 internal enum TextYAlign
 {
	 TOP = 0,
	 BOTTOM = 1,
	 CENTER = 2,
	 BASELINE = 3
 }

 // -------------------------------------------------------------------------
 public static class Processing
 {
	 private static PWindow _p = null;							// Reference to internal window
	 private static Random _rand = new Random();					// Random number generation help
	 private static PTimer _tmr = null;
	 private static int _guiThreadId = -1;						// Thread id of window. -1 means not assigned
	 private static Gdk.Pixbuf _pixbuf = null;					// Internal pixbuf used by loadPixels and updatePixels
	 private static List<PKnot> _shape = null;					// Cache of points for a shape under construction

	 private static int _width;									// Cache of window size
	 private static int _height;
	 private static double _mouseX = 0.0;						// Mouse coordinates
	 private static double _mouseY = 0.0;
	 private static double _pmouseX = 0.0;						// Previously saved mouse coordinates
	 private static double _pmouseY = 0.0;
	 private static bool _mousePressed = false;					// True if the mouse was just pressed
	 private static uint _mouseButton = 0;						// 1 for left, 2 for center, 3 for right
	 private static bool _keyPressed = false;
	 private static string _key;
	 private static long _millis;								// The number of milliseconds when the window was created
	 private static bool _immediateMode = true;					// True if all drawing commands trigger a queue draw

	 public readonly static Pixels pixels = new Pixels();

   /*
	 [method: JigsawTab(null)]
	 public static event ButtonReleaseEventHandler onMouseClicked;	// Mouse events
	 [method: JigsawTab(null)]
	 public static event ButtonPressEventHandler onMousePressed;
	 [method: JigsawTab(null)]
	 public static event ButtonReleaseEventHandler onMouseReleased;
	 [method: JigsawTab(null)]
	 public static event MotionNotifyEventHandler onMouseMoved;
	 [method: JigsawTab(null)]
	 public static event MotionNotifyEventHandler onMouseDragged;
	 [method: JigsawTab(null)]
	 public static event KeyPressEventHandler onKeyPressed;		// Key events
	 [method: JigsawTab(null)]
	 public static event KeyReleaseEventHandler onKeyReleased;
	 [method: JigsawTab(null)]
	 public static event EventHandler<PElapsedEventArgs> onLoop;
	 */



	 [method: JigsawTab(null)]
	 public static event VoidDelegate onMouseClicked;	// Mouse events
	 [method: JigsawTab(null)]
	 public static event VoidDelegate onMousePressed;
	 [method: JigsawTab(null)]
	 public static event VoidDelegate onMouseReleased;
	 [method: JigsawTab(null)]
	 public static event VoidDelegate onMouseMoved;
	 [method: JigsawTab(null)]
	 public static event VoidDelegate onMouseDragged;
	 [method: JigsawTab(null)]
	 public static event VoidDelegate onKeyPressed;		// Key events
	 [method: JigsawTab(null)]
	 public static event VoidDelegate onKeyReleased;
	 [method: JigsawTab(null)]
	 public static event VoidDelegate onLoop;


	 public delegate void VoidDelegate ();				// A delegate that takes no args and returns nothing
	 private delegate double DoubleDelegate ();			// A delegate that takes no args and returns a double

	 private delegate uint UintDelegate ();				// A delegate that takes no args and 
	 private delegate PImage PImageDelegate ();
	 private delegate System.Collections.IList IListDelegate();
	 public static int _debugLevel = 2;							// 0: verbose, 1: informational, 2: unhandled exceptions

	 // Constants
	 public static readonly int CENTER = 0;
	 public static readonly int CORNER = 1;
	 public static readonly int RADIUS = 2;
	 public static readonly int CORNERS = 3;
	 public static readonly int SQUARE = 4;
	 public static readonly int ROUND = 5;
	 public static readonly int PROJECT = 6;
	 public static readonly int BEVEL = 7;
	 public static readonly int MITER = 8;
	 public static readonly int RGB = 9;
	 public static readonly int ARGB = 10;
	 public static readonly int ALPHA = 11;
	 public static readonly int RIGHT = 12;
	 public static readonly int LEFT = 13;
	 public static readonly int TOP = 14;
	 public static readonly int BOTTOM = 15;
	 public static readonly int BASELINE = 16;
	 public static readonly bool CLOSE = true;
	 public static readonly double PI = 3.141592653589793238;
	 public static readonly double HALF_PI = 0.5*Processing.PI;
	 public static readonly double QUARTER_PI = 0.25*Processing.PI;
	 public static readonly double TWO_PI = 2.0*Processing.PI;

	 // String-constant maps
	 private static Dictionary<string, RectMode> _rectModeStr = new Dictionary<string,RectMode> () {
		 {"CORNER", RectMode.CORNER}, {"CENTER", RectMode.CENTER}, {"RADIUS", RectMode.RADIUS}, {"CORNERS", RectMode.CORNERS}
	 };

	 private static Dictionary<string, EllipseMode> _ellipseModeStr = new Dictionary<string,EllipseMode> () {
		 {"CORNER", EllipseMode.CORNER}, {"CENTER", EllipseMode.CENTER}, {"RADIUS", EllipseMode.RADIUS}, {"CORNERS", EllipseMode.CORNERS}
	 };

	 private static Dictionary<string, ImageMode> _imageModeStr = new Dictionary<string,ImageMode> () {
		 {"CORNER", ImageMode.CORNER}, {"CENTER", ImageMode.CENTER}, {"CORNERS", ImageMode.CORNERS}
	 };

	 private static Dictionary<string, Cairo.LineCap> _strokeCapStr = new Dictionary<string,Cairo.LineCap> () {
		 {"SQUARE", LineCap.Butt}, {"ROUND", LineCap.Round}, {"PROJECT", LineCap.Square}
	 };

	 private static Dictionary<string, Cairo.LineJoin> _strokeJoinStr = new Dictionary<string,Cairo.LineJoin> () {
		 {"BEVEL", LineJoin.Bevel}, {"ROUND", LineJoin.Round}, {"MITER", LineJoin.Miter}
	 };

	 private static Dictionary<string, Cairo.Format> _imageFormatStr = new Dictionary<string, Format>() {
		 {"RGB", Format.RGB24}, {"ARGB", Format.ARGB32}, {"ALPHA", Format.A8}	//grayscale alpha channel
	 };

	 // Integer-constant maps
	 private static Dictionary<int, RectMode> _rectModeInt = new Dictionary<int,RectMode> () {
		 {CORNER, RectMode.CORNER}, {CENTER, RectMode.CENTER}, {RADIUS, RectMode.RADIUS}, {CORNERS, RectMode.CORNERS}
	 };

	 private static Dictionary<int, EllipseMode> _ellipseModeInt = new Dictionary<int,EllipseMode> () {
		 {CORNER, EllipseMode.CORNER}, {CENTER, EllipseMode.CENTER}, {RADIUS, EllipseMode.RADIUS}, {CORNERS, EllipseMode.CORNERS}
	 };

	 private static Dictionary<int, ImageMode> _imageModeInt = new Dictionary<int,ImageMode> () {
		 {CORNER, ImageMode.CORNER}, {CENTER, ImageMode.CENTER}, {CORNERS, ImageMode.CORNERS}
	 };

	 private static Dictionary<int, Cairo.LineCap> _strokeCapInt = new Dictionary<int,Cairo.LineCap> () {
		 {SQUARE, LineCap.Butt}, {ROUND, LineCap.Round}, {PROJECT, LineCap.Square}
	 };

	 private static Dictionary<int, Cairo.LineJoin> _strokeJoinInt = new Dictionary<int,Cairo.LineJoin> () {
		 {BEVEL, LineJoin.Bevel}, {ROUND, LineJoin.Round}, {MITER, LineJoin.Miter}
	 };

	 private static Dictionary<int, Cairo.Format> _imageFormatInt = new Dictionary<int, Format>() {
		 {RGB, Format.RGB24}, {ARGB, Format.ARGB32}, {ALPHA, Format.A8}
	 };

	 private static Dictionary<string, TextAlign> _textAlignStr = new Dictionary<string, TextAlign>() {
		 {"LEFT", TextAlign.LEFT}, {"CENTER", TextAlign.CENTER}, {"RIGHT", TextAlign.RIGHT}
	 };

	 private static Dictionary<int, TextAlign> _textAlignInt = new Dictionary<int, TextAlign>() {
		 {LEFT, TextAlign.LEFT}, {CENTER, TextAlign.CENTER}, {RIGHT, TextAlign.RIGHT}
	 };

	 private static Dictionary<string, TextYAlign> _textYAlignStr = new Dictionary<string, TextYAlign>() {
		 {"TOP", TextYAlign.TOP}, {"CENTER", TextYAlign.CENTER}, {"BOTTOM", TextYAlign.BOTTOM}, {"BASELINE", TextYAlign.BASELINE}
	 };

	 private static Dictionary<int, TextYAlign> _textYAlignInt = new Dictionary<int, TextYAlign>() {
		 {TOP, TextYAlign.TOP}, {CENTER, TextYAlign.CENTER}, {BOTTOM, TextYAlign.BOTTOM}, {BASELINE, TextYAlign.BASELINE}
	 };

	 // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	 [JigsawTab(null)]
	 public static void log(object msg, string path) {
		 using (StreamWriter w = File.AppendText(path))
		 {
			 w.WriteLine("{0}: {1}", DateTime.Now.ToString(), msg.ToString());
		 }
	 }

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab(null)]
	public static void MsgBox(object msg) {
		Gtk.MessageDialog md = new Gtk.MessageDialog(
			null, 
			Gtk.DialogFlags.DestroyWithParent, 
			Gtk.MessageType.Info, 
			Gtk.ButtonsType.Close, 
			msg.ToString ());
		md.Run();
		md.Destroy();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab(null)]
	public static void set_gui_thread_id (int gui_thread_id)
	{
		_guiThreadId = gui_thread_id;
		PImage.set_gui_thread_id(gui_thread_id);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab(null)]
	public static int get_gui_thread_id ()
	{
		return _guiThreadId;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static void canvas(int w = 400, int h = 300) 
	{
	    window(w, h, false);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static void window(int w = 400, int h = 300) 
	{
	    window(w, h, true);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab(null)]
	public static void window(int w = 400, int h = 300, bool show = true) 
	{
		// If the current thread is the same as the main GUI thread, use the window2() method instead
		if (_guiThreadId == Thread.CurrentThread.ManagedThreadId)
		{
		        window2 (w, h, show);
			return;
		}

		// Create new window. Do not return until window creation is done.
		ManualResetEvent ev = new ManualResetEvent(false);
		Application.Invoke ( delegate {

			// Destroy currently open window, if there is one, and capture current location and size.
			int[] wchars = exit ();

			// Initialize Gtk
			//Application.Init ();

			// Create a new window with appropriate attributes
			if (wchars != null) {
				int x = wchars[0]; 
				int y = wchars[1];
				_p = new PWindow(w, h, x, y, true, show);
			} else {
			    _p = new PWindow(w, h, true, show);
			}

			// Init window parameters
			_width = w;
			_height = h;
			_p.rectMode ( RectMode.CORNER );
			_p.ellipseMode ( EllipseMode.CENTER );
			_p.imageMode ( ImageMode.CORNER );

			// Attach all event handlers
			_p._cvs.ButtonPressEvent += _onButtonPressEvent;
			_p._cvs.ButtonReleaseEvent += _onButtonReleaseEvent;
			_p._cvs.MotionNotifyEvent += _onMotionNotifyEvent;
			_p._cvs.KeyPressEvent += _onKeyPressEvent;
			_p._cvs.KeyReleaseEvent += _onKeyReleaseEvent;
			_p.windowClosed += _onWindowClosed;

			// Show the new window
			if (show)
			    _p.ShowAll ();

			// Signal the manual reset event
			ev.Set ();

			// Run the new window main loop
			//Application.Run ();
		} );

		// Wait to be signalled	
		ev.WaitOne();
		
		// Set up helper objects
		_tmr = new PTimer();
		_tmr.Elapsed += _onLoop;
		
		// Reset all internal state variables
		_mouseX = 0.0;
		_mouseY = 0.0;
		_pmouseX = 0.0;
		_pmouseY = 0.0;
		_mousePressed = false;
		_mouseButton = 0;
		_keyPressed = false;
		_key = ""; //(Gdk.Key)0;
		_immediateMode = true;
		_millis = DateTime.Now.Ticks / 10000;	// Current number of milliseconds since 12:00:00 midnight, January 1, 0001

		frameRate(30);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	//[JigsawTab("P/Environ")]
	[JigsawTab(null)]
	public static void window2(int w = 400, int h = 300, bool show = true) 
	{	// Create window on current thread, not a new one. Jigsaw needs this.

		//_guiThreadId = Thread.CurrentThread.ManagedThreadId;

		// Create new window. Do not return until window creation is done.
		ManualResetEvent ev = new ManualResetEvent(false);

		// Destroy existing window, if there is one.
		int[] wchars = exit ();

		// Create new window.
		if (wchars != null) {
			int x = wchars[0];
			int y = wchars[1];
			_p = new PWindow(w, h, x, y, false);
		} else {
			_p = new PWindow(w, h, false);
		}

		_width = w;
		_height = h;
		_p.rectMode ( RectMode.CORNER );
		_p.ellipseMode ( EllipseMode.CENTER );
		_p.imageMode ( ImageMode.CORNER );

		_p._cvs.ButtonPressEvent += _onButtonPressEvent;
		_p._cvs.ButtonReleaseEvent += _onButtonReleaseEvent;
		_p._cvs.MotionNotifyEvent += _onMotionNotifyEvent;
		_p._cvs.KeyPressEvent += _onKeyPressEvent;
		_p._cvs.KeyReleaseEvent += _onKeyReleaseEvent;
		_p.windowClosed += _onWindowClosed;
		
		if (show)
		    _p.ShowAll ();

		// Signal the manual reset event
		ev.Set ();
		
		// Wait to be signalled	
		ev.WaitOne();

		// Set up helper objects
		_tmr = new PTimer();
		_tmr.Elapsed += _onLoop;
		
		// Reset all internal state variables
		_mouseX = 0.0;
		_mouseY = 0.0;
		_pmouseX = 0.0;
		_pmouseY = 0.0;
		_mousePressed = false;
		_mouseButton = 0;
		_keyPressed = false;
		_key = ""; //(Gdk.Key)0;
		_immediateMode = true;
		_millis = DateTime.Now.Ticks / 10000;	// Current number of milliseconds since 12:00:00 midnight, January 1, 0001
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void _onWindowClosed (object sender, DeleteEventArgs e)
	{	// PWindow was closed on its own. Clean up.
		_cleanup ();
		e.RetVal = false;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void _cleanup ()
	{	// Clean up all the various helper thingys
		if (_tmr != null) {
			_tmr.Stop ();
			_tmr.Elapsed -= _onLoop;
		}
		_tmr = null;

		// Unhook event handlers.
		onLoop = null;
		onMouseClicked = null;
		onMousePressed = null;
		onMouseReleased = null;
		onMouseDragged = null;
		onMouseMoved = null;
		onKeyPressed = null;
		onKeyReleased = null;

		// Reset various other flags and settings
		//_guiThreadId = -1;
		_mousePressed = false;
		_mouseButton = 0;
		_keyPressed = false;
		_millis = 0;

		_p = null;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static int[] exit() 
	{	// Try to destroy and clean up the window
		int x, y, w, h;
		int[] wchars = null;
		if (_p != null) {
			_p.GetSize(out w, out h);		// Get current window characterstics before destroy
			_p.GetPosition(out x, out y);
			wchars = new int[4];
			wchars[0] = x;
			wchars[1] = y;
			wchars[2] = w;
			wchars[3] = h;
			_p.Destroy ();
		}

		// Clean up all other various doo-dads
		_cleanup ();

		return wchars;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void _invoke( VoidDelegate fxn ) 
	{	// Invoke a void delegate on thread if necessary;
		if (Thread.CurrentThread.ManagedThreadId != _guiThreadId)
		{
			Application.Invoke ( delegate{ fxn(); } );
		} else {
			fxn();
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void _invokeDouble( DoubleDelegate fxn ) 
	{	// Invoke a delegate that returns a double on thread if necessary

		double val = 0.0;
		if (Thread.CurrentThread.ManagedThreadId != _guiThreadId)
		{
			Application.Invoke ( delegate{ val = fxn(); } );
		} else {
			fxn();
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void _invokeUint( UintDelegate fxn ) 
	{	// Invoke a delegate that returns a double on thread if necessary

 	        uint val = 0;
		if (Thread.CurrentThread.ManagedThreadId != _guiThreadId)
		{
			Application.Invoke ( delegate{ val = fxn(); } );
		} else {
			fxn();
		}
	}


	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void _invokeIList( IListDelegate fxn ) 
	{	// Invoke a delegate that returns a list of strings on thread if necessary

	        System.Collections.IList val =null;
		if (Thread.CurrentThread.ManagedThreadId != _guiThreadId)
		{
			Application.Invoke ( delegate{ val = fxn(); } );
		} else {
			fxn();
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void _invokePImage( PImageDelegate fxn ) 
	{	// Invoke a delegate that returns a PImage on thread if necessary

		PImage val = null;
		if (Thread.CurrentThread.ManagedThreadId != _guiThreadId)
		{
			Application.Invoke ( delegate{ val = fxn(); } );
		} else {
			fxn();
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void _onLoop (object o, PElapsedEventArgs e)
	{	// Handle timer elapsed events
		raiseTimerElapsed(o, e);
		resetMatrix();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void _onMotionNotifyEvent (object o, Gtk.MotionNotifyEventArgs args)
	{	// Handle mouse motion events
		_pmouseX = _mouseX;
		_pmouseY = _mouseY;
		_mouseX = args.Event.X;
		_mouseY = args.Event.Y;
		if (_mousePressed) {
			raiseMouseDragged(args);
		} else {
			raiseMouseMoved(args);
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void _onButtonPressEvent (object o, Gtk.ButtonPressEventArgs args)
	{	// Reraise the event from the main class
		_mousePressed = true;
		_mouseButton = args.Event.Button;
		_pmouseX = _mouseX;
		_pmouseY = _mouseY;
		_mouseX = args.Event.X;
		_mouseY = args.Event.Y;
		raiseMousePressed(args);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void _onButtonReleaseEvent (object o, ButtonReleaseEventArgs args)
	{	// Reraise the event from the main class
		_mousePressed = false;
		_mouseButton = 0;
		bool _needsMouseClicked = false;
		if ( Math.Abs(_pmouseX - _mouseX) < 2 && Math.Abs (_pmouseY - _mouseY) < 2) _needsMouseClicked = true;
		_pmouseX = _mouseX;
		_pmouseY = _mouseY;
		_mouseX = args.Event.X;
		_mouseY = args.Event.Y;
		raiseMouseReleased(args);
		if (_needsMouseClicked) raiseMouseClicked(args);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void _onKeyPressEvent(object o, KeyPressEventArgs args)
	{
		_keyPressed = true;
		_key = args.Event.Key.ToString();
		raiseKeyPressed(args);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void _onKeyReleaseEvent(object o, KeyReleaseEventArgs args)
	{
	  if (_key == "Escape") //Gdk.Key.Escape)
		  {
		    noLoop();
		    exit();
		  }
		_keyPressed = false;
		raiseKeyReleased(args);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void raiseTimerElapsed(object o, PElapsedEventArgs a)
	{
        VoidDelegate handler = onLoop;

        if (handler != null)
        {
	    try {
		handler();
	    } catch (Exception e) {
		Console.Error.WriteLine("Error in onLoop: " + e.Message);
		Console.Error.WriteLine("loop is now stopping.");
		stopLoop();
	    }
        }
    }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void raiseMouseMoved(MotionNotifyEventArgs e)
    {
        VoidDelegate handler = onMouseMoved;

        // Event will be null if there are no subscribers
        if (handler != null)
        {	// Use the () operator to raise the event.
	    try {
		handler();
	    } catch (Exception exc) {
		Console.Error.WriteLine("Error in onMouseMoved: " + exc.Message);
	    }
        }
    }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void raiseMouseDragged(MotionNotifyEventArgs e)
    {
        VoidDelegate handler = onMouseDragged;

        // Event will be null if there are no subscribers
        if (handler != null)
        {	// Use the () operator to raise the event.
	    try {
		handler();
	    } catch (Exception exc) {
		Console.Error.WriteLine("Error in onMouseDragged: " + exc.Message);
	    }
        }
    }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void raiseMousePressed(ButtonPressEventArgs e)
    {
        VoidDelegate handler = onMousePressed;

        // Event will be null if there are no subscribers
        if (handler != null)
        {	// Use the () operator to raise the event.
	    try {
		handler();
	    } catch (Exception exc) {
		Console.Error.WriteLine("Error in onMousePressed: " + exc.Message);
	    }
        }
    }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void raiseMouseClicked(ButtonReleaseEventArgs e)
	{
		VoidDelegate handler = onMouseClicked;
		
		// Event will be null if there are no subscribers
		if (handler != null)
		{
		    try {
			handler();
		    } catch (Exception exc) {
			Console.Error.WriteLine("Error in onMouseClicked: " + exc.Message);
		    }
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void raiseMouseReleased(ButtonReleaseEventArgs e)
    {
        VoidDelegate handler = onMouseReleased;

        // Event will be null if there are no subscribers
        if (handler != null)
        {	// Use the () operator to raise the event.
	    try {
		handler();
	    } catch (Exception exc) {
		Console.Error.WriteLine("Error in onMouseReleased: " + exc.Message);
	    }
        }
    }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void raiseKeyPressed(KeyPressEventArgs e)
    {
       VoidDelegate handler = onKeyPressed;
        if (handler != null)
        {
	    try {
		handler();
	    } catch (Exception exc) {
		Console.Error.WriteLine("Error in onKeyPressed: " + exc.Message);
	    }
        }
    }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void raiseKeyReleased(KeyReleaseEventArgs e)
    { 
        VoidDelegate handler = onKeyReleased;
        if (handler != null)
        {
	    try {
		handler();
	    } catch (Exception exc) {
		Console.Error.WriteLine("Error in onKeyReleased: " + exc.Message);
	    }
        }
    }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static void beginShape() 
	{	// Start a new shape
		_shape = new List<PKnot>();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static void vertex(double x = 0.0, double y = 0.0)
	{	// Add a simple vertex to a shape under construction
		_shape.Add(new PKnot(x, y, PKnotType.VERTEX));
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static void bezierVertex(double cx1 = 0.0, double cy1 = 0.0, double cx2 = 0.0, double cy2 = 0.0, double x = 0.0, double y = 0.0)
	{	// Add a bezier vertex to a shape under construction
		_shape.Add(new PKnot(x, y, cx1, cy1, cx2, cy2, PKnotType.BEZIER));
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static void curveVertex(double x = 0.0, double y = 0.0)
	{	// Add a curve vertex to the shape
		_shape.Add(new PKnot(x, y, PKnotType.CURVE));
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static void curveTightness(double tightness = 0.0)
	{
		if (_p == null) return;

		_invoke ( delegate { 
			try {
				_p.curveTightness(tightness);
			} catch (Exception e) {
				debug (String.Format ("curveTightness(): {0}", e.Message), 1);
			}
		} );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static void endShape(bool toClose = false)
	{	// Finish up and render
		if (_p == null) return;
		if (_shape == null) return;

		ManualResetEvent ev = new ManualResetEvent(false);
		_invoke ( delegate {
			try {
				_p.spline( _shape, toClose );
				if (_immediateMode) _p.redraw ();
			} catch (Exception e) {
				debug (String.Format ("endShape(): {0}", e.Message), 1);
			}
			ev.Set ();
			return;
		} );
		ev.WaitOne();
	
	}
	[JigsawTab("P/Shapes")]
	public static void endShape() { endShape (false); }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Images")]
	public static PImage createImage(int width = 100, int height = 100, string format = "ARGB") 
	{	// Create a new PImage object given a string format

		// Validate the format string
		if (!_imageFormatStr.ContainsKey(format)) {
			string[] skeys = new string[_imageFormatStr.Count];
			_imageFormatStr.Keys.CopyTo(skeys, 0);
			string jskeys = String.Join (", ", skeys);
			string msg = String.Format ("Unrecognized image format: '{0}'. Try {1}", format, jskeys);
			debug (msg, 2);
			return null;
		}
		Format frmt = _imageFormatStr[format];

		// Create the image
		return new PImage(width, height, frmt);
	}


	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Images")]
	public static PImage createImage(System.Drawing.Bitmap bitmap) {
	    PImage image = new PImage(bitmap.Width, bitmap.Height, _imageFormatStr["ARGB"]);
	    for (int x=0; x < bitmap.Width; x++) {
		for (int y=0; y < bitmap.Height; y++) {
		    System.Drawing.Color pixel = bitmap.GetPixel (x, y);
		    byte r = pixel.R;
		    byte g = pixel.G;
		    byte b = pixel.B;
		    byte a = pixel.A;
		    image.setPixel(x, y, r, g, b, a);
		}
	    }
	    return image;
	}


	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Images")]
	public static PImage createImage(int width = 100, int height = 100, int format = (int)Format.ARGB32)
	{	// Create a new PImage object given an integer format

		// Validate the format int
		if (!_imageFormatInt.ContainsKey(format)) {
			string[] skeys = new string[_imageFormatStr.Count];
			_imageFormatStr.Keys.CopyTo(skeys, 0);
			string jskeys = String.Join (", ", skeys);
			string msg = String.Format ("Unrecognized image format: '{0}'. Try {1}", format, jskeys);
			debug (msg, 2);
			return null;
		}
		Format frmt = _imageFormatInt[format];

		// Create the image
		return new PImage(width, height, frmt);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Images")]
	public static PImage loadImage(string path = "") 
	{	// Load an image into the PImage object

		PImage img = null;

        if (!(path.StartsWith ("http://") || 
              path.StartsWith ("https://") || 
              path.StartsWith("data:"))){
            
            
            // Make a complete path, if not rooted
            if (!System.IO.Path.IsPathRooted(path)) {
                path = System.IO.Path.Combine ( Directory.GetCurrentDirectory(), path );
            }
            
            // Check if the file exists
            if (!File.Exists (path) ) {
                println ("Error: Can't find image file at ", path);
                return null;
            }
        }
        
		// Attempt to open the file
		img = new PImage(path);

		return img;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Images")]
	public static void image(PImage img = null, double x = 0.0, double y = 0.0, double w = 100.0, double h = 100.0) 
	{
		// Draw the image
		if (_p == null) return;
		if (img == null) return;
		_invoke ( delegate { 
			try {
				_p.image (img, x, y, w, h);
			} catch (Exception e) {
				debug (String.Format ("image(): {0}", e.Message), 1);
			}
		} );
	}
	[JigsawTab("P/Images")]
	public static void image(PImage img = null, double x = 0.0, double y = 0.0) { image (img, x, y, img.width (), img.height ()); }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static void keepAbove(bool ku = false)
	{	// Set flag to tell current window if to remain aboove all others
		if (_p == null) return;
		_invoke ( delegate { 
			try {
				_p.KeepAbove = ku;
			} catch (System.NullReferenceException e){
				debug(String.Format ("keepAbove() ignored extra tick: {0}", e.ToString()), 1);
			}
		});
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static bool focused()
	{	// Check if window has focus
		if (_p == null) return false;
		return _p.focused;			// Should this be invoked on GUI thread?
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static long frameCount()
	{	// Return the number of times the exposed event was fired by the PWindow
		if (_p == null) return 0;
		return _p.frameCount;			// Should this be invoked on GUI thread?
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Input")]
	public static double mouseX()
	{	// Get mouse position
		return _mouseX;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Input")]
	public static double mouseY()
	{	// Get mouse position
		return _mouseY;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Input")]
	public static double pmouseX()
	{	// Get previous mouse position
		return _pmouseX;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Input")]
	public static double pmouseY()
	{	// Get previous mouse position
		return _pmouseY;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Input")]
	public static bool isMousePressed()
	{	// True if mouse is currently pressed
		return _mousePressed;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Input")]
	public static uint mouseButton()
	{	// Number of mouse button currently pressed. 1 for left mouse button, 2 for center mouse button, 3 for right mouse button
		return _mouseButton;
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Input")]
	public static bool isKeyPressed()
	{	// True if key is currently pressed
		return _keyPressed;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Input")]
	public static string key()
	{	// Return key pressed
		return _key;
	}

  	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Input")]
	public static string keyCode()
	{	// Return code of key pressed
		return _key;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static void size(int w = 400, int h = 300) 
	{	// Set window size

		// If no window, create one of the proper size
		if (_p == null) {
			window(w, h);
		} else {
			ManualResetEvent ev = new ManualResetEvent(false);
			_invoke ( delegate {
				_p.size(w, h);
				ev.Set ();
			} );
			_width = w;
			_height = h;
			ev.WaitOne();
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static double width() 
	{	// Get the width of the window
		return _width;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static double height() 
	{	// Get the height of the window
		return _height;
	}


	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static double displayWidth() 
	{	// Get the width of the screen
	  return _p.Screen.Width;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static void fullscreen()
        {
	  _invoke ( delegate { if (_p != null) _p.Fullscreen();});
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static void unfullscreen()
        {
	  _invoke ( delegate { if (_p != null) _p.Unfullscreen();});
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static double displayHeight() 
	{	// Get the width of the screen
	  return _p.Screen.Height;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static void immediateMode( bool value = true )
	{
		//Application.Invoke ( delegate { _immediateMode = value; } );
		_invoke ( delegate { _immediateMode = value; } );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static void ellipseMode( string mode = "CENTER" )
	{
		_invoke ( delegate { _p.ellipseMode (_ellipseModeStr[mode]); } );
	}
	[JigsawTab("P/Shapes")]
	public static void ellipseMode( int mode = (int)EllipseMode.CENTER)
	{
		_invoke ( delegate { _p.ellipseMode (_ellipseModeInt[mode]); } );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static void rectMode( string mode = "CORNER" )
	{
		_invoke ( delegate { _p.rectMode (_rectModeStr[mode]); } );
	}
	[JigsawTab("P/Shapes")]
	public static void rectMode( int mode = (int)RectMode.CORNER )
	{
		_invoke ( delegate { _p.rectMode (_rectModeInt[mode]); } );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Images")]
	public static void imageMode( string mode = "CORNER" )
	{
		_invoke ( delegate { _p.imageMode (_imageModeStr[mode]); } );
	}
	[JigsawTab("P/Images")]
	public static void imageMode( int mode = (int)ImageMode.CORNER )
	{
		_invoke ( delegate { _p.imageMode (_imageModeInt[mode]); } );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static void strokeCap( string style = "ROUND" )
	{
		_invoke ( delegate { _p.strokeCap (_strokeCapStr[style]); } );
	}
	[JigsawTab("P/Shapes")]
	public static void strokeCap( int style = (int)LineCap.Round)
	{
		_invoke ( delegate { _p.strokeCap (_strokeCapInt[style]); } );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static void strokeJoin( string style = "MITER" )
	{
		_invoke ( delegate { _p.strokeJoin (_strokeJoinStr[style]); } );
	}
	[JigsawTab("P/Shapes")]
	public static void strokeJoin( int style = (int)LineJoin.Miter)
	{
		_invoke ( delegate { _p.strokeJoin (_strokeJoinInt[style]); } );
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Text")]
	public static void textAlign( string align = "LEFT" )
	{
		_invoke ( delegate { _p.textAlign (_textAlignStr[align]); } );
	}
	[JigsawTab("P/Text")]
	public static void textAlign( int align = (int)TextAlign.LEFT )
	{
		_invoke ( delegate { _p.textAlign (_textAlignInt[align]); } );
	}
	[JigsawTab("P/Text")]
	public static void textAlign( string align = "LEFT", string yalign = "BASELINE" )
	{
		_invoke ( delegate { _p.textAlign (_textAlignStr[align], _textYAlignStr[yalign]); } );
	}
	[JigsawTab("P/Text")]
	public static void textAlign( int align = (int)TextAlign.LEFT, int yalign = (int)TextYAlign.TOP )
	{
		_invoke ( delegate { _p.textAlign (_textAlignInt[align], _textYAlignInt[yalign]); } );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static void redraw()
	{	// Try to cause the window to redraw itself by queuing up a draw
		if (_p == null) return;
		_invoke ( delegate { 
			try {
				_p.redraw ();
			} catch (Exception e) {
				debug (String.Format ("redraw: {0}", e.Message), 1);
			}
		} );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static void delay(int millis = 1000) 
	{	
		Thread.Sleep (millis);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static void doEvents()
	{	// Process any pending events
		while (Gtk.Application.EventsPending ()) Gtk.Application.RunIteration ();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab(null)]
	public static void noLoop()
	{
		if (_tmr != null)
		  {
		    _tmr.Stop ();
		    unfullscreen();
		  }
	}
	[JigsawTab("P/Environ")]
	public static void stopLoop() { noLoop (); }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab(null)]
	public static void loop()
	{
		_tmr.Start ();
	}
	[JigsawTab("P/Environ")]
	public static void startLoop() { loop (); }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static double frameRate(double fps = 15)
	{	// Sets timer interval
		uint fr = Convert.ToUInt32(1000.0/fps);
		bool enabled = _tmr.Enabled;
		_tmr.Stop ();
		_tmr.Interval = fr;
		if (enabled) _tmr.Start ();
		return fps;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static uint frameRate()
	{	// Gets timer interval
		uint fps = Convert.ToUInt32(Convert.ToDouble (_tmr.Interval)/1000.0);
		return fps;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static long millis()
	{	// Gets the number of milliseconds the window has existed, if there is one
		if (_p == null) return 0;
		return (DateTime.Now.Ticks / 10000 - _millis);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static int second()
	{	// Gets the number of seconds on the system clock [0, 59]
		return DateTime.Now.Second;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static int minute()
	{	// Gets the number of minutes on the system clock [0, 59]
		return DateTime.Now.Minute;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static int hour()
	{	// Gets the number of hours on the system clock [0, 23]
		return DateTime.Now.Hour;
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static int day()
	{	// Gets the number of days on the system clock [1, 31]
		return DateTime.Now.Day;
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static int month()
	{	// Gets the number of months on the system clock [1, 12]
		return DateTime.Now.Month;
	}
		
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static int year()
	{	// Gets the year
		return DateTime.Now.Year;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Data")]
	public static bool toBoolean(object o = null)
	{	// Try to convert the object to a boolean
		return Convert.ToBoolean (o);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Data")]
	public static byte toByte(object o = null)
	{	// Try to convert the object to a byte
		return Convert.ToByte (o);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Data")]
	public static char toChar(object o = null)
	{
		return Convert.ToChar(o);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Data")]
	public static Single toFloat(object o = null)
	{
		return Convert.ToSingle (o);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Data")]
	public static double toDouble(object o = null)
	{
		return Convert.ToDouble (o);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Data")]
	public static Int32 toInt(object o = null)
	{
		return Convert.ToInt32 (o);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Data")]
	public static Int64 toLong(object o = null)
	{
		return Convert.ToInt64 (o);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Data")]
	public static string toString(object o = null)
	{
		return Convert.ToString (o);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Data")]
	public static string toHex(object o = null)
	{
		int v = Convert.ToInt32(o);
		return String.Format("{0:X}", v);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Data")]
	public static string toBinary(object o = null)
	{
		int v = Convert.ToInt32(o);
		return Convert.ToString (v, 2);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Data")]
	public static Int32 fromHex(string hex = "0")
	{
		 return Convert.ToInt32(hex, 16);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Data")]
	public static Int32 fromBinary(string bits = "0")
	{
		int bitCount = 8;
		if (bits.Length == bitCount && bits[0] == '1')
            return Convert.ToInt32(bits.PadLeft(32, '1'),2);
        else
            return Convert.ToInt32(bits,2);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Data")]
	public static void save(string filename) {
		if (_p == null) return;
		_invoke ( delegate { 
			try {
			  PImage img = _p.get (0, 0, _width, _height);
			  img.save(filename);
			} catch (System.NullReferenceException e){
				debug(String.Format ("save() ignored extra tick: {0}", e.ToString()), 1);
			}
		});
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static void smooth() {
		if (_p == null) return;
		//Application.Invoke ( delegate {
		_invoke ( delegate { 
			try {
				_p.smooth ();
			} catch (System.NullReferenceException e){
				debug(String.Format ("smooth() ignored extra tick: {0}", e.ToString()), 1);
			}
		});
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static void noSmooth() {
		if (_p == null) return;
		//Application.Invoke ( delegate { 
		_invoke ( delegate { 
			try {
				_p.noSmooth ();
			} catch (System.NullReferenceException e){
				debug(String.Format ("noSmooth() ignored extra tick: {0}", e.ToString()), 1);
			}
		});
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//	[JigsawTab("P/IO")]
//	public static void print(params object[] items) 
//	{	// Print a message to the console
//		string[] sitems = new string[items.Length];
//		for (int i=0; i<items.Length; i++) sitems[i] = items[i].ToString ();
//		Console.Write ( String.Join(" ", sitems) );
//	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab(null)]
	public static void println(params object[] items) 
	{	// Print a message to the console
		string[] sitems = new string[items.Length];
		for (int i=0; i<items.Length; i++) sitems[i] = items[i].ToString ();
		Console.WriteLine ( String.Join(" ", sitems) );
	}

	public static void println(IList items) 
	{	// Print a message to the console
		string[] sitems = new string[items.Count];
		for (int i=0; i<items.Count; i++) sitems[i] = items[i].ToString ();
		Console.WriteLine ( String.Join(" ", sitems) );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	internal static void debug(string msg, int level) 
	{	// 0: verbose, 1: information, 2: serious exceptions
		if (level >= _debugLevel) Console.WriteLine (msg);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Color")]
	public static void fill(double r = 255.0, double g = 255.0, double b = 255.0, double a = 255.0) 
	{	// Set fill color for all drawing moving forward
		if (_p == null) return;
		//Application.Invoke ( delegate {
		_invoke ( delegate { 
			try {
				_p.fill (r, g, b, a);
			} catch (System.NullReferenceException e){
				debug(String.Format ("fill() ignored extra tick: {0}", e.ToString()), 1);
			}
		});
	}
	[JigsawTab("P/Color")]
	public static void fill(double r = 255.0, double g = 255.0, double b = 255.0) { fill (r, g, b, 255.0); }
	[JigsawTab("P/Color")]
	public static void fill(double g = 255.0, double a = 255.0) { fill (g, g, g, a); }
	[JigsawTab("P/Color")]
	public static void fill(double g = 255.0) { fill (g, g, g, 255); }
	[JigsawTab("P/Color")]
	public static void fill( uint c = 255 ) { fill ( red (c), green (c), blue (c), alpha (c) ); }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Color")]
	public static void noFill()
	{	// Turn off fill color
		if (_p == null) return;
		//Application.Invoke ( delegate {
		_invoke ( delegate { 
			try {
				_p.noFill ();
			} catch (System.NullReferenceException e){
				debug(String.Format ("noFill() ignored extra tick: {0}", e.ToString()), 1);
			}
		});
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Color")]
	public static void stroke(double r = 255.0, double g = 255.0, double b = 255.0, double a = 255.0) 
	{	// Set stroke color for all drawing moving forward
		if (_p == null) return;
		//Application.Invoke ( delegate {
		_invoke ( delegate { 
			try {
				_p.stroke (r, g, b, a);
			} catch (System.NullReferenceException e){
				debug(String.Format ("stroke() ignored extra tick: {0}", e.ToString()), 1);
			}
		});
	}
	[JigsawTab("P/Color")]
	public static void stroke(double r = 255.0, double g = 255.0, double b = 255.0) { stroke (r, g, b, 255); }
	[JigsawTab("P/Color")]
	public static void stroke(double g = 255.0, double a = 255.0) { stroke (g, g, g, a); }
	[JigsawTab("P/Color")]
	public static void stroke(double g = 255.0) { stroke (g, g, g, 255); }
	[JigsawTab("P/Color")]
	public static void stroke( uint c = 255) { stroke ( red (c), green (c), blue (c), alpha (c) ); }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Color")]
	public static void noStroke() 
	{	// Turn off stroke
		if (_p == null) return;
		//Application.Invoke ( delegate {
		_invoke ( delegate { 
			try {
				_p.noStroke ();
			} catch (System.NullReferenceException e){
				debug(String.Format ("noStroke() ignored extra tick: {0}", e.ToString()), 1);
			}
		});
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static double strokeWeight(double w = 1.0) 
	{	// Set and/or return stroke weight
		if (_p == null) return -1.0;
		double cw = w;

		ManualResetEvent ev = new ManualResetEvent(false);
		_invoke ( delegate { 
			try {
				cw = _p.strokeWeight;			// Copy current value
				if (w >= 0.0) {					// If new value is valid
					_p.strokeWeight = w;		// Set new value
					cw = w;						// Copy new value into current
				}
			} catch (System.NullReferenceException e){
				debug(String.Format ("strokeWeight() ignored extra tick: {0}", e.ToString()), 1);
			}
			ev.Set ();
		});
		ev.WaitOne ();
		return cw;								// Return current value
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static void pushMatrix() 
	{
		if (_p == null) return;
		_invoke ( delegate { 
			try {
				_p.pushMatrix ();
			} catch (System.NullReferenceException e){
				debug(String.Format ("pushMatrix() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static void popMatrix() 
	{
		if (_p == null) return;
		_invoke ( delegate { 
			try {
				_p.popMatrix ();
			} catch (System.NullReferenceException e){
				debug ( String.Format("popMatrix() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static void resetMatrix() 
	{
		if (_p == null) return;
		_invoke ( delegate { 
			try {
				_p.resetMatrix ();
			} catch (System.NullReferenceException e){
				debug ( String.Format("resetMatrix() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static void translate(double tx = 0.0, double ty = 0.0) 
	{
		if (_p == null) return;
		_invoke ( delegate { 
			try {
				_p.translate (tx, ty);
			} catch (System.NullReferenceException e){
				debug ( String.Format("translate() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}
		
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static void scale(double sx = 1.0, double sy = 1.0) 
	{
		if (_p == null) return;
		_invoke ( delegate { 
			try {
				_p.scale (sx, sy);
			} catch (System.NullReferenceException e){
				debug ( String.Format("scale() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}
	[JigsawTab("P/Environ")]
	public static void scale(double s = 1.0) { scale (s, s); }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Environ")]
	public static void rotate(double a = 0.0) 
	{
		if (_p == null) return;
		_invoke ( delegate { 
			try {
				_p.rotate (a);
			} catch (System.NullReferenceException e){
				debug ( String.Format("rotate() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Color")]
	public static void background(double r = 255.0, double g = 255.0, double b = 255.0, double a = 255.0) 
	{	// Fill the background of the window
		if (_p == null) return;
		_invoke ( delegate { 
			try {
				_p.background(r, g, b, a);
				if (_immediateMode) _p.redraw ();
			} catch (System.NullReferenceException e){
				debug ( String.Format ("background() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}
	[JigsawTab("P/Color")]
	public static void background(double r = 255.0, double g = 255.0, double b = 255.0) { background (r, g, b, 255.0); }
	[JigsawTab("P/Color")]
	public static void background(double g = 255.0, double a = 255.0) { background (g, g, g, a); }
	[JigsawTab("P/Color")]
	public static void background(double g = 255.0) { background (g, g, g, 255.0); }
	[JigsawTab("P/Color")]
	public static void background( uint c = 255) { background ( red (c), green (c), blue (c), alpha (c) ); }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static void point(double x = 0.0, double y = 0.0) 
	{	// Draw a point
		if (_p == null) return;
		_invoke ( delegate { 
			try {
				_p.point(x, y);
				if (_immediateMode) _p.redraw ();
			} catch (System.NullReferenceException e){
				debug ( String.Format("point() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static void line(double x1 = 0.0, double y1= 0.0, double x2= 100.0, double y2 = 100.0) 
	{	// Draw a line
		if (_p == null) return;

		_invoke ( delegate { 
			try {
				_p.line(x1, y1, x2, y2);
				if (_immediateMode) _p.redraw ();
			} catch (System.NullReferenceException e){
				debug ( String.Format("line() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static void ellipse(double x = 0.0, double y = 0.0, double w = 100.0, double h = 100.0) 
	{	// Draw an ellipse
		if (_p == null) return;

		_invoke ( delegate { 
			try {
				_p.ellipse(x, y, w, h);
				if (_immediateMode) _p.redraw ();
			} catch (System.NullReferenceException e){
				debug ( String.Format("ellipse() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static void arc(double x = 0.0, double y = 0.0, double w = 100.0, double h = 100.0, double start = 100.0, double stop = 3.1415926) 
	{	// Draw an ellipse
		if (_p == null) return;
		_invoke ( delegate { 
			try {
				_p.arc(x, y, w, h, start, stop);
				if (_immediateMode) _p.redraw ();
			} catch (System.NullReferenceException e){
				debug ( String.Format("arc() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static void rect(double x = 0.0, double y = 0.0, double w = 100.0, double h = 100.0) 
	{	// Draw a rectangle
		if (_p == null) return;

		_invoke ( delegate { 
			try {
				_p.rect(x, y, w, h);
				if (_immediateMode) _p.redraw ();
			} catch (System.NullReferenceException e){
				debug ( String.Format("rect() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static void triangle(double x1 = 0.0, double y1 = 0.0, double x2 = 0.0, double y2 = 100.0, double x3 = 100.0, double y3 = 100.0) 
	{	// Draw a triangle
		if (_p == null) return;
		_invoke ( delegate { 
			try {
				_p.triangle(x1, y1, x2, y2, x3, y3);
				if (_immediateMode) _p.redraw ();
			} catch (System.NullReferenceException e){
				debug ( String.Format("triangle() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Shapes")]
	public static void quad(double x1 = 0.0, double y1 = 0.0, double x2 = 0.0, double y2 = 100.0, double x3 = 100.0, double y3 = 100.0, double x4 = 100.0, double y4 = 0.0) 
	{	// Draw a quad
		if (_p == null) return;
		_invoke ( delegate { 
			try {
				_p.quad(x1, y1, x2, y2, x3, y3, x4, y4);
				if (_immediateMode) _p.redraw ();
			} catch (System.NullReferenceException e){
				debug(String.Format ("quad() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Text")]
	public static void textSize(double s = 12.0) 
	{
		if (_p == null) return;
		_invoke ( delegate { 
			try {
				_p.textSize (s);
			} catch (System.NullReferenceException e){
				debug ( String.Format("textSize() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}

  	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public static object listFonts()
        { 
	  System.Collections.IList l = new IronPython.Runtime.List();
	  //System.Collections.IList l = new System.Collections.ArrayList();
		if (_p == null) return l;
		ManualResetEvent ev = new ManualResetEvent(false);

		_invokeIList ( delegate { 
			try {
			        l = _p.listFonts();
			} catch (System.NullReferenceException e){
				debug ( String.Format("listFonts() ignored extra tick: {0}", e.ToString()), 1);
			}
			ev.Set ();
			return l;
		} );
		ev.WaitOne();
		return l;
        }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Text")]
	public static void textFont(string s = "Arial") 
	{
		if (_p == null) return;
		_invoke ( delegate { 
			try {
				_p.textFont (s);
			} catch (System.NullReferenceException e){
				debug ( String.Format("textFont() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Text")]
	public static double textWidth( string txt = "CALICO" ) 
	{
		if (_p == null) return -1.0;
		ManualResetEvent ev = new ManualResetEvent(false);

		double w = -1.0;
		_invokeDouble ( delegate { 
			try {
				w = _p.textWidth (txt);
			} catch (System.NullReferenceException e){
				debug ( String.Format("textWidth() ignored extra tick: {0}", e.ToString()), 1);
			}
			ev.Set ();
			return w;
		} );
		ev.WaitOne();
		return w;
	}


	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Text")]
	public static double textHeight( string txt = "CALICO" ) 
	{
		if (_p == null) return -1.0;
		ManualResetEvent ev = new ManualResetEvent(false);

		double w = -1.0;
		_invokeDouble ( delegate { 
			try {
				w = _p.textHeight (txt);
			} catch (System.NullReferenceException e){
				debug ( String.Format("textHeight() ignored extra tick: {0}", e.ToString()), 1);
			}
			ev.Set ();
			return w;
		} );
		ev.WaitOne();
		return w;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Text")]
	public static void text(object obj, double x = 0.0, double y = 100.0, double w= -1, double h = -1) 
	{	// Draw text
		if (_p == null) return;
		string txt = obj.ToString ();
		_invoke ( delegate { 
			try {
				_p.text(txt, x, y, w, h);
				if (_immediateMode) _p.redraw ();
			} catch (System.NullReferenceException e){
				debug(String.Format ("text() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}

	[JigsawTab("P/Text")]
	public static void text(object obj, double x = 0.0, double y = 100.0) 
	{	// Draw text
		if (_p == null) return;
		string txt = obj.ToString ();
		_invoke ( delegate { 
			try {
				_p.text(txt, x, y);
				if (_immediateMode) _p.redraw ();
			} catch (System.NullReferenceException e){
				debug(String.Format ("text() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Text")]
	public static void markup(object obj, double x = 0.0, double y = 100.0, double w= 100.0, double h = 20.0) 
	{	// Draw text
		if (_p == null) return;
		string txt = obj.ToString ();
		_invoke ( delegate { 
			try {
				_p.markup(txt, x, y, w, h);
				if (_immediateMode) _p.redraw ();
			} catch (System.NullReferenceException e){
				debug(String.Format ("markup() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}

	[JigsawTab("P/Text")]
	public static void markup(object obj, double x = 0.0, double y = 100.0) 
	{	// Draw text
		if (_p == null) return;
		string txt = obj.ToString ();
		_invoke ( delegate { 
			try {
				_p.markup(txt, x, y);
				if (_immediateMode) _p.redraw ();
			} catch (System.NullReferenceException e){
				debug(String.Format ("markup() ignored extra tick: {0}", e.ToString()), 1);
			}
		} );
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Math")]
	public static double random(double min = 0.0, double max = 1.0) 
	{	// Generate a random number between min and max
		return map( _rand.NextDouble(), 0.0, 1.0, min, max );
	}
	[JigsawTab("P/Math")]
	public static double random(double max = 1.0) { return random (0.0, max); }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Math")]
	public static void randomSeed(int seed) {
		_rand = new Random(seed);
	}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public class Pixels
	{
      
	  public int length
	  {
	    get
	      {
		return _width * _height;
	      }	    
	  }
	  public uint this[int i]
	  {
            get
	      {		
                int x = i % _width;
                int y = i / _width;
                return getPixel (x, y);
	      }
            set
	      {
		
                int x = i % _width;
                int y = i / _width;
                setPixel (x, y, value);
	      }
	  }
	  
	  public uint this[int x, int y]
	  {	    
            get
	      {	
                return getPixel (x, y);
	      }
            set
	      {	
                setPixel (x, y, value);
	      }
	  }
	}
  


    //////////////////////////////////////////////////////////////
    
    // from Processing

    // PERLIN NOISE
    
    // [toxi 040903]
    // octaves and amplitude amount per octave are now user controlled
    // via the noiseDetail() function.
    
    // [toxi 030902]
    // cleaned up code and now using bagel's cosine table to speed up
    
    // [toxi 030901]
    // implementation by the german demo group farbrausch
    // as used in their demo "art": http://www.farb-rausch.de/fr010src.zip
    
    static readonly int PERLIN_YWRAPB = 4;
    static readonly int PERLIN_YWRAP = 1<<PERLIN_YWRAPB;
    static readonly int PERLIN_ZWRAPB = 8;
    static readonly int PERLIN_ZWRAP = 1<<PERLIN_ZWRAPB;
    static readonly int PERLIN_SIZE = 4095;
    
    static int perlin_octaves = 4; // default to medium smooth
    static float perlin_amp_falloff = 0.5f; // 50% reduction/octave
    
    // [toxi 031112]
    // new vars needed due to recent change of cos table in PGraphics
    static int perlin_TWOPI, perlin_PI;
    static float[] perlin_cosTable;
    static float[] perlin;
    
    static Random perlinRandom;
    static float DEG_TO_RAD = 0.01745329238474369f; 
    static float SINCOS_PRECISION = 0.5f;
    static int SINCOS_LENGTH = (int) (360f / SINCOS_PRECISION);
    
    static float[] makeCOSArray() {
	float[] temp = new float[SINCOS_LENGTH];
	for (int i = 0; i < SINCOS_LENGTH; i++) {
	    //sinLUT[i] = (float) Math.sin(i * DEG_TO_RAD * SINCOS_PRECISION);
	    //cosLUT[i] = (float) Math.cos(i * DEG_TO_RAD * SINCOS_PRECISION);
	    temp[i] = (float) Math.Cos(i * DEG_TO_RAD * SINCOS_PRECISION);
	}
	return temp;
    }

    static float [] cosLUT = makeCOSArray(); //new float[SINCOS_LENGTH] {

    /**
     */
    public static float noise(float x) {
	// is this legit? it's a dumb way to do it (but repair it later)
	return noise(x, 0f, 0f);
    }
    
    /**
     */
    public static float noise(float x, float y) {
	return noise(x, y, 0f);
    }
    
    /**
     * ( begin auto-generated from noise.xml )
     *
     * Returns the Perlin noise value at specified coordinates. Perlin noise is
     * a random sequence generator producing a more natural ordered, harmonic
     * succession of numbers compared to the standard <b>random()</b> function.
     * It was invented by Ken Perlin in the 1980s and been used since in
     * graphical applications to produce procedural textures, natural motion,
     * shapes, terrains etc.<br /><br /> The main difference to the
     * <b>random()</b> function is that Perlin noise is defined in an infinite
     * n-dimensional space where each pair of coordinates corresponds to a
     * fixed semi-random value (fixed only for the lifespan of the program).
     * The resulting value will always be between 0.0 and 1.0. Processing can
     * compute 1D, 2D and 3D noise, depending on the number of coordinates
     * given. The noise value can be animated by moving through the noise space
     * as demonstrated in the example above. The 2nd and 3rd dimension can also
     * be interpreted as time.<br /><br />The actual noise is structured
     * similar to an audio signal, in respect to the function's use of
     * frequencies. Similar to the concept of harmonics in physics, perlin
     * noise is computed over several octaves which are added together for the
     * final result. <br /><br />Another way to adjust the character of the
     * resulting sequence is the scale of the input coordinates. As the
     * function works within an infinite space the value of the coordinates
     * doesn't matter as such, only the distance between successive coordinates
     * does (eg. when using <b>noise()</b> within a loop). As a general rule
     * the smaller the difference between coordinates, the smoother the
     * resulting noise sequence will be. Steps of 0.005-0.03 work best for most
     * applications, but this will differ depending on use.
     *
     * ( end auto-generated )
     *
     * @webref math:random
     * @param x x-coordinate in noise space
     * @param y y-coordinate in noise space
     * @param z z-coordinate in noise space
     * @see PApplet#noiseSeed(long)
     * @see PApplet#noiseDetail(int, float)
     * @see PApplet#random(float,float)
     */

    public static float noise(float x, float y, float z) {
	if (perlin == null) {
	    if (perlinRandom == null) {
		perlinRandom = new Random();
	    }
	    perlin = new float[PERLIN_SIZE + 1];
	    for (int i = 0; i < PERLIN_SIZE + 1; i++) {
		perlin[i] = (float)perlinRandom.NextDouble(); //(float)Math.random();
	    }
	    // [toxi 031112]
	    // noise broke due to recent change of cos table in PGraphics
	    // this will take care of it
	    perlin_cosTable = cosLUT;
	    perlin_TWOPI = perlin_PI = SINCOS_LENGTH;
	    perlin_PI >>= 1;
	}
	
	if (x<0) x=-x;
	if (y<0) y=-y;
	if (z<0) z=-z;
	
	int xi=(int)x, yi=(int)y, zi=(int)z;
	float xf = x - xi;
	float yf = y - yi;
	float zf = z - zi;
	float rxf, ryf;
	
	float r=0;
	float ampl=0.5f;
	
	float n1,n2,n3;
	
	for (int i=0; i<perlin_octaves; i++) {
	    int of=xi+(yi<<PERLIN_YWRAPB)+(zi<<PERLIN_ZWRAPB);
	    
	    rxf=noise_fsc(xf);
	    ryf=noise_fsc(yf);
	    
	    n1  = perlin[of&PERLIN_SIZE];
	    n1 += rxf*(perlin[(of+1)&PERLIN_SIZE]-n1);
	    n2  = perlin[(of+PERLIN_YWRAP)&PERLIN_SIZE];
	    n2 += rxf*(perlin[(of+PERLIN_YWRAP+1)&PERLIN_SIZE]-n2);
	    n1 += ryf*(n2-n1);
	    
	    of += PERLIN_ZWRAP;
	    n2  = perlin[of&PERLIN_SIZE];
	    n2 += rxf*(perlin[(of+1)&PERLIN_SIZE]-n2);
	    n3  = perlin[(of+PERLIN_YWRAP)&PERLIN_SIZE];
	    n3 += rxf*(perlin[(of+PERLIN_YWRAP+1)&PERLIN_SIZE]-n3);
	    n2 += ryf*(n3-n2);
	    
	    n1 += noise_fsc(zf)*(n2-n1);
	    
	    r += n1*ampl;
	    ampl *= perlin_amp_falloff;
	    xi<<=1; xf*=2;
	    yi<<=1; yf*=2;
	    zi<<=1; zf*=2;
	    
	    if (xf>=1.0f) { xi++; xf--; }
	    if (yf>=1.0f) { yi++; yf--; }
	    if (zf>=1.0f) { zi++; zf--; }
	}
	return r;
    }
    
    // [toxi 031112]
    // now adjusts to the size of the cosLUT used via
    // the new variables, defined above
    private static float noise_fsc(float i) {
	// using bagel's cosine table instead
	return 0.5f*(1.0f-perlin_cosTable[(int)(i*perlin_PI)%perlin_TWOPI]);
    }
    
    // [toxi 040903]
    // make perlin noise quality user controlled to allow
    // for different levels of detail. lower values will produce
    // smoother results as higher octaves are surpressed
    
    /**
     * ( begin auto-generated from noiseDetail.xml )
     *
     * Adjusts the character and level of detail produced by the Perlin noise
     * function. Similar to harmonics in physics, noise is computed over
     * several octaves. Lower octaves contribute more to the output signal and
     * as such define the overal intensity of the noise, whereas higher octaves
     * create finer grained details in the noise sequence. By default, noise is
     * computed over 4 octaves with each octave contributing exactly half than
     * its predecessor, starting at 50% strength for the 1st octave. This
     * falloff amount can be changed by adding an additional function
     * parameter. Eg. a falloff factor of 0.75 means each octave will now have
     * 75% impact (25% less) of the previous lower octave. Any value between
     * 0.0 and 1.0 is valid, however note that values greater than 0.5 might
     * result in greater than 1.0 values returned by <b>noise()</b>.<br /><br
     * />By changing these parameters, the signal created by the <b>noise()</b>
     * function can be adapted to fit very specific needs and characteristics.
     *
     * ( end auto-generated )
     * @webref math:random
     * @param lod number of octaves to be used by the noise
     * @param falloff falloff factor for each octave
     * @see PApplet#noise(float, float, float)
     */
    public static void noiseDetail(int lod) {
	if (lod>0) perlin_octaves=lod;
    }
    
    /**
     * @param falloff falloff factor for each octave
     */
    public static void noiseDetail(int lod, float falloff) {
	if (lod>0) perlin_octaves=lod;
	if (falloff>0) perlin_amp_falloff=falloff;
    }
    
    /**
     * ( begin auto-generated from noiseSeed.xml )
     *
     * Sets the seed value for <b>noise()</b>. By default, <b>noise()</b>
     * produces different results each time the program is run. Set the
     * <b>value</b> parameter to a constant to return the same pseudo-random
     * numbers each time the software is run.
     *
     * ( end auto-generated )
     * @webref math:random
     * @param seed seed value
     * @see PApplet#noise(float, float, float)
     * @see PApplet#noiseDetail(int, float)
     * @see PApplet#random(float,float)
     * @see PApplet#randomSeed(long)
     */
    public static void noiseSeed(int seed) {
	if (perlinRandom == null) perlinRandom = new Random(seed);
	//perlinRandom.Seed(seed);
	// force table reset after changing the random number seed [0122]
	perlin = null;
    }
    
    // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Math")]
	public static double map(double n = 0.0, double min1 = 0.0, double max1 = 1.0, double min2 = 0.0, double max2 = 1.0) 
	{	// Map a number from one range to another
		return ((n - min1)/(max1 - min1)) * (max2 - min2) + min2;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Math")]
	public static double constrain(double n = 0.0, double min = 0.0, double max = 1.0) 
	{	// Constrain a number to a range
		if (n < min) n = min;
		if (n > max) n = max;
		return n;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Math")]
	public static double mag(double x = 0.0, double y = 0.0)
	{	// Computes magnitude of a vector
		return Math.Sqrt (x*x + y*y);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Math")]
	public static double dist(double x1 = 0.0, double y1 = 0.0, double x2 = 0.0, double y2 = 0.0)
	{	// Computes the distance between two points
		double dx = (x2 - x1);
		double dy = (y2 - y1);
		return mag (dx, dy);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Math")]
	public static double max(params double[] vals)
	{	// Compute the maximum value of several numbers
		double tmax = vals[0];
		for (int i=1; i<vals.Length; i++) tmax = Math.Max (tmax, vals[i]);
		return tmax;
	}

	public static double max(IList vals)
	{	// Compute the maximum value of several numbers
		double tmax = System.Convert.ToDouble(vals[0]);
		for (int i=1; i<vals.Count; i++) tmax = Math.Max (tmax, System.Convert.ToDouble(vals[i]));
		return tmax;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Math")]
	public static double min(params double[] vals)
	{	// Compute the minimum value of several numbers
		double tmin = vals[0];
		for (int i=1; i<vals.Length; i++) tmin = Math.Min (tmin, vals[i]);
		return tmin;
	}

	public static double min(IList vals)
	{	// Compute the minimum value of several numbers
	        double tmin = System.Convert.ToDouble(vals[0]);
		for (int i=1; i<vals.Count; i++) tmin = Math.Min (tmin, System.Convert.ToDouble(vals[i]));
		return tmin;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Math")]
	public static int floor(double val = 0.0)
	{
		return (int)Math.Floor (val);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Math")]
	public static int ceil(double val = 0.0)
	{
		return (int)Math.Ceiling (val);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Math")]
	public static double round(double val = 0.0)
	{
		return round (val, 0);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Math")]
	public static double round(double val = 0.0, int ndigits = 1)
	{
		return Math.Round (val, ndigits);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Math")]
	public static double radians(double degrees = 0.0)
	{
		return degrees * (PI/180.0);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Math")]
	public static double degrees(double radians = 0.0)
	{
		return radians * (180.0/PI);
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Color")]
	public static uint color(byte r = 255, byte g = 255, byte b = 255, byte a = 255)
	{	// Create color from color byte components
		 return (uint)( b | (g << 8) | (r << 16) | (a << 24));
	}
	[JigsawTab("P/Color")]
	public static uint color(byte r = 255, byte g = 255, byte b = 255) { return color(r, g, b, 255); }
	[JigsawTab("P/Color")]
	public static uint color(byte g = 255, byte a = 255) { return color(g, g, g, a); }
	[JigsawTab("P/Color")]
	public static uint color(byte g = 255) { return color(g, g, g, 255); }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Color")]
	public static byte blue(uint color = 255)
	{	// Extract color byte from a color (unsigned int)
		 return (byte)((color & 0x000000FF));
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Color")]
	public static byte green(uint color = 255)
	{	// Extract color byte from a color (unsigned int)
		 return (byte)((color & 0x0000FF00) >> 8);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Color")]
	public static byte red(uint color = 255)
	{	// Extract color byte from a color (unsigned int)
		 return (byte)((color & 0x00FF0000) >> 16);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTabAttribute("P/Color")]
	public static byte alpha(uint color = 255)
	{	// Extract color byte from a color (unsigned int)
		 return (byte)((color & 0xFF000000) >> 24);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void _HSB2color( float hu, float sa, float br, out uint c)
	{	// Build an RGBA color from hue saturation and brightness

		// Adapted from http://www.unifycommunity.com/wiki/index.php?title=HSBColor
        float r = br;
        float g = br;
        float b = br;

        if (sa != 0)
        {
            float max = br;
            float dif = br * sa;
            float min = br - dif;

            float h = hu * 360f;

            if (h < 60f)
            {
                r = max;
                g = h * dif / 60f + min;
                b = min;
            }
            else if (h < 120f)
            {
                r = -(h - 120f) * dif / 60f + min;
                g = max;
                b = min;
            }
            else if (h < 180f)
            {
                r = min;
                g = max;
                b = (h - 120f) * dif / 60f + min;
            }
            else if (h < 240f)
            {
                r = min;
                g = -(h - 240f) * dif / 60f + min;
                b = max;
            }
            else if (h < 300f)
            {
                r = (h - 240f) * dif / 60f + min;
                g = min;
                b = max;
            }
            else if (h <= 360f)
            {
                r = max;
                g = min;
                b = -(h - 360f) * dif / 60 + min;
            }
            else
            {
                r = 0;
                g = 0;
                b = 0;
            }
        }

		byte rbyte = (byte)(constrain (r, 0.0, 1.0)*255);
		byte gbyte = (byte)(constrain (g, 0.0, 1.0)*255);
		byte bbyte = (byte)(constrain (b, 0.0, 1.0)*255);
		c = color (rbyte, gbyte, bbyte, 255);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void _color2HSB( uint c, out float hu, out float sa, out float br, out float al)
	{	// Extract hue, saturation, brightness and alpha elements from an RGB color

		float r = red (c);
		float g = green (c);
		float b = blue (c);
		float a = alpha (c);

		// Adapted from http://www.unifycommunity.com/wiki/index.php?title=HSBColor

        hu = 0.0f;
		sa = 0.0f;
		br = 0.0f;
		al = a;

        float max = Math.Max(r, Math.Max(g, b));

        if (max <= 0) return;

        float min = Math.Min(r, Math.Min(g, b));
        float dif = max - min;

        if (max > min)
        {
            if (g == max)
            {
                hu = (b - r) / dif * 60f + 120f;
            }
            else if (b == max)
            {
                hu = (r - g) / dif * 60f + 240f;
            }
            else if (b > g)
            {
                hu = (g - b) / dif * 60f + 360f;
            }
            else
            {
                hu = (g - b) / dif * 60f;
            }
            if (hu < 0)
            {
                hu = hu + 360f;
            }
        }
        else
        {
            hu = 0;
        }

		hu *= 255f / 360f;; //1f / 360f;
        sa = (dif / max) * 255f; //(dif / max) * 1f;
        br = max;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Color")]
	public static float hue(uint c = 255)
	{	// Extract color byte from a color (unsigned int)
		float hu, sa, br, al;
		_color2HSB(c, out hu, out sa, out br, out al);
        return hu;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Color")]
	public static float saturation(uint c = 255)
	{	// Extract color byte from a color (unsigned int)
		float hu, sa, br, al;
		_color2HSB(c, out hu, out sa, out br, out al);
        return sa;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Color")]
	public static float brightness(uint c = 255)
	{	// Extract color byte from a color (unsigned int)
		float hu, sa, br, al;
		_color2HSB(c, out hu, out sa, out br, out al);
        return br;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Images")]
	public static void setPixel(int x = 0, int y = 0, byte r = 255, byte g = 255, byte b = 255, byte a = 255) 
	{	// Set an individual pixel in the pixbuf
		if (_p == null) return;
		if (_pixbuf == null) return;
		
		int offsetr = (y % _pixbuf.Height) * _pixbuf.Rowstride + (x % _pixbuf.Width) * _pixbuf.NChannels;
		System.Runtime.InteropServices.Marshal.WriteByte (_pixbuf.Pixels, offsetr, r);
		System.Runtime.InteropServices.Marshal.WriteByte (_pixbuf.Pixels, offsetr + 1, g);
		System.Runtime.InteropServices.Marshal.WriteByte (_pixbuf.Pixels, offsetr + 2, b);
		if (_pixbuf.NChannels > 3) System.Runtime.InteropServices.Marshal.WriteByte (_pixbuf.Pixels, offsetr + 3, a);
	}
	[JigsawTab("P/Images")]
	public static void setPixel(int x = 0, int y = 0, byte r = 255, byte g = 255, byte b = 255){ setPixel(x, y, r, g, b, 255); }
	[JigsawTab("P/Images")]
	public static void setPixel(int x = 0, int y = 0, byte gray = 255, byte a = 255) { setPixel(x, y, gray, gray, gray, a); }
	[JigsawTab("P/Images")]
	public static void setPixel(int x = 0, int y = 0, byte gray = 255) { setPixel(x, y, gray, gray, gray, 255); }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Images")]
	public static void setPixel(int x = 0, int y = 0, uint c = 255) 
	{	// Set an individual pixel in the pixbuf
	     if (_p == null) return;

		if (_pixbuf == null) return;
		byte r = red (c);
		byte g = green (c);
		byte b = blue (c);
		byte a = 255;
		if ( _pixbuf.NChannels > 3) a = alpha (c);
		setPixel(x, y, r, g, b, a);

		//System.Runtime.InteropServices.Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 0, red (c));
		//System.Runtime.InteropServices.Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 1, green (c));
		//System.Runtime.InteropServices.Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 2, blue (c));
		//System.Runtime.InteropServices.Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 3, alpha (c));	       
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Images")]
	public static uint getPixel(int x = 0, int y = 0) 
	{	// Set an individual pixel in the pixbuf
	     uint c = 0;
	     if (_p == null) return 0;
	     ManualResetEvent ev = new ManualResetEvent(false);
	     _invokeUint ( delegate { 
		if (_pixbuf != null)
		  {
		    
		    byte r = System.Runtime.InteropServices.Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 0);
		    byte g = System.Runtime.InteropServices.Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 1);
		    byte b = System.Runtime.InteropServices.Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 2);
		    byte a = 255;
		    if ( _pixbuf.NChannels > 3)
		      a = System.Runtime.InteropServices.Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 3);
		    c = color (r, g, b, a);
		  }
		ev.Set ();
		return c;
	       });
	     ev.WaitOne();
	     return c;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Images")]
	public static PImage get(int x = 0, int y = 0, int width = 100, int height = 100)	
	{	// Get a portion of the existing image as a new PImage and return it.
		if (_p == null) return null;
		ManualResetEvent ev = new ManualResetEvent(false);

		PImage img = null;

		_invokePImage ( delegate { 
			try {
			    img = _p.get (x, y, width, height); 
			} catch (System.NullReferenceException e){
				debug ( String.Format("get() ignored extra tick: {0}", e.ToString()), 1);
			}
			ev.Set ();
			return img;
		} );
		ev.WaitOne();
		return img;
	}
     
	[JigsawTab("P/Images")]
	public static PImage get() { return get(0, 0, _width, _height); }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Images")]
	public static void loadPixels()
	{	// Copy pixels from current image to pixels array
		if (_p == null) return;
		if (_p._img  == null) return;

		ManualResetEvent ev = new ManualResetEvent (false);
		Gdk.Pixbuf pixbuf = null;

		_invoke ( delegate {
			if (_p != null) {
				Context g = null;
				try {
					// Create a new pixmap and context
					Gdk.Pixmap pm = new Gdk.Pixmap(null, _width, _height, _p.GdkWindow.Depth);
					//using (Context ctx = Gdk.CairoHelper.Create(pm)) {
					g = Gdk.CairoHelper.Create(pm);
					// Paint internal Cairo image onto pixmap
					g.SetSourceSurface (_p._img, 0, 0);
					g.Paint ();
					//}
					((IDisposable) g).Dispose();
					 g = null;

					// Convert pixmap to pixbuf
					Gdk.Colormap colormap = pm.Colormap;
					pixbuf = Gdk.Pixbuf.FromDrawable (pm, colormap, 0, 0, 0, 0, _width, _height);

					// Creates a pixbuf from window
		//			Gdk.Drawable drawable = _p.GdkWindow;			// Gets data from window, not internal image. Causes problem when another window on top.
		//			Gdk.Colormap colormap = drawable.Colormap;
		//			int w = 0;
		//			int h = 0;
		//			drawable.GetSize (out w, out h);
		//			pixbuf = Gdk.Pixbuf.FromDrawable (drawable, colormap, 0, 0, 0, 0, w, h);

				} catch (Exception ex) {
					if (g != null) ((IDisposable) g).Dispose();
					g = null;
					debug ("Processing.loadPixels(): " + ex.Message, 2);
				}
			}
			ev.Set ();
		});
		ev.WaitOne ();

		if (pixbuf != null) {
			_pixbuf = pixbuf;
			if (!_pixbuf.HasAlpha) {
				_pixbuf = _pixbuf.AddAlpha (false, 0, 0, 0);
			}
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	[JigsawTab("P/Images")]
	public static void updatePixels() 
	{	// Copy pixels from pixbuf to image
		try
		{
			if (_p == null) return;
			if (_p._img  == null) return;
			if (_pixbuf == null) return;

			ManualResetEvent ev = new ManualResetEvent (false);
			_invoke ( delegate {
				if (_p != null && _pixbuf != null) {
					Context g = null;
					try {
						//using (Context g = new Cairo.Context(_p._img)) {
						g = new Cairo.Context(_p._img);
						Gdk.CairoHelper.SetSourcePixbuf(g, _pixbuf, 0.0, 0.0);
						g.Paint ();
						//}
						((IDisposable) g).Dispose();
						g = null;
						if (_immediateMode) _p.redraw ();
					} catch (Exception ex) {
						if (g != null) ((IDisposable) g).Dispose();
						g = null;
						debug ( "updatePixels(): " + ex.Message, 2);
					}
				}
				
				ev.Set ();
			});
			ev.WaitOne ();
		} catch (Exception ex) {
			Console.WriteLine ("Processing.updatePixels: " + ex.Message);
		}

//		int nPixels = _width * _height;
//		for (int i=0; i<nPixels; i++) {
//			int n = i*4;
//			byte[] b = BitConverter.GetBytes(_pixels[i]);
//			_p._img.Data[n] = b[0];
//			_p._img.Data[n+1] = b[1];
//			_p._img.Data[n+2] = b[2];
//			_p._img.Data[n+3] = b[3];
//		}
	}
}
// ------------------ PTimer ----------------------------------------------
public class PElapsedEventArgs : EventArgs {
	public long tickCount = 0;

	public PElapsedEventArgs(long tickCount) : base() {
		this.tickCount = tickCount;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
internal class PTimer
{
	private bool _enabled = false;			// true if timer 
	private uint _timerID = 0;				// ID of currently installed timeout handler
	private uint _timeOut = 0;				// interval in milliseconds
	private bool _inTimeout = false;		// Flag to prevent reentrance
	private long _tickCount = 0;			// Keep count of the number of ticks fired
	public event EventHandler<PElapsedEventArgs> Elapsed = null;
//	public event ElapsedEventHandler Elapsed = null;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public PTimer()
	{
		_enabled = false;
		_timerID = 0;
		_timeOut = 0;
		_inTimeout = false;
		_tickCount = 0;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void Start()
	{	// Start Timeout. Remove any existing timeout handler that is running.
		if (_timeOut == 0) return;
		Stop();					// If running, stop first before installing TimeoutHandler
		_timerID = GLib.Timeout.Add(_timeOut, new GLib.TimeoutHandler(_onTimerElapsed));
		_enabled = true;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void Stop()
	{ 	// Remove any existing timeout handler
		if (_timerID > 0) {
			GLib.Source.Remove(_timerID);
			_timerID = 0;
		}
		_enabled = false;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private bool _onTimerElapsed()
	{	// Internal timeout handler
		if (_inTimeout) return true;
		_inTimeout = true;

		// Count ticks
		System.Threading.Interlocked.Increment(ref _tickCount);

		// Raise onLoop event
		EventHandler<PElapsedEventArgs> handler = Elapsed;
		PElapsedEventArgs args = new PElapsedEventArgs(_tickCount);
		if (handler != null) { 
		    try {
			handler(this, args);
		    } catch (Exception e) {
			Console.Error.WriteLine("Error in Elapsed: " + e.Message);
		    }
		}
		
		_inTimeout = false;
		return true;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public uint Interval
	{	// Set/get the timeout interval
		set	{
			// Reset timeout value
			_timeOut = value;

			// If currently enabled, then restart now
			if (_enabled) Start();
		}
		get	{
			return _timeOut;
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public bool Enabled
	{	// Property get/set as alternative interface to Start()/Stop() methods
		set {
			if (value == true) {
				Start();
			} else {
				Stop();
			}
		}
		get{
			return _enabled;
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public long TickCount
	{	// Return internal tick count
		get {
			return _tickCount;
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
}

// ------------------ PImage ----------------------------------------------
public class PImage
{
	// internal cache of loaded image
	internal ImageSurface _img = null;
	private Gdk.Pixbuf _pixbuf = null;					// Internal pixbuf used by loadPixels and updatePixels
	private int _width = 0;
	private int _height = 0;

        private static int guiThreadId = -1;

	public delegate void VoidDelegate ();				 // A delegate that takes no args and returns nothing
	private delegate PImage PImageDelegate ();
	private delegate uint UintDelegate ();

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public PImage(string path)
	{
	  ManualResetEvent ev = new ManualResetEvent(false);
	   _invoke ( delegate {
		// Check file type
		string ext = System.IO.Path.GetExtension(path).ToLower ();

        if (path.StartsWith ("http://") || 
            path.StartsWith ("https://") || 
            path.StartsWith("data:")) {
            Gdk.Pixbuf pb = null;
            
            if (path.StartsWith ("http://") || path.StartsWith ("https://")) {
                HttpWebRequest req = (HttpWebRequest)WebRequest.Create (path);
                req.KeepAlive = false;
                req.Timeout = 10000;        
                WebResponse resp = req.GetResponse ();
                Stream s = resp.GetResponseStream ();
                pb = new Gdk.Pixbuf (s);
            } else if (path.StartsWith("data:")) {                    
                // "data:image/png;base64,..."
                string [] parts = path.Split(new char[] {','}, 2, StringSplitOptions.None);
                Byte [] bytes = System.Convert.FromBase64String(parts[1]);
                pb = new Gdk.Pixbuf(bytes);
            }
            
            if (!pb.HasAlpha) pb = pb.AddAlpha (false, 0, 0, 0); 
            _img = new ImageSurface(Format.Argb32, pb.Width, pb.Height);
            _width = pb.Width;
            _height = pb.Height;
            using (Context g = new Cairo.Context(_img)) {
                Gdk.CairoHelper.SetSourcePixbuf(g, pb, 0.0, 0.0);
                g.Paint ();
            };
        }                            
        else
        {            
            // Load from file 
            switch (ext) {
            case ".png":
                _img = new ImageSurface(path);
                _width = _img.Width;
                _height = _img.Height;
                break;
            case ".gif":
            case ".jpg":	// See http://mono.1490590.n4.nabble.com/checking-the-Gdk-Pixbuf-support-for-JPEG-td3747144.html
            case ".jpeg":
            case ".tif":
            case ".tiff":
            case ".xpm":
            case ".xbm":
                Gdk.Pixbuf pb = new Gdk.Pixbuf(path);
                 if (!pb.HasAlpha) pb = pb.AddAlpha (false, 0, 0, 0); 
                 _img = new ImageSurface(Format.Argb32, pb.Width, pb.Height);
                 _width = pb.Width;
                 _height = pb.Height;
                 using (Context g = new Cairo.Context(_img)) {
                     Gdk.CairoHelper.SetSourcePixbuf(g, pb, 0.0, 0.0);
                     g.Paint ();
                 };
             break;
            default:
                string msg = String.Format ("Don't know how to load an image file with extension {0}", ext);
                throw new Exception(msg);
            }
        }
        ev.Set();
           });
       ev.WaitOne();
    
    }
    
    public PImage(int width, int height, Cairo.Format format, bool flag)
	{	// Create a new PImage from a Cairo ImageSurface
		_img = new ImageSurface(format, width, height);
		_width = width;
		_height = height;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public PImage(int width, int height, Cairo.Format format)
	{	// Create a new PImage from a Cairo ImageSurface
	  ManualResetEvent ev = new ManualResetEvent(false);

	   _invoke ( delegate {
		_img = new ImageSurface(format, width, height);
		_width = width;
		_height = height;
		ev.Set();
	     });
           ev.WaitOne();
	}

	public PImage(int width, int height) : this(width, height, Cairo.Format.ARGB32) { }
	public PImage() : this(300, 300, Cairo.Format.ARGB32) { }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public PImage get(int x, int y, int width, int height)
	{	// Create a new image from a portion of the existing image.
	  PImage img = new PImage(width, height);
	  ManualResetEvent ev = new ManualResetEvent(false);
	  _invokePImage ( delegate { 
	      
	      try {
		using (Context g = new Context(img._img))
		  {			
		      _img.Show (g, -x, -y);
		  }
	      } catch (System.NullReferenceException e){
		string msg = String.Format ("get() ignored extra tick: {0}", e);
		throw new Exception(msg);
	      }
	      ev.Set ();
	      return img;
	    } );
	  ev.WaitOne();
	  return img;
	}

	public PImage get() { return get(0, 0, _width, _height); } 
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public int width() {
		return _width;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public int height() {
		return _height;
	}

	public static void set_gui_thread_id (int gui_thread_id)
	{
	        guiThreadId = gui_thread_id;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void _invoke( VoidDelegate fxn ) 
	{	// Invoke a void delegate on thread if necessary;
		if (Thread.CurrentThread.ManagedThreadId != guiThreadId)
		{
			Application.Invoke ( delegate{ fxn(); } );
		} else {
			fxn();
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void _invokePImage( PImageDelegate fxn ) 
	{	// Invoke a delegate that returns a PImage on thread if necessary

		PImage val = null;
		if (Thread.CurrentThread.ManagedThreadId != guiThreadId)
		{
			Application.Invoke ( delegate{ val = fxn(); } );
		} else {
			fxn();
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	private static void _invokeUint( UintDelegate fxn ) 
	{	// Invoke a delegate that returns a double on thread if necessary

 	        uint val = 0;
		if (Thread.CurrentThread.ManagedThreadId != guiThreadId)
		{
			Application.Invoke ( delegate{ val = fxn(); } );
		} else {
			fxn();
		}
	}


//	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//	public void image(PImage img, double x, double y, double w, double h) 
//	{	// Render an image into the rectangle specified
//
//		// Calculate position and scale parameters based on image mode
////		double ww = img.width ();		// Default is CORNER
////		double hh = img.height ();
////		double sx = w/ww;
////		double sy = h/hh;
//
////		switch (_imageMode) {
////		case ImageMode.CENTER:
////			x = x - 0.5*w;
////			y = y - 0.5*h;
////			//sx = w/ww;		// Same as default
////			//sy = h/hh;
////			break;
////		case ImageMode.CORNERS:
////			// In this mode, w is x2, h is y2
////			sx = (w - x)/ww;
////			sy = (h - y)/hh;
////			break;
////		default: // ImageMode.CORNER:
////			break;
////		}
//
//		using (Context g = new Context(_img)) {
////			g.Matrix = _mat;
//			g.Save ();
//			g.Translate (x, y);
////			g.Scale ( sx, sy );
//			img._img.Show (g, 0, 0);
//			g.Restore ();
//		}
////		this.QueueDraw ();
//	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void loadPixels()
	{	// Copy pixels from current image to pixbuf

	  ManualResetEvent ev = new ManualResetEvent(false);

	   _invoke ( delegate {

		if (_img  != null)
		  {
		    Gdk.Pixbuf pixbuf = null;
		    
		    // Create a new pixmap and context
		    Gdk.Pixmap pm = new Gdk.Pixmap(null, _width, _height, 24);
		    using (Context g = Gdk.CairoHelper.Create(pm)) {
		      // Paint internal Cairo image onto pixmap
		      g.Color = new Cairo.Color(0, 0, 0);
		      g.Paint();			
		      g.SetSourceSurface (_img, 0, 0);
		      g.Paint ();
		    }
		    
		    // Convert pixmap to pixbuf
		    Gdk.Colormap colormap = pm.Colormap;
		    pixbuf = Gdk.Pixbuf.FromDrawable (pm, colormap, 0, 0, 0, 0, _width, _height);
		    
		    if (pixbuf != null) {
		      _pixbuf = pixbuf;
		      if (!_pixbuf.HasAlpha) _pixbuf = _pixbuf.AddAlpha (false, 0, 0, 0);
		    }
		  }

		ev.Set();
	     });
	   ev.WaitOne();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void updatePixels() 
	{	// Copy pixels from pixbuf to image
	  ManualResetEvent ev = new ManualResetEvent(false);

	   _invoke ( delegate {

		if (_img  != null && _pixbuf != null)
		  {
		    using (Context g = new Cairo.Context(_img)) {
		      Gdk.CairoHelper.SetSourcePixbuf(g, _pixbuf, 0.0, 0.0);
		      g.Paint ();
		    }
		  }
		ev.Set();
	     });
	   ev.WaitOne();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void setPixel(int x, int y, byte r, byte g, byte b, byte a) 
	{	// Set an individual pixel in the pixbuf
	  
	  //ManualResetEvent ev = new ManualResetEvent(false);

	       if (_pixbuf != null)
		 {	  
		   System.Runtime.InteropServices.Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 0, r);
		   System.Runtime.InteropServices.Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 1, g);
		   System.Runtime.InteropServices.Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 2, b);
		   System.Runtime.InteropServices.Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 3, a);
		 }
	       //ev.Set();

        //ev.WaitOne();
	}

	public void setPixel(int x, int y, byte r, byte g, byte b) { setPixel(x, y, r, g, b, 255); }
	public void setPixel(int x, int y, byte g, byte a) { setPixel(x, y, g, g, g, a); }
	public void setPixel(int x, int y, byte g) { setPixel(x, y, g, g, g, 255); }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void setPixel(int x, int y, uint c) 
	{	// Set an individual pixel in the pixbuf
	  
	  if (_pixbuf == null) return;

	       System.Runtime.InteropServices.Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 0, red (c));
	       System.Runtime.InteropServices.Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 1, green (c));
	       System.Runtime.InteropServices.Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 2, blue (c));
	       System.Runtime.InteropServices.Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 3, alpha (c));
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public uint getPixel(int x, int y) 
	{	// Set an individual pixel in the pixbuf
	  uint c = 0;
	       if (_pixbuf != null)
		 {
		   
		   byte r = System.Runtime.InteropServices.Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 0);
		   byte g = System.Runtime.InteropServices.Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 1);
		   byte b = System.Runtime.InteropServices.Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 2);
		   byte a = System.Runtime.InteropServices.Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 3);
		   c =color (r, g, b, a);
		 }
	   return c;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public byte blue(uint c)
	{	// Extract color byte from a color (unsigned int)
		 return (byte)((c & 0x000000FF));
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public byte green(uint c)
	{	// Extract color byte from a color (unsigned int)
		 return (byte)((c & 0x0000FF00) >> 8);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public byte red(uint c)
	{	// Extract color byte from a color (unsigned int)
		 return (byte)((c & 0x00FF0000) >> 16);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public byte alpha(uint c)
	{	// Extract color byte from a color (unsigned int)
		 return (byte)((c & 0xFF000000) >> 24);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public uint color(byte r, byte g, byte b, byte a)
	{	// Create color from color byte components
		 return (uint)( b | (g << 8) | (r << 16) | (a << 24));
	}
	public uint color(byte r, byte g, byte b) { return color(r, g, b, 255); }
	public uint color(byte g, byte a) { return color(g, g, g, a); }
	public uint color(byte g) { return color(g, g, g, 255); }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void resize(int w, int h) 
	{	// reset image size
	   _invoke ( delegate {

		if (_img == null) return;
		Cairo.ImageSurface timg = new Cairo.ImageSurface(_img.Format, w, h);

		// TODO: This doesn't do what it is supposed to yet !!!
		// This is supposed to resize the image data.
		// At the moment it just copies 
		// FIX!!!

		// If the internal image already exists, copy to the new one
		using (Context g = new Context(timg)) {
			g.SetSource(_img);
			g.Paint ();
		}
		_img = timg;

		_width = w;
		_height = h;
	     });
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public void save(string path, bool force) 
	{	// Save image to a file
	   _invoke ( delegate {

		if (_img == null) return;

		// Check if exists and not to overwrite.
		if (System.IO.File.Exists(path) == true && force == false) {
			throw new Exception("That path already exists. Use save(path, True) to force the file to be overwritten.");
		}

		// Check extension
		string ext = System.IO.Path.GetExtension(path).ToLower ();
		ext = ext.Replace (".", "");
		if (ext.Length == 0) {
			throw new Exception("Image file must have a valid extension, such as .png or .jpg");
		}

		// Load image data into a pixbuf
		loadPixels();

		// Try to do the save
		_pixbuf.Save (path, ext);
	     });

	}
	public void save(string path) { save(path, false); }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public System.Drawing.Bitmap toBitmap(string sformat="Format24bppRgb") {
	    Dictionary<string,System.Drawing.Imaging.PixelFormat> formats = new Dictionary<string,System.Drawing.Imaging.PixelFormat>();
	    formats["Alpha"] = System.Drawing.Imaging.PixelFormat.Alpha;
	    formats["Canonical"] = System.Drawing.Imaging.PixelFormat.Canonical;
	    formats["DontCare"] = System.Drawing.Imaging.PixelFormat.DontCare;
	    formats["Extended"] = System.Drawing.Imaging.PixelFormat.Extended;
	    formats["Format16bppArgb1555"] = System.Drawing.Imaging.PixelFormat.Format16bppArgb1555;
	    formats["Format16bppGrayScale"] = System.Drawing.Imaging.PixelFormat.Format16bppGrayScale;
	    formats["Format16bppRgb555"] = System.Drawing.Imaging.PixelFormat.Format16bppRgb555;
	    formats["Format16bppRgb565"] = System.Drawing.Imaging.PixelFormat.Format16bppRgb565;
	    formats["Format1bppIndexed"] = System.Drawing.Imaging.PixelFormat.Format1bppIndexed;
	    formats["Format24bppRgb"] = System.Drawing.Imaging.PixelFormat.Format24bppRgb;
	    formats["Format32bppArgb"] = System.Drawing.Imaging.PixelFormat.Format32bppArgb;
	    formats["Format32bppPArgb"] = System.Drawing.Imaging.PixelFormat.Format32bppPArgb;
	    formats["Format32bppRgb"] = System.Drawing.Imaging.PixelFormat.Format32bppRgb;
	    formats["Format48bppRgb"] = System.Drawing.Imaging.PixelFormat.Format48bppRgb;
	    formats["Format4bppIndexed"] = System.Drawing.Imaging.PixelFormat.Format4bppIndexed;
	    formats["Format64bppArgb"] = System.Drawing.Imaging.PixelFormat.Format64bppArgb;
	    formats["Format64bppPArgb"] = System.Drawing.Imaging.PixelFormat.Format64bppPArgb;
	    formats["Format8bppIndexed"] = System.Drawing.Imaging.PixelFormat.Format8bppIndexed;
	    formats["Gdi"] = System.Drawing.Imaging.PixelFormat.Gdi;
	    formats["Indexed"] = System.Drawing.Imaging.PixelFormat.Indexed;
	    formats["Max"] = System.Drawing.Imaging.PixelFormat.Max;
	    formats["PAlpha"] = System.Drawing.Imaging.PixelFormat.PAlpha;
	    formats["Undefined"] = System.Drawing.Imaging.PixelFormat.Undefined;
	    System.Drawing.Imaging.PixelFormat format = formats[sformat];
	    System.Drawing.Bitmap bitmap = new System.Drawing.Bitmap(_width, _height, format);
	    for (int x=0; x < _width; x++) {
		for (int y=0; y < _height; y++) {
		    byte r = System.Runtime.InteropServices.Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 0);
		    byte g = System.Runtime.InteropServices.Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 1);
		    byte b = System.Runtime.InteropServices.Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 2);
		    byte a = System.Runtime.InteropServices.Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride + x * _pixbuf.NChannels + 3);
		    System.Drawing.Color color = System.Drawing.Color.FromArgb(a, r, g, b);
		    bitmap.SetPixel(x, y, color);
		}
	    }
	    return bitmap;
	}

        public Gdk.Pixbuf toPixbuf () {
	    return _pixbuf.Copy();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public IDictionary<string, string> GetRepresentations() {
	    byte [] buffer;
	    string png_string = "";
	    string jpg_string = "";
	    loadPixels();
	    try {
		buffer = _pixbuf.SaveToBuffer("png");
		png_string = System.Convert.ToBase64String(buffer, 0, buffer.Length);
	    } catch {
		png_string = "";
	    }
	    try {
		buffer = _pixbuf.SaveToBuffer("jpeg");
		jpg_string = System.Convert.ToBase64String(buffer, 0, buffer.Length);
	    } catch {
		jpg_string = "";
	    }
	    var retval = new Dictionary<string, string>();
	    retval["text/plain"] =  "<PImage>";
	    if (png_string != "")
		retval["image/png"] = png_string;
	    if (jpg_string != "")
		retval["image/jpeg"] = jpg_string;
	    return retval;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
}

// ------------------ PKnot support ---------------------------------------
internal enum PKnotType {
	VERTEX, CURVE, BEZIER
}

internal class PKnot 
{	// Class to hold knots for splines
	public double x, y;
	public double cx1, cy1, cx2, cy2;		// Control points for bezier and curve vertexes
	public double a;						// Angle of control points, for curve vertexes only
	public double d;						// length of segment from previous knot
	public PKnotType type;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	public PKnot(double x, double y, PKnotType type) {
		this.x = x;
		this.y = y;
		this.type = type;
	}
	public PKnot(double x, double y, double cx1, double cy1, double cx2, double cy2, PKnotType type) 
		: this(x, y, type) {
		this.cx1 = cx1;
		this.cy1 = cy1;
		this.cx2 = cx2;
		this.cy2 = cy2;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
}

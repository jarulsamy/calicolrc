/*
Calico - Scripting Environment

Copyright (c) 2011, Doug Blank <dblank@cs.brynmawr.edu>

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
using IronPython.Runtime;
// Operations, List, Tuple, Dict, ...
using System.Runtime.InteropServices;
// Marshal
using System.Collections.Generic;
using System.Collections;
// IEnumerable
using System.Threading;
using System.Drawing;
// Image, Bitmap
using System.Net;
// WebRequest
using System.IO;
// MemoryStream
using System;

using Microsoft.Xna.Framework;
using System.Diagnostics;// Vector2, Matrix

public static class Extensions
{
    // Can't use extension methods to override existing methods
    public static string MyString<T> (this T[] array)
    {
	string retval = "";
	foreach( object item in array) {
	    if (retval != "")
		retval += ", ";
	    retval += item.ToString();
	}
	return "[" + retval + "]";
    }
}

public static class Graphics
{

	public static int gui_thread_id = -1;
	public static string startup_path;
	public static string os_name;
	public static bool warn_missing_dot = false;
	public delegate void InvokeDelegate ();
	
    public static void initialize_module(string path, string os) {
    	Graphics.startup_path = path;
    	Graphics.os_name = os;
	}
	
	public static void Invoke (InvokeDelegate invoke)
	{
		if (needInvoke ())
			Gtk.Application.Invoke (delegate {
				invoke ();});
		else
			invoke ();
	}

	public static bool needInvoke ()
	{
		if (Graphics.gui_thread_id == -1) {
			return false; // direcly in GUI thread
		} else if (Graphics.gui_thread_id == Thread.CurrentThread.ManagedThreadId) {
			return false; // you are already in the GUI thread
		} else {
			return true; // need to invoke!
		}
	}

	public static void set_gui_thread_id (int gui_thread_id)
	{
		Graphics.gui_thread_id = gui_thread_id;
	}

	private static WindowClass _lastWindow = null;
	public static readonly Dictionary<string,Color> colors = 
    new Dictionary<string,Color> () {
       {"black", new Color(0x00, 0x00, 0x00)}, 
       {"navy", new Color(0x00, 0x00, 0x80)}, 
       {"darkblue", new Color(0x00, 0x00, 0x8B)}, 
       {"mediumblue", new Color(0x00, 0x00, 0xCD)}, 
       {"blue", new Color(0x00, 0x00, 0xFF)}, 
       {"darkgreen", new Color(0x00, 0x64, 0x00)}, 
       {"green", new Color(0x00, 0x80, 0x00)}, 
       {"teal", new Color(0x00, 0x80, 0x80)}, 
       {"darkcyan", new Color(0x00, 0x8B, 0x8B)}, 
       {"deepskyblue", new Color(0x00, 0xBF, 0xFF)}, 
       {"darkturquoise", new Color(0x00, 0xCE, 0xD1)}, 
       {"mediumspringgreen", new Color(0x00, 0xFA, 0x9A)}, 
       {"lime", new Color(0x00, 0xFF, 0x00)}, 
       {"springgreen", new Color(0x00, 0xFF, 0x7F)}, 
       {"aqua", new Color(0x00, 0xFF, 0xFF)}, 
       {"cyan", new Color(0x00, 0xFF, 0xFF)}, 
       {"midnightblue", new Color(0x19, 0x19, 0x70)}, 
       {"dodgerblue", new Color(0x1E, 0x90, 0xFF)}, 
       {"lightseagreen", new Color(0x20, 0xB2, 0xAA)}, 
       {"forestgreen", new Color(0x22, 0x8B, 0x22)}, 
       {"seagreen", new Color(0x2E, 0x8B, 0x57)}, 
       {"darkslategray", new Color(0x2F, 0x4F, 0x4F)}, 
       {"darkslategrey", new Color(0x2F, 0x4F, 0x4F)}, 
       {"limegreen", new Color(0x32, 0xCD, 0x32)}, 
       {"mediumseagreen", new Color(0x3C, 0xB3, 0x71)}, 
       {"turquoise", new Color(0x40, 0xE0, 0xD0)}, 
       {"royalblue", new Color(0x41, 0x69, 0xE1)}, 
       {"steelblue", new Color(0x46, 0x82, 0xB4)}, 
       {"darkslateblue", new Color(0x48, 0x3D, 0x8B)}, 
       {"mediumturquoise", new Color(0x48, 0xD1, 0xCC)}, 
       {"indigo", new Color(0x4B, 0x00, 0x82)}, 
       {"darkolivegreen", new Color(0x55, 0x6B, 0x2F)}, 
       {"cadetblue", new Color(0x5F, 0x9E, 0xA0)}, 
       {"cornflowerblue", new Color(0x64, 0x95, 0xED)}, 
       {"mediumaquamarine", new Color(0x66, 0xCD, 0xAA)}, 
       {"dimgray", new Color(0x69, 0x69, 0x69)}, 
       {"dimgrey", new Color(0x69, 0x69, 0x69)}, 
       {"slateblue", new Color(0x6A, 0x5A, 0xCD)}, 
       {"olivedrab", new Color(0x6B, 0x8E, 0x23)}, 
       {"slategray", new Color(0x70, 0x80, 0x90)}, 
       {"slategrey", new Color(0x70, 0x80, 0x90)}, 
       {"lightslategray", new Color(0x77, 0x88, 0x99)}, 
       {"lightslategrey", new Color(0x77, 0x88, 0x99)}, 
       {"mediumslateblue", new Color(0x7B, 0x68, 0xEE)}, 
       {"lawngreen", new Color(0x7C, 0xFC, 0x00)}, 
       {"chartreuse", new Color(0x7F, 0xFF, 0x00)}, 
       {"aquamarine", new Color(0x7F, 0xFF, 0xD4)}, 
       {"maroon", new Color(0x80, 0x00, 0x00)}, 
       {"purple", new Color(0x80, 0x00, 0x80)}, 
       {"olive", new Color(0x80, 0x80, 0x00)}, 
       {"gray", new Color(0x80, 0x80, 0x80)}, 
       {"grey", new Color(0x80, 0x80, 0x80)}, 
       {"skyblue", new Color(0x87, 0xCE, 0xEB)}, 
       {"lightskyblue", new Color(0x87, 0xCE, 0xFA)}, 
       {"blueviolet", new Color(0x8A, 0x2B, 0xE2)}, 
       {"darkred", new Color(0x8B, 0x00, 0x00)}, 
       {"darkmagenta", new Color(0x8B, 0x00, 0x8B)}, 
       {"saddlebrown", new Color(0x8B, 0x45, 0x13)}, 
       {"darkseagreen", new Color(0x8F, 0xBC, 0x8F)}, 
       {"lightgreen", new Color(0x90, 0xEE, 0x90)}, 
       {"mediumpurple", new Color(0x93, 0x70, 0xD8)}, 
       {"darkviolet", new Color(0x94, 0x00, 0xD3)}, 
       {"palegreen", new Color(0x98, 0xFB, 0x98)}, 
       {"darkorchid", new Color(0x99, 0x32, 0xCC)}, 
       {"yellowgreen", new Color(0x9A, 0xCD, 0x32)}, 
       {"sienna", new Color(0xA0, 0x52, 0x2D)}, 
       {"brown", new Color(0xA5, 0x2A, 0x2A)}, 
       {"darkgray", new Color(0xA9, 0xA9, 0xA9)}, 
       {"darkgrey", new Color(0xA9, 0xA9, 0xA9)}, 
       {"lightblue", new Color(0xAD, 0xD8, 0xE6)}, 
       {"greenyellow", new Color(0xAD, 0xFF, 0x2F)}, 
       {"paleturquoise", new Color(0xAF, 0xEE, 0xEE)}, 
       {"lightsteelblue", new Color(0xB0, 0xC4, 0xDE)}, 
       {"powderblue", new Color(0xB0, 0xE0, 0xE6)}, 
       {"firebrick", new Color(0xB2, 0x22, 0x22)}, 
       {"darkgoldenrod", new Color(0xB8, 0x86, 0x0B)}, 
       {"mediumorchid", new Color(0xBA, 0x55, 0xD3)}, 
       {"rosybrown", new Color(0xBC, 0x8F, 0x8F)}, 
       {"darkkhaki", new Color(0xBD, 0xB7, 0x6B)}, 
       {"silver", new Color(0xC0, 0xC0, 0xC0)}, 
       {"mediumvioletred", new Color(0xC7, 0x15, 0x85)}, 
       {"indianred", new Color(0xCD, 0x5C, 0x5C)}, 
       {"peru", new Color(0xCD, 0x85, 0x3F)}, 
       {"chocolate", new Color(0xD2, 0x69, 0x1E)}, 
       {"tan", new Color(0xD2, 0xB4, 0x8C)}, 
       {"lightgray", new Color(0xD3, 0xD3, 0xD3)}, 
       {"lightgrey", new Color(0xD3, 0xD3, 0xD3)}, 
       {"palevioletred", new Color(0xD8, 0x70, 0x93)}, 
       {"thistle", new Color(0xD8, 0xBF, 0xD8)}, 
       {"orchid", new Color(0xDA, 0x70, 0xD6)}, 
       {"goldenrod", new Color(0xDA, 0xA5, 0x20)}, 
       {"crimson", new Color(0xDC, 0x14, 0x3C)}, 
       {"gainsboro", new Color(0xDC, 0xDC, 0xDC)}, 
       {"plum", new Color(0xDD, 0xA0, 0xDD)}, 
       {"burlywood", new Color(0xDE, 0xB8, 0x87)}, 
       {"lightcyan", new Color(0xE0, 0xFF, 0xFF)}, 
       {"lavender", new Color(0xE6, 0xE6, 0xFA)}, 
       {"darksalmon", new Color(0xE9, 0x96, 0x7A)}, 
       {"violet", new Color(0xEE, 0x82, 0xEE)}, 
       {"palegoldenrod", new Color(0xEE, 0xE8, 0xAA)}, 
       {"lightcoral", new Color(0xF0, 0x80, 0x80)}, 
       {"khaki", new Color(0xF0, 0xE6, 0x8C)}, 
       {"aliceblue", new Color(0xF0, 0xF8, 0xFF)}, 
       {"honeydew", new Color(0xF0, 0xFF, 0xF0)}, 
       {"azure", new Color(0xF0, 0xFF, 0xFF)}, 
       {"sandybrown", new Color(0xF4, 0xA4, 0x60)}, 
       {"wheat", new Color(0xF5, 0xDE, 0xB3)}, 
       {"beige", new Color(0xF5, 0xF5, 0xDC)}, 
       {"whitesmoke", new Color(0xF5, 0xF5, 0xF5)}, 
       {"mintcream", new Color(0xF5, 0xFF, 0xFA)}, 
       {"ghostwhite", new Color(0xF8, 0xF8, 0xFF)}, 
       {"salmon", new Color(0xFA, 0x80, 0x72)}, 
       {"antiquewhite", new Color(0xFA, 0xEB, 0xD7)}, 
       {"linen", new Color(0xFA, 0xF0, 0xE6)}, 
       {"lightgoldenrodyellow", new Color(0xFA, 0xFA, 0xD2)}, 
       {"oldlace", new Color(0xFD, 0xF5, 0xE6)}, 
       {"red", new Color(0xFF, 0x00, 0x00)}, 
       {"fuchsia", new Color(0xFF, 0x00, 0xFF)}, 
       {"magenta", new Color(0xFF, 0x00, 0xFF)}, 
       {"deeppink", new Color(0xFF, 0x14, 0x93)}, 
       {"orangered", new Color(0xFF, 0x45, 0x00)}, 
       {"tomato", new Color(0xFF, 0x63, 0x47)}, 
       {"hotpink", new Color(0xFF, 0x69, 0xB4)}, 
       {"coral", new Color(0xFF, 0x7F, 0x50)}, 
       {"darkorange", new Color(0xFF, 0x8C, 0x00)}, 
       {"lightsalmon", new Color(0xFF, 0xA0, 0x7A)}, 
       {"orange", new Color(0xFF, 0xA5, 0x00)}, 
       {"lightpink", new Color(0xFF, 0xB6, 0xC1)}, 
       {"pink", new Color(0xFF, 0xC0, 0xCB)}, 
       {"gold", new Color(0xFF, 0xD7, 0x00)}, 
       {"peachpuff", new Color(0xFF, 0xDA, 0xB9)}, 
       {"navajowhite", new Color(0xFF, 0xDE, 0xAD)}, 
       {"moccasin", new Color(0xFF, 0xE4, 0xB5)}, 
       {"bisque", new Color(0xFF, 0xE4, 0xC4)}, 
       {"mistyrose", new Color(0xFF, 0xE4, 0xE1)}, 
       {"blanchedalmond", new Color(0xFF, 0xEB, 0xCD)}, 
       {"papayawhip", new Color(0xFF, 0xEF, 0xD5)}, 
       {"lavenderblush", new Color(0xFF, 0xF0, 0xF5)}, 
       {"seashell", new Color(0xFF, 0xF5, 0xEE)}, 
       {"cornsilk", new Color(0xFF, 0xF8, 0xDC)}, 
       {"lemonchiffon", new Color(0xFF, 0xFA, 0xCD)}, 
       {"floralwhite", new Color(0xFF, 0xFA, 0xF0)}, 
       {"snow", new Color(0xFF, 0xFA, 0xFA)}, 
       {"yellow", new Color(0xFF, 0xFF, 0x00)}, 
       {"lightyellow", new Color(0xFF, 0xFF, 0xE0)}, 
       {"ivory", new Color(0xFF, 0xFF, 0xF0)}, 
       {"white", new Color(0xFF, 0xFF, 0xFF)}, 
    };

	public static PythonTuple getMouse ()
	{
		return getWindow ().getMouse ();
	}

	public static PythonTuple getMouseNow ()
	{
		return getWindow ().getMouseNow ();
	}

	public static string getMouseState ()
	{
		return getWindow ().getMouseState ();
	}

	public static string getKeyState ()
	{
		return getWindow ().getKeyState ();
	}

	public static string getKeyPressed ()
	{
		return getWindow ().getKeyPressed ();
	}

	public static void run ()
	{
		getWindow ().run ();
	}

	public static void run (Func<object> function)
	{
		getWindow ().run (function);
	}

	// Callbacks:

	public static void onMouseUp (Func<object,Event,object> function)
	{
		getWindow ().onMouseUp (function);
	}

	public static void onMouseDown (Func<object,Event,object> function)
	{
		getWindow ().onMouseDown (function);
	}

	public static void onMouseMovement (Func<object,Event,object> function)
	{
		getWindow ().onMouseMovement (function);
	}

	public static void onKeyPress (Func<object,Event,object> function)
	{
		getWindow ().onKeyPress (function);
	}

	public static void onKeyRelease (Func<object,Event,object> function)
	{
		getWindow ().onKeyRelease (function);
	}

	public static List getColorNames ()
	{
		List retval = new List ();
		foreach (String key in colors.Keys) {
			retval.append (key);
		}
		return retval;
	}
  
	public static List PyList (params object [] items)
	{
		// make a list from an array
		List retval = new List ();
		for (int i = 0; i < items.Length; i++) {
			retval.append (items [i]);
		}
		return retval;
	}
  
	public static PythonTuple PyTuple (params object [] items)
	{
		// make a tuple from an array
		return new PythonTuple (items);
	}

	private static Dictionary<string, Graphics.WindowClass> _windows =
    new Dictionary<string, Graphics.WindowClass> ();
  
	public static Color getColor (Picture picture, int x, int y)
	{
		return picture.getColor (x, y);
	}
  
	public static Pixel getPixel (Picture picture, int x, int y)
	{
		return picture.getPixel (x, y);
	}
  
	public static IEnumerable getPixels (Picture picture)
	{
		for (int x=0; x < picture.width; x++) {
			for (int y=0; y < picture.height; y++) {
				yield return picture.getPixel(x, y);
			}
		}
	}

	public static void setPixels (Picture picture, Picture picture2)
	{
		for (int x=0; x < picture.width; x++) {
			for (int y=0; y < picture.height; y++) {
				picture.setPixel (x, y, picture2.getPixel (x, y));
			}
		}
	}

	public static void setPixel (Picture picture, int x, int y, Color color)
	{
		picture.setPixel (x, y, color);
	}

	public static void setPixel (Picture picture, int x, int y, Pixel pixel)
	{
		picture.setPixel (x, y, pixel);
	}

	public static Color getColor (Pixel pixel)
	{
		return pixel.getColor ();
	}

	public static void setColor (Pixel pixel, Color color)
	{
		pixel.setColor (color);
	}

	public static PythonTuple getRGB (Pixel pixel)
	{
		return pixel.getRGB ();
	}

	public static PythonTuple getRGBA (Pixel pixel)
	{
		return pixel.getRGBA ();
	}

	public static int getGray (Pixel pixel)
	{
		return pixel.getGray ();
	}

	public static int getRed (Pixel pixel)
	{
		return pixel.getRed ();
	}

	public static int getGreen (Pixel pixel)
	{
		return pixel.getGreen ();
	}

	public static int getBlue (Pixel pixel)
	{
		return pixel.getBlue ();
	}

	public static int getAlpha (Pixel pixel)
	{
		return pixel.getAlpha ();
	}

	public static void setRGB (Pixel pixel, int red, int green, int blue)
	{
		pixel.setRGB ((byte)red, (byte)green, (byte)blue);
	}

	public static void setRGB (Pixel pixel, IList rgb)
	{
		pixel.setRGB ((byte)rgb [0], (byte)rgb [1], (byte)rgb [2]);
	}

	public static void setRGB (Pixel pixel, float red, float green, float blue)
	{
		pixel.setRGB ((byte)red, (byte)green, (byte)blue);
	}

	public static void setRGB (Pixel pixel, byte red, byte green, byte blue)
	{
		pixel.setRGB (red, green, blue);
	}

	public static void setRGBA (Pixel pixel, byte red, byte green, byte blue, byte alpha)
	{
		pixel.setRGBA (red, green, blue, alpha);
	}

	public static void setGray (Pixel pixel, byte value)
	{
		pixel.setGray (value);
	}

	public static void setRed (Pixel pixel, byte value)
	{
		pixel.setRed (value);
	}

	public static void setGreen (Pixel pixel, byte value)
	{
		pixel.setGreen (value);
	}

	public static void setBlue (Pixel pixel, byte value)
	{
		pixel.setBlue (value);
	}

	public static void setAlpha (Pixel pixel, byte value)
	{
		pixel.setAlpha (value);
	}
  
	public static void savePicture (Picture picture, string filename)
	{
		picture.savePicture (filename);
	}

	public static void savePicture (List list, string filename)
	{
		savePicture (list, filename, 10, false);
	}

	public static void savePicture (List list, string filename, short delay)
	{
		savePicture (list, filename, delay, false);
	}

	public static void savePicture (List list, string filename, short delay, bool repeat)
	{
		if (filename.Substring (filename.Length - 3, 3) != "gif") {
			throw new Exception ("saving a list of pictures creates an animated gif; use the gif extension");
		}
		List<GifLib.GifFrame> frameList = new List<GifLib.GifFrame> ();
		foreach (Graphics.Picture picture in list) {
			Gdk.Pixbuf pixbuf = picture.getPixbuf ();
			Byte [] buffer = pixbuf.SaveToBuffer ("png");
			MemoryStream ms = new MemoryStream (buffer);
			Bitmap bitmap = new Bitmap (ms);
			//bitmap.MakeTransparent(backColor);
			GifLib.GifFrame frame = GifLib.GifHelper.BitmapToFrame (bitmap);
			frameList.Add (frame);
		}
		GifLib.GifHelper.Merge (frameList, filename, delay, repeat);
	}

	public static Picture makePicture (int x, int y)
	{
		return new Picture (x, y);
	}

	public static Picture makePicture (int x, int y, Color c)
	{
		return new Picture (x, y, c);
	}

	public static Picture makePicture (string filename)
	{
		return new Picture (filename);
	}
  
	public static Picture copyPicture (Picture picture)
	{
		return new Picture (picture);
	}
  
	public static void Init ()
	{ 
		// Start a thread in Background to run Graphics
		// Only for use in non-GUI environments
		Thread t = new Thread (GraphicsLoop);
		t.Start ();
	}
  
	public static void GraphicsLoop ()
	{
		Gtk.Application.Init ();
		Gtk.Application.Run ();
	}
  
	public static Picture makePicture (WindowClass window)
	{ 
		return new Picture (window);
	}

	public static Graphics.WindowClass makeWindowFast (string title="Calico Graphics",
                                           int width=300,
                                           int height=300)
	{
		if (_windows.ContainsKey (title)) {
			_windows [title].clear (false);
			_lastWindow = _windows [title];
			_lastWindow.KeepAbove = true;
			return _windows [title];
		} else {
			_windows [title] = new Graphics.WindowClass (title, width, height);
			_lastWindow = _windows [title];
			_lastWindow.KeepAbove = true;
			Thread.Sleep ((int)(.1 * 1000)); // FIXME: wait for realize
			return _windows [title];
		}
	}

	public static void wait (double seconds)
	{
		Thread.Sleep ((int)(seconds * 1000));
    
		/*    ManualResetEvent mre = new ManualResetEvent(false);
        GLib.Timeout.Add(((uint)seconds * 1000), new GLib.TimeoutHandler( delegate {
                          mre.Set();
                          return false;
        }));
        mre.WaitOne();*/
	}

	public static Graphics.WindowClass makeWindow (string title="Calico Graphics",
                                           int width=300, 
                                           int height=300)
	{
		if (_windows.ContainsKey (title)) {
			_windows [title].clear ();
			_windows [title].mode = "auto";
			_windows [title].ShowAll ();
			_windows [title].Resize (width, height);
			_windows [title].QueueDraw ();
			Thread.Sleep ((int)(.1 * 1000)); // FIXME: wait for redraw
			/*
      Gtk.Application.Invoke(delegate { 
          _windows[title].GdkWindow.UrgencyHint = true;
        });
      */
			_lastWindow = _windows [title];
			return _windows [title];
		} else {
			_windows [title] = new Graphics.WindowClass (title, width, height);
			/*
      Gtk.Application.Invoke(delegate { 
          _windows[title].GdkWindow.UrgencyHint = true;
        });
      */
			_lastWindow = _windows [title];
			_lastWindow.KeepAbove = true;
			Thread.Sleep ((int)(.1 * 1000)); // FIXME: wait for realize
			return _windows [title];
		}
	}

	public static Graphics.WindowClass getWindow ()
	{
		if (_lastWindow != null) {
			return _lastWindow;
		} else {
			throw new Exception ("no windows exist yet");
		}
	}
  
	public static Graphics.WindowClass getWindow (string title)
	{
		if (_windows.ContainsKey (title)) {
			return _windows [title];
		} else {
			return null;
		}
	}

	public static Graphics.WindowClass Window (string title="Calico Graphics",
                                       int width=300, 
                                       int height=300)
	{
		return makeWindow (title, width, height);
	}

	public static Graphics.WindowClass Window (int width,
                                            int height)
	{
		return makeWindow ("Calico Graphics", width, height);
	}

	public static Color makeColor (string color)
	{
		return new Color (color);
	}

	public static Color makeColor (int r, int g, int b)
	{
		return new Color (r, g, b);
	}

	public static Color makeColor (int r, int g, int b, int a)
	{
		return new Color (r, g, b, a);
	}

	public static Color makeColor (double r, double g, double b)
	{
		return new Color (r, g, b);
	}

	public static Color makeColor (double r, double g, double b, double a)
	{
		return new Color (r, g, b, a);
	}

	public class Event
	{
		public double x;
		public double y;
		public object value;
		public double time;
		public string type;
		public string key;
    
		public Event (Gtk.ButtonReleaseEventArgs args)
		{
			type = "mouse-release";
			x = args.Event.X;
			y = args.Event.Y;
			time = args.Event.Time;
		}

		public Event (Gtk.KeyPressEventArgs args)
		{
			type = "key-press";
			time = args.Event.Time;
			key = args.Event.Key.ToString ();
		}

		public Event (Gtk.KeyReleaseEventArgs args)
		{
			type = "key-release";
			time = args.Event.Time;
			key = args.Event.Key.ToString ();
		}

		public Event (Gtk.ButtonPressEventArgs args)
		{
			type = "mouse-press";
			x = args.Event.X;
			y = args.Event.Y;
			time = args.Event.Time;
		}

		public Event (Gtk.MotionNotifyEventArgs args)
		{
			type = "mouse-motion";
			x = args.Event.X;
			y = args.Event.Y;
			time = args.Event.Time;
		}

		public Event (System.EventArgs args)
		{
			type = "system-event";
		}

		public Event (string args)
		{
			type = args;
		}

		public Event (string args, double time)
		{
			type = args;
			this.time = time;
		}

		public Event (string args, object value, double time)
		{
			type = args;
			this.value = value;
			this.time = time;
		}

		public override string ToString ()
		{
			if (time == 0 && x == 0 && y == 0) {
				return String.Format ("<Event \"{0}\">", type);
			} else if (x == 0 && y == 0) {
				return String.Format ("<Event \"{0}\" at {1}>", type, time);
			} else {
				return String.Format ("<Event \"{0}\" ({1},{2}) at {3}>", 
			     type, x, y, time);
			}
		}

		public string __repr__ ()
		{
			return ToString ();
		}

	}
  
	public class Plot
	{

		public WindowClass window;
		public List data = new List ();
		public Line line = new Line ();
		int border = 50;
		public Text xLabel;
		public Text yLabel;

		public Plot (IList list) : this("Plot", 640, 480)
		{
			foreach (double item in list) {
				append (item);
			}
		}

		public Plot (string title, int width, int height)
		{
			window = makeWindow (title, width, height);
			Line tick;
			Rectangle rect = new Rectangle (new Point (border, border), 
                                     new Point (width - border, height - border));
			rect.fill = new Color ("white");
			rect.outline = new Color ("black");
			rect.tag = "line";
			rect.draw (window);
			// x ticks:
			int interval = (width - border * 2) / 10;
			for (int x = border; x <= width - border; x += interval) {
				tick = new Line (new Point (x, height - border), 
                        new Point (x, height - border + 10));
				tick.outline = new Color ("black");
				tick.tag = "line";
				tick.draw (window);
			}
			// y ticks:
			interval = (height - border * 2) / 10;
			for (int y = height - border; y >= border; y -= interval) {
				tick = new Line (new Point (border - 10, y), 
                        new Point (border, y));
				tick.outline = new Color ("black");
				tick.tag = "line";
				tick.draw (window);
			}
			yLabel = new Text (new Point (border / 3, height / 2), "y legend");
			yLabel.fill = new Color ("black");
			yLabel.rotate (90);
			yLabel.draw (window);
			xLabel = new Text (new Point (width / 2, height - border / 3), "x legend");
			xLabel.fill = new Color ("black");
			xLabel.draw (window);
		}

		public void append (int datum)
		{
			append (System.Convert.ToDouble (datum));
		}

		public void append (double datum)
		{
			line.undraw ();
			data.Add (datum);
			if (data.Count > 1) {
				line = new Line ();
				line.outline = new Color ("red");
				int col = 0;
				double h;
				int increment = (window.width - 2 * border) / (data.Count - 1);
				double min = 10000;
				double max = -10000;
				foreach (double i in data) {
					min = Math.Min (min, i);
					max = Math.Max (max, i);
				}
				if (increment == 0) {
					increment = 1;
				}
				foreach (double i in data) {
					if (max != min) {
						h = (window.height - border * 2) * (i - min) / (max - min);
					} else {
						h = (window.height - border * 2) * .5;
					}
					line.append (new Point (border + col, window.height - border - h));
					col += increment;
				}
				line.set_points ();
				line.draw (window);

				// remove previous tick numbers:
				List to_remove = new List ();
				lock (window.canvas.shapes) {
					foreach (Shape shape in window.canvas.shapes) {
						if (shape.tag == "tick")
							to_remove.Add (shape);
					}
					foreach (Shape shape in to_remove) {
						window.canvas.shapes.Remove (shape);
					}
				}
				// x ticks:
				int interval = (window.width - border * 2) / 10;
				int int_value = data.Count / 10;
				int count = 1;
				Text text;
				for (int x = border; x <= window.width - border; x += interval) {
					text = new Text (new Point (x, window.height - border + 20), count.ToString ());
					text.outline = new Color ("black");
					text.fontSize = 9;
					text.tag = "tick";
					text.draw (window);
					count += int_value;
				}
				// y ticks:
				interval = (window.height - border * 2) / 10;
				double interval_value = (max - min) / 10;
				double sum = min;
				for (int y = window.height - border; y >= border; y -= interval) {
					text = new Text (new Point (border - 20, y), String.Format("{0:0.###}", sum));
					text.outline = new Color ("black");
					text.fontSize = 9;
					text.tag = "tick";
					text.draw (window);
					sum += interval_value;
				}
			}
		}
    
	}

	public class BarChart
	{

		public WindowClass window;
		public List data = new List ();
		int border = 50;
		public Text xLabel;
		public Text yLabel;

		public BarChart (string title, int width, int height)
		{
			window = makeWindow (title, width, height);
			Line tick;
			Rectangle rect = new Rectangle (new Point (border, border), 
                                     new Point (width - border, height - border));
			rect.fill = new Color ("white");
			rect.outline = new Color ("black");
			rect.tag = "line";
			rect.draw (window);
			// x ticks:
			int interval = (width - border * 2) / 10;
			for (int x = border; x <= width - border; x += interval) {
				tick = new Line (new Point (x, height - border), 
                        new Point (x, height - border + 10));
				tick.outline = new Color ("black");
				tick.tag = "line";
				tick.draw (window);
			}
			// y ticks:
			interval = (height - border * 2) / 10;
			for (int y = height - border; y >= border; y -= interval) {
				tick = new Line (new Point (border - 10, y), 
                        new Point (border, y));
				tick.outline = new Color ("black");
				tick.tag = "line";
				tick.draw (window);
			}
			yLabel = new Text (new Point (border / 3, height / 2), "y legend");
			yLabel.fill = new Color ("black");
			yLabel.rotate (90);
			yLabel.draw (window);
			xLabel = new Text (new Point (width / 2, height - border / 3), "x legend");
			xLabel.fill = new Color ("black");
			xLabel.draw (window);
		}

		public void append (int datum)
		{
			append (System.Convert.ToDouble (datum));
		}

		public void append (double datum)
		{
			data.Add (datum);
		}
	}

	public class Color
	{
		internal Cairo.Color _cairo;
		public WindowClass window;  // for setting color of a Window()
		public Picture picture; // for setting color of Picture()
		public int x = -1; // for use with picture, above
		public int y = -1; // for use with picture, above

		public Cairo.Color getCairo ()
		{
			return _cairo;
		}
    
		public Color (string name)
		{
			if (name.StartsWith ("#")) {
				int r = (int)System.Convert.ToUInt32 (name.Substring (1, 2), 16);
				int g = (int)System.Convert.ToUInt32 (name.Substring (3, 2), 16);
				int b = (int)System.Convert.ToUInt32 (name.Substring (5, 2), 16);
				_cairo = new Cairo.Color (ToCairo (r), ToCairo (g), ToCairo (b));
			} else if (colors.ContainsKey (name.ToLower ())) {
				_cairo = colors [name.ToLower ()]._cairo;
			} else {
				throw new Exception (String.Format ("unknown colorname '{0}'", name));
			}
		}

		public Color (Color color)
		{
			_cairo = new Cairo.Color (color._cairo.R, color._cairo.G, color._cairo.B, color._cairo.A);
		}

		public Color (int r, int g, int b)
		{
			_cairo = new Cairo.Color (ToCairo (r), ToCairo (g), ToCairo (b), 1.0);
		}

		public Color (int r, int g, int b, int a)
		{
			_cairo = new Cairo.Color (ToCairo (r), ToCairo (g), ToCairo (b), ToCairo (a));
		}

		public Color (double r, double g, double b)
		{
			_cairo = new Cairo.Color (ToCairo (r), ToCairo (g), ToCairo (b), 1.0);
		}

		public Color (double r, double g, double b, double a)
		{
			_cairo = new Cairo.Color (ToCairo (r), ToCairo (g), ToCairo (b), ToCairo (a));
		}

		public Color Copy ()
		{
			return new Color (red, green, blue, alpha);
		}

		public void QueueDraw ()
		{ // color
			if (window is WindowClass) {
				if (window.getMode () == "auto" || 
	    window.getMode () == "bitmap" || 
            window.getMode () == "physics")
					window.update ();
				// else, manually call step()
			} 
		}
    
		public int red {
			get {
				return FromCairo (_cairo.R);
			}
			set {
				_cairo.R = ToCairo (value);
				if (picture is Picture) {
					if (x >= 0 && y >= 0)
						picture.setRed (x, y, (byte)value);
					else {
						foreach (Pixel pixel in picture.getPixels()) {
							pixel.setRed ((byte)value);
						}
					}
				} else
					QueueDraw ();
			}
		}

		public int green {
			get {
				return FromCairo (_cairo.G);
			}
			set {
				_cairo.G = ToCairo (value);
				if (picture is Picture) {
					if (x >= 0 && y >= 0)
						picture.setGreen (x, y, (byte)value);
					else {
						foreach (Pixel pixel in picture.getPixels()) {
							pixel.setGreen ((byte)value);
						}
					}
				} else
					QueueDraw ();
			}
		}

		public int blue {
			get {
				return FromCairo (_cairo.B);
			}
			set {
				_cairo.B = ToCairo (value);
				if (picture is Picture) {
					if (x >= 0 && y >= 0)
						picture.setBlue (x, y, (byte)value);
					else {
						foreach (Pixel pixel in picture.getPixels()) {
							pixel.setBlue ((byte)value);
						}
					}
				} else
					QueueDraw ();
			}
		}

		public int alpha {
			get {
				return FromCairo (_cairo.A);
			}
			set {
				_cairo.A = ToCairo (value);
				if (picture is Picture) {
					if (x >= 0 && y >= 0)
						picture.setAlpha (x, y, (byte)value);
					else {
						foreach (Pixel pixel in picture.getPixels()) {
							pixel.setAlpha ((byte)value);
						}
					}
				} else
					QueueDraw ();
			}
		}

		double ToCairo (int value)
		{
			return Math.Max (Math.Min ((value / 255.0), 1.0), 0.0);
		}
    
		double ToCairo (double value)
		{
			return Math.Max (Math.Min ((value / 255.0), 1.0), 0.0);
		}
    
		int FromCairo (double value)
		{
			return (int)(value * 255);
		}
    
		public override string ToString ()
		{
			return String.Format ("<Color (r={0},g={1},b={2},a={3})>", 
                           red, green, blue, alpha);
		}
		public string __repr__ ()
		{
		    return ToString();
		}    
	}

	public class Gradient
	{
		public string gtype;
		public Color c1, c2;
		public Point p1, p2;
		public double radius1, radius2;
    
		// ("linear", (100, 200), Color("red"), (200, 100), Color("blue"))
		public Gradient (string gtype, IList p1, Color c1, IList p2, Color c2)
		{
			this.gtype = gtype;
			this.p1 = new Point (p1 [0], p1 [1]);
			this.p2 = new Point (p2 [0], p2 [1]);
			this.c1 = c1;
			this.c2 = c2;
		}

		public Gradient (string gtype, 
                    IList p1, double radius1, Color c1, 
                    IList p2, double radius2, Color c2)
		{
			this.gtype = gtype;
			this.p1 = new Point (p1 [0], p1 [1]);
			this.radius1 = radius1;
			this.c1 = c1;
			this.p2 = new Point (p2 [0], p2 [1]);
			this.radius2 = radius2;
			this.c2 = c2;
		}
	}
  
	public class WindowClass : Gtk.Window
	{
		internal Canvas _canvas;
		internal bool _dirty = false;
		private bool timer_running = false;
		private DateTime last_update = new DateTime (2000, 1, 1);
		internal double _update_interval = .1; // how often, in seconds, to update
		public List onClickCallbacks = new List ();
		public List onMouseMovementCallbacks = new List ();
		public List onMouseUpCallbacks = new List ();
		public List onKeyPressCallbacks = new List ();
		public List onKeyReleaseCallbacks = new List ();
		public PythonTuple _lastClick;
		public string _lastKey = "";
		public string _mouseState = "up";
		public string _keyState = "up";
		ManualResetEvent _lastClickFlag = new ManualResetEvent (false);
		public double time = 0.0;
		public double simulationStepTime = 0.01;
		public string state = "init";
		public Gdk.Color bg = new Gdk.Color (255, 255, 255);
		public bool requestStop = false;
		Gtk.ScrolledWindow _scrolledWindow = null;

		public WindowClass (string title="Calico Graphics",
                  int width=300, 
                  int height=300) : base(title)
		{
			_canvas = new Canvas ("auto", width, height);
			//DoubleBuffered = false;
			AllowGrow = true;
			AllowShrink = true;
			SetDefaultSize (width, height);
			AddEvents ((int)Gdk.EventMask.ButtonPressMask);
			AddEvents ((int)Gdk.EventMask.ButtonReleaseMask);
			AddEvents ((int)Gdk.EventMask.PointerMotionMask);
			AddEvents ((int)Gdk.EventMask.KeyReleaseMask);
			AddEvents ((int)Gdk.EventMask.KeyPressMask);
			ButtonPressEvent += HandleClickCallbacks;
			ButtonReleaseEvent += HandleMouseUpCallbacks;
			ButtonPressEvent += saveLastClick;
			ButtonReleaseEvent += updateMouseState;
			MotionNotifyEvent += HandleMouseMovementCallbacks;
			KeyPressEvent += HandleKeyPressCallbacks;
			KeyReleaseEvent += HandleKeyReleaseCallbacks;
			//ConfigureEvent += configureEventBefore;
			DeleteEvent += OnDelete;
			Add(_canvas);
			ShowAll ();
		}
		
		public void removeTagged(String tag) {
		    List to_remove = new List ();
		    lock (_canvas.shapes) {
			foreach (Shape shape in _canvas.shapes) {
			    if (shape.tag == tag)
				to_remove.Add (shape);
			}
			foreach (Shape shape in to_remove) {
			    _canvas.shapes.Remove (shape);
			}
		    }
		}
		
		public void addScrollbars(int width, int height) {
			if (Child == _canvas) {
				Invoke( delegate {
					Remove(_canvas);
					_scrolledWindow = new Gtk.ScrolledWindow();
					Add(_scrolledWindow);
					_scrolledWindow.Add (_canvas);
					_canvas.resize(width, height);
					_scrolledWindow.Show();
				});
			} else {
				Invoke( delegate {
					_canvas.resize(width, height);
				});
			}
		}
		
		public void clear ()
		{
			clear (true);
		}
    
		public void clear (bool redraw)
		{
			_canvas.surface = new Cairo.ImageSurface (Cairo.Format.Argb32, 
			// FIXME: w,h of Window?
					       (int)800, 
					       (int)600);
			_canvas.need_to_draw_surface = false;

			mode = "auto";
			Resize (width, height);
			timer_running = false;
			last_update = new DateTime (2000, 1, 1);
			_update_interval = .1; // how often, in seconds, to update
			onClickCallbacks = new List ();
			onMouseMovementCallbacks = new List ();
			onMouseUpCallbacks = new List ();
			onKeyPressCallbacks = new List ();
			onKeyReleaseCallbacks = new List ();
			_lastKey = "";
			_mouseState = "up";
			_keyState = "up";
			time = 0.0;
			simulationStepTime = 0.01;
			state = "init";
			lock (_canvas.shapes)
				_canvas.shapes.Clear ();
			Invoke (delegate {
				foreach (Gtk.Widget child in _canvas.Children) {
					_canvas.Remove (child);
				}
			});
			if (redraw)
				QueueDraw ();
		}
		
		public Gtk.ScrolledWindow ScrolledWindow {
			get { return _scrolledWindow; }
		}

		public Microsoft.Xna.Framework.Vector2 gravity {
			get {
				return canvas.world.Gravity;
			}
			set {
				canvas.world.Gravity = value;
			}
		}

		public void close ()
		{
			Invoke (delegate { 
				Hide ();
			});
		}

		public void setBackground (Color color)
		{
			bg = new Gdk.Color ((byte)color.red, 
                  (byte)color.green, 
                  (byte)color.blue);
			_canvas.ModifyBg (Gtk.StateType.Normal, bg);
		}
    
		public Gdk.Drawable getDrawable ()
		{
			return GdkWindow;
		}

		private void OnDelete (object obj, Gtk.DeleteEventArgs args)
		{
			_windows.Remove (Title);
		}
    
		public int getWidth ()
		{
			int _width, _height;
			this.GetSize (out _width, out _height);
			return width;
		}
    
		public int getHeight ()
		{
			int _width, _height;
			this.GetSize (out _width, out _height);
			return _height;
		}
    
		public void draw (Shape shape)
		{
			shape.draw (this);
		}

		public void undraw (Shape shape)
		{
			shape.undraw ();
		}

		public void stackOnTop (Shape shape)
		{
			// last drawn is on top
			if (_canvas.shapes.Contains (shape)) {
				lock (_canvas.shapes) {
					_canvas.shapes.Remove (shape);
					_canvas.shapes.Insert (_canvas.shapes.Count, shape);
				}
				QueueDraw ();
			}
		}

		public void stackOnBottom (Shape shape)
		{
			// first drawn is on bottom
			if (_canvas.shapes.Contains (shape)) {
				lock (_canvas.shapes) {
					_canvas.shapes.Remove (shape);
					_canvas.shapes.Insert (0, shape);
				}
				QueueDraw ();
			} else {
				throw new Exception ("shape not drawn on window");
			}
		}

		void saveLastClick (object obj, Gtk.ButtonPressEventArgs args)
		{
			_mouseState = "down";
			_lastClick = PyTuple (args.Event.X, args.Event.Y);
			_lastClickFlag.Set ();
		}
    
		void updateMouseState (object obj, Gtk.ButtonReleaseEventArgs args)
		{
			_mouseState = "up";
		}
    
		private void HandleMouseMovementCallbacks (object obj,
                      Gtk.MotionNotifyEventArgs args)
		{
			Event evt = new Event (args);
			foreach (Func<object,Event,object> function in onMouseMovementCallbacks) {
				try {
					Invoke (delegate {
						Func<object,Event,object > f = (Func<object,Event,object>)function;
						f (obj, evt);
					});
				} catch (Exception e) {
					Console.Error.WriteLine ("Error in onMouseMove function");
					Console.Error.WriteLine (e.Message);
				}        
			}
		}
    
		private void HandleClickCallbacks (object obj,
                                      Gtk.ButtonPressEventArgs args)
		{
			Event evt = new Event (args);
			foreach (Func<object,Event,object> function in onClickCallbacks) {
				try {
					Invoke (delegate {
						Func<object,Event,object > f = (Func<object,Event,object>)function;
						f (obj, evt);
					});
				} catch (Exception e) {
					Console.Error.WriteLine ("Error in onMouseDown function");
					Console.Error.WriteLine (e.Message);
				}        
			}
		}
    
		private void HandleMouseUpCallbacks (object obj,
                                      Gtk.ButtonReleaseEventArgs args)
		{
			Event evt = new Event (args);
			foreach (Func<object,Event,object> function in onMouseUpCallbacks) {
				try {
					Invoke (delegate {
						Func<object,Event,object > f = (Func<object,Event,object>)function;
						f (obj, evt);
					});
				} catch (Exception e) {
					Console.Error.WriteLine ("Error in onMouseUp function");
					Console.Error.WriteLine (e.Message);
				}        
			}
		}
    
    [GLib.ConnectBefore]
		private void HandleKeyPressCallbacks (object obj,
                                      Gtk.KeyPressEventArgs args)
		{
			_lastKey = args.Event.Key.ToString ();
			_keyState = "down";
			Event evt = new Event (args);
			foreach (Func<object,Event,object> function in onKeyPressCallbacks) {
				try {
					Invoke (delegate {
						Func<object,Event,object > f = (Func<object,Event,object>)function;
						f (obj, evt);
					});
				} catch (Exception e) {
					Console.Error.WriteLine ("Error in onKeypress function");
					Console.Error.WriteLine (e.Message);
				}        
			}
		}
    
		private void HandleKeyReleaseCallbacks (object obj,
                                      Gtk.KeyReleaseEventArgs args)
		{
			_keyState = "up";
			Event evt = new Event (args);
			foreach (Func<object,Event,object> function in onKeyReleaseCallbacks) {
				try {
					Invoke (delegate {
						Func<object,Event,object > f = (Func<object,Event,object>)function;
						f (obj, evt);
					});
				} catch (Exception e) {
					Console.Error.WriteLine ("Error in onKeyRelease function");
					Console.Error.WriteLine (e.Message);
				}        
			}
		}
    
		public void onClick (Func<object,Event,object> function)
		{
			onClickCallbacks.Add (function);
		}
    
		public void onMouseDown (Func<object,Event,object> function)
		{
			onClickCallbacks.Add (function);
		}

		public void run (Func<object> function)
		{
			try {
				// FIXME: Understand why:
				// This does not like a Gtk Application Invoke here
				function ();
			} catch (Exception e) {
				if (!e.Message.Contains ("Thread was being aborted")) {
					Console.Error.WriteLine ("Error in run function");
					Console.Error.WriteLine (e.Message);
				}
			}
		}

		public void run ()
		{
		  requestStop = false;
		  while (! requestStop)
			step (.01);
		}

		public void onMouseUp (Func<object,Event,object> function)
		{
			onMouseUpCallbacks.Add (function);
		}
    
		public void onMouseMovement (Func<object,Event,object> function)
		{
			onMouseMovementCallbacks.Add (function);
		}
    
		public void onKeyPress (Func<object,Event,object> function)
		{
			onKeyPressCallbacks.Add (function);
		}
    
		public void onKeyRelease (Func<object,Event,object> function)
		{
			onKeyReleaseCallbacks.Add (function);
		}
    
		public double updateInterval {
			get {
				return _update_interval;
			}
			set {
				_update_interval = value;
			}
		}
    
		public int height {
			get {
			  if (Child == _canvas) {
			    int _width, _height;
			    this.GetSize (out _width, out _height);
			    return _height;
			  } else { // scrollbars
			    return _canvas.height;
			  }
			}
		}

		public int width {
			get {
			  if (Child == _canvas) {
				int _width, _height;
				this.GetSize (out _width, out _height);
				return _width;
			  } else { // scrollbars
			    return _canvas.width;
			  }
			}
		}

		public Canvas getCanvas ()
		{
			return _canvas;
		}
    
		public Canvas canvas {
			get {
				return _canvas;
			}
		}

		public PythonTuple getMouse ()
		{
			while (Gtk.Application.EventsPending())
				Gtk.Application.RunIteration ();
			_lastClickFlag = new ManualResetEvent (false);
			_lastClickFlag.WaitOne ();
			return _lastClick;
		}
    
		public PythonTuple getMouseNow ()
		{
			int x = 0, y = 0;
			ManualResetEvent mre = new ManualResetEvent (false);
			Invoke (delegate { 
				GetPointer (out x, out y);
				mre.Set ();
			});
			mre.WaitOne ();
			return PyTuple (x, y);
		}
    
		public string getMouseState ()
		{
			return _mouseState;
		}
    
		public string getKeyPressed ()
		{
			string lk = _lastKey;
			_lastKey = "";
			return lk;
		}
    
		public string getKeyState ()
		{
			return _keyState;
		}
    
		public new void Show ()
		{
			Invoke (delegate { 
				DateTime now = DateTime.Now;
				last_update = now;
				_dirty = false;
				base.Show (); 
			});
		}

		public new void ShowAll ()
		{
			Invoke (delegate { 
				DateTime now = DateTime.Now;
				last_update = now;
				_dirty = false;
				base.ShowAll (); 
			});
		}

		public new void Resize (int width, int height)
		{
			_canvas.resize (width, height);
			Invoke (delegate {
				base.Resize (width, height);
			});
		}
    
		public void need_to_redraw ()
		{
			_dirty = true;
			DateTime now = DateTime.Now;
			// diff is TimeSpan
			if ((now - last_update).TotalMilliseconds < (updateInterval * 1000)) {
				// pass, too soon!
				// but we need to make sure that someone checks
				// in the future. 
				if (timer_running) {
					// already one running!
					// we'll just wait
				} else {
					// let's spawn one to check in 100 ms or so
					timer_running = true;
					GLib.Timeout.Add ((uint)(updateInterval * 1000), 
                           new GLib.TimeoutHandler (_redraw_now));
				}
			} else { // it is not too soon
				if (timer_running) {
					// no need to start another
				} else {
					// let's spawn one to check in 100 ms or so
					timer_running = true;
					GLib.Timeout.Add ((uint)(updateInterval * 1000), 
                           new GLib.TimeoutHandler (_redraw_now));
				}
			}
		}
    
		private bool _redraw_now ()
		{
			DateTime now = DateTime.Now;
			if (_dirty) {
				last_update = now;
				_dirty = false;
				QueueDraw (); // gtk
			}
			timer_running = false;
			return false; // return true to try again
		}

		public string getMode ()
		{
			return _canvas.mode;
		}

		public string mode {
			get {
				return _canvas.mode;
			}
			set {
				if (value == "auto" || value == "manual" || value == "physics" || 
	    value == "bitmap" || value == "bitmapmanual")
					canvas.mode = value;
				else
					throw new Exception ("window mode must be 'auto', 'manual', 'bitmap', 'bitmapmanual', or 'physics'");
			}
		}

		public void updateNow ()
		{ // Window
			need_to_redraw ();
			// Manual call to update, let's update then:
			while (Gtk.Application.EventsPending())
				Gtk.Application.RunIteration ();
		}

		public void update ()
		{ // Window
			if (mode == "manual" || mode == "bitmapmanual") {
				_canvas.need_to_draw_surface = true;
				QueueDraw ();
			} else {
				need_to_redraw ();
			}
		}

		public void step ()
		{ // Window
			step (0);
		}

		public void step (double step_time)
		{ // Window, in seconds
			// Same as update, but will make sure it 
			// doesn't update too fast.
			// handle physics
		  if (_canvas == null ) {
			requestStop = true;
			return;
		  }
			// kjo
			_canvas.need_to_draw_surface = true;
      
			if (mode == "physics") {
				_canvas.world.Step ((float)simulationStepTime); 
				time += simulationStepTime; 
				// update the sprites
				lock (_canvas.shapes) {
					foreach (Shape shape in _canvas.shapes) {
						shape.updateFromPhysics ();
					}
				}
			}
			// and now the update
			DateTime now = DateTime.Now;
			// diff is TimeSpan, converted to seconds:
			double diff = (now - last_update).TotalMilliseconds / 1000.0;
			while (diff < step_time) {
				Thread.Sleep ((int)(diff / 10 * 1000)); // 10 times per diff
				now = DateTime.Now;
				diff = (now - last_update).TotalMilliseconds / 1000.0;
			}
			last_update = DateTime.Now;
      
			if (mode == "bitmapmanual") {
				using (Cairo.Context g = new Cairo.Context(_canvas.finalsurface)) {
					g.Save ();
					g.Operator = Cairo.Operator.Source;
					g.SetSourceSurface (_canvas.surface, 0, 0);
					g.Paint ();
					g.Restore ();
				}	   
			}      
	  
			_dirty = false;
			ManualResetEvent ev = new ManualResetEvent (false);
			Invoke (delegate { 
				QueueDraw ();
				GdkWindow.ProcessUpdates (true);
				ev.Set ();
			});
			ev.WaitOne ();
		}

		public override string ToString ()
		{
			return String.Format ("<Window (title='{0}',width={1},height={2})>", 
                           Title, width, height);
		}
		public string __repr__ ()
		{
		    return ToString();
		}
	}
  
	public static void ShowAll (object o)
	{
		Invoke (delegate {
			((Gtk.Widget)o).ShowAll (); });
	}
  
	public static void Show (object o)
	{
		Invoke (delegate {
			((Gtk.Widget)o).Show (); });
	}
  
	public static Vector2 VectorRotate (Vector2 v, double angle)
	{
		Microsoft.Xna.Framework.Matrix m = Microsoft.Xna.Framework.Matrix.CreateRotationZ ((float)angle);
		return Vector2.Transform (v, m);
	}

	public static Vector2 Vector (int x, int y)
	{
		return new Vector2 ((float)x, (float)y);
	}
  
	public static Vector2 Vector (double x, double y)
	{
		return new Vector2 ((float)x, (float)y);
	}

	public class Point : IList
	{
		public double x;
		public double y;

		public Point (IList iterable): this(iterable[0], iterable[1])
		{
		}
    
		public Point (object x, object y)
		{
			this.x = System.Convert.ToDouble (x);
			this.y = System.Convert.ToDouble (y);
		}
    
		public Point (int x, int y)
		{
			this.x = System.Convert.ToDouble (x);
			this.y = System.Convert.ToDouble (y);
		}
    
		public Point (double x, double y)
		{
		    this.x = x;
		    this.y = y;
		}
    
		public Point (Dot dot)
		{
			this.x = dot.x;
			this.y = dot.y;
		}

		public double distance (Point p)
		{
			return Math.Sqrt (Math.Pow (x - p.x, 2) + Math.Pow (y - p.y, 2));
		}

		public override string ToString ()
		{
			return String.Format ("<Point (x={0},y={1})>", x, y);
		}

		public string __repr__ ()
		{
			return String.Format ("<Point (x={0},y={1})>", x, y);
		}

		public void draw (WindowClass window)
		{
			throw new Exception ("Can't draw a point; use Dot instead");
		}

		// Items necessary for it to be an IList
		public bool IsFixedSize {
			get {
				return true;
			}
		}
    
		public bool IsReadOnly {
			get {
				return false;
			}
		}
    
		public bool IsSynchronized {
			get {
				return false;
			}
		}
    
		public void CopyTo (System.Array array, int index)
		{
		}
    
		public int Add (object value)
		{
			return 1; // should return count
		}
    
		public int Count {
			get {
				return 2;
			}
		}
    
		public void Remove (object value)
		{
		}
    
		public void RemoveAt (int index)
		{
		}
    
		public void Clear ()
		{
		}
    
		public bool Contains (object value)
		{
			return false;
		}
    
		public int IndexOf (object value)
		{
			return -1;
		}
    
		public void Insert (int index, object value)
		{
		}
    
		public object this [int index] {
			get {
				if (index == 0) {
					return x;
				} else if (index == 1) {
					return y;
				} else {
					throw new Exception ("invalid access of Point");
				}
			}
			set { // value is the item
				if (index == 0) {
					x = System.Convert.ToDouble (value);
				} else if (index == 1) {
					y = System.Convert.ToDouble (value);
				}
			}
		}

		public object SyncRoot {
			get {
				return this;
			}
		}
    
		public IEnumerator GetEnumerator ()
		{
			// Refer to the IEnumerator documentation for an example of
			// implementing an enumerator.
			throw new Exception ("The method or operation is not implemented.");
		}
	}
  
	public class Canvas : Gtk.Layout
	{
    
		// Shape.draw() will add them here:
		public List<Shape> shapes = new List<Shape> ();
		private string _mode;
		public FarseerPhysics.Dynamics.World world;
		public object document;
		public int width = 800, height = 600 ;
		public Cairo.ImageSurface surface = null;
		public Cairo.ImageSurface finalsurface;
		public bool need_to_draw_surface = false;
    
		public Canvas (string mode, int width, int height) : base(null, null)
		{
			this.mode = mode;
			resize (width, height);
		}
    
		public Canvas (string mode, Gtk.Adjustment h, Gtk.Adjustment v) : base(h, v)
		{
			this.mode = mode;
			resize (width, height);
		}
        
		public string mode {
			get {
				return _mode;
			}
			set {
				if (value == "manual" || value == "auto" || value == "physics" || 
	    			value == "bitmap" || value == "bitmapmanual") {
					_mode = value;
	  
					if (value == "physics")
						initPhysics ();
					resetSurfaces ();
	  
				} else
					throw new Exception ("canvas mode must be 'manual', 'auto', 'bitmap', 'bitmapmanual' or 'physics'");
			}
		}
    
		void initPhysics ()
		{
			world = new FarseerPhysics.Dynamics.World (new Vector2 (0.0f, 9.8f));
		}

		void resetSurfaces ()
		{
			surface = new Cairo.ImageSurface (Cairo.Format.Argb32, 
				       width,
				       height);	  
			if (mode == "bitmapmanual") {
				finalsurface = new Cairo.ImageSurface (Cairo.Format.Argb32, 
				// FIXME: w,h of Window?
						 width, 
						 height);
			} else {
				finalsurface = surface;      	      
			}      
		}

		public void resize (int width, int height)
		{
			this.width = width;
			this.height = height;
			SetSize((uint)width, (uint)height);
			resetSurfaces ();
		}

		protected override bool OnExposeEvent (Gdk.EventExpose args)
		{
			using (Cairo.Context g = Gdk.CairoHelper.Create(args.Window)) {
				if (need_to_draw_surface) {
					g.Save ();
					g.SetSourceSurface (finalsurface, 0, 0);
					g.Paint ();
					g.Restore ();
				}
				lock (shapes) {
					foreach (Shape shape in shapes) {
						shape.render (g);
						shape.updateGlobalPosition (g);
					}
				}
			}
			return base.OnExposeEvent (args);
		}
	}
  
	public class Shape
	{
		public Point center;
		public string tag;
		public WindowClass window;
	        public Shape drawn_on_shape = null;
		internal double _rotation; // internally radians
		internal double _scaleFactor; // percent
		public FarseerPhysics.Dynamics.World world;
		public FarseerPhysics.Dynamics.Body body;
		internal FarseerPhysics.Dynamics.BodyType _bodyType;
		internal float _bounce;
		internal float _friction;
		internal float _density;
		public List<Shape> shapes = new List<Shape> ();
		public double gx = 0.0;
		public double gy = 0.0;
		public bool visible = true;
		public Point[] points;
		internal Color _fill;
		internal Color _outline;
		public Gradient _gradient;
		private int _border;
		public bool wrap = false;
		private Pen _pen;
		private bool _has_pen;
		internal bool close_path = true;
        
		public Shape (bool has_pen=true)
		{
			_rotation = 0;
			_scaleFactor = 1.0;
			center = new Point (0, 0);
			this.has_pen = has_pen;
			if (this.has_pen) 
				pen = new Pen (new Color (0, 0, 0), 1);
			color = new Color ("purple");
			outline = new Color ("black");
			border = 1;
			_bounce = 0.8f;
			_friction = 0.5f;
			_density = 1.0f;
			_bodyType = FarseerPhysics.Dynamics.BodyType.Dynamic;
		}
    
		// FIXME: points are in relative to center coordinates
		// FIXME: set x,y of points should go from screen_coords to relative
		// FIXME: should call QueueDraw on set

		public virtual bool hit (double x, double y)
		{
			return false;
		}

		public void connect (string signal, Func<object,Event,object> function)
		{
			if (signal == "click") {
				window.onMouseDown (delegate (object obj, Event evt) {
					if (hit (evt.x, evt.y)) {
						try {
							Invoke (delegate {
								function (obj, evt);
							});
						} catch (Exception e) {
							Console.Error.WriteLine ("Error in connect('click') function");
							Console.Error.WriteLine (e.Message);
						}        
						return true;
					}
					return false;
				});
			} else {
				throw new Exception ("invalid signal for this object");
			}
		}

		public double bounce {
			get {
				if (body != null)
					return body.Restitution;
				else
					return _bounce;
			}
			set {
				if (body != null)
					body.Restitution = (float)value;
				else
					_bounce = (float)value;
			}
		}

		public double friction {
			get {
				if (body != null)
					return body.Friction;
				else
					return _friction;
			}
			set {
				if (body != null)
					body.Friction = (float)value;
				else
					_friction = (float)value;
			}
		}

		public double density {
			get {
				return _density;
			}
			set {
				_density = (float)value;
			}
		}

		public double mass {
			get {
				if (body != null)
					return body.Mass;
				else
					throw new Exception ("need to draw shape first");
			}
			set {
				if (body != null)
					body.Mass = (float)value;
				else
					throw new Exception ("need to draw shape first");
			}
		}

		public string bodyType { // shape
			get {
				if (_bodyType == FarseerPhysics.Dynamics.BodyType.Dynamic) {
					return "dynamic";
				} else if (_bodyType == FarseerPhysics.Dynamics.BodyType.Static) {
					return "static";
				} else {
					return "unkown";
				}
			}
			set {
				if (value == "dynamic") {
					_bodyType = FarseerPhysics.Dynamics.BodyType.Dynamic;
					if (body != null)
						body.IsStatic = false;
				} else if (value == "static") {
					_bodyType = FarseerPhysics.Dynamics.BodyType.Static;
					if (body != null)
						body.IsStatic = true;
				} else {
					throw new Exception ("bodyType must be 'dynamic' or 'static'");
				}
			}
		}

		public virtual void addToPhysics ()
		{ // Shape
		}

		public virtual void removeFromPhysics ()
		{ // Shape
			//if (body != null) {
			//	body.UserData = null; // point back to this shape
			//	body.FixtureList[0].UserData = null; // point back to this shape
			//}
		    if (body != null) {
			body.DestroyFixture(body.FixtureList[0]);
			body = null;
		    }
		}

		public virtual void updateFromPhysics ()
		{
			// get from body, put in sprite
			if (body != null) {
				float MeterInPixels = 64.0f;
				if (wrap) {
					float x = (float)wrap_width ((float)(body.Position.X * MeterInPixels));
					float y = (float)wrap_height ((float)(body.Position.Y * MeterInPixels));
					body.Position = new Vector2 (x / MeterInPixels, y / MeterInPixels);
				}
				Vector2 position = body.Position * MeterInPixels;
				double rotation = body.Rotation * 180.0 / Math.PI; 
				// Move it
				_moveTo (position.X, position.Y);
				_rotateTo (rotation);
			}
		}

		public void updatePhysics ()
		{
			// get from sprite, put in body
			if (body != null) {
				float MeterInPixels = 64.0f;
				body.Position = new Vector2 (((float)x) / MeterInPixels, 
                                    ((float)y) / MeterInPixels);
				// FIXME: undo operation; call rotateTo()?
				body.Rotation = (float)_rotation;
			}
		}

		public void stackOnTop ()
		{
			if (window != null) {
				window.stackOnTop (this);
			}
		}

		public void stackOnBottom ()
		{
			if (window != null) {
				window.stackOnBottom (this);
			}
		}

		public Point getP1 ()
		{
			return getScreenPoint (points [0]);
		}

		public virtual Point getP2 ()
		{
			return getScreenPoint (points [2]);
		}

		public Point getScreenPoint (IList iterable)
		{
			// p is relative to center, rotate, and scale; returns
			// screen coordinate of p
			double px = 0, py = 0;
			//Cairo.Context g = new Cairo.Context(surface);
			// FIXME: all this for a Context?!
			using (Cairo.ImageSurface draw = new Cairo.ImageSurface (Cairo.Format.Argb32, 70, 150)){
	    		using (Cairo.Context g = new Cairo.Context(draw)) {
					//using (Cairo.Context g = Gdk.CairoHelper.Create(window.canvas.GdkWindow)) {
					Point temp = screen_coord (center);
					g.Translate (temp.x, temp.y);
					g.Rotate (_rotation);
					g.Scale (_scaleFactor, _scaleFactor);
					px = System.Convert.ToDouble (iterable [0]);
					py = System.Convert.ToDouble (iterable [1]);
					g.UserToDevice (ref px, ref py);
				}
			}
			return new Point (px, py);
		}

		public Point getCenter ()
		{
			return center;
		}

		public bool has_pen {
			get {
				return _has_pen;
			}
			set {
				_has_pen = value;
			}
		}
    
		public double rotation {
			get {
				return _rotation * 180.0 / Math.PI;
			}
			set {
				rotateTo (value);
			}
		}

		public double scaleFactor {
			get {
				return _scaleFactor;
			}
			set {
				scaleTo (value);
			}
		}

		public double x {
			get {
				return center.x;
			}
			set {
				moveTo (value, center.y);
			}
		}
        
		public double getX ()
		{
			return center.x;
		}
        
		public double getY ()
		{
			return center.y;
		}
        
		public void setX (double value)
		{
			moveTo (value, center.y);
		}
        
		public void setY (double value)
		{
			moveTo (center.x, value);
		}
        
		public double y {
			get {
				return center.y;
			}
			set {
				moveTo (center.x, value);
			}
		}

		public void setWidth (int value)
		{
			_border = value;
			QueueDraw ();
		}
        
		public int border {
			get {
				return _border;
			}
			set {
				_border = value;
				QueueDraw ();
			}
		}
    
		public Pen pen {
			get {
				return _pen;
			}
			set {
				if (has_pen) {
					_pen = value;
					_pen.center.x = center.x;
					_pen.center.y = center.y;
					QueueDraw ();
				} else
					throw new Exception ("this shape cannot have a pen");
			}
		}
    
		public void QueueDraw ()
		{ // shape
			if (window is WindowClass) {
				if (window.getMode () == "auto" ||
	    window.getMode () == "bitmap" || 
            window.getMode () == "physics")
					window.update ();
				// else, manually call step()
			}
		}
    
		public bool contains (IList iterable)
		{
			Point p = new Point (iterable);
			int counter = 0;
			double xinters;
			Point p1, p2;
			if (points != null) {
				p1 = points [0];
				for (int i=1; i<=points.Length; i++) {
					p2 = points [i % points.Length];
					if (p.y > Math.Min (p1.y, p2.y)) {
						if (p.y <= Math.Max (p1.y, p2.y)) {
							if (p.x <= Math.Max (p1.x, p2.x)) {
								if (p1.y != p2.y) {
									xinters = (p.y - p1.y) * (p2.x - p1.x) / (p2.y - p1.y) + p1.x;
									if (p1.x == p2.x || p.x <= xinters)
										counter++;
								}
							}
						}
					}
					p1 = p2;
				}
				return (counter % 2 == 0); // hit?
			} else {
				return false;
			}
		}

		public void set_points (params Point [] ps)
		{
			// 1. set center to absolute
			double sumx = 0.0;
			double sumy = 0.0;
			if (ps.Length > 0) {
				for (int i = 0; i < ps.Length; i++) {
					sumx += ps [i].x;
					sumy += ps [i].y;
				}
				center.x = sumx / ps.Length;
				center.y = sumy / ps.Length;
			} else {
				center.x = 0;
				center.y = 0;
			}
			// 2. compute this.points in relative terms to center
			points = new Point [ps.Length];
			for (int i = 0; i < ps.Length; i++) {
				points [i] = new Point (ps [i].x - center.x, 
                              ps [i].y - center.y);
			}
		}
    
		public void append (IList iterable)
		{
			// add a new point to list of points
			// first copy old points
			// FIXME: could make points a list
			Point [] new_points = new Point [points.Length + 1];
			for (int i = 0; i < points.Length; i++) {
				new_points [i] = points [i];
			}
			// Now add new point:
			new_points [points.Length] = new Point (iterable);
			points = new_points;
		}

		public void set_points ()
		{
			// 1. set center to absolute
			double sumx = 0.0;
			double sumy = 0.0;
			if (points.Length > 0) {
				for (int i = 0; i < points.Length; i++) {
					sumx += points [i].x;
					sumy += points [i].y;
				}
				center.x = sumx / points.Length;
				center.y = sumy / points.Length;
			} else {
				center.x = 0;
				center.y = 0;
			}
			// 2. compute this.points in relative terms to center
			for (int i = 0; i < points.Length; i++) {
				points [i].x = points [i].x - center.x;
				points [i].y = points [i].y - center.y;
			}
		}
    
		public double screen_angle (double dir)
		{
			// Screen coords are 45 degrees from system
			return dir - (45 * Math.PI / 180.0);
		}

		public void forward (double distance)
		{
			double angle = screen_angle (_rotation);
			double x = ((distance) * Math.Cos (angle) - (distance) * Math.Sin (angle));
			double y = ((distance) * Math.Sin (angle) + (distance) * Math.Cos (angle));
			center.x += x;
			center.y += y;
			updatePen ();
			QueueDraw ();
		}
    
		public void updatePen ()
		{
			if (has_pen && pen.down)
				pen.appendPath (new Point (center.x, center.y));
		}

		public void  updateGlobalPosition (Cairo.Context g)
		{
			gx = center.x;
			gy = center.y;
			g.UserToDevice (ref gx, ref gy);
		}

		public Line penUp ()
		{
			pen._down = false;
			return pen.resetPath ();
		}
    
		public void penDown ()
		{
			pen._down = true;
			pen.appendPath (new Point (center.x, center.y));
		}
    
		public void backward (double distance)
		{
			forward (-distance);
		}
    
		public virtual void render (Cairo.Context g)
		{ // Shape
			if (!visible)
				return;
			g.Save ();
			Point temp;
			if (points != null) {
				g.LineWidth = border;
				temp = screen_coord (center);
				g.Translate (temp.x, temp.y);
				g.Rotate (_rotation);
				g.Scale (_scaleFactor, _scaleFactor);
				temp = screen_coord (points [0]);
				g.MoveTo (temp.x, temp.y);
				for (int p = 1; p < points.Length; p++) {
					temp = screen_coord (points [p]);
					g.LineTo (temp.x, temp.y);
				}
				if (close_path)
					g.ClosePath ();
				if (gradient != null) {
					Cairo.Gradient pat;
					if (gradient.gtype == "linear")
						pat = new Cairo.LinearGradient (gradient.p1.x,
                                           gradient.p1.y, 
                                           gradient.p2.x, 
                                           gradient.p2.y);
					else
						pat = new Cairo.RadialGradient (gradient.p1.x,
                                           gradient.p1.y, 
                                           gradient.radius1,
                                           gradient.p2.x, 
                                           gradient.p2.y,
                                           gradient.radius2);
          
					pat.AddColorStop (0, gradient.c1.getCairo ());
					pat.AddColorStop (1, gradient.c2.getCairo ());
					g.Pattern = pat;
					g.FillPreserve ();
				} else if (_fill != null) {
					g.Color = _fill.getCairo ();
					g.FillPreserve ();
				}
				if (_outline != null) {
					g.Color = _outline.getCairo ();
					g.Stroke ();
				}
			}
			foreach (Shape shape in shapes) {
				shape.render (g);
				shape.updateGlobalPosition (g);
			}
			g.Restore ();
			if (has_pen)
				pen.render (g);
		}
    
		public Point screen_coord (IList iterable)
		{
			// FIXME: return in coords of screen
			return new Point (iterable); 
			// new Point(point.x - center.x, point.y - center.y);
		}
    
		internal double wrap_width (double x)
		{
			if (x < 0)
				return wrap_width (window.width + x);
			else if (x >= window.width)
				return wrap_width (x - window.width);
			else
				return x;
		}

		internal double wrap_height (double y)
		{
			if (y < 0)
				return wrap_height (window.height + y);
			else if (y >= window.height)
				return wrap_height (y - window.height);
			else
				return y;
		}

		public virtual void move (double dx, double dy)
		{
			center.x += dx;
			center.y += dy;
			updatePen ();
			if (wrap) {
				center.x = wrap_width (center.x);
				center.y = wrap_height (center.y);
			}
			if (body != null)
				updatePhysics ();
			QueueDraw ();
		}

		public virtual void moveShape (double dx, double dy)
		{
		    // move points relative to center point
		    // this will appear when shape rotates about center
		    for (int i = 0; i < points.Length; i++) {
			points [i].x += dx;
			points [i].y += dy;
		    }
		    if (body != null)
			updatePhysics ();
		    QueueDraw ();
		}

		public virtual void moveShapeTo (double dx, double dy)
		{
		    // move points relative to center point
		    // this will appear when shape rotates about center
		    for (int i = 0; i < points.Length; i++) {
			points [i].x -= (dx + center.x);
			points [i].y -= (dy + center.y);
		    }
		    if (body != null)
			updatePhysics ();
		    QueueDraw ();
		}

	        public virtual void connect(Shape s2) {
		    // Connect two shapes together
		    // The shape's center is now shared
		    // with the second shape
		    double dx = center.x;
		    double dy = center.y;
		    s2.center = center;
		    s2.moveShape(dx, dy);
		}

	        public virtual void disconnect(Shape s2) {
		    // Disconnect two shapes who share a center
		    double dx = center.x;
		    double dy = center.y;
		    s2.center = new Point(0, 0);
		    s2.set_points();
		    s2.moveTo(dx, dy);
		}

		public virtual void recenter ()
		{
		    double dx = x;
		    double dy = y;
		    set_points();
		    center.x = dx;
		    center.y = dy;
		    if (body != null)
			updatePhysics ();
		    QueueDraw ();
		}

		public virtual void moveTo (double x, double y)
		{
			double dx = x - center.x;
			double dy = y - center.y;
			move (dx, dy);
		}

		public void _moveTo (double x, double y)
		{
			double dx = x - center.x;
			double dy = y - center.y;
			_move (dx, dy);
		}
        
		public void _move (double dx, double dy)
		{
			center.x += dx;
			center.y += dy;
			updatePen ();
		}

		public virtual void rotate (double degrees)
		{
			_rotation -= (Math.PI / 180.0) * degrees;
			if (body != null)
				updatePhysics ();
			QueueDraw ();
		}
    
		public virtual void rotateTo (double degrees)
		{
			_rotation = degrees * (Math.PI) / 180.0;
			if (body != null)
				updatePhysics ();
			QueueDraw ();
		}

		public void _rotate (double degrees)
		{
			_rotation -= (Math.PI / 180.0) * degrees;
		}
    
		public void _rotateTo (double degrees)
		{
			_rotation = degrees * (Math.PI) / 180.0;
		}
    
		public void scale (double percent)
		{
			_scaleFactor *= percent;
			QueueDraw ();
		}
        
		public void scaleTo (double percent)
		{
			_scaleFactor = percent;
			if (body != null)
				updatePhysics ();
			QueueDraw ();
		}
    
		public void update ()
		{ // Shape
			// Alias to QueueDraw
			QueueDraw (); 
		}
    
		public void draw (WindowClass win)
		{ // Shape
			// Add this shape to the Canvas list.
			if (win.mode == "bitmap" || win.mode == "bitmapmanual") {
				win.canvas.need_to_draw_surface = true;
				using (Cairo.Context g = new Cairo.Context(win.canvas.surface)) {
					render (g);
				}
			} else {
				lock (win.getCanvas().shapes) {
					if (! win.getCanvas ().shapes.Contains (this)) {
						win.getCanvas ().shapes.Add (this);
						//System.Console.Error.WriteLine("Added to win!");
					}
				}
				// Make sure each subshape is associated with this window
				// so QueueDraw will redraw:
				lock (shapes) {
					foreach (Shape shape in shapes) {
						shape.window = win;
					}
				}
			}
			window = win;
			if (window._canvas.world != null) {
				addToPhysics ();
			}
			QueueDraw ();
		}

		public void draw (Canvas canvas)
		{ // Shape
			// Add this shape to the Canvas list.
			if (canvas.mode == "bitmap" || canvas.mode == "bitmapmanual") {
				canvas.need_to_draw_surface = true;
				using (Cairo.Context g = new Cairo.Context(canvas.surface)) {
					render (g);
				}
			} else {
				lock (canvas.shapes) {
					if (! canvas.shapes.Contains (this)) 
						canvas.shapes.Add (this);
				}
			}
			if (canvas.world != null) {
				addToPhysics ();
			}
			QueueDraw ();
		}
    
		public void draw (Shape shape)
		{ // Shape
			// Add this shape to the shape's list.
			lock (shape.shapes) {
				if (! shape.shapes.Contains (this)) {
					shape.shapes.Add (this);
					//System.Console.Error.WriteLine("Added to shape!");
				}
			}
			window = shape.window;
			drawn_on_shape = shape;
			QueueDraw ();
		}
    
		public void undraw ()
		{
	      if (drawn_on_shape != null) {
			lock (drawn_on_shape.shapes) {
			  if (drawn_on_shape.shapes.Contains (this)) {
			    drawn_on_shape.shapes.Remove (this);
				//System.Console.Error.WriteLine("Removed from shape!");
			  }
			}
			drawn_on_shape = null;
		      }
		      if (window != null) {
			lock (window.getCanvas().shapes) {
   			  if (window._canvas.world != null) {
					removeFromPhysics ();
		      }
			  if (window.getCanvas ().shapes.Contains (this)) {
			    window.getCanvas ().shapes.Remove (this);
				//System.Console.Error.WriteLine("Removed from win!");
			    if (window is WindowClass)
			      ((WindowClass)window).QueueDraw ();
			    window = null;
			  }
			}
	      }
		}
    
		public Gradient gradient {
			set {
				_gradient = value;
				QueueDraw ();
			}
			get {
				return _gradient;
			} 
		}

		public Color color {
			set {
				if (value != null) {
					_fill = ((Color)value).Copy ();
					_outline = _fill;
				} else {
					_fill = null;
					_outline = null;
				}
				QueueDraw ();
			}
			get {
				if (_fill != null) {
					_fill.window = this.window;
					return _fill; // share!
				} else
					return null;
			}
		}

		public void setFill (Color value)
		{
			if (value == null) {
				_fill = null;
			} else {
				_fill = ((Color)value).Copy ();
			}
			QueueDraw ();
		}
    
		public virtual Color fill {
			set {
				if (value == null) {
					_fill = null;
				} else {
					_fill = ((Color)value).Copy ();
				}
				QueueDraw ();
			}
			get {
				if (_fill != null) {
					_fill.window = this.window;
					return _fill;
				} else
					return null;
			}
		}

		public void setOutline (Color value)
		{
			if (value == null) {
				_outline = null;
			} else {
				_outline = ((Color)value).Copy ();
			}
			QueueDraw ();
		}
    
		public Color outline {
			set {
				if (value == null) {
					_outline = null;
				} else {
					_outline = ((Color)value).Copy ();
				}
				QueueDraw ();
			}
			get {
				if (_outline != null) {
					_outline.window = this.window;
					return _outline; // share!
				} else
					return null;
			}
		}
	}
  
	public class Text : Shape
	{
		public string _text;
		public string fontFace = "sans serif";
		public Cairo.FontWeight fontWeight = Cairo.FontWeight.Normal;
		public Cairo.FontSlant fontSlant = Cairo.FontSlant.Normal;
		double _fontSize = 18;
		public string xJustification = "center"; // left, center, right
		public string yJustification = "center"; // top, center, bottom

		// FIXME: add wrappers around justifications, weight, slant, face

		public double fontSize {
			get {
				return _fontSize;
			}
			set {
				_fontSize = value;
				QueueDraw ();
			}
		}

		public string text {
			get {
				return _text;
			}
			set {
				_text = value;
				QueueDraw ();
			}
		}

		public Text (IList iterable, string text)
		{
			this.text = text;
			set_points (new Point (iterable));
		}

		public override void render (Cairo.Context g)
		{
			if (!visible)
				return;
			g.Save ();
			Point temp = screen_coord (center);
			g.Translate (temp.x, temp.y);
			g.Rotate (_rotation);
			g.Scale (_scaleFactor, _scaleFactor);
			if (gradient != null) {
				Cairo.Gradient pat;
				if (gradient.gtype == "linear")
					pat = new Cairo.LinearGradient (gradient.p1.x,
                                         gradient.p1.y, 
                                         gradient.p2.x, 
                                         gradient.p2.y);
				else
					pat = new Cairo.RadialGradient (gradient.p1.x,
                                         gradient.p1.y, 
                                         gradient.radius1,
                                         gradient.p2.x, 
                                         gradient.p2.y,
                                         gradient.radius2);
        
				pat.AddColorStop (0, gradient.c1.getCairo ());
				pat.AddColorStop (1, gradient.c2.getCairo ());
				g.Pattern = pat;
				g.FillPreserve ();
			} else if (_fill != null)
				g.Color = _fill._cairo;
			else
				g.Color = new Cairo.Color (0, 0, 0); // default color when none given
			Pango.Layout layout = Pango.CairoHelper.CreateLayout (g);
			Pango.FontDescription desc = Pango.FontDescription.FromString (
                            String.Format ("{0} {1}", fontFace, fontSize));
			layout.FontDescription = desc;
			layout.SetText (text);
			layout.Alignment = Pango.Alignment.Center;
			int layoutWidth, layoutHeight;
			layout.GetSize (out layoutWidth, out layoutHeight);
			double teHeight = (double)layoutHeight / Pango.Scale.PangoScale; 
			double teWidth = (double)layoutWidth / Pango.Scale.PangoScale;
			Point p = new Point (0, 0);
			if (xJustification == "center") {
				p.x = points [0].x - teWidth / 2; // - te.XBearing;
			} else if (xJustification == "left") {
				p.x = points [0].x;
			} else if (xJustification == "right") {
				p.x = points [0].x + teWidth;
			}
			if (yJustification == "center") {
				p.y = points [0].y - teHeight / 2; // - te.YBearing;
			} else if (yJustification == "bottom") {
				p.y = points [0].y;
			} else if (yJustification == "top") {
				p.y = points [0].y - teHeight;
			}
			temp = screen_coord (p);
			g.MoveTo (temp.x, temp.y);
			Pango.CairoHelper.ShowLayout (g, layout);
			foreach (Shape shape in shapes) {
				shape.render (g);
				shape.updateGlobalPosition (g);
			}
			g.Stroke ();
			g.Restore ();
		}

		public double width {
			get {
				using (Cairo.Context g = Gdk.CairoHelper.Create(window.canvas.GdkWindow)) {
					Cairo.TextExtents te = g.TextExtents (text);
					return te.Width * 2;
				}
			}
		}

		public double height {
			get {
				using (Cairo.Context g = Gdk.CairoHelper.Create(window.canvas.GdkWindow)) {
					Cairo.TextExtents te = g.TextExtents (text);
					return te.Height * 2;
				}
			}
		}

		public override void addToPhysics ()
		{ // Text
			world = window._canvas.world;
			double width = 0;
			double height = 0;
			using (Cairo.Context g = Gdk.CairoHelper.Create(window.canvas.GdkWindow)) {
				Cairo.TextExtents te = g.TextExtents (text);
				// FIXME: need to adjust based on justification
				// This works with x centered, y centered
				width = te.Width * 2;
				height = te.Height * 2;
			}
			float MeterInPixels = 64.0f;
			// from x,y to meters of window
			// arbitrary:
			Vector2 position = new Vector2 (((float)x) / MeterInPixels, 
                                     ((float)y) / MeterInPixels);
			body = FarseerPhysics.Factories.BodyFactory.CreateRectangle (
                 world,
                 (float)(width / MeterInPixels), // radius in meters
                 (float)(height / MeterInPixels), // radius in meters
                 _density, // density
                 position);                        // center
			// Give it some bounce and friction
			body.Restitution = _bounce;
			body.Rotation = (float)_rotation;
			body.Friction = _friction;
			body.BodyType = _bodyType;
			body.IsStatic = (_bodyType == FarseerPhysics.Dynamics.BodyType.Static);
			body.UserData = this; // point back to this shape
			body.FixtureList [0].UserData = this; // point back to this shape
		}
	}

	public class Line : Shape
	{

		public Line (params object [] points) : base(true)
		{
			Point [] temp = new Point [points.Length];
			int count = 0;
			foreach (object o in points) {
				if (o is Point)
					temp [count] = (Point)o;
				else if (o is IList) {
					IList i = (IList)o;
					temp [count] = new Point (i [0], i [1]);
				} else {
					throw new Exception ("Line: can't convert arg to a point");
				}
				count++;
			}
			set_points (temp);
            close_path = false;
            fill = null;
		}

		public Line (IList iterable1, IList iterable2) :
       this(true, iterable1, iterable2)
		{
            close_path = false;
            fill = null;
		}

		public Line (bool has_pen, IList iterable1, IList iterable2)
		{
			set_points (new Point (iterable1), 
		 new Point (iterable2));
			close_path = false;
			fill = null;
		}

		public Line () : base(true)
		{
			points = new Point[0];
			close_path = false;
			fill = null;
		}

		public override Point getP2 ()
		{
			return getScreenPoint (points [1]);
		}

	}

	public class Curve : Shape
	{
		public Curve (IList iterable0, 
                 IList iterable1, 
                 IList iterable2, 
                 IList iterable3):
      this(true, iterable0, iterable1, iterable2, iterable3)
		{
		}

		public Curve (bool has_pen, 
                 IList iterable0, 
                 IList iterable1, 
                 IList iterable2, 
                 IList iterable3) : 
    base(has_pen)
		{
			set_points (new Point (iterable0), new Point (iterable1), 
		 new Point (iterable2), new Point (iterable3));
			fill = null;
		}

		public override void render (Cairo.Context g)
		{
			if (!visible)
				return;
			g.Save ();
			Point temp, p1, p2, p3;
			if (points != null) {
				g.LineWidth = border;
				temp = screen_coord (center);
				g.Translate (temp.x, temp.y);
				g.Rotate (_rotation);
				g.Scale (_scaleFactor, _scaleFactor);
				temp = screen_coord (points [0]);
				g.MoveTo (temp.x, temp.y);
				for (int p = 1; p < points.Length; p += 3) {
					p1 = screen_coord (points [p]);
					p2 = screen_coord (points [p + 1]);
					p3 = screen_coord (points [p + 2]);
					g.CurveTo (p1.x, p1.y, p2.x, p2.y, p3.x, p3.y);
				}
				if (gradient != null) {
					Cairo.Gradient pat;
					if (gradient.gtype == "linear")
						pat = new Cairo.LinearGradient (gradient.p1.x,
                                           gradient.p1.y, 
                                           gradient.p2.x, 
                                           gradient.p2.y);
					else
						pat = new Cairo.RadialGradient (gradient.p1.x,
                                           gradient.p1.y, 
                                           gradient.radius1,
                                           gradient.p2.x, 
                                           gradient.p2.y,
                                           gradient.radius2);
          
					pat.AddColorStop (0, gradient.c1.getCairo ());
					pat.AddColorStop (1, gradient.c2.getCairo ());
					g.Pattern = pat;
					g.FillPreserve ();
				} else if (_fill != null) {
					g.Color = _fill._cairo;
					g.FillPreserve ();
				}
				if (_outline != null) {
					g.Color = _outline._cairo;
					g.Stroke ();
				}
			}
			foreach (Shape shape in shapes) {
				shape.render (g);
				shape.updateGlobalPosition (g);
			}
			g.Restore ();
			if (has_pen)
				pen.render (g);
		}
	}
  
	public class Arrow : Shape
	{
		public Arrow (IList iterable) :  this(iterable, 0)
		{
		}

		public Arrow (IList iterable, double degrees) :  base(true)
		{
			set_points (new Point (0, 0),
		 new Point (0, -5), 
		 new Point (11, 0),
		 new Point (0, 5) 
		 );
			center.x = System.Convert.ToDouble (iterable [0]);
			center.y = System.Convert.ToDouble (iterable [1]);
			rotate (degrees);
		}
	}
  
	public class Turtle : Shape
	{
		public Turtle (IList iterable) :  this(iterable, 0)
		{
		}

		public Turtle (IList iterable, double degrees) :  base(true)
		{
			set_points (new Point (0, 0),
		 new Point (-5, -5), 
		 new Point (5, 0),
		 new Point (-5, 5) 
		 );
			center.x = System.Convert.ToDouble (iterable [0]);
			center.y = System.Convert.ToDouble (iterable [1]);
			rotate (degrees);
		}
	}
  
	public class Pen : Shape
	{
		private List<Point> _path; // = new List<Point>();
		public bool _down;
		public double minDistance = 1;
    
		public Pen (Color color, int border) : base(false)
		{
			_down = false;
			_path = new List<Point> ();
			this.color = color;
			this.border = border;
		}
    
		public Pen (int border) : base(false)
		{
			_down = false;
			_path = new List<Point> ();
			this.color = new Color (0, 0, 0);
			this.border = border;
		}

		public List<Point> getPath ()
		{
			return _path;
		}
    
		public Line resetPath ()
		{
			Line temp = new Line (_path.ToArray ());
			_path = new List<Point> ();
			return temp;
		}

		public void appendPath (IList iterable)
		{
			Point temp = new Point (iterable);
			if (_path.Count > 0) {
				if (_path [_path.Count - 1].distance (temp) > minDistance)
					_path.Add (temp);
			} else {
				_path.Add (temp);
			}
		}
    
		public bool down {
			get {
				return _down;
			}
		}
    
		public List<Point> path {
			get {
				return _path;
			}
		}
    
		public override void render (Cairo.Context g)
		{
			if (!visible)
				return;
			// render path
			g.Save ();
			Point temp = screen_coord (center);
			g.Translate (temp.x, temp.y);
			g.Rotate (_rotation);
			g.Scale (_scaleFactor, _scaleFactor);
			if (path != null && path.Count > 0) {
				g.LineWidth = border;
				temp = screen_coord (path [0]);
				g.MoveTo (temp.x, temp.y);
				for (int p = 1; p < path.Count; p++) {
					temp = screen_coord (path [p]);
					g.LineTo (temp.x, temp.y);
				}
				if (_outline != null) {
					g.Color = _outline._cairo;
					g.Stroke ();
				}
			}
			foreach (Shape shape in shapes) {
				shape.render (g);
				shape.updateGlobalPosition (g);
			}
			g.Restore ();
		}
	}

	public class Pixel
	{
		private Picture picture;
		public int x;
		public int y;
    
		public Pixel (Picture picture, int x, int y)
		{
			this.picture = picture;
			this.x = picture.wrap_width (x);
			this.y = picture.wrap_height (y);
		}

		public Color getColor ()
		{
			return picture.getColor (x, y);
		}

		public PythonTuple getRGB ()
		{
			return picture.getRGB (x, y);
		}

		public PythonTuple getRGBA ()
		{
			return picture.getRGBA (x, y);
		}

		public int getGray ()
		{
			return picture.getGray (x, y);
		}

		public int getRed ()
		{
			return picture.getRed (x, y);
		}

		public int getGreen ()
		{
			return picture.getGreen (x, y);
		}

		public int getBlue ()
		{
			return picture.getBlue (x, y);
		}

		public int getAlpha ()
		{
			return picture.getAlpha (x, y);
		}

		public void setColor (Color color)
		{
			picture.setColor (x, y, color);
		}

		public void setRGB (byte red, byte green, byte blue)
		{
			picture.setRGB (x, y, red, green, blue);
		}

		public void setRGBA (byte red, byte green, byte blue, byte alpha)
		{
			picture.setRGBA (x, y, red, green, blue, alpha);
		}

		public void setGray (byte value)
		{
			picture.setGray (x, y, value);
		}

		public void setRed (byte value)
		{
			picture.setRed (x, y, value);
		}

		public void setGreen (byte value)
		{
			picture.setGreen (x, y, value);
		}

		public void setBlue (byte value)
		{
			picture.setBlue (x, y, value);
		}

		public void setAlpha (byte value)
		{
			picture.setAlpha (x, y, value);
		}
	}
  
	public class Picture : Shape
	{
		Gdk.Pixbuf _pixbuf; // in memory rep of picture
    
		public Gdk.Pixbuf pixbuf {
			get {
				return _pixbuf;
			}
		}

		public Picture (string filename) : this(true)
		{
			if (filename.StartsWith ("http://")) {
				HttpWebRequest req = (HttpWebRequest)WebRequest.Create (filename);
				req.KeepAlive = false;
				req.Timeout = 10000;        
				WebResponse resp = req.GetResponse ();
				Stream s = resp.GetResponseStream ();
				_pixbuf = new Gdk.Pixbuf (s);
			} else {
				_pixbuf = new Gdk.Pixbuf (filename);
			}
			if (!_pixbuf.HasAlpha) {
			  _pixbuf = _pixbuf.AddAlpha (false, 0, 0, 0); 
			}
			set_points (new Point (0, 0), 
                 new Point (_pixbuf.Width, 0),
                 new Point (_pixbuf.Width, _pixbuf.Height), 
                 new Point (0, _pixbuf.Height));
		}
		
		public Picture (WindowClass window) : this(true)
		{ 
			ManualResetEvent ev = new ManualResetEvent (false);
			Gdk.Pixbuf pixbuf = null;
			Invoke (delegate {
				Gdk.Drawable drawable = window.getDrawable ();
				Gdk.Colormap colormap = drawable.Colormap;
				int _width = 0;
				int _height = 0;
				drawable.GetSize (out _width, out _height);
				pixbuf = Gdk.Pixbuf.FromDrawable (drawable, colormap, 0, 0, 0, 0, _width, _height);
				ev.Set ();
			});
			ev.WaitOne ();
			// Now, do what Picture(pixbuf) does:
			_pixbuf = pixbuf;
			if (!_pixbuf.HasAlpha) {
				_pixbuf = _pixbuf.AddAlpha (true, 0, 0, 0); // alpha color?
			}
			set_points (new Point (0, 0), 
                 new Point (_pixbuf.Width, 0),
                 new Point (_pixbuf.Width, _pixbuf.Height), 
                 new Point (0, _pixbuf.Height));			
		}

		public Picture (bool has_pen) : base(has_pen)
		{
			this._fill.picture = this;
		}

		public Picture (Picture original) : this(true)
		{
			// Colorspace, has_alpha, bits_per_sample, width, height:
			_pixbuf = new Gdk.Pixbuf (original._pixbuf.Colorspace, true, 8, original.getWidth (), original.getHeight ());
			if (!_pixbuf.HasAlpha) {
				_pixbuf = _pixbuf.AddAlpha (true, 0, 0, 0); // alpha color?
			}
			for (int x=0; x < _pixbuf.Width; x++) {
				for (int y=0; y < _pixbuf.Height; y++) {
					byte r = (byte)original.getRed (x, y);
					byte g = (byte)original.getGreen (x, y);
					byte b = (byte)original.getBlue (x, y);
					byte a = (byte)original.getAlpha (x, y);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
              x * _pixbuf.NChannels + 0, r);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
              x * _pixbuf.NChannels + 1, g);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
              x * _pixbuf.NChannels + 2, b);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
              x * _pixbuf.NChannels + 3, a);
				}
			}
			set_points (original.points);
			center = original.center;
		}
    
		public Picture (Gdk.Pixbuf pixbuf) : this(true)
		{
			_pixbuf = pixbuf;
			if (!_pixbuf.HasAlpha) {
				_pixbuf = _pixbuf.AddAlpha (true, 0, 0, 0); // alpha color?
			}
			set_points (new Point (0, 0), 
                 new Point (_pixbuf.Width, 0),
                 new Point (_pixbuf.Width, _pixbuf.Height), 
                 new Point (0, _pixbuf.Height));
		}

	  public Picture (System.Drawing.Bitmap bitmap, int width, int height, bool fluke1=false) : this(true)
		{
			// Colorspace, has_alpha, bits_per_sample, width, height:
			// FIXME: convert bitmap.palette to colormap
			_pixbuf = new Gdk.Pixbuf (new Gdk.Colorspace (), true, 8, width, height);
			if (!_pixbuf.HasAlpha) {
				_pixbuf = _pixbuf.AddAlpha (true, 0, 0, 0); // alpha color?
			}
			int xstep = 1;
			if (fluke1) xstep = 2;
			for (int x=0; x < _pixbuf.Width; x += xstep) {
				for (int y=0; y < _pixbuf.Height; y++) {
					System.Drawing.Color pixel = bitmap.GetPixel (x / xstep, y);
					// First pixel
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                            x * _pixbuf.NChannels + 0, pixel.R);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                            x * _pixbuf.NChannels + 1, pixel.G);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                            x * _pixbuf.NChannels + 2, pixel.B);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                            x * _pixbuf.NChannels + 3, pixel.A);
					if (fluke1){
					  // Second pixel
					  int x2 = x + 1;
					  Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
							     x2 * _pixbuf.NChannels + 0, pixel.R);
					  Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
							     x2 * _pixbuf.NChannels + 1, pixel.G);
					  Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
							     x2 * _pixbuf.NChannels + 2, pixel.B);
					  Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
							     x2 * _pixbuf.NChannels + 3, pixel.A);
					}
				}
			}
			set_points (new Point (0, 0), 
                 new Point (_pixbuf.Width, 0),
                 new Point (_pixbuf.Width, _pixbuf.Height), 
                 new Point (0, _pixbuf.Height));
		}

		public void fromArray (Byte [] buffer, string format)
		{
			if (format == "BGRX") { // b, r, g, ignore
				int count = 0;
				for (int i=0; i < buffer.Length; i+=4) {
					byte b = buffer [i];
					byte g = buffer [i + 1];
					byte r = buffer [i + 2];
					// NOTE: x is from the other side
					int x = _pixbuf.Width - count % _pixbuf.Width;
					int y = count / _pixbuf.Width;
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
              x * _pixbuf.NChannels + 0, r);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
              x * _pixbuf.NChannels + 1, g);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
              x * _pixbuf.NChannels + 2, b);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
              x * _pixbuf.NChannels + 3, 255);
					count++;
				}
			} else if (format == "GRAY") { 
				int count = 0;
				for (int i=0; i < buffer.Length; i++) {
					byte g = buffer [i];
					// NOTE: x is from the other side
					int x = _pixbuf.Width - count % _pixbuf.Width;
					int y = count / _pixbuf.Width;
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
              x * _pixbuf.NChannels + 0, g);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
              x * _pixbuf.NChannels + 1, g);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
              x * _pixbuf.NChannels + 2, g);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
              x * _pixbuf.NChannels + 3, 255);
					count++;
				}
			} else {
				throw new Exception ("Picture.fromArray(array, format): invalid format");
			}
			QueueDraw ();
		}
    
		public Picture (int width, int height, byte [] buffer, int depth) : this(true)
		{
			// depth should be 1
			// Colorspace, has_alpha, bits_per_sample, width, height:
			_pixbuf = new Gdk.Pixbuf (new Gdk.Colorspace (), true, 8, width, height);
			if (!_pixbuf.HasAlpha) {
				_pixbuf = _pixbuf.AddAlpha (true, 0, 0, 0); // alpha color?
			}
			for (int x=0; x < _pixbuf.Width; x++) {
				for (int y=0; y < _pixbuf.Height; y++) {
					byte r = buffer [(y * width + x)];
					byte g = r;
					byte b = r;
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                    x * _pixbuf.NChannels + 0, r);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                    x * _pixbuf.NChannels + 1, g);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                    x * _pixbuf.NChannels + 2, b);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                    x * _pixbuf.NChannels + 3, 255);
				}
			}
			set_points (new Point (0, 0), 
                     new Point (_pixbuf.Width, 0),
                     new Point (_pixbuf.Width, _pixbuf.Height), 
                     new Point (0, _pixbuf.Height));
		}

		public Picture (int width, int height, byte [] buffer) : this(true)
		{
			// Colorspace, has_alpha, bits_per_sample, width, height:
			_pixbuf = new Gdk.Pixbuf (new Gdk.Colorspace (), true, 8, width, height);
			if (!_pixbuf.HasAlpha) {
				_pixbuf = _pixbuf.AddAlpha (true, 0, 0, 0); // alpha color?
			}
			for (int x=0; x < _pixbuf.Width; x++) {
				for (int y=0; y < _pixbuf.Height; y++) {
					byte r = buffer [(y * width + x) * 3 + 0];
					byte g = buffer [(y * width + x) * 3 + 1];
					byte b = buffer [(y * width + x) * 3 + 2];
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                    x * _pixbuf.NChannels + 0, r);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                    x * _pixbuf.NChannels + 1, g);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                    x * _pixbuf.NChannels + 2, b);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                    x * _pixbuf.NChannels + 3, 255);
				}
			}
			set_points (new Point (0, 0), 
                     new Point (_pixbuf.Width, 0),
                     new Point (_pixbuf.Width, _pixbuf.Height), 
                     new Point (0, _pixbuf.Height));
		}

		public Picture (int width, int height) : this(true)
		{
			// Colorspace, has_alpha, bits_per_sample, width, height:
			_pixbuf = new Gdk.Pixbuf (new Gdk.Colorspace (), true, 8, width, height);
			if (!_pixbuf.HasAlpha) {
				_pixbuf = _pixbuf.AddAlpha (true, 0, 0, 0); // alpha color?
			}
			// WORKAROUND: image needs alpha set to zero (full opacity/no
			// transparency). Might as well set default color, too:
			for (int x=0; x < _pixbuf.Width; x++) {
				for (int y=0; y < _pixbuf.Height; y++) {
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                          x * _pixbuf.NChannels + 0, 255);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                          x * _pixbuf.NChannels + 1, 255);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                          x * _pixbuf.NChannels + 2, 255);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                          x * _pixbuf.NChannels + 3, 255);
				}
			}
			set_points (new Point (0, 0), 
                     new Point (_pixbuf.Width, 0),
                     new Point (_pixbuf.Width, _pixbuf.Height), 
                     new Point (0, _pixbuf.Height));
		}

		public Picture (int width, int height, Color color) : this(true)
		{
			// Colorspace, has_alpha, bits_per_sample, width, height:
			_pixbuf = new Gdk.Pixbuf (new Gdk.Colorspace (), true, 8, width, height);
			if (!_pixbuf.HasAlpha) {
				_pixbuf = _pixbuf.AddAlpha (true, 0, 0, 0); // alpha color?
			}
			// WORKAROUND: image needs alpha set to zero (full opacity/no
			// transparency). Might as well set default color, too:
			for (int x=0; x < _pixbuf.Width; x++) {
				for (int y=0; y < _pixbuf.Height; y++) {
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                            x * _pixbuf.NChannels + 0, (byte)color.red);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                            x * _pixbuf.NChannels + 1, (byte)color.green);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                            x * _pixbuf.NChannels + 2, (byte)color.blue);
					Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                            x * _pixbuf.NChannels + 3, (byte)color.alpha);
				}
			}
			set_points (new Point (0, 0), 
                 new Point (_pixbuf.Width, 0),
                 new Point (_pixbuf.Width, _pixbuf.Height), 
                 new Point (0, _pixbuf.Height));
		}

		public Picture getRegion (IList iterable, int width, int height, double degrees)
		{
			Point p = new Point (iterable);
			Picture pic = new Picture (width, height);
			double angle = degrees * Math.PI / 180.0;
			double px, py;
			int ox = 0;
			for (int x = -width/2; x < width/2; x++) {
				int oy = 0;
				for (int y = -height/2; y < height/2; y++) {
					// rotate that x,y:
					px = x * Math.Cos (angle) - y * Math.Sin (angle);
					py = x * Math.Sin (angle) + y * Math.Cos (angle);
					// set the color of the new image from the offset of this:
					pic.setColor (ox, oy, this.getPixel ((int)(p.x + px), 
                                             (int)(p.y + py)).getColor ());
					oy += 1;
				}
				ox += 1;
			}
			return pic;
		}

		public Picture getRegion (IList iterable, int width, int height)
		{
			Point p = new Point (iterable);
			Picture pic = new Picture (width, height);
			for (int x = 0; x < width; x++) {
				for (int y = 0; y < height; y++) {
					pic.setColor (x, y, this.getPixel ((int)(p.x + x), 
					   (int)(p.y + y)).getColor ());
				}
			}
			return pic;
		}

		public void setRegion (IList iterable, Picture picture)
		{
			// FIXME: better way would use Context to draw the image onto
			// another image. Also, consider making this pic.draw(picture)
			// This doesn't respect the color pallette of picture.
			Point p = new Point (iterable);
			for (int x = 0; x < picture.width; x++) {
				for (int y = 0; y < picture.height; y++) {
					Color c1 = this.getPixel ((int)(p.x + x), 
				   (int)(p.y + y)).getColor ();
					Color c2 = picture.getPixel ((int)(x), 
				      (int)(y)).getColor ();
					int t2 = c2.alpha;
					int t1 = Math.Max (Math.Min (255 - t2, 255), 0);
					this.setColor ((int)(p.x + x), 
			(int)(p.y + y), 
			new Color (t1 * c1.red + t2 * c2.red,
				  t1 * c1.green + t2 * c2.green,
				  t1 * c1.blue + t2 * c2.blue));
				}
			}
		}

		public void flipHorizontal ()
		{
		    for (int x = 0; x < width/2; x++) {
			for (int y = 0; y < height; y++) {
			    Color c1 = this.getPixel (x, y).getColor ();
			    Color c2 = this.getPixel (width - x - 1, y).getColor ();
			    setColor(x, y, c2);
			    setColor(width - x - 1, y, c1);
			}
		    }
		}

		public void flipVertical ()
		{
		    for (int x = 0; x < width; x++) {
			for (int y = 0; y < height/2; y++) {
			    Color c1 = this.getPixel (x, y).getColor ();
			    Color c2 = this.getPixel (x, height - y - 1).getColor ();
			    setColor(x, y, c2);
			    setColor(x, height - y - 1, c1);
			}
		    }
		}

		public void setRegion (IList iterable, int width, int height, double degrees,
                          Picture picture)
		{
			Point p = new Point (iterable);
			double angle = degrees * Math.PI / 180.0;
			double px, py;
			int tx, ty;
			for (int x = -width/2; x < width/2; x++) {
				for (int y = -height/2; y < height/2; y++) {
					// rotate that x,y:
					px = x * Math.Cos (angle) - y * Math.Sin (angle);
					py = x * Math.Sin (angle) + y * Math.Cos (angle);
					// set the color of the new image from the offset of this:
					tx = (int)(p.x + px);
					ty = (int)(p.y + py);
					this.getPixel (tx, ty).setColor (color);
					// FIXME: a lame way to not skip any pixels:
					// Need a region fill algorithm
					if ((int)px + 1 < width / 2) {
						this.getPixel (tx + 1, ty).setColor (color);
						if ((int)py + 1 < height / 2) {
							this.getPixel (tx + 1, ty + 1).setColor (color);
							this.getPixel (tx, ty + 1).setColor (color);
						}
					} else {
						if ((int)py + 1 < height / 2) {
							this.getPixel (tx, ty + 1).setColor (picture.getColor (x + width / 2, 
								  y + height / 2));
						}
					}
				}
			}
		}

		public void setRegion (IList iterable, int width, int height, double degrees,
                          Color color)
		{
			Point p = new Point (iterable);
			double angle = degrees * Math.PI / 180.0;
			double px, py;
			int tx, ty;
			for (int x = -width/2; x < width/2; x++) {
				for (int y = -height/2; y < height/2; y++) {
					// rotate that x,y:
					px = x * Math.Cos (angle) - y * Math.Sin (angle);
					py = x * Math.Sin (angle) + y * Math.Cos (angle);
					// set the color of the new image from the offset of this:
					tx = (int)(p.x + px);
					ty = (int)(p.y + py);
					this.getPixel (tx, ty).setColor (color);
					// FIXME: a lame way to not skip any pixels:
					// Need a region fill algorithm
					if ((int)px + 1 < width / 2) {
						this.getPixel (tx + 1, ty).setColor (color);
						if ((int)py + 1 < height / 2) {
							this.getPixel (tx + 1, ty + 1).setColor (color);
							this.getPixel (tx, ty + 1).setColor (color);
						}
					} else {
						if ((int)py + 1 < height / 2) {
							this.getPixel (tx, ty + 1).setColor (color);
						}
					}
				}
			}
		}
    
		public Gdk.Pixbuf getPixbuf ()
		{
			return _pixbuf;
		}

		public int getWidth ()
		{
			return _pixbuf.Width;
		}

		public int getHeight ()
		{
			return _pixbuf.Height;
		}

		public void savePicture (string filename)
		{
			// png, and jpg
			String format;
			if (filename.Substring (filename.Length - 3, 3) == "png") {
				format = "png";
			} else if (filename.Substring (filename.Length - 3, 3) == "jpg") {
				format = "jpeg";
			} else if (filename.Substring (filename.Length - 3, 3) == "gif") {
				//format = "png";
				// FIXME: use LibGif
				throw new Exception ("not implemented yet; use jpg or png");
			} else {
				//format = "png";
				throw new Exception ("unknown image type; use jpg or png");
			}
			_pixbuf.Save (filename, format);
		}
    
		public Pixel getPixel (int x, int y)
		{
			return new Pixel (this, x, y);
		}
    
		public void setPixel (int x, int y, Color color)
		{
			int red = color.red;
			int green = color.green;
			int blue = color.blue;
			int alpha = color.alpha;
			this.setRGBA (x, y, (byte)red, (byte)green, (byte)blue, (byte)alpha);
		}

		public void setPixel (int x, int y, Pixel pixel)
		{
			int red = pixel.getRed ();
			int green = pixel.getGreen ();
			int blue = pixel.getBlue ();
			int alpha = pixel.getAlpha ();
			this.setRGBA (x, y, (byte)red, (byte)green, (byte)blue, (byte)alpha);
		}

		public IEnumerable getPixels ()
		{
			for (int x=0; x < width; x++) {
				for (int y=0; y < height; y++) {
					yield return getPixel(x, y);
				}
			}
		}

		public void setPixels (Picture picture)
		{
			for (int x=0; x < width; x++) {
				for (int y=0; y < height; y++) {
					setPixel (x, y, picture.getPixel (x, y));
				}
			}
		}

		public Color getColor (int x, int y)
		{
			// red, green, blue, alpha
			Color temp = new Color (getRed (x, y), getGreen (x, y), getBlue (x, y), getAlpha (x, y));
			temp.picture = this;
			temp.x = x;
			temp.y = y;
			return temp;
		}
    
		public PythonTuple getRGB (int x, int y)
		{
			// red, green, blue, alpha
			return PyTuple (getRed (x, y), getGreen (x, y), getBlue (x, y));
		}
    
		public PythonTuple getRGBA (int x, int y)
		{
			// red, green, blue, alpha
			return PyTuple (getRed (x, y), getGreen (x, y), getBlue (x, y), getAlpha (x, y));
		}

		internal int wrap_width (int x)
		{
			if (x < 0)
				return wrap_width ((int)(width + x));
			else if (x >= width)
				return wrap_width ((int)(x - width));
			else
				return x;
		}

		internal int wrap_height (int y)
		{
			if (y < 0)
				return wrap_height ((int)(height + y));
			else if (y >= height)
				return wrap_height ((int)(y - height));
			else
				return y;
		}

		public int getGray (int x, int y)
		{
			// red, green, blue, alpha
			x = wrap_width (x);
			y = wrap_height (y);
			int r = Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                  x * _pixbuf.NChannels + 0);
			int g = Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                  x * _pixbuf.NChannels + 1);
			int b = Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                  x * _pixbuf.NChannels + 2);
			return (int)(((double)(r + g + b)) / 3.0);
		}
    
		public int getRed (int x, int y)
		{
			// red, green, blue, alpha
			x = wrap_width (x);
			y = wrap_height (y);
			return Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                  x * _pixbuf.NChannels + 0);
		}
    
		public int getGreen (int x, int y)
		{
			// red, green, blue, alpha
			x = wrap_width (x);
			y = wrap_height (y);
			return Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                  x * _pixbuf.NChannels + 1);
		}
    
		public int getBlue (int x, int y)
		{
			// red, green, blue, alpha
			x = wrap_width (x);
			y = wrap_height (y);
			return Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                  x * _pixbuf.NChannels + 2);
		}
    
		public int getAlpha (int x, int y)
		{
			// red, green, blue, alpha
			x = wrap_width (x);
			y = wrap_height (y);
			return Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                  x * _pixbuf.NChannels + 3);
		}
    
		public void setColor (int x, int y, Color color)
		{
			// red, green, blue, alpha
			x = wrap_width (x);
			y = wrap_height (y);
			Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 0, (byte)color.red);
			Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 1, (byte)color.green);
			Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 2, (byte)color.blue);
			Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 3, (byte)color.alpha);
			QueueDraw ();
		}

		public void setGray (int x, int y, byte value)
		{
			// red, green, blue, alpha
			x = wrap_width (x);
			y = wrap_height (y);
			Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                  x * _pixbuf.NChannels + 0, value);
			Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                  x * _pixbuf.NChannels + 1, value);
			Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                  x * _pixbuf.NChannels + 2, value);
			QueueDraw ();
		}
    
		public void setRed (int x, int y, byte value)
		{
			// red, green, blue, alpha
			x = wrap_width (x);
			y = wrap_height (y);
			Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                x * _pixbuf.NChannels + 0, value);
			QueueDraw ();
		}

		public void setGreen (int x, int y, byte value)
		{
			// red, green, blue, alpha
			x = wrap_width (x);
			y = wrap_height (y);
			Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                            x * _pixbuf.NChannels + 1, value);
			QueueDraw ();
		}

		public void setBlue (int x, int y, byte value)
		{
			// red, green, blue, alpha
			x = wrap_width (x);
			y = wrap_height (y);
			Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                  x * _pixbuf.NChannels + 2, value);
			QueueDraw ();
		}
    
		public void setAlpha (int x, int y, byte value)
		{
			// red, green, blue, alpha
			x = wrap_width (x);
			y = wrap_height (y);
			Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                            x * _pixbuf.NChannels + 3, value);
			QueueDraw ();
		}

		public void setAlpha (byte value)
		{
			for (int x = 0; x < width; x++) {
			    for (int y = 0; y < height; y++) {
				if (getRed(x,y) == 0 && getGreen(x,y) == 0 && getBlue(x,y) == 0) {
				    // Don't change alpha here
				} else {
				    Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
						       x * _pixbuf.NChannels + 3, value);
				}
			    }
			}
			QueueDraw ();
		}

		public void setRGB (int x, int y, byte red, byte green, byte blue)
		{
			// red, green, blue, alpha
			x = wrap_width (x);
			y = wrap_height (y);
			Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                  x * _pixbuf.NChannels + 0, red);
			Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                  x * _pixbuf.NChannels + 1, green);
			Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                  x * _pixbuf.NChannels + 2, blue);
			QueueDraw ();
		}
    
		public void setRGBA (int x, int y, byte red, byte green, byte blue, 
                            byte alpha)
		{
			// red, green, blue, alpha
			x = wrap_width (x);
			y = wrap_height (y);
			Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                  x * _pixbuf.NChannels + 0, red);
			Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                  x * _pixbuf.NChannels + 1, green);
			Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                  x * _pixbuf.NChannels + 2, blue);
			Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                  x * _pixbuf.NChannels + 3, alpha);
			QueueDraw ();
		}

		public override Color fill {
        // operate on picture
			get {
				return _fill;
			}
			set {
				value.picture = this;
			}
		}

		public override void render (Cairo.Context g)
		{ // picture
			if (!visible)
				return;
			g.Save ();
			Point temp = screen_coord (center);
			g.Translate (temp.x, temp.y);
			g.Rotate (_rotation);
			g.Scale (_scaleFactor, _scaleFactor);
			Gdk.CairoHelper.SetSourcePixbuf (g, _pixbuf, -_pixbuf.Width / 2, -_pixbuf.Height / 2);
			g.Paint ();
			g.LineWidth = border;
			g.MoveTo (-_pixbuf.Width / 2, -_pixbuf.Height / 2);
			if (_outline != null) {
				g.LineTo (_pixbuf.Width / 2, -_pixbuf.Height / 2);
				g.LineTo (_pixbuf.Width / 2, _pixbuf.Height / 2);
				g.LineTo (-_pixbuf.Width / 2, _pixbuf.Height / 2);
				g.ClosePath ();
				g.Color = _outline._cairo;
			}
			g.Stroke ();
			foreach (Shape shape in shapes) {
				shape.render (g);
				shape.updateGlobalPosition (g);
			}
			g.Restore ();
			if (has_pen)
				pen.render (g);
		}
    
		public int width {
			get {
				//return (int)(_pixbuf.Width*_scaleFactor);
				return _pixbuf.Width;
			}
		}

		public int height {
			get {
				//return (int)(_pixbuf.Height*_scaleFactor);
				return _pixbuf.Height;
			}
		}

		public Pixel [] get_pixels ()
		{
			return new Pixel[10];
		}

		public override string ToString ()
		{
			return String.Format ("<Picture (width={0}, height={1})>", width, height);
		}

		public string __repr__ ()
		{
		    return ToString();
		}
	} // -- end of Picture class


	private static double currentTime ()
	{
		System.TimeSpan t = System.DateTime.UtcNow - new System.DateTime (1970, 1, 1);
		return t.TotalSeconds;
	}

	public class Button : Gtk.Button
	{
		public double _x, _y;
		public WindowClass window;
    
		public Button (IList iterable, string label) : base(label)
		{
			_x = System.Convert.ToDouble (iterable [0]);
			_y = System.Convert.ToDouble (iterable [1]);
		}
    
		public double x {
			get {
				return _x;
			}
			set {
				moveTo (value, _y);
				window.QueueDraw ();
			}
		}

		public double y {
			get {
				return _y;
			}
			set {
				moveTo (_x, value);
				window.QueueDraw ();
			}
		}

		public void moveTo (object x, object y)
		{
			_x = System.Convert.ToDouble (x);
			_y = System.Convert.ToDouble (y);
			// FIXME: actually move it
		}

		public void draw (WindowClass win)
		{ // button
			window = win;
			Invoke (delegate {
				Show ();
				window.getCanvas ().Put (this, (int)_x, (int)_y);
				window.QueueDraw ();
			});
		}

		public void connect (string signal, Func<object,Event,object> function)
		{
			Clicked += delegate(object obj, System.EventArgs args) {
				Event evt = new Event ("click", Graphics.currentTime ());
				try {
					Invoke (delegate {
						function (obj, evt);
					});
				} catch (Exception e) {
					Console.Error.WriteLine ("Error in connected function");
					Console.Error.WriteLine (e.Message);
				}        
			};
		}
	}

 public class Entry : Gtk.Entry
 {
     public double _x, _y;
     public WindowClass window;

     public Entry (IList iterable, int size) : base()
     {
         _x = System.Convert.ToDouble (iterable [0]);
         _y = System.Convert.ToDouble (iterable [1]);
         WidthChars = size;
         Text = "";
     }

     public string text {
        get {return Text;}
        set {Text = value;}
     }

     public double x {
         get {
             return _x;
         }
         set {
             moveTo (value, _y);
             window.QueueDraw ();
         }
     }

     public double y {
         get {
             return _y;
         }
         set {
             moveTo (_x, value);
             window.QueueDraw ();
         }
     }

     public void moveTo (object x, object y)
     {
         _x = System.Convert.ToDouble (x);
         _y = System.Convert.ToDouble (y);
         // FIXME: actually move it
     }

     public void draw (WindowClass win)
     { // button
         window = win;
         Invoke (delegate {
             Show ();
             window.getCanvas ().Put (this, (int)_x, (int)_y);
             window.QueueDraw ();
         });
     }

/*
     public void connect (string signal, Func<object,Event,object> function)
     {
         Clicked += delegate(object obj, System.EventArgs args) {
             Event evt = new Event ("click", Graphics.currentTime ());
             try {
                 Invoke (delegate {
                     function (obj, evt);
                 });
             } catch (Exception e) {
                 Console.Error.WriteLine ("Error in connected function");
                 Console.Error.WriteLine (e.Message);
             }
         };
     }
     */
 }

 	public class HSlider : Gtk.HScale
	{
		public WindowClass window;
		public double _width;
		public double _x, _y;
    
		public HSlider (IList iterable, object width) : 
      base(new Gtk.Adjustment (0.0, 0.0, 101.0, 0.1, 1.0, 1.0))
		{
			UpdatePolicy = Gtk.UpdateType.Continuous;
			Digits = 0;
			ValuePos = Gtk.PositionType.Top;
			DrawValue = true;    
			this.width = System.Convert.ToDouble (width);
			_x = System.Convert.ToDouble (iterable [0]);
			_y = System.Convert.ToDouble (iterable [1]);
		}
    
		public double x {
			get {
				return _x;
			}
			set {
				moveTo (value, _y);
				window.QueueDraw ();
			}
		}

		public double y {
			get {
				return _y;
			}
			set {
				moveTo (_x, value);
				window.QueueDraw ();
			}
		}

		public double width {
			get {
				return _width;
			}
			set {
				_width = value;
				SetSizeRequest ((int)_width, -1);
			}
		}

		public void moveTo (object x, object y)
		{
			_x = System.Convert.ToDouble (x);
			_y = System.Convert.ToDouble (y);
			// FIXME: actually move it
		}

		public void draw (WindowClass win)
		{ // hslider
			window = win;
			Invoke (delegate {
				Show ();
				window.getCanvas ().Put (this, (int)_x, (int)_y);
				window.QueueDraw ();
			});
		}

		public void connect (string signal, Func<object,Event,object> function)
		{
			if (signal.Equals ("change-value")) {
				ChangeValue += delegate(object obj, Gtk.ChangeValueArgs args) {
					Event evt = new Event (signal, (object)Value, Graphics.currentTime ());
					try {
						Invoke (delegate {
							function (obj, evt);
						});
					} catch (Exception e) {
						Console.Error.WriteLine ("Error in connected function");
						Console.Error.WriteLine (e.Message);
					}        
				};
			} else {
				throw new Exception ("invalid signal for this object");
			}
		}
	}
  
	public class Rectangle : Shape
	{
		public Rectangle (IList iterable1, IList iterable2) : base(true)
		{
			set_points (new Point (iterable1 [0], iterable1 [1]),
		 new Point (iterable2 [0], iterable1 [1]),
		 new Point (iterable2 [0], iterable2 [1]),
		 new Point (iterable1 [0], iterable2 [1]));
		}
    
		public double width {
			get { return points [2].x - points [0].x;}
		}

		public double height {
			get { return points [2].y - points [0].y;}
		}

		public override void addToPhysics ()
		{ // Rectangle
			world = window._canvas.world;
			float MeterInPixels = 64.0f;
			// from x,y to meters of window
			// arbitrary:
			Vector2 position = new Vector2 (((float)x) / MeterInPixels, 
                                     ((float)y) / MeterInPixels);
			body = FarseerPhysics.Factories.BodyFactory.CreateRectangle (
                 world,
                 (float)(width / MeterInPixels), // radius in meters
                 (float)(height / MeterInPixels), // radius in meters
                 _density, // density
                 position);                        // center
			// Give it some bounce and friction
			body.Restitution = _bounce;
			body.Rotation = (float)_rotation;
			body.Friction = _friction;
			body.BodyType = _bodyType;
			body.IsStatic = (_bodyType == FarseerPhysics.Dynamics.BodyType.Static);
			body.UserData = this; // point back to this shape
			body.FixtureList [0].UserData = this; // point back to this shape
		}
	}

	public class RoundedRectangle : Shape
	{
		public double radius = 0.0;

		public RoundedRectangle (IList iterable1, IList iterable2, double radius) : base(true)
		{
			set_points (new Point (iterable1 [0], iterable1 [1]),
                 new Point (iterable2 [0], iterable1 [1]),
                 new Point (iterable2 [0], iterable2 [1]),
                 new Point (iterable1 [0], iterable2 [1]));
			this.radius = radius;
		}
    
		public override void render (Cairo.Context g)
		{
			if (!visible)
				return;
			// draws rectangles with rounded (circular arc) corners 
			g.Save ();
			Point temp = screen_coord (center);
			g.Translate (temp.x, temp.y);
			g.Rotate (_rotation);
			g.Scale (_scaleFactor, _scaleFactor);
			if (points != null) {
				g.LineWidth = border;
				double top = points [0].y + radius;
				double bottom = points [2].y - radius;
				double left = points [0].x + radius;
				double right = points [2].x - radius;

				//g.MoveTo(p1.x, p2.y);
				g.Arc (left, top, radius, .50 * (Math.PI * 2.0), .75 * (Math.PI * 2.0));
				g.Arc (right, top, radius, .75 * (Math.PI * 2.0), 1.0 * (Math.PI * 2.0));
				g.Arc (right, bottom, radius, 1.0 * (Math.PI * 2.0), .25 * (Math.PI * 2.0));
				g.Arc (left, bottom, radius, .25 * (Math.PI * 2.0), .50 * (Math.PI * 2.0));

				g.ClosePath ();
				if (_fill != null) {
					g.Color = _fill._cairo;
					g.FillPreserve ();
				}
				if (_outline != null) {
					g.Color = _outline._cairo;
					g.Stroke ();
				}
				foreach (Shape shape in shapes) {
					shape.render (g);
					shape.updateGlobalPosition (g);
				}
				g.Restore ();
				if (has_pen)
					pen.render (g);
			}
		}
	}

	public class Polygon : Shape
	{

		public Polygon (Line line) : this(line.points)
		{
			center = line.center;
		}
    
		public Polygon (params object [] points) : base(true)
		{
			Point [] temp = new Point [points.Length];
			int count = 0;
			foreach (object o in points) {
				if (o is Point)
					temp [count] = (Point)o;
				else if (o is IList) {
					IList i = (IList)o;
					temp [count] = new Point (i [0], i [1]);
				} else {
					throw new Exception ("Polygon: can't convert arg to a point");
				}
				count++;
			}
			set_points (temp);
		}

		public override void addToPhysics ()
		{ // Polygon
			world = window._canvas.world;
			float MeterInPixels = 64.0f;
			// from x,y to meters of window
			FarseerPhysics.Common.Vertices vertices = new FarseerPhysics.Common.Vertices ();
			// Position is absolute:
			Vector2 position = new Vector2 (((float)x) / MeterInPixels, 
                                     ((float)y) / MeterInPixels);
			// Points should be relative to position:
			foreach (Point point in points) {
				vertices.Add (new Vector2 ((float)((point.x) / MeterInPixels), 
                                  (float)((point.y) / MeterInPixels)));
			}
			body = FarseerPhysics.Factories.BodyFactory.CreatePolygon (
                 world,
                 vertices, 
                 _density,
                 position);
			// Give it some bounce and friction
			body.BodyType = _bodyType;
			body.IsStatic = (_bodyType == FarseerPhysics.Dynamics.BodyType.Static);
			body.Restitution = _bounce;
			body.Rotation = (float)_rotation;
			body.Friction = _friction;
			body.UserData = this; // point back to this shape
			body.FixtureList [0].UserData = this; // point back to this shape
		}
	}

	public class Dot : Shape
	{
		public Dot (int x, int y) : base(true)
		{
			set_points (new Point (x, y));
		}

		public Dot (double x, double y) : base(true)
		{
			set_points (new Point (x, y));
		}

		public Dot (IList iterable) : base(true)
		{
			set_points (new Point (iterable));
		}
    
		public Dot (Dot dot) : base(true)
		{
			set_points (new Point (dot.x, dot.y));
		}
        
		public override void render (Cairo.Context g)
		{
			if (!visible)
				return;
			g.Save ();
			Point temp = screen_coord (center);
			g.Translate (temp.x, temp.y);
			g.Rotate (_rotation);
			g.Scale (_scaleFactor, _scaleFactor);
			if (points != null) {
				g.LineWidth = border;
				temp = screen_coord (points [0]);
				g.MoveTo (temp.x, temp.y);
				g.LineTo (temp.x + 1, temp.y + 1);
				g.ClosePath ();
				g.Stroke ();
			}
			foreach (Shape shape in shapes) {
				shape.render (g);
				shape.updateGlobalPosition (g);
			}
			g.Restore ();
			if (has_pen)
				pen.render (g);
		}
	}

	public class Circle : Shape
	{
		int _radius;

		public int radius {
			get {
				return _radius;
			}
			set {
				_radius = value;
				QueueDraw ();
			}
		}

		public Circle (IList iterable, int radius) : base(true)
		{
			set_points (new Point (iterable));
			_radius = radius;
		}

		public override void addToPhysics ()
		{ // Circle
			world = window._canvas.world;
			float MeterInPixels = 64.0f;
			// from x,y to meters of window
			// arbitrary:
			Vector2 position = new Vector2 (((float)x) / MeterInPixels, 
                                     ((float)y) / MeterInPixels);
			body = FarseerPhysics.Factories.BodyFactory.CreateCircle (
                 world,
                 radius / MeterInPixels, // radius in meters
                 _density, // density
                 position);                        // center
			// Give it some bounce and friction
			body.BodyType = _bodyType;
			body.IsStatic = (_bodyType == FarseerPhysics.Dynamics.BodyType.Static);
			body.Restitution = _bounce;
			body.Rotation = (float)_rotation;
			body.Friction = _friction;
			body.UserData = this; // point back to this shape
			body.FixtureList [0].UserData = this; // point back to this shape
		}

		public override void render (Cairo.Context g)
		{
			if (!visible)
				return;
			g.Save ();
			// Center is in global screen coords, whatever they are
			Point temp = screen_coord (center);
			// Temp is in Gtk coordinate system
			g.Translate (temp.x, temp.y);
			g.Rotate (_rotation);
			g.Scale (_scaleFactor, _scaleFactor);
			// Now move to 0,0 as origin of shape
			temp = screen_coord (points [0]);
			g.LineWidth = border;
			g.Arc (temp.x, temp.y, radius, 0.0, 2.0 * Math.PI); // x, y, radius, start, end
			g.ClosePath ();
			if (gradient != null) {
				Cairo.Gradient pat;
				if (gradient.gtype == "linear")
					pat = new Cairo.LinearGradient (gradient.p1.x,
                                         gradient.p1.y, 
                                         gradient.p2.x, 
                                         gradient.p2.y);
				else
					pat = new Cairo.RadialGradient (gradient.p1.x,
                                         gradient.p1.y, 
                                         gradient.radius1,
                                         gradient.p2.x, 
                                         gradient.p2.y,
                                         gradient.radius2);
        
				pat.AddColorStop (0, gradient.c1.getCairo ());
				pat.AddColorStop (1, gradient.c2.getCairo ());
				g.Pattern = pat;
				g.FillPreserve ();
			} else if (_fill != null) {
				g.Color = _fill._cairo;
				g.FillPreserve ();
			}
			if (_outline != null) {
				g.Color = _outline._cairo;
				g.Stroke ();
			}
			foreach (Shape shape in shapes) {
				shape.render (g);
				shape.updateGlobalPosition (g);
			}
			g.Restore ();
		}
	}

	public class Oval : Shape
	{
		int _xRadius;
		int _yRadius;
    
		public int xRadius {
			get {
				return _xRadius;
			}
			set {
				_xRadius = value;
				QueueDraw ();
			}
		}
    
		public int yRadius {
			get {
				return _yRadius;
			}
			set {
				_yRadius = value;
				QueueDraw ();
			}
		}

		public Oval (IList iterable, int xRadius, int yRadius) : base(true)
		{
			set_points (new Point (iterable));
			_xRadius = xRadius;
			_yRadius = yRadius;
		}
    
		public override void render (Cairo.Context g)
		{
			if (!visible)
				return;
			g.Save ();
			// Center is in global screen coords, whatever they are
			Point temp = screen_coord (center);
			// Temp is in Gtk coordinate system
			g.Translate (temp.x, temp.y);
			g.Rotate (_rotation);
			g.Scale (_scaleFactor, _scaleFactor);
			// Now, turn into an Oval:
			g.Scale (1.0, ((double)_yRadius) / ((double)_xRadius));
			// Now move to 0,0 as origin of shape
			temp = screen_coord (points [0]);
			g.LineWidth = border;
			g.Arc (temp.x, temp.y, _xRadius, 0.0, 2.0 * Math.PI); // x, y, radius, start, end
			g.ClosePath ();
			if (gradient != null) {
				Cairo.Gradient pat;
				if (gradient.gtype == "linear")
					pat = new Cairo.LinearGradient (gradient.p1.x,
                                         gradient.p1.y, 
                                         gradient.p2.x, 
                                         gradient.p2.y);
				else
					pat = new Cairo.RadialGradient (gradient.p1.x,
                                         gradient.p1.y, 
                                         gradient.radius1,
                                         gradient.p2.x, 
                                         gradient.p2.y,
                                         gradient.radius2);
        
				pat.AddColorStop (0, gradient.c1.getCairo ());
				pat.AddColorStop (1, gradient.c2.getCairo ());
				g.Pattern = pat;
				g.FillPreserve ();
			} else if (_fill != null) {
				g.Color = _fill._cairo;
				g.FillPreserve ();
			}
			if (_outline != null) {
				g.Color = _outline._cairo;
				g.Stroke ();
			}
			foreach (Shape shape in shapes) {
				shape.render (g);
				shape.updateGlobalPosition (g);
			}
			g.Stroke ();
			g.Restore ();
		}

		public override void addToPhysics ()
		{ // Circle
			world = window._canvas.world;
			float MeterInPixels = 64.0f;
			// from x,y to meters of window
			// arbitrary:
			Vector2 position = new Vector2 (((float)x) / MeterInPixels, 
                                     ((float)y) / MeterInPixels);
			body = FarseerPhysics.Factories.BodyFactory.CreateEllipse (
                 world,
                 xRadius / MeterInPixels, // x radius in meters
                 yRadius / MeterInPixels, // y radius in meters
                 20,
                 _density, // density
                 position);                        // center
			// Give it some bounce and friction
			body.BodyType = _bodyType;
			body.IsStatic = (_bodyType == FarseerPhysics.Dynamics.BodyType.Static);
			body.Restitution = _bounce;
			body.Rotation = (float)_rotation;
			body.Friction = _friction;
			body.UserData = this; // point back to this shape
			body.FixtureList [0].UserData = this; // point back to this shape
		}
	}

	public class Pie : Shape
	{
		int _radius;
		double _start;
		double _stop;

		public Pie (IList iterable, int radius, double start, double stop) : base(true)
		{
			set_points (new Point (iterable));
			_radius = radius;
			_start = start;
			_stop = stop;
		}

		public override void addToPhysics ()
		{ // Circle
			world = window._canvas.world;
			float MeterInPixels = 64.0f;
			// from x,y to meters of window
			// arbitrary:
			Vector2 position = new Vector2 (((float)x) / MeterInPixels, 
                                     ((float)y) / MeterInPixels);
			body = FarseerPhysics.Factories.BodyFactory.CreateCircle (
                 world,
                 radius / MeterInPixels, // radius in meters
                 _density, // density
                 position);                        // center
			// Give it some bounce and friction
			body.BodyType = _bodyType;
			body.IsStatic = (_bodyType == FarseerPhysics.Dynamics.BodyType.Static);
			body.Restitution = _bounce;
			body.Rotation = (float)_rotation;
			body.Friction = _friction;
			body.UserData = this; // point back to this shape
			body.FixtureList [0].UserData = this; // point back to this shape
		}

		/*
    public override void addToPhysics() { // Pie
      world = window._canvas.world;
      float MeterInPixels = 64.0f;
      // from x,y to meters of window
      // arbitrary:
      Vector2 position = new Vector2(((float)x)/MeterInPixels, 
                                     ((float)y)/MeterInPixels);
      // FIXME: set rotation in radians
      body = FarseerPhysics.Factories.BodyFactory.CreateLineArc(
                 world,
                 (float)((stop - start) * Math.PI/ 180.0),   // radians
                 10,                                         // sides
                 (float)(radius / MeterInPixels),            // radius in meters
                 position,                                   // position
                 (float)(0 * Math.PI/ 180.0),                // angle
                 true);                                      // close
      // Give it some bounce and friction
      body.BodyType = _bodyType;
      body.IsStatic = (_bodyType == FarseerPhysics.Dynamics.BodyType.Static);
      body.Restitution = _bounce;
      body.Friction = _friction;
    }
    */

		public override void render (Cairo.Context g)
		{
			if (!visible)
				return;
			g.Save ();
			// Center is in global screen coords, whatever they are
			Point temp = screen_coord (center);
			// Temp is in Gtk coordinate system
			g.Translate (temp.x, temp.y);
			g.Rotate (_rotation);
			g.Scale (_scaleFactor, _scaleFactor);
			// Now move to 0,0 as origin of shape
			temp = screen_coord (points [0]);
			g.LineWidth = border;
			double tstart = start * (Math.PI) / 180.0;
			double tstop = stop * (Math.PI) / 180.0;
			g.MoveTo (temp.x, temp.y);
			g.Arc (temp.x, temp.y, radius, tstart, tstop); // x, y, radius, start, end
			g.ClosePath ();
			if (gradient != null) {
				Cairo.Gradient pat;
				if (gradient.gtype == "linear")
					pat = new Cairo.LinearGradient (gradient.p1.x,
                                         gradient.p1.y, 
                                         gradient.p2.x, 
                                         gradient.p2.y);
				else
					pat = new Cairo.RadialGradient (gradient.p1.x,
                                         gradient.p1.y, 
                                         gradient.radius1,
                                         gradient.p2.x, 
                                         gradient.p2.y,
                                         gradient.radius2);
        
				pat.AddColorStop (0, gradient.c1.getCairo ());
				pat.AddColorStop (1, gradient.c2.getCairo ());
				g.Pattern = pat;
				g.FillPreserve ();
			} else if (_fill != null) {
				g.Color = _fill._cairo;
				g.FillPreserve ();
			}
			if (_outline != null) {
				g.Color = _outline._cairo;
				g.Stroke ();
			}
			foreach (Shape shape in shapes) {
				shape.render (g);
				shape.updateGlobalPosition (g);
			}
			g.Restore ();
		}

		public int radius {
			get {
				return _radius;
			}
			set {
				_radius = value;
				QueueDraw ();
			}
		}

		public double start {
			get {
				return _start;
			}
			set {
				_start = value;
				QueueDraw ();
			}
		}

		public double stop {
			get {
				return _stop;
			}
			set {
				_stop = value;
				QueueDraw ();
			}
		}
	}

	public class Arc : Shape
	{
		int _radius;
		double _start;
		double _stop;

		public Arc (IList iterable, int radius, double start, double stop) 
    : base(true)
		{
			set_points (new Point (iterable));
			fill = null;
			_radius = radius;
			_start = start;
			_stop = stop;
		}

		public override void addToPhysics ()
		{ // Circle
			world = window._canvas.world;
			float MeterInPixels = 64.0f;
			// from x,y to meters of window
			// arbitrary:
			Vector2 position = new Vector2 (((float)x) / MeterInPixels, 
                                     ((float)y) / MeterInPixels);
			body = FarseerPhysics.Factories.BodyFactory.CreateCircle (
                 world,
                 radius / MeterInPixels, // radius in meters
                 _density, // density
                 position);                        // center
			// Give it some bounce and friction
			body.BodyType = _bodyType;
			body.IsStatic = (_bodyType == FarseerPhysics.Dynamics.BodyType.Static);
			body.Restitution = _bounce;
			body.Rotation = (float)_rotation;
			body.Friction = _friction;
			body.UserData = this; // point back to this shape
			body.FixtureList [0].UserData = this; // point back to this shape
		}

		public override void render (Cairo.Context g)
		{
			if (!visible)
				return;
			g.Save ();
			// Center is in global screen coords, whatever they are
			Point temp = screen_coord (center);
			// Temp is in Gtk coordinate system
			g.Translate (temp.x, temp.y);
			g.Rotate (_rotation);
			g.Scale (_scaleFactor, _scaleFactor);
			// Now move to 0,0 as origin of shape
			temp = screen_coord (points [0]);
			g.LineWidth = border;
			double tstart = start * (Math.PI) / 180.0;
			double tstop = stop * (Math.PI) / 180.0;
			g.Arc (temp.x, temp.y, radius, tstart, tstop); // x, y, radius, start, end
			if (gradient != null) {
				Cairo.Gradient pat;
				if (gradient.gtype == "linear")
					pat = new Cairo.LinearGradient (gradient.p1.x,
                                         gradient.p1.y, 
                                         gradient.p2.x, 
                                         gradient.p2.y);
				else
					pat = new Cairo.RadialGradient (gradient.p1.x,
                                         gradient.p1.y, 
                                         gradient.radius1,
                                         gradient.p2.x, 
                                         gradient.p2.y,
                                         gradient.radius2);
        
				pat.AddColorStop (0, gradient.c1.getCairo ());
				pat.AddColorStop (1, gradient.c2.getCairo ());
				g.Pattern = pat;
				g.FillPreserve ();
			} else if (_fill != null) {
				g.Color = _fill._cairo;
				g.FillPreserve ();
			}
			if (_outline != null) {
				g.Color = _outline._cairo;
				g.Stroke ();
			}
			foreach (Shape shape in shapes) {
				shape.render (g);
				shape.updateGlobalPosition (g);
			}
			g.Restore ();
		}

		public int radius {
			get {
				return _radius;
			}
			set {
				_radius = value;
				QueueDraw ();
			}
		}

		public double start {
			get {
				return _start;
			}
			set {
				_start = value;
				QueueDraw ();
			}
		}

		public double stop {
			get {
				return _stop;
			}
			set {
				_stop = value;
				QueueDraw ();
			}
		}
	}

	public class Frame : Shape
	{
		public Frame (int x, int y)
		{
			set_points (new Point (x, y));
		}

		public Frame (IList iterable)
		{
			set_points (new Point (iterable));
		}
	}
	
	public class Node
	{
		public Graph graph;
		public string name;
		public string label;
		public string shape;
		
		public Node(string name) {
			this.name = name;
		}
		public Shape getShape() {
			return graph.vertices[name]["shape"];
		}
	}
	
	public class Edge
	{
		public Graph graph;
		public string label;
		public Node nodeFrom;
		public Node nodeTo;
		public string recordFrom;
		public string recordTo;
		public string nameFrom;
		public string nameTo;
		
		public Edge(string nameFrom, string recordFrom, Node nodeFrom, 
					string nameTo, string recordTo, Node nodeTo) {
			this.nameFrom = nameFrom;
			this.recordFrom = recordFrom;
			this.nodeFrom = nodeFrom;
			this.nameTo = nameTo;
			this.recordTo = recordTo;
			this.nodeTo = nodeTo;
		}
		
		public string getFromName() {
			if (recordFrom != null)
				return String.Format("{0}:{1}", nameFrom, recordFrom);
			return nameFrom;
		}
		public string getToName() {
			if (recordTo != null)
				return String.Format("{0}:{1}", nameTo, recordTo);
			return nameTo;
		}
	}

	public class Graph
	{
		public static double graph_count = 1;
		public Graphics.WindowClass window = null;
		public Dictionary<string,Dictionary<string,Shape>> vertices = new Dictionary<string,Dictionary<string,Shape>> ();
		public Dictionary<string,Dictionary<string,object>> edges = new Dictionary<string, Dictionary<string,object>> ();
		public Dictionary<string,object> options = new Dictionary<string, object> {
			{"default_shape", "circle"},
			{"default_outline", "black"},
			{"fill", "white"},
			{"line_type", "curve"},
		};
	  public Graphviz4Net.Dot.AntlrParser.AntlrParserAdapter<string> parser = null;
	  public Graphviz4Net.Dot.DotGraph<string> graph = null;
		public Dictionary<string,Node> graphNodes = new Dictionary<string,Node>();
		public List<Edge> graphEdges = new List<Edge>();
		public string pre_text;
		public string post_text;
		double scale = 1.0;
		
		public Graph ()
		{
		}
		
		public void addNode(string name) {
			graphNodes[name] = new Graphics.Node(name);
			graphNodes[name].graph = this;
		}
		
		public void addNode(Node node) {
			graphNodes[node.name] = node;
			graphNodes[node.name].graph = this;
		}
		
		public Node lookupNode(string name) {
			foreach(Node node in graphNodes.Values) {
				if (node.name == name)
					return node;
			}
			return null;
		}
		
		public List<Shape> getEdgeLines(object id) {
			return (List<Shape>)edges[id.ToString()]["line"];
		}

		public Shape getNode(object id) {
			return (Shape)vertices[id.ToString()]["shape"];
		}

		public void addEdge(string nameFrom, string nameTo) {
			string recordFrom = null;
			string recordTo = null;
			if (nameFrom.Contains(":")) {
				string [] parts = nameFrom.Split(':');
				nameFrom = parts[0];
				recordFrom = parts[1];
			}
			if (nameTo.Contains(":")) {
				string [] parts = nameTo.Split(':');
				nameTo = parts[0];
				recordTo = parts[1];
			}
			Edge edge = new Edge(nameFrom, recordFrom, lookupNode(nameFrom), 
						  		 nameTo, recordTo, lookupNode(nameTo));
			graphEdges.Add(edge);
			edge.graph = this;
		}

		Point translate(double x, double y) {
        	return new Point(x * scale + window.width/2 - graph.Width/ 2 * scale,
                			(graph.Height - y) * scale + window.height/2 - graph.Height/ 2 * scale);
		}

		public string recurseEdges(IList list, int left, int root, int right) {
			string edges = "";
			if (list == null || list.Count == 0) {
				// Pass
			} else {
				if (left < list.Count && list[left] != null) {
					if (list[left] is IList && ((IList)list[left]).Count > 0) {
						edges += String.Format("  {0}:left -> {1};\n", list[root], ((IList)list[left])[root]);
						edges += recurseEdges((IList)list[left], left, root, right);
					} else if (list[left] != null && !(list[left] is IList)) {
						edges += String.Format("  {0}:left -> {1};\n", list[root], list[left]);
					}
				}
				if (right < list.Count && list[right] != null) {
					if (list[right] is IList && ((IList)list[right]).Count > 0) {
						edges += String.Format("  {0}:right -> {1};\n", list[root], ((IList)list[right])[root]);
						edges += recurseEdges((IList)list[right], left, root, right);
					} else if (list[right] != null  && !(list[right] is IList)) {
						edges += String.Format("  {0}:right -> {1};\n", list[root], list[right]);
					}
				}
			}
			return edges;
		}
		
		public string recurseNodes(IList list, int left, int root, int right) {
			string nodes = "";
			if (list != null && list.Count > 0) {
				// Left:
				if (left < list.Count && list[left] is IList)
					nodes += recurseNodes((IList)list[left], left, root, right);
				else if (left < list.Count && list[left] != null)
					nodes += String.Format("  {0} [label=\"<left> | {0} | <right>\"];\n", list[left].ToString());
				// Root:
				nodes += String.Format("  {0} [label=\"<left> | {0} | <right>\"];\n", list[root].ToString());
				// Right:
				if (right < list.Count && list[right] is IList)
					nodes += recurseNodes((IList)list[right], left, root, right);
				else if (right < list.Count && list[right] != null)
					nodes += String.Format("  {0} [label=\"<left> | {0} | <right>\"];\n", list[right].ToString());
			}
			return nodes;
		}

		public void layout(IList list) {
			layout(list, 0, 1, 2);
		}
		
		public void layout(IList list, int left, int root, int right) {
			string edges = "digraph {\n";
			edges += @"  node [label=""\\N"", shape=record];";
			edges += "\n";
			edges += recurseNodes(list, left, root, right); // left, root, right
			edges += recurseEdges(list, left, root, right); // left, root, right
			edges += "}";
			layout(edges);
		}
		
		public void layout() {
			string edges = "digraph {\n";
			foreach (Edge edge in graphEdges) {
				edges += String.Format("{0} -> {1}\n", edge.getFromName(), edge.getToName());
			}
			edges += "}";
			layout(edges);
		}
		
		public void layout (string contents) {
			layout(contents, true);
		}

		public void layout (string contents, bool process)
		{
			pre_text = contents; // for debugging
			if (process)
				contents = processDot(contents);
			post_text = contents; // for debugging
			parser = Graphviz4Net.Dot.AntlrParser.AntlrParserAdapter<string>.GetParser();
			graph = parser.Parse (contents);
		}

		public void draw(WindowClass window) {
			this.window = window;
			draw();
		}

		public void draw(WindowClass window, IDictionary options) {
			this.window = window;
			draw(options);
		}

		public void draw(IDictionary options) {
			foreach(KeyValuePair<object,object> kvp in (IDictionary<object,object>)options) {
				this.options[kvp.Key.ToString()] = kvp.Value;
			}
			draw();
		}
		
		public void draw() {
			if (window == null) {
				string label;
				if (options.ContainsKey("label")) {
                	label = (string)options["label"];
				} else if (graph.Attributes.ContainsKey("label")) {
                	label = graph.Attributes["label"].Trim().Split('\n')[0].Trim();
				} else {
                	label = String.Format("Graph #{0}", Graph.graph_count);
					Graph.graph_count++;
				}
				int width, height;
				if (options.ContainsKey("width")) {
					width = (int)options["width"];
				} else {
					width = (int)graph.Width;
				}
				if (options.ContainsKey("height")) {
					height = (int)options["height"];
				} else {
					height = (int)graph.Height;
				}
				window = Graphics.Window(label, width, height);
			}
			if (options.ContainsKey("scale")) {
            	scale = (double)options["scale"];
			} else {
            	scale = Math.Min(window.width/(double)graph.Width, window.height/(double)graph.Height) * .95;
			}
	        foreach(Graphviz4Net.Dot.DotVertex<string> v in graph.Vertices) {
	            if (v.Position == null) {
	                continue;
				}
	            Point c = translate(((Graphviz4Net.Point)v.Position).X, ((Graphviz4Net.Point)v.Position).Y);
	            int width = (int)(((double)v.Width) * 72 * scale);
				int height = (int)(((double)v.Height) * 72 * scale);
				string shape;
				if (options.ContainsKey("shape")) {
	                shape = (string)options["shape"];
				} else if (v.Attributes.ContainsKey("shape")) {
	                shape = v.Attributes["shape"];
				} else {
	                shape = (string)options["default_shape"];
				}
				Graphics.Color outline;
				if (options.ContainsKey("outline")) {
	                outline = new Graphics.Color((string)options["outline"]);
				} else if  (v.Attributes.ContainsKey("color")) {
	                outline = new Graphics.Color(v.Attributes["color"]);
				} else {
	                outline = new Graphics.Color((string)options["default_outline"]);
				}
	            // Shapes:
				Shape obj1 = null;
				Shape obj2 = null;
	            if (shape == "circle" || shape == "ellipse") {
	                obj1 = new Graphics.Oval(c, width/2, height/2);
				} else if (shape == "doublecircle") {
	                obj1 = new Graphics.Oval(c, width/2, height/2);
	                obj2 = new Graphics.Oval(c, width/2 - 4, height/2 - 4);
				} else if (shape == "box") {
	                obj1 = new Graphics.Rectangle(new Point(c.x - width/2, c.y - height/2),
	                                          	  new Point(c.x + width/2, c.y + height/2));
	            //elif shape == "diamond":
				} else {
	                throw new Exception(String.Format("unknown shape: {0}", shape));
				}
	            if (obj1 != null) {
	                obj1.outline = new Graphics.Color(outline);
	                obj1.fill = new Graphics.Color((string)options["fill"]);
	                obj1.border = 2;
	                obj1.draw(window);
				}
	            if (obj2 != null) {
	                obj2.outline = new Graphics.Color(outline);
	                obj2.fill = new Graphics.Color((string)options["fill"]);
	                obj2.border = 2;
	                obj2.draw(window);
				}
	            // Text:
				string label;
	            if (v.Attributes.ContainsKey("label")) {
	                label = v.Attributes["label"].Trim();
				} else {
	                label = v.Id.Trim();
				}
	            if (label.Contains("|")) {
	                string [] labels = label.Split('|');
	                int parts = labels.Length;
	                for (int divider = 0; divider < labels.Length - 1; divider++) {
	                    double x1 = c.x - width/2 + (divider + 1) * width/parts;
	                    double y1 = c.y - height/2;
	                    double x2 = x1;
	                    double y2 = c.y + height/2;
	                    Line line = new Graphics.Line(new Point(x1, y1), new Point(x2, y2));
	                    line.outline = new Graphics.Color("black");
	                    line.draw(window);
					}
	                label = labels[1].Trim(); // FIXME: should draw each part
				}
				// Add label as tag to objects
	            if (obj1 != null) {
	                obj1.tag = label;
				}
	            if (obj2 != null) {
	                obj2.tag = label;
				}
				// Draw text:
	            Graphics.Text text = new Graphics.Text(c, label);
	            text.fontSize = 10 * scale;
	            text.color = new Graphics.Color("black");
	            text.draw(window);
				// Add group to shape list:
	            vertices[label] = new Dictionary<string,Shape>();
	            if (obj1 != null && obj2 != null) {
	                vertices[label]["shape"] = new Graphics.Group(obj1, obj2);
				} else {
	                vertices[label]["shape"] = obj1;
				}
	            vertices[label]["label"] = text;
			}
	        int count = 0;
	        foreach(Graphviz4Net.Dot.DotEdge<string> e in graph.Edges) {
				string index;
	            if (e.LabelPos != null) {
	                index = e.Label.Trim();
				} else {
	                index = count.ToString();
				}
	            edges[index] = new Dictionary<string,object>();
	            edges[index]["line"] = new List<Shape>();
				List<Point> points = new List<Point>();
				foreach(Graphviz4Net.Point p in e.Path) {
	            	points.Add(new Graphics.Point(translate(p.X, p.Y)));
				}
				string color;
	            if (e.Attributes.ContainsKey("color")) {
	                color = e.Attributes["color"];
				} else {
	                color = "black";
				}
	            // Line:
	            if ((string)options["line_type"] == "curve") {
	                for (int i = 0; i < points.Count/3; i++) {
	                    int j = i * 3;
	                    Curve line = new Graphics.Curve(points[j], points[j + 1], points[j + 2], points[j + 3]);
	                    line.outline = new Graphics.Color(color);
	                    line.border = 2;
	                    line.draw(window);
	                    ((List<Shape>)edges[index]["line"]).Add(line);
					}
				} else {
	                Line line = new Graphics.Line(points[0], points[points.Count - 1]);
	                line.outline = new Graphics.Color(color);
	                line.border = 2;
	                line.draw(window);
	                ((List<Shape>)edges[index]["line"]).Add(line);
				}
				// Arrows:
		        double w;
				double h;
	           	if (e.SourceArrowEnd != null) {
	                Arrow arrow = new Graphics.Arrow(points[0]);
	                if ((string)options["line_type"] == "curve") {
	                    w = points[0].x - points[1].x;
	                    h = points[0].y - points[1].y;
					} else {
	                    w = points[0].x - points[points.Count - 1].x;
	                    h = points[0].y - points[points.Count - 1].y;
					}
	                double degrees = System.Math.Atan2(w, h) * 180/System.Math.PI + 90;
	                arrow.fill = new Graphics.Color(color);
	                arrow.rotate(degrees);
	                arrow.scale(scale);
	                arrow.draw(window);
	                edges[index]["source_arrow"] = arrow;
				}
	            if (e.DestinationArrowEnd != null) {
	                Arrow arrow = new Graphics.Arrow(points[points.Count - 1]);
	                if ((string)options["line_type"] == "curve") { // FIXME: these may be backwards:
	                    w = points[points.Count - 2].x - points[points.Count - 1].x;
	                    h = points[points.Count - 2].y - points[points.Count - 1].y;
					} else {
	                    w = points[0].x - points[points.Count - 1].x;
	                    h = points[0].y - points[points.Count - 1].y;
					}
	                double degrees = System.Math.Atan2(w, h) * 180/System.Math.PI + 90;
	                arrow.fill = new Graphics.Color(color);
	                arrow.rotate(degrees);
	                arrow.scale(scale);
	                arrow.draw(window);
	                edges[index]["destination_arrow"] = arrow;
				}
	            if (e.LabelPos != null) {
	                Point p = translate(((Graphviz4Net.Point)e.LabelPos).X, 
										((Graphviz4Net.Point)e.LabelPos).Y);
	                Text text = new Graphics.Text(p, e.Label);
	                text.fontSize = 10 * scale;
	                text.color = new Graphics.Color("black");
	                text.draw(window);
	                edges[index]["label"] = text;
				}
	            count++;
			}
		}
		
		public static string processDot(string text) {
			string retval = null;
		    Process myProcess = new Process();

		    // create a temporary file with the text to be processed
		    var textpath = System.IO.Path.GetTempFileName();
		    using (TextWriter writer = File.CreateText(textpath)){
		        writer.WriteLine(text);
		    }
		
		    try {
		      myProcess.StartInfo.UseShellExecute = false;
		
		      if (Graphics.os_name == "Windows") {
		        string file = Graphics.startup_path;
		        //file = Path.Combine(file, "bin");
		        file = Path.Combine(file, "windows");
		        file = Path.Combine(file, "dot");
		        myProcess.StartInfo.FileName = Path.Combine(file, "dot.exe");
		      } else {
		        if (File.Exists("/usr/bin/dot")){
		        // assumes espeak is in /usr/bin/ on macs
		            myProcess.StartInfo.FileName = "dot";
		        }
		        else if (File.Exists("/usr/local/bin/dot")){
			// or look for espeak is in /usr/local/bin/ on macs
		            myProcess.StartInfo.FileName = "dot";
		        }
		        else{
		        // assumes in path
		            myProcess.StartInfo.FileName = "dot";
		        }
		      }
		      myProcess.StartInfo.CreateNoWindow = true;
			  myProcess.StartInfo.RedirectStandardOutput = true;
		      myProcess.StartInfo.Arguments = ("-Tdot " + textpath);
		      myProcess.Start();
			  retval = myProcess.StandardOutput.ReadToEnd();			  
			  myProcess.WaitForExit();
		#pragma warning disable 219
		    } catch (Exception e) {
		#pragma warning restore 219
		      if (warn_missing_dot) {
		        Console.WriteLine("WARNING: missing dot command");
		        warn_missing_dot = false; // just once
		      }
		    }
			return retval;
		}

	}

	public class Group : Shape
	{
		public List<Shape> items = new List<Shape> ();
		public string mode = "individual"; // squadron or individual
    
		public Group (params Shape [] shapes) : base(false)
		{
			foreach (Shape shape in shapes) {
				items.Add (shape);
			}
			center = new Point (0, 0);
		}
    
		public override void rotate (double degrees)
		{
			// rotate group
			if (mode == "individual") {
				foreach (Shape shape in items) {
					shape.rotate (degrees);
				}
			} else {
				// find center of screen positions
				double x = 0, y = 0;
				foreach (Shape shape in items) {
					x += shape.center.x;
					y += shape.center.y;
				}
				center.x = x / (items.Count);
				center.y = y / (items.Count);
				// rotate them
				foreach (Shape shape in items) {
					shape.rotate (degrees);
				}
			}
		}
    
		public override void rotateTo (double degrees)
		{
			foreach (Shape shape in items) {
				shape.rotateTo (degrees);
			}
		}
    
		public override void moveTo (double x, double y)
		{
			foreach (Shape shape in items) {
				shape.moveTo (x, y);
			}
		}
    
		public override void move (double x, double y)
		{
			foreach (Shape shape in items) {
				shape.move (x, y);
			}
		}
	}

}


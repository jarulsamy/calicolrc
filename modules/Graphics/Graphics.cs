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
using IronPython.Runtime; // Operations, List, Tuple, Dict, ...
using System.Runtime.InteropServices; // Marshal
using System.Collections.Generic;
using System.Collections; // IEnumerable
using System.Threading;
using System.Drawing; // Image, Bitmap
using System.Net; // WebRequest
using System.IO; // MemoryStream
using System;

public static class Graphics {

  private static WindowClass _lastWindow = null;

  public static readonly Dictionary<string,Color> colors = 
    new Dictionary<string,Color>() {
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
  
  public static List PyList(params object [] items) {
    // make a list from an array
    List retval = new List();
    for (int i = 0; i < items.Length; i++) {
      retval.append(items[i]);
    }
    return retval;
  }
  
  public static PythonTuple PyTuple(params object [] items) {
    // make a tuple from an array
    return new PythonTuple(items);
  }

  private static Dictionary<string, Graphics.WindowClass> _windows =
    new Dictionary<string, Graphics.WindowClass>();
  
  public static IEnumerable getPixels(Picture picture) {
    for (int x=0; x < picture.width; x++) {
      for (int y=0; y < picture.height; y++) {
	yield return picture.getPixel(x, y);
      }
    }
  }
  public static Color getColor(Pixel pixel) {
    return pixel.getColor();
  }
  public static PythonTuple getRGB(Pixel pixel) {
    return pixel.getRGB();
  }
  public static PythonTuple getRGBA(Pixel pixel) {
    return pixel.getRGBA();
  }
  public static int getGray(Pixel pixel) {
    return pixel.getGray();
  }
  public static int getRed(Pixel pixel) {
    return pixel.getRed();
  }
  public static int getGreen(Pixel pixel) {
    return pixel.getGreen();
  }
  public static int getBlue(Pixel pixel) {
    return pixel.getBlue();
  }
  public static int getAlpha(Pixel pixel) {
    return pixel.getAlpha();
  }
  public static void setRGB(Pixel pixel, int red, int green, int blue) {
    pixel.setRGB((byte)red, (byte)green, (byte)blue);
  }
  public static void setRGB(Pixel pixel, PythonTuple rgb) {
    pixel.setRGB((byte)rgb[0], (byte)rgb[1], (byte)rgb[2]);
  }
  public static void setRGB(Pixel pixel, float red, float green, float blue) {
    pixel.setRGB((byte)red, (byte)green, (byte)blue);
  }
  public static void setRGB(Pixel pixel, byte red, byte green, byte blue) {
    pixel.setRGB(red, green, blue);
  }
  public static void setRGBA(Pixel pixel, byte red, byte green, byte blue, byte alpha) {
    pixel.setRGBA(red, green, blue, alpha);
  }
  public static void setGray(Pixel pixel, byte value) {
    pixel.setGray(value);
  }
  public static void setRed(Pixel pixel, byte value) {
    pixel.setRed(value);
  }
  public static void setGreen(Pixel pixel, byte value) {
    pixel.setGreen(value);
  }
  public static void setBlue(Pixel pixel, byte value) {
    pixel.setBlue(value);
  }
  public static void setAlpha(Pixel pixel, byte value) {
    pixel.setAlpha(value);
  }
  
  public static void savePicture(Picture picture, string filename) {
    picture.savePicture(filename);
  }
  public static void savePicture(List list, string filename, short delay, bool repeat) {
    List<GifLib.GifFrame> frameList = new List<GifLib.GifFrame>();
    foreach (Graphics.Picture picture in list) {
      Gdk.Pixbuf pixbuf = picture.getPixbuf();
      Byte [] buffer = pixbuf.SaveToBuffer("png");
      MemoryStream ms = new MemoryStream(buffer);
      Bitmap bitmap = new Bitmap(ms);
      //bitmap.MakeTransparent(backColor);
      GifLib.GifFrame frame = GifLib.GifHelper.BitmapToFrame(bitmap);
      frameList.Add(frame);
    }
    GifLib.GifHelper.Merge(frameList, filename, delay, repeat);
  }
  public static Picture makePicture(int x, int y) {
    return new Picture(x, y);
  }
  public static Picture makePicture(int x, int y, Color c) {
    return new Picture(x, y, c);
  }
  public static Picture makePicture(string filename) {
    return new Picture(filename);
  }
  
  public static Picture copyPicture(Picture picture) {
    return new Picture(picture);
  }
  
  public static void Init() { 
    // Start a thread in Background to run Graphics
    // Only for use in non-GUI environments
    Thread t = new Thread(GraphicsLoop);
    t.Start();
  }
  
  public static void GraphicsLoop() {
    Gtk.Application.Init();
    Gtk.Application.Run();
  }
  
  public static Picture makePicture(ICanvas can) { //, string filename) {
    ManualResetEvent ev = new ManualResetEvent(false);
    Gdk.Pixbuf pixbuf = null;
    Gtk.Application.Invoke( delegate {
	Gdk.Drawable drawable = can.getDrawable();
	Gdk.Colormap colormap = drawable.Colormap;
	int _width = 0;
	int _height = 0;
	drawable.GetSize(out _width, out _height);
	pixbuf = Gdk.Pixbuf.FromDrawable(drawable, colormap, 0, 0, 0, 0, _width, _height);
	ev.Set();
      });
    ev.WaitOne();
    return new Picture(pixbuf);
  }
  
  public static Graphics.WindowClass makeWindow(string title="Calico Graphics",
					   int width=300, 
					   int height=300) {
    if (_windows.ContainsKey(title)) {
      _windows[title]._canvas.shapes.Clear();
      _windows[title].ShowAll();
      _windows[title].Resize(width, height);
      _windows[title].QueueDraw();
      /*
      Gtk.Application.Invoke(delegate { 
	  _windows[title].GdkWindow.UrgencyHint = true;
	});
      */
      return _windows[title];
    } else {
      _windows[title] = new Graphics.WindowClass(title, width, height);
      /*
      Gtk.Application.Invoke(delegate { 
	  _windows[title].GdkWindow.UrgencyHint = true;
	});
      */
      _lastWindow = _windows[title];
      return _windows[title];
    }
  }

  public static Graphics.WindowClass getWindow() {
    if (_lastWindow != null) {
      return _lastWindow;
    } else {
      throw new Exception("no windows exist yet");
    }
  }
  
  public static Graphics.WindowClass getWindow(string title) {
    if (_windows.ContainsKey(title)) {
      return _windows[title];
    } else {
      return null;
    }
  }

  public static Graphics.WindowClass Window(string title="Calico Graphics Window",
				       int width=300, 
				       int height=300) {
    return makeWindow(title, width, height);
  }

  public static Color makeColor(string color) {
    return new Color(color);
  }

  public static Color makeColor(int r, int g, int b) {
    return new Color(r, g, b);
  }

  public static Color makeColor(int r, int g, int b, int a) {
    return new Color(r, g, b, a);
  }

  public static Color makeColor(double r, double g, double b) {
    return new Color(r, g, b);
  }

  public static Color makeColor(double r, double g, double b, double a) {
    return new Color(r, g, b, a);
  }

  public class Color {
    internal Cairo.Color _cairo;
    public ICanvas window;  // for setting color of a Window()
    public Picture picture; // for setting color of Picture()
    public int x = -1; // for use with picture, above
    public int y = -1; // for use with picture, above

    public Cairo.Color getCairo() {
      return _cairo;
    }
    
    public Color(string name) {
      if (colors.ContainsKey(name.ToLower())) {
	_cairo = colors[name.ToLower()]._cairo;
      } else {
	throw new Exception(String.Format("unknown colorname '{0}'", name));
      }
    }

    public Color(Color color) {
      _cairo = new Cairo.Color(color._cairo.R, color._cairo.G, color._cairo.B, color._cairo.A);
    }

    public Color(int r, int g, int b) {
      _cairo = new Cairo.Color(ToCairo(r), ToCairo(g), ToCairo(b), 1.0);
    }

    public Color(int r, int g, int b, int a) {
      _cairo = new Cairo.Color(ToCairo(r), ToCairo(g), ToCairo(b), ToCairo(a));
    }

    public Color(double r, double g, double b) {
      _cairo = new Cairo.Color(ToCairo(r), ToCairo(g), ToCairo(b), 1.0);
    }

    public Color(double r, double g, double b, double a) {
      _cairo = new Cairo.Color(ToCairo(r), ToCairo(g), ToCairo(b), ToCairo(a));
    }

    public Color Copy() {
      return new Color(red, green, blue, alpha);
    }

    public void QueueDraw() { // color
      if (window is ICanvas) {
	if (window.getMode() == "auto")
	  window.update();
	// else, manually call step()
      } 
    }
    
    public int red {
      get {
	return FromCairo(_cairo.R);
      }
      set {
	_cairo.R = ToCairo(value);
	if (picture is Picture) {
	  if (x >= 0 && y >= 0)
	    picture.setRed(x, y, (byte)value);
	  else {
            foreach (Pixel pixel in picture.getPixels()) {
	      pixel.setRed((byte)value);
            }
	  }
	} else
	    QueueDraw();
      }
    }

    public int green {
      get {
	return FromCairo(_cairo.G);
      }
      set {
	_cairo.G = ToCairo(value);
	if (picture is Picture) {
	  if (x >= 0 && y >= 0)
            picture.setGreen(x, y, (byte)value);
	  else {
            foreach (Pixel pixel in picture.getPixels()) {
	      pixel.setGreen((byte)value);
            }
	  }
	} else
	    QueueDraw();
      }
    }

    public int blue {
      get {
	return FromCairo(_cairo.B);
      }
      set {
	_cairo.B = ToCairo(value);
	if (picture is Picture) {
	  if (x >= 0 && y >= 0)
            picture.setBlue(x, y, (byte)value);
	  else {
            foreach (Pixel pixel in picture.getPixels()) {
	      pixel.setBlue((byte)value);
            }
	  }
	} else
	    QueueDraw();
      }
    }

    public int alpha {
      get {
	return FromCairo(_cairo.A);
      }
      set {
	_cairo.A = ToCairo(value);
	if (picture is Picture) {
	  if (x >= 0 && y >= 0)
            picture.setAlpha(x, y, (byte)value);
	  else {
            foreach (Pixel pixel in picture.getPixels()) {
	      pixel.setAlpha((byte)value);
            }
	  }
	} else
	    QueueDraw();
      }
    }

    double ToCairo(int value) {
      return Math.Max(Math.Min((value / 255.0), 1.0), 0.0);
    }
    
    double ToCairo(double value) {
      return Math.Max(Math.Min((value / 255.0), 1.0), 0.0);
    }
    
    int FromCairo(double value) {
      return (int)(value * 255);
    }
    
    public override string ToString() {
      return String.Format("<Color (r={0},g={1},b={2},a={3})>", 
			   red, green, blue, alpha);
    }
    
  }

  public interface ICanvas {
    // Interface for allowing drawing on Windows and Canvases.
    // Canvases are for off-screen drawing.
    _Canvas getCanvas();
    //int getWidth();
    //int getHeight();
    void stackOnTop(Shape shape);
    void stackOnBottom(Shape shape);
    string getMode();
    Gdk.Drawable getDrawable();
    void update();
  }

  public class Canvas : ICanvas {
    public int canvas_width = 0;
    public int canvas_height = 0;
    internal _Canvas _canvas;

    public Canvas(int width=300, 
		  int height=300) {
      canvas_width = width;
      canvas_height = height;
      _canvas = new _Canvas("auto");
    }

    public Gdk.Drawable getDrawable() {
      // canvas is a drawingarea
      return _canvas.GdkWindow; // FIXME: get a Drawable from DrawingArea
    }
    public _Canvas getCanvas() {
      return _canvas;
    }
    public string getMode() {
      return _canvas.mode;
    }
    public void update() { 
    }
    public void stackOnTop(Shape shape) {
    }
    public void stackOnBottom(Shape shape) {
    }
  }

  public class WindowClass : Gtk.Window, ICanvas {
    internal _Canvas _canvas;
    internal bool _dirty = false;
    private bool timer_running = false;
    private DateTime last_update = new DateTime(2000,1,1);
    internal uint _update_interval = 100; // how often, in ms, to auto update
    public List onClickCallbacks = new List();
    public PythonTuple lastClick;
    ManualResetEvent lastClickFlag = new ManualResetEvent(false);
    
    public WindowClass(string title="Calico Graphics Window",
		  int width=300, 
		  int height=300) : base(title) {
      _canvas = new _Canvas("auto");
      AllowGrow = true;
      AllowShrink = true;
      SetDefaultSize(width, height);
      AddEvents((int)Gdk.EventMask.ButtonPressMask);
      ButtonPressEvent += HandleClickCallbacks;
      ButtonPressEvent += saveLastClick;
      DeleteEvent += OnDelete;
      Add(_canvas);
      ShowAll();
    }

    public void close() {
      Gtk.Application.Invoke(delegate { 
	  Hide();
	});
    }

    public void setBackground(Color color) {
      Gtk.Application.Invoke(delegate { 
	  Gdk.Color bg = new Gdk.Color((byte)color.red, 
				       (byte)color.green, 
				       (byte)color.blue);
	  Gdk.Colormap colormap = Gdk.Colormap.System;
	  colormap.AllocColor(ref bg, true, true);
	  _canvas.GdkWindow.Background = bg;
	  QueueDraw();
	});
    }
    
    public Gdk.Drawable getDrawable() {
      return GdkWindow;
    }

    private void OnDelete(object obj, Gtk.DeleteEventArgs args)  {
      _windows.Remove(Title);
    }
    
    public int getWidth() {
      int _width, _height;
      this.GetSize(out _width, out _height);
      return height;
    }
    
    public int getHeight() {
      int _width, _height;
      this.GetSize(out _width, out _height);
      return _width;
    }
    
    public void draw(Shape shape) {
      shape.draw(this);
    }

    public void undraw(Shape shape) {
      shape.undraw();
    }

    public void stackOnTop(Shape shape) {
      // last drawn is on top
      if (_canvas.shapes.Contains(shape)) {
	_canvas.shapes.Remove(shape);
	_canvas.shapes.Insert(_canvas.shapes.Count, shape);
	QueueDraw();
      } else {
	throw new Exception("shape not drawn on window");
      }
    }
    public void stackOnBottom(Shape shape) {
      // first drawn is on bottom
      if (_canvas.shapes.Contains(shape)) {
	_canvas.shapes.Remove(shape);
	_canvas.shapes.Insert(0, shape);
	QueueDraw();
      } else {
	throw new Exception("shape not drawn on window");
      }
    }

    void saveLastClick(object obj, Gtk.ButtonPressEventArgs args) {
      lastClick = PyTuple(args.Event.X, args.Event.Y);
      lastClickFlag.Set();
    }
    
    private void HandleClickCallbacks(object obj,
				      Gtk.ButtonPressEventArgs args) {
      foreach (object function in onClickCallbacks) {
	if (function is PythonFunction) {
	  IronPython.Runtime.Operations.PythonCalls.Call(function, obj, args);
	} else {
	  Func<object,Gtk.ButtonPressEventArgs,object> f = (Func<object,Gtk.ButtonPressEventArgs,object>)function;
	  f(obj, args);
	}
      }
    }
    
    public void onClick(PythonFunction function) {
      onClickCallbacks.Add(function);
    }
    
    public uint updateInterval {
      get {
	return _update_interval;
      }
      set {
	_update_interval = value;
      }
    }
    
    public int height {
      get {
	int _width, _height;
	this.GetSize(out _width, out _height);
	return _height;
      }
    }
    public int width {
      get {
	int _width, _height;
	this.GetSize(out _width, out _height);
	return _width;
      }
    }

    public _Canvas getCanvas() {
      return _canvas;
    }
    
    public _Canvas canvas {
      get {
	return _canvas;
      }
    }
    
    public PythonTuple getMouse() {
      lastClickFlag = new ManualResetEvent(false);
      lastClickFlag.WaitOne();
      return lastClick;
    }
    
    public new void Show() {
      Gtk.Application.Invoke(delegate { 
	  DateTime now = DateTime.Now;
	  last_update = now;
	  _dirty = false;
	  base.Show(); 
	});
    }
    public new void ShowAll() {
      Gtk.Application.Invoke(delegate { 
	  DateTime now = DateTime.Now;
	  last_update = now;
	  _dirty = false;
	  base.ShowAll(); 
	});
    }
    public new void Resize(int width, int height) {
      Gtk.Application.Invoke(delegate {
	  base.Resize(width, height);
	});
    }
    
    public void need_to_redraw() {
      _dirty = true;
      DateTime now = DateTime.Now;
      // diff is TimeSpan
      if ((now - last_update).TotalMilliseconds < updateInterval) {
	// pass, too soon!
	// but we need to make sure that someone checks
	// in the future. 
	if (timer_running) {
	  // already one running!
	  // we'll just wait
	} else {
	  // let's spawn one to check in 100 ms or so
	  timer_running = true;
	  GLib.Timeout.Add(updateInterval, 
			   new GLib.TimeoutHandler(_redraw_now) );
	}
      } else { // it is not too soon
	if (timer_running) {
	  // no need to start another
	} else {
	  // let's spawn one to check in 100 ms or so
	  timer_running = true;
	  GLib.Timeout.Add(updateInterval, 
			   new GLib.TimeoutHandler(_redraw_now) );
	}
      }
    }
    
    private bool _redraw_now() {
      DateTime now = DateTime.Now;
      if (_dirty) {
	last_update = now;
	_dirty = false;
	QueueDraw(); // gtk
      }
      timer_running = false;
      return false; // return true to try again
    }

    public string getMode() {
      return _canvas.mode;
    }

    public string mode {
      get {
	return _canvas.mode;
      }
      set {
	if (value == "auto" || value == "manual")
	  _canvas.mode = value;
	else
	  throw new Exception("window mode must be 'auto' or 'manual'");
      }
    }	  

    public void update() { // Window
      need_to_redraw();
    }

    public void step() { // Window
      step(0);
    }
    public void step(int step_time) { // Window
      // Same as update, but will make sure it 
      // doesn't update too fast.
      DateTime now = DateTime.Now;
      // diff is TimeSpan
      double diff = (now - last_update).TotalMilliseconds;
      if (diff < step_time) {
	Thread.Sleep((int)(step_time - diff));
      }
      last_update = DateTime.Now;
      _dirty = false;
      ManualResetEvent ev = new ManualResetEvent(false);
      Gtk.Application.Invoke(delegate { 
	  QueueDraw();
	  GdkWindow.ProcessUpdates(true);
	  ev.Set();
	});
      ev.WaitOne();
    }

    public override string ToString()
    {
      return String.Format("<Window (title='{0}',width={1},height={2})>", 
			   Title, width, height);
    }
  }
  
 public static void ShowAll(object o) {
    Gtk.Application.Invoke(delegate { ((Gtk.Widget)o).ShowAll(); });
  }
  
  public static void Show(object o) {
    Gtk.Application.Invoke(delegate { ((Gtk.Widget)o).Show(); });
  }
  
  public class Point {
    // FIXME: should call QueueDraw on change of x,y
    public double x;
    public double y;
    public Point(object x, object y) {
      if (x is int) 
	this.x = (int)x;
      else if (x is float) 
	this.x = (float)x;
      else if (x is double)
	this.x = (double)x;
      else
	throw new Exception("Point: cannot convert x to a number");
      if (y is int) 
	this.y = (int)y;
      else if (y is float) 
	this.y = (float)y;
      else if (y is double)
	this.y = (double)y;
      else
	throw new Exception("Point: cannot convert y to a number");
    }
    public Point(int x, int y) {
      this.x = (double)x;
      this.y = (double)y;
    }
    public Point(float x, float y) {
      this.x = (double)x;
      this.y = (double)y;
    }
    public Point(double x, double y) {
      this.x = x;
      this.y = y;
    }
    
    public override string ToString()
    {
      return String.Format("<Point (x={0},y={1})>", x, y);
    }

    public void draw(ICanvas canvas) {
      throw new Exception("Can't draw a point; use Dot instead");
    }

  }
  
  public class _Canvas : Gtk.DrawingArea {
    
    // Shape.draw() will add them here:
    public List<Shape> shapes = new List<Shape>();
    private string _mode;
    
    public string mode {
	  get {
	    return _mode;
	  }
	  set {
		if (value == "manual" || value == "auto")
		  _mode = value;
		else
		  throw new Exception("canvas mode must be 'manual' or 'auto'");
	  }
    }
	
    public _Canvas(string mode) : base() {
	  this.mode = mode;
    }
	
    protected override bool OnExposeEvent (Gdk.EventExpose args) {
	  using (Cairo.Context g = Gdk.CairoHelper.Create(args.Window)) {
	    // clip to the visible part
		g.Rectangle(args.Area.X, args.Area.Y,
			    args.Area.Width, args.Area.Height);
		g.Clip();
		try {
		  foreach (Shape shape in shapes) {
		    shape.render(g);
		  }
		} catch {
		  // updating the window while someone changed the shapes
		  // list.
		}
	  }
	  return true;
    }
  } 
  
  public class Shape {
    public Point center;
    public ICanvas window;
    internal double _rotation; // radians
    internal double _scaleFactor; // percent
    
    public Point [] points;
    // FIXME: when done debugging
    //internal Cairo.Color? _fill;
    //internal Cairo.Color? _outline;
    internal Color _fill;
    internal Color _outline;
    private int _border;
    
    private Pen _pen;
    private bool _has_pen;
	
    public Shape(bool has_pen=true) {
      _rotation = 0;
      _scaleFactor = 1.0;
      center = new Point(0,0);
      this.has_pen = has_pen;
      if (this.has_pen) 
	pen = new Pen(new Color(0, 0, 0), 1);
      color = new Color("gray");
      outline = new Color("black");
      border = 3;
    }
    
    // FIXME: points are in relative to center coordinates
    // FIXME: set x,y of points should go from screen_coords to relative
    // FIXME: should call QueueDraw on set

    public void stackOnTop() {
      if (window != null) {
	window.stackOnTop(this);
      }
    }

    public void stackOnBottom() {
      if (window != null) {
	window.stackOnBottom(this);
      }
    }

    public Point getP1()
    {
      return points[0];
    }
    public Point getP2()
    {
      return points[1];
    }

    public Point getCenter()
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
	return _rotation;
      }
      set {
	rotateTo(value);
      }
    }

    public double scaleFactor {
      get {
	return _scaleFactor;
      }
      set {
	scaleTo(value);
      }
    }

    public double x {
      get {
	return center.x;
      }
      set {
	moveTo(value, center.y);
      }
    }
	
    public double y {
      get {
	return center.y;
      }
      set {
	moveTo(center.x, value);
      }
    }
	
    public int border {
      get {
	return _border;
      }
      set {
	_border = value;
	QueueDraw();
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
	  QueueDraw();
	} else
	    throw new Exception("this shape cannot have a pen");
      }
    }
    
    public void QueueDraw() { // shape
      if (window is ICanvas) {
	if (window.getMode() == "auto")
	  window.update();
	// else, manually call step()
      }
    }
    
    public bool hit(IList iterable) {
      return hit(new Point(iterable[0], iterable[1]));
    }

    public bool hit(Point p) {
	  int counter = 0;
	  double xinters;
	  Point p1, p2;
	  if (points != null) {
		p1 = points[0];
		for (int i=1; i<=points.Length; i++) {
		  p2 = points[i % points.Length];
		  if (p.y > Math.Min(p1.y, p2.y)) {
			if (p.y <= Math.Max(p1.y, p2.y)) {
			  if (p.x <= Math.Max(p1.x, p2.x)) {
				if (p1.y != p2.y) {
				  xinters = (p.y - p1.y) * (p2.x - p1.x)/(p2.y - p1.y) + p1.x;
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

    public void set_points(params IList [] iterables) {
      // 1. set center to absolute
      double sumx = 0.0;
      double sumy = 0.0;
      if (points.Length > 0) {
	for (int i = 0; i < iterables.Length; i++) {
	  sumx += (double)iterables[i][0];
	  sumy += (double)iterables[i][1];
	}
	center.x = sumx/iterables.Length;
	center.y = sumy/iterables.Length;
      } else {
	center.x = 0;
	center.y = 0;
      }
      // 2. compute this.points in relative terms to center
      points = new Point [iterables.Length];
      for (int i = 0; i < iterables.Length; i++) {
	points[i] = new Point((double)iterables[i][0] - center.x, 
			      (double)iterables[i][1] - center.y);
      }
    }
    
    public void set_points(params Point [] new_points) {
      // 1. set center to absolute
      double sumx = 0.0;
      double sumy = 0.0;
      if (new_points.Length > 0) {
	for (int i = 0; i < new_points.Length; i++) {
	  sumx += new_points[i].x;
	  sumy += new_points[i].y;
	}
	center.x = sumx/new_points.Length;
	center.y = sumy/new_points.Length;
      } else {
	center.x = 0;
	center.y = 0;
      }
      // 2. compute this.points in relative terms to center
      points = new Point [new_points.Length];
      for (int i = 0; i < new_points.Length; i++) {
	points[i] = new Point(new_points[i].x - center.x, 
			      new_points[i].y - center.y);
      }
    }
    
    public double screen_angle(double dir) {
      // Screen coords are 45 degrees from system
      return dir - (45 * Math.PI/180.0);
    }

    public void forward(double distance) {
      double angle = screen_angle(_rotation);
      double x = ((distance) * Math.Cos(angle) - (distance) * Math.Sin(angle));
      double y = ((distance) * Math.Sin(angle) + (distance) * Math.Cos(angle));
      center.x += x;
      center.y += y;
      if (has_pen && pen.down)
	pen.append_path(new Point(center.x, center.y));
      QueueDraw();
    }
    
    public Polygon penUp() {
      pen.down = false;
      return new Polygon(pen.path.ToArray()); //, new Color(0,0,0));
    }
    
    public void penDown() {
      pen.down = true;
      pen.reset_path();
      pen.append_path(new Point(center.x, center.y));
    }
    
    public void backward(double distance) {
      forward(-distance);
    }
    
    public virtual void render(Cairo.Context g) {
      g.Save();
      Point temp;
      if (points != null) {
	g.LineWidth = border;
	temp = screen_coord(center);
	g.Translate(temp.x, temp.y);
	g.Rotate(_rotation);
	g.Scale(_scaleFactor, _scaleFactor);
	temp = screen_coord(points[0]);
	g.MoveTo(temp.x, temp.y);
	for (int p = 1; p < points.Length; p++) {
	  temp = screen_coord(points[p]);
	  g.LineTo(temp.x, temp.y);
	}
	g.ClosePath();
	if (_fill != null) {
	  g.Color = _fill.getCairo();
	  g.FillPreserve();
	}
	if (_outline != null) {
	  g.Color = _outline.getCairo();
	  g.Stroke();
	}
	if (has_pen)
	  pen.render(g);
      }
      g.Restore();
    }
    
    public Point screen_coord(Point point) {
      // FIXME: return in coords of screen
      return point; // new Point(point.x - center.x, point.y - center.y);
    }
    
    public void move(double dx, double dy) {
      if (has_pen && pen.down)
	pen.append_path(new Point(center.x + dx, center.y + dy));
      center.x += dx;
      center.y += dy;
      QueueDraw();
    }

    public void moveTo(double x, double y) {
      double dx = x - center.x;
      double dy = y - center.y;
      move(dx, dy);
    }
        
    public void rotate(double degrees) {
      _rotation -= (Math.PI / 180.0) * degrees;
      QueueDraw();
    }
    
    public void rotateTo(double degrees) {
      _rotation = degrees * (Math.PI) / 180.0;
      QueueDraw();
    }
    
    public void scale(double percent) {
      _scaleFactor *= percent;
      QueueDraw();
    }
	
    public void scaleTo(double percent) {
      _scaleFactor = percent;
      QueueDraw();
    }
    
    public void update() { // Shape
      // Alias to QueueDraw
      QueueDraw(); 
    }
    
    public void draw(ICanvas can) {
      // Add this shape to the _Canvas list.
      can.getCanvas().shapes.Add(this);
      // FIXME: QueueDrawRect
      // FIXME: invalidate from and to rects on move
      window = can;
      QueueDraw();
    }
    
    public void undraw() {
      Gtk.Application.Invoke(delegate { 
	  window.getCanvas().shapes.Remove(this);
	  if (window is WindowClass)
	    ((WindowClass)window).QueueDraw();
	  window = null;
	});
    }

    public Color color {
      set {
	    if (value != null) {
	        _fill = ((Color)value).Copy();
	        _outline = _fill;
	    } else {
	        _fill = null;
	        _outline = null;
	    }
	    QueueDraw();
      }
      get {
	    if (_fill != null) {
	      _fill.window = this.window;
	      return _fill; // share!
	    } 
	  else
	    return null;
	}
    }
    
    public virtual Color fill {
      set {
	if (value == null) {
	  _fill = null;
	} else {
	  _fill = ((Color)value).Copy();
	}
	QueueDraw();
      }
      get
	{
	  if (_fill != null)
	    {
	      _fill.window = this.window;
	      return _fill;
	    } 
	  else
	    return null;
	}
    }
    
    public Color outline {
      set {
	if (value == null) {
	  _outline = null;
	} else {
	  _outline = ((Color)value).Copy();
	}
	QueueDraw();
      }
      get
	{
	  if (_outline != null)
	    {
	      _outline.window = this.window;
	      return _outline; // share!
	    } 
	  else
	    return null;
	}
    }
  }
  
  public class Text : Shape {
    public string text;
    public string fontFace = "Georgia";
    public Cairo.FontWeight fontWeight = Cairo.FontWeight.Normal;
    public Cairo.FontSlant fontSlant = Cairo.FontSlant.Normal;
    public double size = 18;
    public string xJustification = "left"; // left, center, right
    public string yJustification = "bottom"; // top, center, bottom

    public Text(Point point, string text) {
      this.text = text;
      set_points(point);
    }

    public override void render(Cairo.Context g) {
      g.Save();
      Point temp = screen_coord(center);
      g.Translate(temp.x, temp.y);
      g.Rotate(_rotation);
      g.Scale(_scaleFactor, _scaleFactor);
      if (_fill != null)
	g.Color = _fill._cairo;
      else
	g.Color = new Cairo.Color(0,0,0); // default color when none given
      g.SelectFontFace(fontFace, fontSlant, fontWeight);
      g.SetFontSize(size);
      Cairo.TextExtents te = g.TextExtents(text);
      Point p = new Point(0,0);
      if (xJustification == "center") {
	p.x = points[0].x - te.Width  / 2 - te.XBearing;
      } else if (xJustification == "left") {
	p.x = points[0].x;
      } else if (xJustification == "right") {
	p.x = points[0].x + te.Width;
      }
      if (yJustification == "center") {
	p.y = points[0].y - te.Height / 2 - te.YBearing;
      } else if (yJustification == "bottom") {
	p.y = points[0].y;
      } else if (yJustification == "top") {
	p.y = points[0].y - te.Height;
      }
      temp = screen_coord(p);
      g.MoveTo(temp.x, temp.y);
      // FIXME: how to rotate text?
      // Possible hints: see TextPath, GlyphPath, ShowGlyph
      g.ShowText(text);    
      g.Restore();
    }
  }

  public class Line : Shape {
	public Line(IList iterable1, IList iterable2) :
	this(new Point(iterable1[0], iterable1[1]), 
	     new Point(iterable2[0], iterable2[1])) {
	}
    public Line(Point p1, Point p2) : this(true, p1, p2) {
    }
    public Line(bool has_pen, IList iterable1, IList iterable2) : 
              this(has_pen, 
		   new Point(iterable1[0], iterable1[1]), 
		   new Point(iterable2[0], iterable2[1])) {
    }
    public Line(bool has_pen, Point p1, Point p2) : base(has_pen) {
      set_points(p1, p2);
    }
  }

  public class Curve : Shape {
    public Curve(IList iterable0, 
		 IList iterable1, 
		 IList iterable2, 
		 IList iterable3) :
    this(new Point(iterable0[0], iterable0[1]), 
	 new Point(iterable1[0], iterable1[1]), 
	 new Point(iterable2[0], iterable2[1]),
	 new Point(iterable3[0], iterable3[1])
	 ) {
    }
    public Curve(Point p0, Point p1, Point p2, Point p3) : 
    this(true, p0, p1, p2, p3) {
    }
    public Curve(bool has_pen, 
		 IList iterable0, 
		 IList iterable1, 
		 IList iterable2, 
		 IList iterable3) : 
    this(has_pen, 
	 new Point(iterable0[0], iterable0[1]), 
	 new Point(iterable1[0], iterable1[1]), 
	 new Point(iterable2[0], iterable2[1]),
	 new Point(iterable3[0], iterable3[1])
	 ) {
    }
    public Curve(bool has_pen, Point p0, Point p1, Point p2, Point p3) : 
    base(has_pen) {
      set_points(p0, p1, p2, p3);
      fill = null;
    }    

    public override void render(Cairo.Context g) {
      g.Save();
      Point temp, p1, p2, p3;
      if (points != null) {
	g.LineWidth = border;
	temp = screen_coord(center);
	g.Translate(temp.x, temp.y);
	g.Rotate(_rotation);
	g.Scale(_scaleFactor, _scaleFactor);
	temp = screen_coord(points[0]);
	g.MoveTo(temp.x, temp.y);
	for (int p = 1; p < points.Length; p += 3) {
	  p1 = screen_coord(points[p]);
	  p2 = screen_coord(points[p+1]);
	  p3 = screen_coord(points[p+2]);
	  g.CurveTo(p1.x, p1.y, p2.x, p2.y, p3.x, p3.y);
	}
	if (_fill != null) {
	  g.Color = _fill._cairo;
	  g.FillPreserve();
	}
	if (_outline != null) {
	  g.Color = _outline._cairo;
	  g.Stroke();
	}
	if (has_pen)
	  pen.render(g);
      }
      g.Restore();
    }
  }
  
  public class Arrow : Shape {
	public Arrow(IList iterable) :
		this(new Point(iterable[0], iterable[1])) {
	}
	public Arrow(Point new_center) : this(new_center, 0) {
	}

	public Arrow(IList iterable, double degrees) :
		this(new Point(iterable[0], iterable[1]), degrees) {
	}

	public Arrow(Point new_center, double degrees) : base(true) {
	  set_points(
		  new Point(  0,  0),
		  new Point( -5, -5), 
		  new Point(  5,  0),
		  new Point( -5,  5) 
				 );
	  center.x = new_center.x;
	  center.y = new_center.y;
	  rotate(degrees);
	}
  }
  
  public class Pen : Shape {
    private List<Point> _path; // = new List<Point>();
	public bool _down;
    
    public Pen(Color color, int border) : base(false) {
      down = false;
      this.color = color;
      this.border = border;
    }
    
    public Pen(int border) : base(false) {
      down = false;
      this.color = new Color(0, 0, 0);
      this.border = border;
    }
    
    public void reset_path() {
	  _path = new List<Point>();
    }

    public void append_path(IList iterable) {
	  append_path(new Point(iterable[0], iterable[1]));
	}
	
    public void append_path(Point point) {
	  _path.Add(point);
    }
	
    public bool down {
	  get {
	    return _down;
	  }
	  set {
	    _down = value;
	  }
	}
    
	public List<Point> path {
	  get {
		return _path;
	  }
	}
    
	public override void render(Cairo.Context g) {
	  // render path
	  g.Save();
	  Point temp = screen_coord(center);
	  g.Translate(temp.x, temp.y);
	  g.Rotate(_rotation);
	  g.Scale(_scaleFactor, _scaleFactor);
	  if (path != null) {
	    g.LineWidth = border;
	    temp = screen_coord(path[0]);
	    g.MoveTo(temp.x, temp.y);
	    for (int p = 1; p < path.Count; p++) {
	      temp = screen_coord(path[p]);
	      g.LineTo(temp.x, temp.y);
	    }
	    g.ClosePath();
	    if (_fill != null) {
	      g.Color = _fill._cairo;
	      g.FillPreserve();
	    }
	    if (_outline != null) {
	      g.Color = _outline._cairo;
	      g.Stroke();
	    }
	  }
	  g.Restore();
	}
  }

  public class Pixel {
    private Picture picture;
    public int x;
    public int y;
    
    public Pixel(Picture picture, int x, int y) {
      this.picture = picture;
      this.x = x;
      this.y = y;
    }
    
    public Color getColor() {
      return picture.getColor(x, y);
    }
    public PythonTuple getRGB() {
      return picture.getRGB(x, y);
    }
    public PythonTuple getRGBA() {
      return picture.getRGBA(x, y);
    }
    public int getGray() {
      return picture.getGray(x, y);
    }
    public int getRed() {
      return picture.getRed(x, y);
    }
    public int getGreen() {
      return picture.getGreen(x, y);
    }
    public int getBlue() {
      return picture.getBlue(x, y);
    }
    public int getAlpha() {
      return picture.getAlpha(x, y);
    }
    public void setRGB(byte red, byte green, byte blue) {
      picture.setRGB(x, y, red, green, blue);
    }
    public void setRGBA(byte red, byte green, byte blue, byte alpha) {
      picture.setRGBA(x, y, red, green, blue, alpha);
    }
    public void setGray(byte value) {
      picture.setGray(x, y, value);
    }
    public void setRed(byte value) {
      picture.setRed(x, y, value);
    }
    public void setGreen(byte value) {
      picture.setGreen(x, y, value);
    }
    public void setBlue(byte value) {
      picture.setBlue(x, y, value);
    }
    public void setAlpha(byte value) {
      picture.setAlpha(x, y, value);
    }
  }
  
  public class Picture : Shape {
    Gdk.Pixbuf _pixbuf; // in memory rep of picture
    Cairo.Format format = Cairo.Format.Rgb24;
    public Cairo.Surface surface;
    public Cairo.Context context;
    
    public Picture(string filename) : this(true) {
      if (filename.StartsWith("http://")) {
	HttpWebRequest req = (HttpWebRequest) WebRequest.Create(filename);
	req.KeepAlive = false;
	req.Timeout = 10000;	
	WebResponse resp = req.GetResponse();
	Stream s = resp.GetResponseStream();
	_pixbuf = new Gdk.Pixbuf(s);
      } else {
	_pixbuf = new Gdk.Pixbuf(filename);
      }
      if (!_pixbuf.HasAlpha) {
        _pixbuf = _pixbuf.AddAlpha(true, 0, 0, 0); // alpha color?
      }
      format = Cairo.Format.Argb32;
      // Create a new ImageSurface
      surface = new Cairo.ImageSurface(format, _pixbuf.Width, _pixbuf.Height);
      set_points(new Point(0, 0), 
		 new Point(_pixbuf.Width, 0),
		 new Point(_pixbuf.Width, _pixbuf.Height), 
		 new Point(0, _pixbuf.Height));
    }

    public Picture(bool has_pen) : base(has_pen) {
        this._fill.picture = this;
    }

    public Picture(Picture original) : this(true) {
      // Colorspace, has_alpha, bits_per_sample, width, height:
      _pixbuf = new Gdk.Pixbuf(new Gdk.Colorspace(), true, 8, original.getWidth(), original.getHeight());
      if (!_pixbuf.HasAlpha) {
        _pixbuf = _pixbuf.AddAlpha(true, 0, 0, 0); // alpha color?
      }
      for (int x=0; x < _pixbuf.Width; x++) {
        for (int y=0; y < _pixbuf.Height; y++) {
          byte r = (byte)original.getRed(x, y);
          byte g = (byte)original.getGreen(x, y);
          byte b = (byte)original.getBlue(x, y);
          byte a = (byte)original.getAlpha(x, y);
          Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
              x * _pixbuf.NChannels + 0, r);
          Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
              x * _pixbuf.NChannels + 1, g);
          Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
              x * _pixbuf.NChannels + 2, b);
          Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
              x * _pixbuf.NChannels + 3, a);
        }
      }
      format = Cairo.Format.Argb32;
      // Create a new ImageSurface
      surface = new Cairo.ImageSurface(format, _pixbuf.Width, _pixbuf.Height);
      set_points(original.points);
    }
    
    public Picture(Gdk.Pixbuf pixbuf) : this(true) {
      _pixbuf = pixbuf;
      if (!_pixbuf.HasAlpha) {
	_pixbuf = _pixbuf.AddAlpha(true, 0, 0, 0); // alpha color?
      }
      format = Cairo.Format.Argb32;
      // Create a new ImageSurface
      surface = new Cairo.ImageSurface(format, _pixbuf.Width, _pixbuf.Height);
      set_points(new Point(0, 0), 
		 new Point(_pixbuf.Width, 0),
		 new Point(_pixbuf.Width, _pixbuf.Height), 
		 new Point(0, _pixbuf.Height));
    }

    public Picture(System.Drawing.Bitmap bitmap, int width, int height) : this(true) {
      // Colorspace, has_alpha, bits_per_sample, width, height:
      _pixbuf = new Gdk.Pixbuf(new Gdk.Colorspace(), true, 8, width, height);
      if (!_pixbuf.HasAlpha) {
	_pixbuf = _pixbuf.AddAlpha(true, 0, 0, 0); // alpha color?
      }
      for (int x=0; x < _pixbuf.Width; x += 2) {
	for (int y=0; y < _pixbuf.Height; y++) {
	  System.Drawing.Color pixel = bitmap.GetPixel(x/2, y);
	  // First pixel
	  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
			    x * _pixbuf.NChannels + 0, pixel.R);
	  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
			    x * _pixbuf.NChannels + 1, pixel.G);
	  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
			    x * _pixbuf.NChannels + 2, pixel.B);
	  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
			    x * _pixbuf.NChannels + 3, pixel.A);
	  // Second pixel
	  int x2 = x + 1;
	  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
			    x2 * _pixbuf.NChannels + 0, pixel.R);
	  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
			    x2 * _pixbuf.NChannels + 1, pixel.G);
	  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
			    x2 * _pixbuf.NChannels + 2, pixel.B);
	  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
			    x2 * _pixbuf.NChannels + 3, pixel.A);
	}
      }
      format = Cairo.Format.Argb32;
      // Create a new ImageSurface
      surface = new Cairo.ImageSurface(format, _pixbuf.Width, _pixbuf.Height);
      set_points(new Point(0, 0), 
		 new Point(_pixbuf.Width, 0),
		 new Point(_pixbuf.Width, _pixbuf.Height), 
		 new Point(0, _pixbuf.Height));
    }
    
    public Picture(int width, int height, byte [] buffer, int depth) : this(true) {
      // depth should be 1
	  // Colorspace, has_alpha, bits_per_sample, width, height:
      _pixbuf = new Gdk.Pixbuf(new Gdk.Colorspace(), true, 8, width, height);
	  if (!_pixbuf.HasAlpha) {
	    _pixbuf = _pixbuf.AddAlpha(true, 0, 0, 0); // alpha color?
	  }
	  for (int x=0; x < _pixbuf.Width; x++) {
		for (int y=0; y < _pixbuf.Height; y++) {
		  byte r = buffer[(y * width + x)];
		  byte g = r;
		  byte b = r;
		  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
				    x * _pixbuf.NChannels + 0, r);
		  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
				    x * _pixbuf.NChannels + 1, g);
		  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
				    x * _pixbuf.NChannels + 2, b);
		  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
				    x * _pixbuf.NChannels + 3, 255);
		}
	  }
	  format = Cairo.Format.Argb32;
	  // Create a new ImageSurface
	  surface = new Cairo.ImageSurface(format, _pixbuf.Width, _pixbuf.Height);
	  set_points(new Point(0, 0), 
		     new Point(_pixbuf.Width, 0),
		     new Point(_pixbuf.Width, _pixbuf.Height), 
		     new Point(0, _pixbuf.Height));
    }

    public Picture(int width, int height, byte [] buffer) : this(true) {
	  // Colorspace, has_alpha, bits_per_sample, width, height:
      _pixbuf = new Gdk.Pixbuf(new Gdk.Colorspace(), true, 8, width, height);
	  if (!_pixbuf.HasAlpha) {
	    _pixbuf = _pixbuf.AddAlpha(true, 0, 0, 0); // alpha color?
	  }
	  for (int x=0; x < _pixbuf.Width; x++) {
		for (int y=0; y < _pixbuf.Height; y++) {
		  byte r = buffer[(y * width + x) * 3 + 0];
		  byte g = buffer[(y * width + x) * 3 + 1];
		  byte b = buffer[(y * width + x) * 3 + 2];
		  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
				    x * _pixbuf.NChannels + 0, r);
		  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
				    x * _pixbuf.NChannels + 1, g);
		  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
				    x * _pixbuf.NChannels + 2, b);
		  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
				    x * _pixbuf.NChannels + 3, 255);
		}
	  }
	  format = Cairo.Format.Argb32;
	  // Create a new ImageSurface
	  surface = new Cairo.ImageSurface(format, _pixbuf.Width, _pixbuf.Height);
	  set_points(new Point(0, 0), 
		     new Point(_pixbuf.Width, 0),
		     new Point(_pixbuf.Width, _pixbuf.Height), 
		     new Point(0, _pixbuf.Height));
    }

    public Picture(int width, int height) : this(true) {
	  // Colorspace, has_alpha, bits_per_sample, width, height:
      _pixbuf = new Gdk.Pixbuf(new Gdk.Colorspace(), true, 8, width, height);
	  if (!_pixbuf.HasAlpha) {
	    _pixbuf = _pixbuf.AddAlpha(true, 0, 0, 0); // alpha color?
	  }
	  // WORKAROUND: image needs alpha set to zero (full opacity/no
	  // transparency). Might as well set default color, too:
	  for (int x=0; x < _pixbuf.Width; x++) {
		for (int y=0; y < _pixbuf.Height; y++) {
		  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
			  x * _pixbuf.NChannels + 0, 255);
		  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
			  x * _pixbuf.NChannels + 1, 255);
		  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
			  x * _pixbuf.NChannels + 2, 255);
		  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
			  x * _pixbuf.NChannels + 3, 255);
		}
	  }
	  format = Cairo.Format.Argb32;
	  // Create a new ImageSurface
	  surface = new Cairo.ImageSurface(format, _pixbuf.Width, _pixbuf.Height);
	  set_points(new Point(0, 0), 
		     new Point(_pixbuf.Width, 0),
		     new Point(_pixbuf.Width, _pixbuf.Height), 
		     new Point(0, _pixbuf.Height));
    }

    public Picture(int width, int height, Color color) : this(true) {
	  // Colorspace, has_alpha, bits_per_sample, width, height:
      _pixbuf = new Gdk.Pixbuf(new Gdk.Colorspace(), true, 8, width, height);
      if (!_pixbuf.HasAlpha) {
	_pixbuf = _pixbuf.AddAlpha(true, 0, 0, 0); // alpha color?
      }
      // WORKAROUND: image needs alpha set to zero (full opacity/no
      // transparency). Might as well set default color, too:
      for (int x=0; x < _pixbuf.Width; x++) {
	for (int y=0; y < _pixbuf.Height; y++) {
	  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
			    x * _pixbuf.NChannels + 0, (byte)color.red);
	  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
			    x * _pixbuf.NChannels + 1, (byte)color.green);
	  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
			    x * _pixbuf.NChannels + 2, (byte)color.blue);
	  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
			    x * _pixbuf.NChannels + 3, (byte)color.alpha);
	}
      }
      format = Cairo.Format.Argb32;
      // Create a new ImageSurface
      surface = new Cairo.ImageSurface(format, _pixbuf.Width, _pixbuf.Height);
      set_points(new Point(0, 0), 
		 new Point(_pixbuf.Width, 0),
		 new Point(_pixbuf.Width, _pixbuf.Height), 
		 new Point(0, _pixbuf.Height));
    }
    
    public Gdk.Pixbuf getPixbuf()
    {
      return _pixbuf;
    }

    public int getWidth() {
        return _pixbuf.Width;
    }

    public int getHeight() {
        return _pixbuf.Height;
    }

    public void savePicture(string filename) {
      // png, and jpg
      _pixbuf.Save(filename, filename.Substring(filename.Length - 3, 3));
    }
    
    public Pixel getPixel(int x, int y) {
      return new Pixel(this, x, y);
    }
    
    public void setPixel(int x, int y, Color color) {
      int red = color.red;
      int green = color.green;
      int blue = color.blue;
      this.setRGB(x, y, (byte)red, (byte)green, (byte)blue);
    }

    public IEnumerable getPixels() {
	  for (int x=0; x < width; x++) {
	    for (int y=0; y < height; y++) {
		  yield return getPixel(x, y);
	    }
	  }
    }

    public Color getColor(int x, int y) {
      // red, green, blue, alpha
      Color temp = new Color(getRed(x, y), getGreen(x, y), getBlue(x, y), getAlpha(x, y));
      temp.picture = this;
      temp.x = x;
      temp.y = y;
      return temp;
    }
    
    public PythonTuple getRGB(int x, int y) {
      // red, green, blue, alpha
      return PyTuple(getRed(x, y), getGreen(x, y), getBlue(x, y));
    }
    
	public PythonTuple getRGBA(int x, int y) {
	  // red, green, blue, alpha
	  return PyTuple(getRed(x, y), getGreen(x, y), getBlue(x, y), getAlpha(x, y));
	}

    public int getGray(int x, int y) {
	  // red, green, blue, alpha
      int r = Marshal.ReadByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
		  x * _pixbuf.NChannels + 0);
      int g = Marshal.ReadByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
		  x * _pixbuf.NChannels + 1);
      int b = Marshal.ReadByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
		  x * _pixbuf.NChannels + 2);
      return (int)(((double)(r + g + b))/3.0);
	}
    
	public int getRed(int x, int y) {
	  // red, green, blue, alpha
	  return Marshal.ReadByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
				  x * _pixbuf.NChannels + 0);
	}
    
	public int getGreen(int x, int y) {
	  // red, green, blue, alpha
	  return Marshal.ReadByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
				  x * _pixbuf.NChannels + 1);
	}
    
	public int getBlue(int x, int y) {
	  // red, green, blue, alpha
	  return Marshal.ReadByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
				  x * _pixbuf.NChannels + 2);
	}
    
	public int getAlpha(int x, int y) {
	  // red, green, blue, alpha
	  return Marshal.ReadByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
				  x * _pixbuf.NChannels + 3);
	}
    
    public void setGray(int x, int y, byte value) {
	  // red, green, blue, alpha
      Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
		  x * _pixbuf.NChannels + 0, value);
      Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
		  x * _pixbuf.NChannels + 1, value);
      Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
		  x * _pixbuf.NChannels + 2, value);
      QueueDraw();
	}
    
    public void setRed(int x, int y, byte value) {
      // red, green, blue, alpha
      Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
                x * _pixbuf.NChannels + 0, value);
      QueueDraw();
    }

	public void setGreen(int x, int y, byte value) {
	  // red, green, blue, alpha
	  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
			    x * _pixbuf.NChannels + 1, value);
	  QueueDraw();
	}

    public void setBlue(int x, int y, byte value) {
	  // red, green, blue, alpha
      Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
		  x * _pixbuf.NChannels + 2, value);
      QueueDraw();
	}
    
	public void setAlpha(int x, int y, byte value) {
	  // red, green, blue, alpha
	  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
			    x * _pixbuf.NChannels + 3, value);
	  QueueDraw();
	}

    public void setRGB(int x, int y, byte red, byte green, byte blue) {
	  // red, green, blue, alpha
      Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
		  x * _pixbuf.NChannels + 0, red);
      Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
		  x * _pixbuf.NChannels + 1, green);
      Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
		  x * _pixbuf.NChannels + 2, blue);
      QueueDraw();
	}
    
	public void setRGBA(int x, int y, byte red, byte green, byte blue, 
			    byte alpha) {
	  // red, green, blue, alpha
	  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
		  x * _pixbuf.NChannels + 0, red);
	  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
		  x * _pixbuf.NChannels + 1, green);
	  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
		  x * _pixbuf.NChannels + 2, blue);
	  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
		  x * _pixbuf.NChannels + 3, alpha);
	  QueueDraw();
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

	public override void render(Cairo.Context g) {
	  g.Save();
	  Point temp = screen_coord(center);
	  g.Translate(temp.x, temp.y);
	  g.Rotate(_rotation);
	  g.Scale(_scaleFactor, _scaleFactor);
	  Gdk.CairoHelper.SetSourcePixbuf(g, _pixbuf, -_pixbuf.Width/2, -_pixbuf.Height/2);
	  g.Paint();
	  g.LineWidth = border;
	  g.MoveTo(-_pixbuf.Width/2, -_pixbuf.Height/2);
	  g.LineTo(_pixbuf.Width/2, -_pixbuf.Height/2);
	  g.LineTo(_pixbuf.Width/2, _pixbuf.Height/2);
	  g.LineTo(-_pixbuf.Width/2, _pixbuf.Height/2);
	  g.ClosePath();
	  if (_outline != null) {
	    g.Color = _outline._cairo;
	    g.Stroke();
	  }
	  g.Restore();
	  // FIXME: add pen
	}
    
    public int width {
      get {
	return _pixbuf.Width;
      }
    }
    public int height {
      get {
	return _pixbuf.Height;
      }
    }

    public Pixel [] get_pixels() {
	  return new Pixel[10];
    }

    public override string ToString() {
	  return String.Format("<Picture (width={0}, height={1})>", width, height);
    }

  } // -- end of Picture class

  public class Rectangle : Shape {
	public Rectangle(IList iterable1, IList iterable2) :
		this(new Point(iterable1[0], iterable1[1]), 
			new Point(iterable2[0], iterable2[1])) {
	}
	public Rectangle(Point point1, Point point2) : base(true) {
          set_points(new Point(point1.x, point1.y),
		     new Point(point2.x, point1.y),
		     new Point(point2.x, point2.y),
		     new Point(point1.x, point2.y));
	}
    
  }

  public class Polygon : Shape {
    
    public Polygon(params object [] points) : base(true) {
	  Point [] temp = new Point [points.Length];
	  int count = 0;
	  foreach (object o in points) {
	    if (o is Point)
	      temp[count] = (Point)o ;
	    else if (o is IList) {
	      IList i = (IList)o;
	      temp[count] = new Point(i[0], i[1]);
	    } else {
	      throw new Exception("Polygon: can't convert arg to a point");
	    }
	    count++;
	  }
	  set_points(temp);
	  //FIXME: add color to list
	  //this.color = color;
	}
  }

  public class Dot : Shape {
    public Dot(int x, int y) : base(true) {
      set_points(new Point(x,y));
    }
    public Dot(double x, double y) : base(true) {
      set_points(new Point(x,y));
    }

    public override void render(Cairo.Context g) {
      g.Save();
      Point temp = screen_coord(center);
      g.Translate(temp.x, temp.y);
      g.Rotate(_rotation);
      g.Scale(_scaleFactor, _scaleFactor);
      if (points != null) {
        g.LineWidth = border;
        temp = screen_coord(points[0]);
        g.MoveTo(temp.x, temp.y);
        g.LineTo(temp.x + 1, temp.y + 1);
        g.ClosePath();
        g.Stroke();
        if (has_pen)
          pen.render(g);
      }
      g.Restore();
    }
  }

  public class Circle : Shape {
    int _radius;
    public int radius {
      get {
	return _radius;
      }
      set {
	_radius = value;
	QueueDraw();
      }
    }

    public Circle(IList iterable, int radius) :  this(new Point(iterable[0], iterable[1]), radius) {
    }

    public Circle(Point point, int radius) : base(true) {
      set_points(point);
      _radius = radius;
    }

    public override void render(Cairo.Context g) {
      g.Save();
      // Center is in global screen coords, whatever they are
      Point temp = screen_coord(center);
      // Temp is in Gtk coordinate system
      g.Translate(temp.x, temp.y);
      g.Rotate(_rotation);
      g.Scale(_scaleFactor, _scaleFactor);
      // Now move to 0,0 as origin of shape
      temp = screen_coord(points[0]);
      g.LineWidth = border;
      g.Arc(temp.x, temp.y, radius, 0.0, 2.0 * Math.PI); // x, y, radius, start, end
      g.ClosePath();
      if (_fill != null) {
	g.Color = _fill._cairo;
	g.FillPreserve();
      }
      if (_outline != null) {
	g.Color = _outline._cairo;
	g.Stroke();
      }
      g.Restore();
    }
  }

  public class Oval : Shape {
    int _xRadius;
    int _yRadius;
    
    public int xRadius {
      get {
	return _xRadius;
      }
      set {
	_xRadius = value;
	QueueDraw();
      }
    }
    
    public int yRadius {
      get {
	return _yRadius;
      }
      set {
	_yRadius = value;
	QueueDraw();
      }
    }

    public Oval(IList iterable, int xRadius, int yRadius) :  this(new Point(iterable[0], iterable[1]), xRadius, yRadius) {
    }

    public Oval(Point point, int xRadius, int yRadius) : base(true) {
      set_points(point);
      _xRadius = xRadius;
      _yRadius = yRadius;
    }
    
    public override void render(Cairo.Context g) {
      g.Save();
      // Center is in global screen coords, whatever they are
      Point temp = screen_coord(center);
      // Temp is in Gtk coordinate system
      g.Translate(temp.x, temp.y);
      g.Rotate(_rotation);
      g.Scale(_scaleFactor, _scaleFactor);
      // Now, turn into an Oval:
      g.Scale(1.0, ((double)_yRadius)/((double)_xRadius));
      // Now move to 0,0 as origin of shape
      temp = screen_coord(points[0]);
      g.LineWidth = border;
      g.Arc(temp.x, temp.y, _xRadius, 0.0, 2.0 * Math.PI); // x, y, radius, start, end
      g.ClosePath();
      if (_fill != null) {
	g.Color = _fill._cairo;
	g.FillPreserve();
      }
      if (_outline != null) {
	g.Color = _outline._cairo;
	g.Stroke();
      }
      g.Restore();
    }
  }

  public class Pie : Shape {
    int _radius;
    double _start;
    double _stop;

    public Pie(IList iterable, int radius, double start, double stop) 
    :  this(new Point(iterable[0], iterable[1]), radius, start, stop) {
    }

    public Pie(Point point, int radius, double start, double stop) : base(true) {
      set_points(point);
      _radius = radius;
      _start = start;
      _stop = stop;
    }

    public override void render(Cairo.Context g) {
      g.Save();
      // Center is in global screen coords, whatever they are
      Point temp = screen_coord(center);
      // Temp is in Gtk coordinate system
      g.Translate(temp.x, temp.y);
      g.Rotate(_rotation);
      g.Scale(_scaleFactor, _scaleFactor);
      // Now move to 0,0 as origin of shape
      temp = screen_coord(points[0]);
      g.LineWidth = border;
      double tstart = start * (Math.PI) / 180.0;
      double tstop = stop * (Math.PI) / 180.0;
      g.MoveTo(temp.x, temp.y);
      g.Arc(temp.x, temp.y, radius, tstart, tstop); // x, y, radius, start, end
      g.ClosePath();
      if (_fill != null) {
	g.Color = _fill._cairo;
	g.FillPreserve();
      }
      if (_outline != null) {
	g.Color = _outline._cairo;
	g.Stroke();
      }
      g.Restore();
    }

    public int radius {
      get {
	return _radius;
      }
      set {
	_radius = value;
	QueueDraw();
      }
    }

    public double start {
      get {
	return _start;
      }
      set {
	_start = value;
	QueueDraw();
      }
    }

    public double stop {
      get {
	return _stop;
      }
      set {
	_stop = value;
	QueueDraw();
      }
    }
  }

  public class Arc : Shape {
    int _radius;
    double _start;
    double _stop;

    public Arc(IList iterable, int radius, double start, double stop) 
    :  this(new Point(iterable[0], iterable[1]), radius, start, stop) {
    }

    public Arc(Point point, int radius, double start, double stop) : base(true) {
      set_points(point);
      fill = null;
      _radius = radius;
      _start = start;
      _stop = stop;
    }

    public override void render(Cairo.Context g) {
      g.Save();
      // Center is in global screen coords, whatever they are
      Point temp = screen_coord(center);
      // Temp is in Gtk coordinate system
      g.Translate(temp.x, temp.y);
      g.Rotate(_rotation);
      g.Scale(_scaleFactor, _scaleFactor);
      // Now move to 0,0 as origin of shape
      temp = screen_coord(points[0]);
      g.LineWidth = border;
      double tstart = start * (Math.PI) / 180.0;
      double tstop = stop * (Math.PI) / 180.0;
      g.Arc(temp.x, temp.y, radius, tstart, tstop); // x, y, radius, start, end
      if (_fill != null) {
	g.Color = _fill._cairo;
	g.FillPreserve();
      }
      if (_outline != null) {
	g.Color = _outline._cairo;
	g.Stroke();
      }
      g.Restore();
    }

    public int radius {
      get {
	return _radius;
      }
      set {
	_radius = value;
	QueueDraw();
      }
    }

    public double start {
      get {
	return _start;
      }
      set {
	_start = value;
	QueueDraw();
      }
    }

    public double stop {
      get {
	return _stop;
      }
      set {
	_stop = value;
	QueueDraw();
      }
    }
  }
    
  public class Group {
    
    public List<Shape> items = new List<Shape>();
    public string mode = "individual"; // squadron or individual
    public Point center = new Point(0,0);
    
    public Group(params Shape [] shapes) {
      foreach (Shape shape in shapes) {
	items.Add(shape);
      }
    }
    
    public void rotate(double degrees) {
      // rotate group
      if (mode == "individual") {
	foreach (Shape shape in items) {
	  shape.rotate(degrees);
	}
      } else {
	// find center of screen positions
	double x = 0, y = 0;
	foreach (Shape shape in items) {
	  x += shape.center.x;
	  y += shape.center.y;
	}
	center.x = x/(items.Count);
	center.y = y/(items.Count);
	// rotate them
	foreach (Shape shape in items) {
	  shape.rotate(degrees);
	}
      }
    }
    
    public void rotateTo(double degrees) {
      foreach (Shape shape in items) {
	shape.rotateTo(degrees);
      }
    }
    
    public void moveTo(int x, int y) {
      foreach (Shape shape in items) {
	shape.moveTo(x, y);
      }
    }
    
    public void move(int x, int y) {
      foreach (Shape shape in items) {
	shape.move(x, y);
      }
    }
  }
}

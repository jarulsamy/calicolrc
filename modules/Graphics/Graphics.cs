/*
Pyjama - Scripting Environment

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
using System.Collections; // IEnumerator
using System.Threading;
using System;

public static class Graphics {
  
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

  private static Dictionary<string, Graphics.GWindow> _windows = 
	  new Dictionary<string, Graphics.GWindow>();

  private static Dictionary<string, Cairo.Color> _color_map = 
	  new Dictionary<string, Cairo.Color>() {
	{"black", new Cairo.Color(0, 0, 0, 1)},
	{"white", new Cairo.Color(255, 255, 255, 1)},
	{"red", new Cairo.Color(255, 0, 0, 1)},
	{"green", new Cairo.Color(0, 255, 0, 1)},
	{"blue", new Cairo.Color(0, 0, 255, 1)},
	{"yellow", new Cairo.Color(255, 255, 0, 1)},
  };
  
  public static Cairo.Color color_map(string name) {
	if (_color_map.ContainsKey(name))
	  return _color_map[name];
	else
	  throw new Exception(String.Format("unknown color '{0}'", name));
  }
  
  public static Cairo.Color color_rgb(int r, int g, int b) {
	return new Cairo.Color(r/255.0, g/255.0, b/255.0, 1);
  }
  
  public static List<string> color_names() {
	return new List<string>(_color_map.Keys);
  }
  
  public static IEnumerator getPixels(Picture picture) {
	for (int x=0; x < picture.width; x++) {
	  for (int y=0; y < picture.height; y++) {
		yield return picture.getPixel(x, y);
	  }
	}
  }
  public static PythonTuple getRGB(Pixel pixel) {
	return pixel.getRGB();
  }
  public static PythonTuple getRGBA(Pixel pixel) {
	return pixel.getRGBA();
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

  public static Picture makePicture(int x, int y) {
	return new Picture(x, y);
  }
  public static Picture makePicture(string filename) {
    return new Picture(filename);
  }

  public static Picture copyPicture(Picture picture) {
    return new Picture(picture);
  }

  public static Picture makePicture(Graphics.GWindow window) { //, string filename) {
	ManualResetEvent ev = new ManualResetEvent(false);
	Gdk.Pixbuf pixbuf = null;
	Gtk.Application.Invoke( delegate {
		  Gdk.Drawable drawable = window.GdkWindow;
		  Gdk.Colormap colormap = drawable.Colormap;
		  int width = 0;
		  int height = 0;
		  drawable.GetSize(out width, out height);
		  pixbuf = Gdk.Pixbuf.FromDrawable(drawable, colormap, 0, 0, 0, 0, width, height);
		  ev.Set();
		});
	ev.WaitOne();
	return new Picture(pixbuf);
  }

  public static Graphics.GWindow makeWindow(string title="Pyjama Graphics", 
	  int width=300, 
	  int height=300) {
	if (_windows.ContainsKey(title)) {
	  _windows[title].ShowAll();
	  return _windows[title];
	} else {
	  _windows[title] = new Graphics.GWindow(title, width, height);
	  return _windows[title];
	}
  }

  public static Graphics.GWindow getWindow(string title) {
	if (_windows.ContainsKey(title)) {
	  return _windows[title];
	} else {
	  return null;
	}
  }

  public static Graphics.GWindow Window(string title="Pyjama Graphics", 
	  int width=300, 
	  int height=300) {
	if (_windows.ContainsKey(title)) {
	  return _windows[title];
	} else {
	  _windows[title] = new Graphics.GWindow(title, width, height);
	  return _windows[title];
	}
  }

  public class GWindow : Gtk.Window {
	private Gtk.EventBox eventbox;
	private Canvas _canvas;
	private bool _dirty = false;
	private bool timer_running = false;
	private DateTime last_update = new DateTime(2000,1,1);
	private uint _update_interval = 100; // how often, in ms, to auto update
	public uint step_time = 200; // how often, in ms, to
					    			  // automatically step
	public List<PythonFunction> onClickCallbacks = new List<PythonFunction>();
	
	public GWindow(string title="Pyjama Graphics Window", 
		int width=300, 
		int height=300) : base(title) {
	  _canvas = new Canvas("auto");
	  SetDefaultSize(width, height);
	  eventbox = new Gtk.EventBox();
	  this.Add(eventbox);
	  eventbox.Add(_canvas);
	  eventbox.ButtonPressEvent += HandleClickCallbacks;
	  DeleteEvent += OnDelete;
	  ShowAll();
	}
	
	private void OnDelete(object obj, Gtk.DeleteEventArgs args)  {
	  _windows.Remove(Title);
	}

    public int getWidth() {
        int width, height;
        this.GetSize(out width, out height);
        return height;
    }

    public int getHeight() {
        int width, height;
        this.GetSize(out width, out height);
        return width;
    }

    private void HandleClickCallbacks(object obj,
		Gtk.ButtonPressEventArgs args) {
	  foreach (PythonFunction function in onClickCallbacks) {
		try {
		  IronPython.Runtime.Operations.PythonCalls.Call(function, obj, args);
		} catch {
		  Console.Error.WriteLine("callback failed to fire.");
		}
	  }
	}

	public void onClick(PythonFunction function) {
	  onClickCallbacks.Add(function);
	}
	
	public uint update_interval {
	  get {
		return _update_interval;
	  }
	  set {
		_update_interval = value;
	  }
	}
	
	public int height {
	  get {
		int width, height;
		this.GetSize(out width, out height);
		return height;
	  }
	}
	public int width {
	  get {
		int width, height;
		this.GetSize(out width, out height);
		return width;
	  }
	}

	public Canvas canvas {
	  get {
		return _canvas;
	  }
	}

	public PythonTuple getMouse() {
	  return PyTuple(0, 0);
	}

	public void step() {
	  // Same as update, but will make sure it 
	  // doesn't update too fast.
	  DateTime now = DateTime.Now;
	  // diff is TimeSpan
	  double diff = (now - last_update).TotalMilliseconds * 100;
	  if (diff < step_time) {
		Thread.Sleep((int)(step_time - diff));
	  }
	  update(); 
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
	
	public void need_to_redraw() {
	  _dirty = true;
	  DateTime now = DateTime.Now;
	  // diff is TimeSpan
	  if ((now - last_update).TotalMilliseconds < update_interval) {
		// pass, too soon!
		// but we need to make sure that someone checks
		// in the future. 
		if (timer_running) {
		  // already one running!
		  // we'll just wait
		} else {
		  // let's spawn one to check in 100 ms or so
		  timer_running = true;
		  GLib.Timeout.Add(update_interval, 
			  new GLib.TimeoutHandler(_redraw_now) );
		}
	  } else { // it is not too soon
		if (timer_running) {
		  // no need to start another
		} else {
		  // let's spawn one to check in 100 ms or so
		  timer_running = true;
		  GLib.Timeout.Add(update_interval, 
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
	public void update() {
	  Gtk.Application.Invoke(delegate { 
			DateTime now = DateTime.Now;
			last_update = now;
			_dirty = false;
			base.QueueDraw(); 
		  });
	}
  }

  public class Button : Gtk.Button {
	public Button(string label) : base(label) {
	}
	public new void Show() {
	  Gtk.Application.Invoke(delegate { base.Show(); });
	}
	public new void ShowAll() {
	  Gtk.Application.Invoke(delegate { base.ShowAll(); });
	}
  }

  public static void ShowAll(object o) {
	Gtk.Application.Invoke(delegate { ((Gtk.Widget)o).ShowAll(); });
  }

  public static void Show(object o) {
	Gtk.Application.Invoke(delegate { ((Gtk.Widget)o).Show(); });
  }

  public class Point {
	public double x;
	public double y;
	public Point(double x, double y) {
	  this.x = x;
	  this.y = y;
	}
  }

  public class GPoint : Shape {
	public double x;
	public double y;
	
	public GPoint(double x, double y) : base(false) {
	  this.x = x;
	  this.y = y;
	  set_points(new Point(x,y));
	  move_to(points_center.x, points_center.y);
	}
  }
  
  public class Canvas : Gtk.DrawingArea {
	
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
	
	public Canvas(string mode) : base() {
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
	public GWindow window;
	public double _direction; // radians
	public bool fill_path;
	
	public Point [] points;
	public Point points_center;
	private Cairo.Color _fill_color;
	private Cairo.Color _outline_color;
	private int _line_width;
	
	private Pen _pen;
	private bool _has_pen;
	
	public Shape(bool has_pen=true) {
	  _direction = 0;
	  center = new Point(0,0);
	  this.has_pen = has_pen;
	  if (this.has_pen) 
		pen = new Pen("black", 1);
	  points_center = new Point(0,0);
	  fill_color = "black";
	  outline_color = "black";
	  fill_path = true;
	  line_width = 1;
	}
	
	public bool has_pen {
	  get {
		return _has_pen;
	  }
	  set {
		_has_pen = value;
	  }
	}
	public double direction {
	  get {
		return _direction;
	  }
	  set {
		rotate_to(value);
	  }
	}
	
	public int line_width {
	  get {
		return _line_width;
	  }
	  set {
		_line_width = value;
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
		} else
		  throw new Exception("this shape cannot have a pen");
	  }
	}
	
	public void QueueDraw() { // shape
	  if (window is GWindow) {
		Gtk.Application.Invoke(delegate {
			  if (window.mode == "auto") { 
				// else, we will issue an update() or step()
				window.need_to_redraw();
			  }
			});
	  }
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

	public double alpha {
	  get {
		return _outline_color.A;
	  }
	  set {
		_outline_color.A = value;
		_fill_color.A = value;
		QueueDraw();
	  }
	}
	
	public string outline_color {
	  get {
		return "#FIXME";
	  }
	  set {
		_outline_color = Graphics.color_map(value);
		QueueDraw();
	  }
	}
	
	public string fill_color {
	  get {
		return "#FIXME";
	  }
	  set {
		_fill_color = Graphics.color_map(value);
		QueueDraw();
	  }
	}
	
	public Cairo.Color raw_fill_color {
	  // Set color from cairo color object
	  get {
		return _fill_color;
	  }
	  set {
		_fill_color = value;
		QueueDraw();
	  }
	}
	
	public Cairo.Color raw_outline_color {
	  // Set color from cairo color object
	  get {
		return _outline_color;
	  }
	  set {
		_outline_color = value;
		QueueDraw();
	  }
	}
	
	public string color {
	  set {
		_fill_color = Graphics.color_map(value);
		_outline_color = Graphics.color_map(value);
		QueueDraw();
	  }
	}
	
	public void set_points(params Point [] new_points) {
	  points = new Point [new_points.Length];
	  // copies
	  for (int p = 0; p < points.Length; p++) {
		points[p] = new Point(new_points[p].x, 
			new_points[p].y);
	  }
	  compute_center_point();
	}
	
	public void move_to(double x, double y) {
	  double dx = x - center.x;
	  double dy = y - center.y;
	  move(dx, dy);
	}
	
	public void move(double dx, double dy) {
	  if (has_pen && pen.down)
		pen.append_path(new Point(center.x + dx, center.y + dy));
	  center.x += dx;
	  center.y += dy;
	  QueueDraw();
	}
	
	public double screen_angle(double dir) {
	  // Screen coords are 45 degrees from system
	  return dir - (45 * Math.PI/180.0);
	}

	public void forward(double distance) {
	  double angle = screen_angle(_direction);
	  double x = ((distance) * Math.Cos(angle) - (distance) * Math.Sin(angle));
	  double y = ((distance) * Math.Sin(angle) + (distance) * Math.Cos(angle));
	  center.x += x;
	  center.y += y;
	  if (has_pen && pen.down)
		pen.append_path(new Point(center.x, center.y));
	  QueueDraw();
	}

	public Polygon pen_up() {
	  pen.down = false;
	  return new Polygon(pen.raw_fill_color, pen.path);
	}
	
	public void pen_down() {
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
		g.LineWidth = line_width;
		temp = screen_coord(points[0]);
		g.MoveTo(temp.x, temp.y);
		for (int p = 1; p < points.Length; p++) {
		  temp = screen_coord(points[p]);
		  g.LineTo(temp.x, temp.y);
		}
		if (fill_path) {
		  g.ClosePath();
		  g.Color = _fill_color;
		  g.FillPreserve();
		  g.Color = _outline_color;
		  g.Stroke();
		} else {
		  g.Color = _outline_color;
		  g.Stroke();
		}
		if (has_pen)
		  pen.render(g);
	  }
	  g.Restore();
	}
	
	public Point screen_coord(Point point) {
	  // first we rotate
	  double x = ((point.x - points_center.x) * Math.Cos(_direction) - 
		  (point.y - points_center.y) * Math.Sin(_direction));
	  double y = ((point.x - points_center.x) * Math.Sin(_direction) + 
		  (point.y - points_center.y) * Math.Cos(_direction));
	  // now we translate:
	  return new Point(center.x + x - points_center.x, 
		  center.y + y - points_center.y);
	}
	
	public void rotate(double degrees) {
	  _direction -= (Math.PI / 180.0) * degrees;
	  QueueDraw();
	}
	
	public void rotate_to(double degrees) {
	  _direction = degrees * (Math.PI) / 180.0;
	  QueueDraw();
	}
	
	public void compute_points_around_origin() {
	  if (points.Length == 0) {
		points_center.x = 0;
		points_center.y = 0;
	  } else if (points.Length == 1) {
		points_center.x = 0;
		points_center.y = 0;
	  } else if (points.Length > 1) {
		for (int p = 0; p < points.Length; p++) {
		  points[p].x -= points_center.x;
		  points[p].y -= points_center.y;
		}
		points_center.x = 0;
		points_center.y = 0;
	  }
	}
	
	public void compute_center_point() {
	  double sum_x = 0, sum_y = 0;
	  if (points.Length == 0) {
		points_center.x = 0;
		points_center.y = 0;
	  } else if (points.Length == 1) {
		points_center.x = points[0].x;
		points_center.y = points[0].y;
	  } else if (points.Length > 1) {
		for (int p = 0; p < points.Length; p++) {
		  sum_x += points[p].x;
		  sum_y += points[p].y;
		}
		points_center.x = sum_x/points.Length;
		points_center.y = sum_y/points.Length;
	  }
	}
	
	public void update() {
	  QueueDraw();
	}
	
	public void draw(GWindow win) {
	  // Add this shape to the Canvas dictionary.
	  win.canvas.shapes.Add(this);
	  // FIXME: QueueDrawRect
	  // FIXME: invalidate from and to rects on move
	  window = win;
	  QueueDraw();
	}

	public void undraw() {
	  Gtk.Application.Invoke(delegate { 
        	  window.canvas.shapes.Remove(this);
        	  window = null;
        	  QueueDraw();
	  });
	}
  }
  
  public class Line : Shape {
	public Line(Point p1, Point p2) : this(true, p1, p2) {
	}
	public Line(bool has_pen, Point p1, Point p2) : base(has_pen) {
	  set_points(p1, p2);
	  compute_center_point();
	  center.x = points_center.x;
	  center.y = points_center.y;
	  compute_points_around_origin();
	}
  }

  /*
  public class Text : Shape {
    public Text(Point p1, string text) : base(has_pen) {
    }
    public override void render(Cairo.Context g) {
      // render path
      g.Save();
      Point temp;
      if (path != null) {
        g.LineWidth = line_width;
        temp = screen_coord(path[0]);
        g.MoveTo(temp.x, temp.y);
        for (int p = 1; p < path.Count; p++) {
          temp = screen_coord(path[p]);
          g.LineTo(temp.x, temp.y);
        }
        if (fill_path) {
          g.ClosePath();
          g.Color = raw_fill_color;
          g.FillPreserve();
          g.Color = raw_outline_color;
          g.Stroke();
        } else {
          g.Color = raw_outline_color;
          g.Stroke();
        }
      }
      g.Restore();
    }
  }
  */
  public class Arrow : Shape {
	public Arrow(Point new_center) : this(new_center, 0) {
	  
	}
	
	public Arrow(Point new_center, double degrees) : base(true) {
	  set_points(
		  new Point(  0,  0),
		  new Point( -5, -5), 
		  new Point(  5,  0),
		  new Point( -5,  5) 
				 );
	  move_to(new_center.x, new_center.y);
	  rotate(degrees);
	}
  }
  
  public class Pen : Shape {
	private List<Point> _path; // = new List<Point>();
	public bool _down;
	
	public Pen(string color, int width) : base(false) {
	  down = false;
	  this.color = color;
	  this.line_width = width;
	  fill_path = false;
	}
	
	public void reset_path() {
	  _path = new List<Point>();
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
	  Point temp;
	  if (path != null) {
		g.LineWidth = line_width;
		temp = screen_coord(path[0]);
		g.MoveTo(temp.x, temp.y);
		for (int p = 1; p < path.Count; p++) {
		  temp = screen_coord(path[p]);
		  g.LineTo(temp.x, temp.y);
		}
		if (fill_path) {
		  g.ClosePath();
		  g.Color = raw_fill_color;
		  g.FillPreserve();
		  g.Color = raw_outline_color;
		  g.Stroke();
		} else {
		  g.Color = raw_outline_color;
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

	public PythonTuple getRGB() {
	  return picture.getRGB(x, y);
	}
	public PythonTuple getRGBA() {
	  return picture.getRGBA(x, y);
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
	public Gdk.Pixbuf _pixbuf; // in memory rep of picture
	public Cairo.Format format = Cairo.Format.Rgb24;
	public Cairo.Surface surface;
	public Cairo.Context context;

    public Picture(string filename) : base(true) {
      _pixbuf = new Gdk.Pixbuf(filename);
      if (!_pixbuf.HasAlpha) {
        _pixbuf = _pixbuf.AddAlpha(true, 0, 0, 0); // alpha color?
      }
      format = Cairo.Format.Argb32;
      // Create a new ImageSurface
      surface = new Cairo.ImageSurface(format, _pixbuf.Width, _pixbuf.Height);
      center.x = _pixbuf.Width/2;
      center.y = _pixbuf.Height/2;
    }

    public Picture(Picture original) : base(true) {
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
      center.x = original.center.x;
      center.y = original.center.y;
    }

	public Picture(Gdk.Pixbuf pixbuf) : base(true) {
	  _pixbuf = pixbuf;
	  if (!_pixbuf.HasAlpha) {
		_pixbuf = _pixbuf.AddAlpha(true, 0, 0, 0); // alpha color?
	  }
	  format = Cairo.Format.Argb32;
	  // Create a new ImageSurface
	  surface = new Cairo.ImageSurface(format, _pixbuf.Width, _pixbuf.Height);
	  center.x = _pixbuf.Width/2;
	  center.y = _pixbuf.Height/2;
	}

	public Picture(System.Drawing.Bitmap bitmap, int width, int height) : base(true) {
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
	  center.x = _pixbuf.Width/2;
	  center.y = _pixbuf.Height/2;
	}

	public Picture(int width, int height, byte [] buffer, int depth) : base(true) {
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
	  center.x = _pixbuf.Width/2;
	  center.y = _pixbuf.Height/2;
	}

	public Picture(int width, int height, byte [] buffer) : base(true) {
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
	  center.x = _pixbuf.Width/2;
	  center.y = _pixbuf.Height/2;
	}

	public Picture(int width, int height) : base(true) {
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
	  center.x = _pixbuf.Width/2;
	  center.y = _pixbuf.Height/2;
	}

    public int getWidth() {
        return _pixbuf.Width;
    }

    public int getHeight() {
        return _pixbuf.Height;
    }

	public void saveToFile(string filename) {
	  // png, and jpg
	  _pixbuf.Save(filename, filename.Substring(filename.Length - 3, 3));
	}

    public Pixel getPixel(int x, int y) {
      return new Pixel(this, x, y);
    }

    public void setPixel(int x, int y, Cairo.Color color) {
      int red = (int)(color.R * 255);
      int green = (int)(color.G * 255);
      int blue = (int)(color.B * 255);
      this.setRGB(x, y, (byte)red, (byte)green, (byte)blue);
    }

	public IEnumerator getPixels() {
	  for (int x=0; x < width; x++) {
		for (int y=0; y < height; y++) {
		  yield return getPixel(x, y);
		}
	  }
	}

	public PythonTuple getRGB(int x, int y) {
	  // red, green, blue, alpha
	  return PyTuple(getRed(x, y), getGreen(x, y), getBlue(x, y));
	}

	public PythonTuple getRGBA(int x, int y) {
	  // red, green, blue, alpha
	  return PyTuple(getRed(x, y), getGreen(x, y), getBlue(x, y), getAlpha(x, y));
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

	public void setRed(int x, int y, byte value) {
	  // red, green, blue, alpha
	  Marshal.WriteByte(_pixbuf.Pixels, y * _pixbuf.Rowstride +
		  x * _pixbuf.NChannels + 0, value);
	  QueueDraw();
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

	public override void render(Cairo.Context g) {
	  g.Save();
	  //g.Translate(-_pixbuf.Width/2, -_pixbuf.Height/2);
	  g.Translate(center.x, center.y);
	  g.Rotate(_direction);
	  Gdk.CairoHelper.SetSourcePixbuf(g, _pixbuf, -_pixbuf.Width/2, -_pixbuf.Height/2);
	  g.Paint();
	  g.LineWidth = line_width;
	  g.MoveTo(-_pixbuf.Width/2, -_pixbuf.Height/2);
	  g.LineTo(_pixbuf.Width/2, -_pixbuf.Height/2);
	  g.LineTo(_pixbuf.Width/2, _pixbuf.Height/2);
	  g.LineTo(-_pixbuf.Width/2, _pixbuf.Height/2);
	  g.ClosePath();
	  g.Color = raw_outline_color;
	  g.Stroke();
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

	public string __repr__() {
	  return String.Format("<Picture ({0}, {1})>", width, height);
	}

  } // -- end of Picture class

  public class Rectangle : Shape {
	public Rectangle(Point point1, Point point2) : base(true) {
	  this.points = new Point [4];
          this.points[0] = new Point(point1.x, point1.y);
          this.points[1] = new Point(point2.x, point1.y);
          this.points[2] = new Point(point2.x, point2.y);
          this.points[3] = new Point(point1.x, point2.y);
	  compute_center_point();
	  center.x = points_center.x;
	  center.y = points_center.y;
	  compute_points_around_origin();
	}

  }

  public class Polygon : Shape {
	
	public Polygon(string color, params object [] points) : base(true) {
	  Cairo.Color raw_color = color_map(color);
	  this.points = new Point [points.Length];
	  int count = 0;
	  foreach (Point p in points) {
		this.points[count] = p ;
		count++;
	  }
	  compute_center_point();
	  center.x = points_center.x;
	  center.y = points_center.y;
	  compute_points_around_origin();
	  raw_outline_color = raw_color;
	  raw_fill_color = raw_color;
	}
	
	public Polygon(Cairo.Color color, List<Point> points) : base(true) {
	  this.points = new Point [points.Count];
	  int count = 0;
	  foreach (Point p in points) {
		this.points[count] = new Point(p.x, p.y);
		count++;
	  }
	  compute_center_point();
	  center.x = points_center.x;
	  center.y = points_center.y;
	  compute_points_around_origin();
	  raw_outline_color = color;
	  raw_fill_color = color;
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
		// save the original points_center
		// set all points_center to main center
		foreach (Shape shape in items) {
		  shape.points_center.x = center.x;
		  shape.points_center.y = center.y;
		}
		// rotate them
		foreach (Shape shape in items) {
		  shape.rotate(degrees);
		}
		// set points_center's back?
	  }
	}

	public void rotate_to(double degrees) {
	  foreach (Shape shape in items) {
		shape.rotate_to(degrees);
	  }
	}

	public void move_to(int x, int y) {
	  foreach (Shape shape in items) {
		shape.move_to(x, y);
	  }
	}

	public void move(int x, int y) {
	  foreach (Shape shape in items) {
		shape.move(x, y);
	  }
	}
  }
}

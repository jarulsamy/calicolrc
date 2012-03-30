using IronPython.Runtime;
// Operations, List, Tuple, Dict, ...
using System.Collections.Generic;
using System.Collections;// IEnumerable

public static class Shapes
{
	
	static Graphics.Shape _shape = null;
	
	public static PythonTuple getMouse ()
	{
		return Graphics.getMouse ();
	}

	public static PythonTuple getMouseNow ()
	{
		return Graphics.getMouseNow ();
	}

	public static string getMouseState ()
	{
		return Graphics.getMouseState ();
	}

	public static string getKeyState ()
	{
		return Graphics.getKeyState ();
	}

	public static string getKeyPressed ()
	{
		return Graphics.getKeyPressed ();
	}

	public static List getColorNames ()
	{
		return Graphics.getColorNames ();
	}

	public static Graphics.Color getColor (Graphics.Picture picture, int x, int y)
	{
		return Graphics.getColor (picture, x, y);
	}

	public static Graphics.Pixel getPixel (Graphics.Picture picture, int x, int y)
	{
		return Graphics.getPixel (picture, x, y);
	}

	public static IEnumerable getPixels (Graphics.Picture picture)
	{
		return Graphics.getPixels (picture);
	}

	public static void setPixels (Graphics.Picture picture, Graphics.Picture picture2)
	{
		Graphics.setPixels (picture, picture2);
	}

	public static void setPixel (Graphics.Picture picture, int x, int y, Graphics.Color color)
	{
		Graphics.setPixel (picture, x, y, color);
	}

	public static void setPixel (Graphics.Picture picture, int x, int y, Graphics.Pixel pixel)
	{
		Graphics.setPixel (picture, x, y, pixel);
	}

	public static Graphics.Color getColor (Graphics.Pixel pixel)
	{
		return Graphics.getColor (pixel);
	}

	public static void setColor (Graphics.Pixel pixel, Graphics.Color color)
	{
		Graphics.setColor (pixel, color);
	}

	public static PythonTuple getRGB (Graphics.Pixel pixel)
	{
		return Graphics.getRGB (pixel);
	}

	public static PythonTuple getRGBA (Graphics.Pixel pixel)
	{
		return Graphics.getRGBA (pixel);
	}

	public static int getGray (Graphics.Pixel pixel)
	{
		return Graphics.getGray (pixel);
	}

	public static int getRed (Graphics.Pixel pixel)
	{
		return Graphics.getRed (pixel);
	}

	public static int getGreen (Graphics.Pixel pixel)
	{
		return Graphics.getGreen (pixel);
	}

	public static int getBlue (Graphics.Pixel pixel)
	{
		return Graphics.getBlue (pixel);
	}

	public static int getAlpha (Graphics.Pixel pixel)
	{
		return Graphics.getAlpha (pixel);
	}

	public static void setRGB (Graphics.Pixel pixel, int red, int green, int blue)
	{
		Graphics.setRGB (pixel, red, green, blue);
	}

	public static void savePicture (Graphics.Picture picture, string filename)
	{
		Graphics.savePicture (picture, filename);
	}

	public static void savePicture (List list, string filename)
	{
		Graphics.savePicture (list, filename);
	}

	public static Graphics.Picture makePicture (int x, int y)
	{
		_shape = Graphics.makePicture (x, y);
		return (Graphics.Picture)_shape;
	}

	public static Graphics.Picture makePicture (int x, int y, Graphics.Color c)
	{
		_shape = Graphics.makePicture (x, y, c);
		return (Graphics.Picture)_shape;
	}

	public static Graphics.Picture makePicture (string filename)
	{
		_shape = Graphics.makePicture (filename);
		return (Graphics.Picture)_shape;
	}

	public static Graphics.Picture copyPicture (Graphics.Picture picture)
	{
		return Graphics.copyPicture (picture);
	}

	public static Graphics.Picture makePicture (Graphics.WindowClass window)
	{ 
		_shape = Graphics.makePicture (window) ;
		return (Graphics.Picture)_shape;
	}

	public static Graphics.WindowClass makeWindow (string title="Calico Graphics",
						  int width=300, 
						  int height=300)
	{
		return Graphics.makeWindow (title, width, height);
	}

	public static Graphics.WindowClass getWindow ()
	{
		return Graphics.getWindow ();
	}

	public static Graphics.Color makeColor (string color)
	{
		return Graphics.makeColor (color);
	}

	public static Graphics.Color makeColor (int r, int g, int b, int a)
	{
		return Graphics.makeColor (r, g, b, a);
	}

	public static Graphics.Text makeText (IList list, string text)
	{
		_shape = new Graphics.Text (list, text);
		return (Graphics.Text)_shape;
	}

	public static Graphics.Line makeLine (IList p1, IList p2)
	{
		_shape = new Graphics.Line (p1, p2);
		return (Graphics.Line)_shape;
	}

	public static Graphics.Curve makeCurve (IList p1, IList p2, IList p3, IList p4)
	{
		_shape = new Graphics.Curve (p1, p2, p3, p4);
		return (Graphics.Curve)_shape;
	}

	public static Graphics.Arrow makeArrow (IList point, double degrees)
	{
		_shape = new Graphics.Arrow (point, degrees);
		return (Graphics.Arrow)_shape;      
	}

	public static Graphics.Rectangle makeRectangle (IList p1, IList p2)
	{
		_shape = new Graphics.Rectangle (p1, p2);
		return (Graphics.Rectangle)_shape;
	}

	public static Graphics.RoundedRectangle makeRoundedRectangle (IList p1, 
								 IList p2, 
								 int radius)
	{
		_shape = new Graphics.RoundedRectangle (p1, p2, radius);
		return (Graphics.RoundedRectangle)_shape;
	}

	public static Graphics.Polygon makePolygon (params IList [] points)
	{
		_shape = new Graphics.Polygon (points);
		return (Graphics.Polygon)_shape;
	}

	public static Graphics.Point makePoint (int x, int y)
	{
		return new Graphics.Point (x, y);
	}

	public static Graphics.Dot makeDot (IList p)
	{
		_shape = new Graphics.Dot (p);
		return (Graphics.Dot)_shape;
	}

	public static Graphics.Circle makeCircle (IList point, int radius)
	{
		_shape = new Graphics.Circle (point, radius);
		return (Graphics.Circle)_shape;
	}

	public static Graphics.Oval makeOval (IList point, int xRadius, int yRadius)
	{
		_shape = new Graphics.Oval (point, xRadius, yRadius);
		return (Graphics.Oval)_shape;
	}

	public static Graphics.Pie makePie (IList point, int radius, double start, double stop)
	{
		_shape = new Graphics.Pie (point, radius, start, stop);
		return (Graphics.Pie)_shape;
	}

	public static Graphics.Arc makeArc (IList point, int radius, double start, double stop)
	{
		_shape = new Graphics.Arc (point, radius, start, stop);
		return (Graphics.Arc)_shape;
	}

	public static Graphics.Frame makeFrame (IList point)
	{
		return new Graphics.Frame (point);
	}
	
	public static Graphics.Shape getShape ()
	{
		return _shape;
	}

	public static void draw (Graphics.Shape shape=null, Graphics.WindowClass window=null)
	{
		if (shape == null)
			shape = getShape ();
		if (window == null)
			window = getWindow ();
		shape.draw (window);
	}

	public static void draw (Graphics.Shape shape1, Graphics.Shape shape2)
	{
		shape1.draw (shape2);
	}

	public static void undraw (Graphics.Shape shape=null)
	{
		if (shape == null)
			shape = getShape ();
		shape.undraw ();
	}

	public static void move (int dx, int dy)
	{
		Graphics.Shape shape = getShape ();
		shape.move (dx, dy);
	}

	public static void move (Graphics.Shape shape, int dx, int dy)
	{
		shape.move (dx, dy);
	}

	public static void moveTo (Graphics.Shape shape, int x, int y)
	{
		shape.moveTo (x, y);
	}

	public static void scale (Graphics.Shape shape, double scale)
	{
		shape.scale (scale);
	}

	public static void scaleTo (Graphics.Shape shape, double scale)
	{
		shape.scaleTo (scale);
	}

	public static void rotate (Graphics.Shape shape, double degrees)
	{
		shape.rotate (degrees);
	}

	public static void rotateTo (Graphics.Shape shape, double degrees)
	{
		shape.rotateTo (degrees);
	}

	public static void setFill (Graphics.Shape shape, Graphics.Color color)
	{
		shape.fill = color;
	}

	public static void setColor (Graphics.Shape shape, Graphics.Color color)
	{
		shape.color = color;
	}

	public static void setOutline (Graphics.Shape shape, Graphics.Color color)
	{
		shape.outline = color;
	}

	public static void setBackground (Graphics.WindowClass window, Graphics.Color color)
	{
		window.setBackground (color);
	}

	public static void setMode (Graphics.WindowClass window, string mode)
	{
		window.mode = mode;
	}

	public static void step (Graphics.WindowClass window, double time)
	{
		window.step (time);
	}

	public static void setBorder (Graphics.Shape shape, int width)
	{
		shape.border = width;
	}

	public static void forward (Graphics.Shape shape, double distance)
	{
		shape.forward (distance);
	}

	public static void backward (Graphics.Shape shape, double distance)
	{
		shape.backward (distance);
	}

	public static void right (Graphics.Shape shape, double angle)
	{
		shape.rotate (-angle);
	}

	public static void forward (double distance)
	{
		Graphics.Shape shape = getShape ();
		shape.forward (distance);
	}

	public static void backward (double distance)
	{
		Graphics.Shape shape = getShape ();
		shape.backward (distance);
	}

	public static void right (double angle)
	{
		Graphics.Shape shape = getShape ();
		shape.rotate (-angle);
	}

	public static void left (Graphics.Shape shape, double angle)
	{
		shape.rotate (angle);
	}

	public static void left (double angle)
	{
		Graphics.Shape shape = getShape ();
		shape.rotate (angle);
	}

	public static void penUp (Graphics.Shape shape=null)
	{
		if (shape == null)
			shape = getShape ();
		Graphics.Line line = shape.penUp ();
		line.draw (Graphics.getWindow ());
	}

	public static void penDown (Graphics.Shape shape=null)
	{
		if (shape == null)
			shape = getShape ();
		shape.penDown ();
	}

	public static object pick (IList items)
	{
		return Myro.pickOne (items);
	}	
}


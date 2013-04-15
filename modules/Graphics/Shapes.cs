using IronPython.Runtime;
// Operations, List, Tuple, Dict, ...
using System.Collections.Generic;
using System.Collections;// IEnumerable

public static class Shapes
{
	
	static Graphics.Shape _shape = null;
	
        [method: JigsawTab("S/Windows")]
	public static PythonTuple getMouse ()
	{
		return Graphics.getMouse ();
	}

        [method: JigsawTab("S/Windows")]
	public static PythonTuple getMouseNow ()
	{
		return Graphics.getMouseNow ();
	}

        [method: JigsawTab("S/Windows")]
	public static string getMouseState ()
	{
		return Graphics.getMouseState ();
	}

        [method: JigsawTab("S/Windows")]
	public static string getKeyState ()
	{
		return Graphics.getKeyState ();
	}

        [method: JigsawTab("S/Windows")]
	public static string getKeyPressed ()
	{
		return Graphics.getKeyPressed ();
	}

        [method: JigsawTab("S/Pictures")]
	public static List getColorNames ()
	{
		return Graphics.getColorNames ();
	}

        [method: JigsawTab("S/Pictures")]
	public static Graphics.Color getColor (Graphics.Picture picture, int x, int y)
	{
		return Graphics.getColor (picture, x, y);
	}

        [method: JigsawTab("S/Pictures")]
	public static Graphics.Pixel getPixel (Graphics.Picture picture, int x, int y)
	{
		return Graphics.getPixel (picture, x, y);
	}

        [method: JigsawTab("S/Pictures")]
	public static IEnumerable getPixels (Graphics.Picture picture)
	{
		return Graphics.getPixels (picture);
	}

        [method: JigsawTab("S/Pictures")]
	public static void setPixels (Graphics.Picture picture, Graphics.Picture picture2)
	{
		Graphics.setPixels (picture, picture2);
	}

        [method: JigsawTab("S/Pictures")]
	public static void setPixel (Graphics.Picture picture, int x, int y, Graphics.Color color)
	{
		Graphics.setPixel (picture, x, y, color);
	}

        [method: JigsawTab("S/Pictures")]
	public static void setPixel (Graphics.Picture picture, int x, int y, Graphics.Pixel pixel)
	{
		Graphics.setPixel (picture, x, y, pixel);
	}

        [method: JigsawTab("S/Pictures")]
	public static Graphics.Color getColor (Graphics.Pixel pixel)
	{
		return Graphics.getColor (pixel);
	}

        [method: JigsawTab("S/Pictures")]
	public static void setColor (Graphics.Pixel pixel, Graphics.Color color)
	{
		Graphics.setColor (pixel, color);
	}

        [method: JigsawTab("S/Pictures")]
	public static PythonTuple getRGB (Graphics.Pixel pixel)
	{
		return Graphics.getRGB (pixel);
	}

        [method: JigsawTab("S/Pictures")]
	public static PythonTuple getRGBA (Graphics.Pixel pixel)
	{
		return Graphics.getRGBA (pixel);
	}

        [method: JigsawTab("S/Pictures")]
	public static int getGray (Graphics.Pixel pixel)
	{
		return Graphics.getGray (pixel);
	}

        [method: JigsawTab("S/Pictures")]
	public static int getRed (Graphics.Pixel pixel)
	{
		return Graphics.getRed (pixel);
	}

        [method: JigsawTab("S/Pictures")]
	public static int getGreen (Graphics.Pixel pixel)
	{
		return Graphics.getGreen (pixel);
	}

        [method: JigsawTab("S/Pictures")]
	public static int getBlue (Graphics.Pixel pixel)
	{
		return Graphics.getBlue (pixel);
	}

        [method: JigsawTab("S/Pictures")]
	public static int getAlpha (Graphics.Pixel pixel)
	{
		return Graphics.getAlpha (pixel);
	}

        [method: JigsawTab("S/Pictures")]
	public static void setRGB (Graphics.Pixel pixel, int red, int green, int blue)
	{
		Graphics.setRGB (pixel, red, green, blue);
	}

        [method: JigsawTab("S/Pictures")]
        public static void savePicture (Graphics.Picture picture, string filename)
	{
	        Graphics.savePicture (picture, filename);
	}

        [method: JigsawTab("S/Pictures")]
        public static void savePicture (List list, string filename)
	{
   	        Graphics.savePicture (list, filename);
	}

        [method: JigsawTab("S/Pictures")]
	public static Graphics.Picture makePicture (int x, int y)
	{
		_shape = Graphics.makePicture (x, y);
		return (Graphics.Picture)_shape;
	}

        [method: JigsawTab("S/Pictures")]
	public static Graphics.Picture makePicture (int x, int y, Graphics.Color c)
	{
		_shape = Graphics.makePicture (x, y, c);
		return (Graphics.Picture)_shape;
	}

        [method: JigsawTab("S/Pictures")]
	public static Graphics.Picture makePicture (string filename)
	{
		_shape = Graphics.makePicture (filename);
		return (Graphics.Picture)_shape;
	}

        [method: JigsawTab("S/Pictures")]
	public static Graphics.Picture copyPicture (Graphics.Picture picture)
	{
		return Graphics.copyPicture (picture);
	}

        [method: JigsawTab("S/Pictures")]
	public static Graphics.Picture makePicture (Graphics.WindowClass window)
	{ 
		_shape = Graphics.makePicture (window) ;
		return (Graphics.Picture)_shape;
	}

        [method: JigsawTab("S/Windows")]
	public static Graphics.WindowClass makeWindow (string title="Calico Graphics",
						  int width=300, 
						  int height=300)
	{
		return Graphics.makeWindow (title, width, height);
	}

        [method: JigsawTab("S/Windows")]
	public static Graphics.WindowClass getWindow ()
	{
		return Graphics.getWindow ();
	}

        [method: JigsawTab("S/Pictures")]
	public static Graphics.Color makeColor (string color)
	{
		return Graphics.makeColor (color);
	}

        [method: JigsawTab("S/Pictures")]
	public static Graphics.Color makeColor (int r, int g, int b, int a)
	{
		return Graphics.makeColor (r, g, b, a);
	}

        [method: JigsawTab("S/Shapes")]
	public static Graphics.Text makeText (IList list, string text)
	{
		_shape = new Graphics.Text (list, text);
		return (Graphics.Text)_shape;
	}

        [method: JigsawTab("S/Shapes")]
	public static Graphics.Line makeLine (IList p1, IList p2)
	{
		_shape = new Graphics.Line (p1, p2);
		return (Graphics.Line)_shape;
	}

        [method: JigsawTab("S/Shapes")]
	public static Graphics.Curve makeCurve (IList p1, IList p2, IList p3, IList p4)
	{
		_shape = new Graphics.Curve (p1, p2, p3, p4);
		return (Graphics.Curve)_shape;
	}

        [method: JigsawTab("S/Shapes")]
	public static Graphics.Arrow makeArrow (IList point, double degrees)
	{
		_shape = new Graphics.Arrow (point, degrees);
		return (Graphics.Arrow)_shape;      
	}

        [method: JigsawTab("S/Shapes")]
	public static Graphics.Rectangle makeRectangle (IList p1, IList p2)
	{
		_shape = new Graphics.Rectangle (p1, p2);
		return (Graphics.Rectangle)_shape;
	}

        [method: JigsawTab("S/Shapes")]
	public static Graphics.RoundedRectangle makeRoundedRectangle (IList p1, 
								 IList p2, 
								 int radius)
	{
		_shape = new Graphics.RoundedRectangle (p1, p2, radius);
		return (Graphics.RoundedRectangle)_shape;
	}

        [method: JigsawTab("S/Shapes")]
	public static Graphics.Polygon makePolygon (params IList [] points)
	{
		_shape = new Graphics.Polygon (points);
		return (Graphics.Polygon)_shape;
	}

        [method: JigsawTab("S/Shapes")]
	public static Graphics.Point makePoint (int x, int y)
	{
		return new Graphics.Point (x, y);
	}

        [method: JigsawTab("S/Shapes")]
	public static Graphics.Dot makeDot (IList p)
	{
		_shape = new Graphics.Dot (p);
		return (Graphics.Dot)_shape;
	}

        [method: JigsawTab("S/Shapes")]
	public static Graphics.Circle makeCircle (IList point, int radius)
	{
		_shape = new Graphics.Circle (point, radius);
		return (Graphics.Circle)_shape;
	}

        [method: JigsawTab("S/Shapes")]
	public static Graphics.Oval makeOval (IList point, int xRadius, int yRadius)
	{
		_shape = new Graphics.Oval (point, xRadius, yRadius);
		return (Graphics.Oval)_shape;
	}

        [method: JigsawTab("S/Shapes")]
	public static Graphics.Pie makePie (IList point, int radius, double start, double stop)
	{
		_shape = new Graphics.Pie (point, radius, start, stop);
		return (Graphics.Pie)_shape;
	}

        [method: JigsawTab("S/Shapes")]
	public static Graphics.Arc makeArc (IList point, int radius, double start, double stop)
	{
		_shape = new Graphics.Arc (point, radius, start, stop);
		return (Graphics.Arc)_shape;
	}

        [method: JigsawTab("S/Shapes")]
	public static Graphics.Frame makeFrame (IList point)
	{
		return new Graphics.Frame (point);
	}
	
        [method: JigsawTab("S/Shapes")]
	public static Graphics.Shape getShape ()
	{
		return _shape;
	}

        [method: JigsawTab("S/Actions")]
	public static void draw (Graphics.Shape shape=null, Graphics.WindowClass window=null)
	{
		if (shape == null)
			shape = getShape ();
		if (window == null)
			window = getWindow ();
		shape.draw (window);
	}

        [method: JigsawTab("S/Actions")]
	public static void draw (Graphics.Shape shape1, Graphics.Shape shape2)
	{
		shape1.draw (shape2);
	}

        [method: JigsawTab("S/Actions")]
	public static void undraw (Graphics.Shape shape=null)
	{
		if (shape == null)
			shape = getShape ();
		shape.undraw ();
	}

        [method: JigsawTab("S/Actions")]
	public static void move (int dx, int dy)
	{
		Graphics.Shape shape = getShape ();
		shape.move (dx, dy);
	}

        [method: JigsawTab("S/Actions")]
	public static void move (Graphics.Shape shape, int dx, int dy)
	{
		shape.move (dx, dy);
	}

        [method: JigsawTab("S/Actions")]
	public static void moveTo (Graphics.Shape shape, int x, int y)
	{
		shape.moveTo (x, y);
	}

        [method: JigsawTab("S/Actions")]
	public static void scale (Graphics.Shape shape, double scale)
	{
		shape.scale (scale);
	}

        [method: JigsawTab("S/Actions")]
	public static void scaleTo (Graphics.Shape shape, double scale)
	{
		shape.scaleTo (scale);
	}

        [method: JigsawTab("S/Actions")]
	public static void rotate (Graphics.Shape shape, double degrees)
	{
		shape.rotate (degrees);
	}

        [method: JigsawTab("S/Actions")]
	public static void rotateTo (Graphics.Shape shape, double degrees)
	{
		shape.rotateTo (degrees);
	}

        [method: JigsawTab("S/Actions")]
	public static void setFill (Graphics.Shape shape, Graphics.Color color)
	{
		shape.fill = color;
	}

        [method: JigsawTab("S/Actions")]
	public static void setColor (Graphics.Shape shape, Graphics.Color color)
	{
		shape.color = color;
	}

        [method: JigsawTab("S/Actions")]
	public static void setOutline (Graphics.Shape shape, Graphics.Color color)
	{
		shape.outline = color;
	}

        [method: JigsawTab("S/Actions")]
	public static void setBackground (Graphics.WindowClass window, Graphics.Color color)
	{
		window.setBackground (color);
	}

        [method: JigsawTab("S/Actions")]
	public static void setMode (Graphics.WindowClass window, string mode)
	{
		window.mode = mode;
	}

        [method: JigsawTab("S/Actions")]
	public static void step (Graphics.WindowClass window, double time)
	{
		window.step (time);
	}

        [method: JigsawTab("S/Actions")]
	public static void setBorder (Graphics.Shape shape, int width)
	{
		shape.border = width;
	}

        [method: JigsawTab("S/Actions")]
	public static void forward (Graphics.Shape shape, double distance)
	{
		shape.forward (distance);
	}

        [method: JigsawTab("S/Actions")]	
	public static void backward (Graphics.Shape shape, double distance)
	{
		shape.backward (distance);
	}

        [method: JigsawTab("S/Actions")]
	public static void right (Graphics.Shape shape, double angle)
	{
		shape.rotate (-angle);
	}

        [method: JigsawTab("S/Actions")]
	public static void forward (double distance)
	{
		Graphics.Shape shape = getShape ();
		shape.forward (distance);
	}

        [method: JigsawTab("S/Actions")]
	public static void backward (double distance)
	{
		Graphics.Shape shape = getShape ();
		shape.backward (distance);
	}

        [method: JigsawTab("S/Actions")]
	public static void right (double angle)
	{
		Graphics.Shape shape = getShape ();
		shape.rotate (-angle);
	}

        [method: JigsawTab("S/Actions")]
	public static void left (Graphics.Shape shape, double angle)
	{
		shape.rotate (angle);
	}

        [method: JigsawTab("S/Actions")]
	public static void left (double angle)
	{
		Graphics.Shape shape = getShape ();
		shape.rotate (angle);
	}

        [method: JigsawTab("S/Actions")]
	public static void penUp (Graphics.Shape shape=null)
	{
		if (shape == null)
			shape = getShape ();
		Graphics.Line line = shape.penUp ();
		line.draw (Graphics.getWindow ());
	}

        [method: JigsawTab("S/Actions")]
	public static void penDown (Graphics.Shape shape=null)
	{
		if (shape == null)
			shape = getShape ();
		shape.penDown ();
	}

        [method: JigsawTab("S/Actions")]
	public static object pick (IList items)
	{
		return Myro.pickOne (items);
	}	
}


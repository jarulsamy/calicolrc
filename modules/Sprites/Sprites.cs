using System.Threading;
using System.Collections.Generic;

public static class Sprites
{
    public static Graphics.WindowClass window;
    public delegate void InvokeDelegate ();
    public static Dictionary<string,Sprite> sprites = 
	new Dictionary<string,Sprite> ();
    public static string default_sprite = null;

    [method: JigsawTab("Sprites")]
    public static Sprite selectSprite(string name) {
	if (sprites.ContainsKey(name)) {
	    default_sprite = name;
	    return sprites[name];
	} else {
	    return null;
	}
    }

    [method: JigsawTab("Sprites Move")]
    public static void move(double x, double y) {
	if (sprites.ContainsKey(default_sprite)) {
	    sprites[default_sprite].move(x, y);
	}
    }

    [JigsawTab("Sprites Move")]
    public static void moveTo(double x, double y) {
	if (sprites.ContainsKey(default_sprite)) {
	    sprites[default_sprite].moveTo(x, y);
	}
    }

    [JigsawTab("Sprites Move")]
    public static void rotateTo(double degrees) {
	if (sprites.ContainsKey(default_sprite)) {
	    sprites[default_sprite].rotateTo(degrees);
	}
    }

    [JigsawTab("Sprites Move")]
    public static void rotate(double degrees) {
	if (sprites.ContainsKey(default_sprite)) {
	    sprites[default_sprite].rotate(degrees);
	}
    }

    [JigsawTab("Sprites Move")]
    public static void forward(double pixels) {
	if (sprites.ContainsKey(default_sprite)) {
	    sprites[default_sprite].forward(pixels);
	}
    }

    [JigsawTab("Sprites Move")]
    public static void backward(double pixels) {
	if (sprites.ContainsKey(default_sprite)) {
	    sprites[default_sprite].backward(pixels);
	}
    }

    [JigsawTab("Sprites Move")]
    public static void scale(double factor) {
	if (sprites.ContainsKey(default_sprite)) {
	    sprites[default_sprite].scale(factor);
	}
    }

    [JigsawTab("Sprites Move")]
    public static void scaleTo(double factor) {
	if (sprites.ContainsKey(default_sprite)) {
	    sprites[default_sprite].scaleTo(factor);
	}
    }

    [JigsawTab("Sprites Move")]
    public class Sprite : Graphics.Picture {
	public Sprite(string filename) :  base(filename) {
	}
    }

    [JigsawTab("Sprites")]
    public static void hide() {
	if (sprites.ContainsKey(default_sprite)) {
	    sprites[default_sprite].undraw();
	}
    }

    [JigsawTab("Sprites Look")]
    public static void show() {
	if (sprites.ContainsKey(default_sprite)) {
	    sprites[default_sprite].draw(window);
	}
    }

    [JigsawTab("Sprites Look")]
    public static void transparency(double value) {
	if (sprites.ContainsKey(default_sprite)) {
	    sprites[default_sprite].setAlpha((byte)(int)(255 * value));
	}
    }

    [JigsawTab("Sprites Look")]
    public static void flipHorizontal() {
	if (sprites.ContainsKey(default_sprite)) {
	    sprites[default_sprite].rotate(180);
	    sprites[default_sprite].flipVertical();
	}
    }

    [JigsawTab("Sprites Look")]
    public static void flipVertical() {
	if (sprites.ContainsKey(default_sprite)) {
	    sprites[default_sprite].flipVertical();
	}
    }

    [JigsawTab("Sprites")]
    public static Sprite makeSprite() {
	int i = 1;
	while (sprites.ContainsKey(System.String.Format("Sprite{0}", i))) {
	    i++;
	}
	return makeSprite(System.String.Format("Sprite{0}", i));
    }

    [JigsawTab("Sprites")]
    public static Sprite makeSprite(string name) {
	return makeSprite(name, getImagePath("sluginpjs-alone-sm.png"));
    }

    [JigsawTab("Sprites Sound")]
    public static void speak(string text) {
	Myro.speak(text);
    }

    [JigsawTab("Sprites Look")]
    public static void bubble(string text) {
	System.Console.WriteLine(text);
    }

    [JigsawTab("Sprites")]
    public static Sprite makeSprite(string name, string filename) {
	if (sprites.ContainsKey(name)) {
	    sprites[name].undraw();
	    sprites.Remove(name);
	}
	Sprite sprite = new Sprite(filename);
	sprite.border = 0;
	sprites.Add(name, sprite);
	sprite.draw(window);
	default_sprite = name;
	return sprite;
    }

    [method: JigsawTab("Sprites")]
    public static void init() {
	
	Invoke( delegate {
		System.Console.WriteLine(AssemblyDirectory);
		if (window == null) {
		    window = new Window();
		}
		Graphics.Picture background = new Graphics.Picture(getImagePath("sluginpjs-background.png"));
		background.draw(window);
		Sprite sprite = makeSprite();
		sprite.moveTo(250, 125);
		window.ShowAll();
	    });
    }

    [JigsawTab(null)]
    public static void Invoke (InvokeDelegate invoke) {
	if (needInvoke ())
	    Gtk.Application.Invoke (delegate {
		    invoke ();
		});
	else
	    invoke ();
    }

    [JigsawTab(null)]
    public static bool needInvoke () {
	  return (Thread.CurrentThread.ManagedThreadId != 1);
	}

    public class Window : Graphics.WindowClass {
	public Gtk.Notebook notebook1;
	public Gtk.Label label1;
	public Gtk.VPaned vpaned = new Gtk.VPaned();

	public Window() : base("Calico Sprites", 500, 500) {
	    Remove(canvas);
	    Add(vpaned);
	    canvas.SetSizeRequest(-1, 50);
	    Gtk.Frame frame = new Gtk.Frame();
	    frame.Add(canvas);
	    vpaned.Pack1(frame, true, true);
	    notebook1 = new Gtk.Notebook ();
	    notebook1.CanFocus = true;
	    notebook1.Name = "notebook1";
	    notebook1.CurrentPage = 0;
	    // Notebook tab
	    Gtk.Label w5 = new Gtk.Label ();
	    w5.Visible = true;
	    notebook1.Add (w5);
	    label1 = new Gtk.Label ();
	    label1.Name = "label1";
	    label1.LabelProp = "Sprite1";
	    notebook1.SetTabLabel (w5, label1);
	    vpaned.Pack2(notebook1, true, true);
	    DeleteEvent += new Gtk.DeleteEventHandler (this.OnDeleteEvent);
	}
	protected void OnDeleteEvent (object sender, Gtk.DeleteEventArgs a)
	{
	    Invoke( delegate {
		    this.Hide();
		});
	    a.RetVal = true;
	}
    }
    
    static string getImagePath(string image) {
	return System.IO.Path.Combine(PictureDirectory, image);
    }

    static string AssemblyDirectory {
	get {
	    string codeBase = System.Reflection.Assembly.GetExecutingAssembly().CodeBase;
	    System.UriBuilder uri = new System.UriBuilder(codeBase);
	    string path = System.Uri.UnescapeDataString(uri.Path);
	    return System.IO.Path.GetDirectoryName(path);
	}
    }

    static string PictureDirectory {
	get {
	    string path = System.IO.Path.Combine(AssemblyDirectory, "..", "examples");
	    path = System.IO.Path.Combine(path, "images");
	    return path;
	}
    }
}

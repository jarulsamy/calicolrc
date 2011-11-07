using System;
using System.Collections.Generic;

public class JigsawWidget : Gtk.ScrolledWindow
{
  public Jigsaw.Canvas cvs = null;
  //Gtk.Fixed cvsFixed = null;
  public object document = null;

  public JigsawWidget() {
	this.CanFocus = true;
	cvs = new Jigsaw.Canvas(900, 600);
	this.Add (cvs);
  }
	
//  public JigsawWidget() {
//    this.CanFocus = true;
//	this.VscrollbarPolicy = Gtk.PolicyType.Always; //Gtk.PolicyType.Never; //((global::Gtk.PolicyType)(0));
//	this.HscrollbarPolicy = Gtk.PolicyType.Always; //Gtk.PolicyType.Never; //((global::Gtk.PolicyType)(0));
//	
//	// Add the Canvas and other utility widgets
//	cvsFixed = new Gtk.Fixed();
//	cvs = new Jigsaw.Canvas(900, 600);
//	cvsFixed.Put(cvs, 0,0);
//	this.AddWithViewport(cvsFixed);
//  }
}

// -----------------------------------------------------------------------
public class MainWindow : Gtk.Window
{
	Jigsaw.Canvas cvs;

	public MainWindow () : base(Gtk.WindowType.Toplevel)
	{

		// Main window
		this.Title = "Jigsaw";
		this.Icon = new global::Gdk.Pixbuf (global::System.IO.Path.Combine (global::System.AppDomain.CurrentDomain.BaseDirectory, "plugin.png"));
		this.WindowPosition = ((global::Gtk.WindowPosition)(4));
		//this.DefaultWidth = 1000;
		//this.DefaultHeight = 700;
		this.DeleteEvent += new Gtk.DeleteEventHandler (this.OnDeleteEvent);
		
		// Embedded scrolled window
//		Gtk.ScrolledWindow sw = new Gtk.ScrolledWindow ();
//		sw.CanFocus = true;
//		sw.VscrollbarPolicy = Gtk.PolicyType.Always; //Gtk.PolicyType.Never; //((global::Gtk.PolicyType)(0));
//		sw.HscrollbarPolicy = Gtk.PolicyType.Always; //Gtk.PolicyType.Never; //((global::Gtk.PolicyType)(0));
//		this.Add(sw);
		
		// Add the Canvas and other utility widgets
//		Jigsaw.Canvas cvs = new Jigsaw.Canvas(1200, 900);
//		Gtk.Fixed cvsFixed = new Gtk.Fixed();
//		cvsFixed.Put(cvs, 0,0);
//		sw.AddWithViewport(cvsFixed);
		
		cvs = new Jigsaw.Canvas(900, 600);
		this.Add(cvs);
		this.KeyPressEvent    += new Gtk.KeyPressEventHandler( this.OnKeyPressEvent);

		// Let it rip
		this.ShowAll();
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnDeleteEvent (object sender, Gtk.DeleteEventArgs a)
	{	// Close the application when the main window is closed 
		Gtk.Application.Quit ();
		a.RetVal = true;
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	[GLib.ConnectBeforeAttribute]
	protected virtual void OnKeyPressEvent(object o, Gtk.KeyPressEventArgs args) {
        if (args.Event.Key == Gdk.Key.Up) {
			cvs.DoZoom(1.05);
        } else if (args.Event.Key == Gdk.Key.Down) {
			cvs.DoZoom(1.0/1.05);
		}
	}
}
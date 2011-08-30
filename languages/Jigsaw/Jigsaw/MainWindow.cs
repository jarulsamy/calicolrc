using System;
using System.Drawing;
using System.Collections.Generic;

public class MainWindow : Gtk.Window
{
	public MainWindow () : base(Gtk.WindowType.Toplevel)
	{
		// Main window
		this.Title = "Jigsaw";
		this.Icon = new global::Gdk.Pixbuf (global::System.IO.Path.Combine (global::System.AppDomain.CurrentDomain.BaseDirectory, ".\\plugin.png"));
		this.WindowPosition = ((global::Gtk.WindowPosition)(4));
		this.DefaultWidth = 1000;
		this.DefaultHeight = 700;
		this.DeleteEvent += new Gtk.DeleteEventHandler (this.OnDeleteEvent);
		
		// Embedded scrolled window
		Gtk.ScrolledWindow sw = new Gtk.ScrolledWindow ();
		sw.CanFocus = true;
		sw.VscrollbarPolicy = Gtk.PolicyType.Always; //((global::Gtk.PolicyType)(0));
		sw.HscrollbarPolicy = Gtk.PolicyType.Always; //((global::Gtk.PolicyType)(0));
		this.Add(sw);
		
		// Add the Canvas and other utility widgets
		Gtk.Fixed cvsFixed = new Gtk.Fixed();
		Jigsaw.Canvas cvs = new Jigsaw.Canvas(1600, 1000);
		cvsFixed.Put(cvs, 0,0);
		sw.AddWithViewport(cvsFixed);

		
		
		// Let it rip
		this.ShowAll();
	}
	
	/// <summary>
	/// Close the application when the main window is closed 
	/// </summary>
	/// <param name="sender">
	/// A <see cref="System.Object"/>
	/// </param>
	/// <param name="a">
	/// A <see cref="DeleteEventArgs"/>
	/// </param>
	protected void OnDeleteEvent (object sender, Gtk.DeleteEventArgs a)
	{
		Gtk.Application.Quit ();
		a.RetVal = true;
	}
}
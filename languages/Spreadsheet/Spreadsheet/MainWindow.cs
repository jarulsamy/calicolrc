using System;
using Gtk;

public partial class MainWindow: Gtk.Window
{	
	
	public MainWindow (): base (Gtk.WindowType.Toplevel)
	{
		Build ();
		SpreadsheetWidget sheet = new SpreadsheetWidget();
		scrolledwindow1.AddWithViewport(sheet);
	}

	protected void OnDeleteEvent (object sender, DeleteEventArgs a)
	{
		Application.Quit ();
		a.RetVal = true;
	}
}

using System;
using Gtk;
using System.Collections.Generic;

public partial class MainWindow: Gtk.Window
{	
	
	public MainWindow (): base (Gtk.WindowType.Toplevel)
	{
		Build ();
		CalicoSpreadsheetDocument doc = new CalicoSpreadsheetDocument(null, null);
		SpreadsheetWidget sheet = new SpreadsheetWidget(doc);
		scrolledwindow1.AddWithViewport(sheet);
	}

	protected void OnDeleteEvent (object sender, DeleteEventArgs a)
	{
		Application.Quit ();
		a.RetVal = true;
	}
}

using System;
using Dinah;

public partial class MainWindow: Gtk.Window
{	
	public MainWindow (): base (Gtk.WindowType.Toplevel)
	{
		DinahWidget dinah = new DinahWidget();
		dinah.Show();
		this.Add(dinah);
		this.DefaultWidth = 570;
		this.DefaultHeight = 421;
	}
}

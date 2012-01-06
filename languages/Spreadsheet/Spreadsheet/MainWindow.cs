using System;
using Gtk;

public partial class MainWindow: Gtk.Window
{	
	Gtk.ListStore liststore;
	
	public MainWindow (): base (Gtk.WindowType.Toplevel)
	{
		Build ();
		
		// Environment table:
		Gtk.CellRendererText renderer = new Gtk.CellRendererText();
		renderer.Editable = true;
		renderer.Edited += (o, args) => OnRendererEdited(o, args, 0);
        treeview1.AppendColumn("Column 1", renderer, "text", 0);
		renderer = new Gtk.CellRendererText();
		renderer.Editable = true;
		renderer.Edited += (o, args) => OnRendererEdited(o, args, 1);
        treeview1.AppendColumn("Column 2", renderer, "text", 1);
		treeview1.Columns[0].SortIndicator = true;
		treeview1.Columns[1].SortIndicator = true;
		treeview1.Columns[0].Clickable = true;
		treeview1.Columns[1].Clickable = true;
		treeview1.Columns[0].SortColumnId = 0;
		treeview1.Columns[1].SortColumnId = 1;
		
		
        // Create a ListStore as the Model
        liststore = new Gtk.ListStore(typeof(string), typeof(string));
        treeview1.Model = liststore;
        treeview1.ShowAll();
		
		liststore.AppendValues(new object [] {"row1-1", "0"});
		liststore.AppendValues(new object [] {"row2-1", "1"});
		liststore.AppendValues(new object [] {"row3-1", "2"});
		liststore.AppendValues(new object [] {"row4-1", "3"});
		
	}

	void OnRendererEdited (object o, EditedArgs args, int column)
	{
		Gtk.TreeIter iter;
		liststore.GetIter(out iter, new Gtk.TreePath(args.Path));
		liststore.SetValue(iter, column, args.NewText);
	}
	
	protected void OnDeleteEvent (object sender, DeleteEventArgs a)
	{
		Application.Quit ();
		a.RetVal = true;
	}
}

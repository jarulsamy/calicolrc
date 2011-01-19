public class DragDropBug {
  public static Gtk.TargetEntry[] make_target() {
	return new Gtk.TargetEntry[] {new Gtk.TargetEntry("application/x-test", 0, 0)};
  }

  public static Gtk.TreeStore make_store() {
	Gtk.TreeStore store = new Gtk.TreeStore(typeof(string));
	store.AppendValues("<b>Control</b>");
	return store;
  }
  
  public static void Main(string [] args) {
	Gtk.Application.Init();

	Gtk.Window win = new Gtk.Window("Gtk Drag Drop Bug");
	win.DeleteEvent += new Gtk.DeleteEventHandler(delegate { 
		  Gtk.Application.Quit();
		});

	
	Gtk.TreeView treeview = new Gtk.TreeView();
	treeview.Model = make_store();
	
	Gtk.TreeViewColumn column = new Gtk.TreeViewColumn();
	column.Title = "Module";
	treeview.AppendColumn(column);
	Gtk.CellRendererText cell = new Gtk.CellRendererText();
	// Set item up as a drag source:
	treeview.EnableModelDragSource(
		Gdk.ModifierType.Button1Mask,
		make_target(), 
		Gdk.DragAction.Copy | Gdk.DragAction.Move);

	column.PackStart(cell, true);
	column.AddAttribute(cell, "markup", 0);
	Gtk.VBox layout = new Gtk.VBox();
	Gtk.EventBox block = new Gtk.EventBox();
	Gtk.Label label = new Gtk.Label("Start");
	block.Add(label);
	Gtk.Drag.DestSet(block, 
		Gtk.DestDefaults.All, 
		make_target(),
		Gdk.DragAction.Copy | Gdk.DragAction.Move);
	
	layout.PackStart(block, false, true, 0);
	block = new Gtk.EventBox();
	label = new Gtk.Label("End");
	block.Add(label);
	Gtk.Drag.DestSet(block, 
		Gtk.DestDefaults.All, 
		make_target(), 
		Gdk.DragAction.Copy | Gdk.DragAction.Move);
	layout.PackEnd(block, true, true, 0);
	Gtk.HBox hbox = new Gtk.HBox();
	hbox.PackStart(treeview, true, true, 0);
	hbox.PackStart(layout, true, true, 0);
	win.Add(hbox);
	win.ShowAll();

	Gtk.Application.Run();
  }
}

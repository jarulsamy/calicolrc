using System;
using Dinah;

public partial class MainWindow: Gtk.Window
{	
	public Gtk.Expander BeginArea {
		get { return expander4; }
	}
	
	public Gtk.VBox VBox {
		get { return vbox1; }
	}
		

	public MainWindow (): base (Gtk.WindowType.Toplevel)
	{
		Build ();
		//IfButton
		// Begin Drop Area:
		Gtk.Drag.DestSet (BeginArea, 
			Gtk.DestDefaults.All, Dinah.DBlock.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
        BeginArea.DragDataReceived += new Gtk.DragDataReceivedHandler (HandleLabelDragDataReceived);
		
		DCollection collection = new DCollection("Control");
		DBlock begin = new DBlock("Begin");	
		collection.Add(begin);
		DBlock ifblock = new DBlock("If");	
		collection.Add(ifblock);
		DBlock ifthenblock = new DBlock("If Then");	
		collection.Add(ifthenblock);
		DBlock whileblock = new DBlock("While");	
		collection.Add(whileblock);
		DBlock repeatblock = new DBlock("Repeat");	
		collection.Add(repeatblock);
		collection.ShowAll();
		collection.AddToVBox(VBox, 0);
	}
	
	// Drop object:
    private static void HandleLabelDragDataReceived (object sender, Gtk.DragDataReceivedArgs args)
    {
            if (args.SelectionData.Length >=0 && args.SelectionData.Format == 8) {
                    Console.WriteLine ("Received {0} in label", args.SelectionData);
                    Gtk.Drag.Finish (args.Context, true, false, args.Time);
            }
            Gtk.Drag.Finish (args.Context, false, false, args.Time);
    }

	protected void OnDeleteEvent (object sender, Gtk.DeleteEventArgs a)
	{
		Gtk.Application.Quit ();
		a.RetVal = true;
	}
}

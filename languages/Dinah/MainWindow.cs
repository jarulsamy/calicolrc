using System;

public partial class MainWindow: Gtk.Window
{	
   	enum TargetType {
            String,
            RootWindow
    };

	public Gtk.Button IfButton {
		get { return button1; }
	}

	public Gtk.Expander BeginArea {
		get { return expander4; }
	}

	private static Gtk.TargetEntry [] target_table = new Gtk.TargetEntry [] {
            new Gtk.TargetEntry ("STRING", 0, (uint) TargetType.String ),
            new Gtk.TargetEntry ("text/plain", 0, (uint) TargetType.String),
            new Gtk.TargetEntry ("application/x-rootwindow-drop", 0, (uint) TargetType.RootWindow)
    };

	public MainWindow (): base (Gtk.WindowType.Toplevel)
	{
		Build ();
		//IfButton
		Gtk.Drag.SourceSet (IfButton, 
				Gdk.ModifierType.Button1Mask | Gdk.ModifierType.Button3Mask,
                target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
		IfButton.DragDataGet += new Gtk.DragDataGetHandler (HandleSourceDragDataGet);
        IfButton.DragDataDelete += new Gtk.DragDataDeleteHandler (HandleSourceDragDataDelete);
		
		
		// Begin Drop Area:
		Gtk.Drag.DestSet (BeginArea, 
			Gtk.DestDefaults.All, target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
        BeginArea.DragDataReceived += new Gtk.DragDataReceivedHandler (HandleLabelDragDataReceived);		
	}
	
	// Drag object:
    private static void HandleSourceDragDataGet (object sender, Gtk.DragDataGetArgs args)
    {
        if (args.Info == (uint) TargetType.RootWindow)
            Console.WriteLine ("I was dropped on the rootwin");
        else
            args.SelectionData.Text = "I'm data!";
    }

	// Drag object:
    private static void HandleSourceDragDataDelete (object sender, Gtk.DragDataDeleteArgs args)
    {
        Console.WriteLine ("Delete the data!");
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

	protected void OnButton1DragBegin (object o, Gtk.DragBeginArgs args)
	{
		Console.WriteLine("DragBegin");
	}

	protected void OnButton1DragDataGet (object o, Gtk.DragDataGetArgs args)
	{
		Console.WriteLine("DragDataGet");
	}

	protected void OnButton1DragDataReceived (object o, Gtk.DragDataReceivedArgs args)
	{
		Console.WriteLine("DragDataReceived");
	}

	protected void OnButton1DragDrop (object o, Gtk.DragDropArgs args)
	{
		Console.WriteLine("DragDrop");
	}

	protected void OnButton1DragEnd (object o, Gtk.DragEndArgs args)
	{
		Console.WriteLine("DragEnd");
	}

	protected void OnButton1DragLeave (object o, Gtk.DragLeaveArgs args)
	{
		Console.WriteLine("DragLeave");
	}

	protected void OnButton1DragMotion (object o, Gtk.DragMotionArgs args)
	{
		Console.WriteLine("DragMotion");
	}

	protected void OnButton1DragDataDelete (object o, Gtk.DragDataDeleteArgs args)
	{
		Console.WriteLine("DragDataDelete");
	}
}

using System;
using Dinah;

public partial class MainWindow: Gtk.Window
{	
	int menu_position = 0;

	public Gtk.VBox Menu {
		get { return _menu; }
	}
		
	public Gtk.VBox Program {
		get { return _program; }
	}
		
	public MainWindow (): base (Gtk.WindowType.Toplevel)
	{
		Build ();
		//IfButton
		// Begin Drop Area:
		//Gtk.Drag.DestSet (BeginArea, 
		//	Gtk.DestDefaults.All, Dinah.DBlock.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
		//Gtk.Drag.DestSet (Frame, 
		//	Gtk.DestDefaults.All, Dinah.DBlock.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
		//Frame.DragDataReceived += new Gtk.DragDataReceivedHandler (HandleLabelDragDataReceived);

		//((Gtk.Viewport)((Gtk.ScrolledWindow)BeginArea.Child).Child).Child.DragDataReceived += new Gtk.DragDataReceivedHandler (HandleLabelDragDataReceived);
		//BeginArea.DragDataReceived += new Gtk.DragDataReceivedHandler (HandleLabelDragDataReceived);
		
		// Begin a Library:
		Category category = new Category("Control");
		StatementFactory block = new StatementFactory("Begin", "begin");	
		category.Add(block);
		block = new StatementFactory("If", "if");	
		category.Add(block);
		block = new StatementFactory("If Then", "ifthen");	
		category.Add(block);
		block = new StatementFactory("While", "while");	
		category.Add(block);
		block = new StatementFactory("Repeat", "repeat");	
		category.Add(block);
		category.ShowAll();
		category.AddToMenu(Menu, menu_position++);
		// End Library
		
		// Begin a Library:
		category = new Category("Variables");
		block = new StatementFactory("Let", "let");	
		category.Add(block);
		category.ShowAll();
		category.AddToMenu(Menu, menu_position++);
		// End Library

		// Begin a Library:
		category = new Category("Expressions");
		block = new ExpressionFactory("[expression]");	
		category.Add(block);
		category.ShowAll();
		category.AddToMenu(Menu, menu_position++);
		// End Library

		// Begin a Library:
		category = new Category("Shapes");
		block = new StatementFactory("draw()", "Shapes.draw");	
		category.Add(block);
		category.ShowAll();
		category.AddToMenu(Menu, menu_position++);
		// End Library

		// BEGIN Program Block
		Statement statement = new LetStatement("X");
		Program.Add( statement);
		Gtk.Box.BoxChild w7 = ((Gtk.Box.BoxChild)(Program[statement]));
		//w7.Position = 0;
		w7.Expand = false;
		w7.Fill = false;
		statement.Show();
		// END
		
		// BEGIN Program Block
		statement = new MethodStatement("draw()");
		Program.Add( statement);
		w7 = ((global::Gtk.Box.BoxChild)(Program[statement]));
		//w7.Position = 0;
		w7.Expand = false;
		w7.Fill = false;
		statement.Show();
		// END

		Gtk.Drag.DestSet(Program, Gtk.DestDefaults.All, TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
		Program.DragDataReceived += HandleProgramDragDataReceived;
		Program.Show();
	}

	void HandleProgramDragDataReceived (object sender, Gtk.DragDataReceivedArgs args)
	{
        Console.WriteLine ("Received: {0}", args.SelectionData.Text);
        Console.WriteLine ("From: {0}", sender);
		
	}
	
	// Drop object:
    private static void HandleLabelDragDataReceived (object sender, Gtk.DragDataReceivedArgs args)
    {
		// sender is what you dropped on
		// args.SelectionData.Text says what to make
        Console.WriteLine ("Received: {0}", args.SelectionData.Text);
        Console.WriteLine ("From: {0}", sender);
        if (args.SelectionData.Length >=0 && args.SelectionData.Format == 8) {
			// Gtk.SelectionData
			StatementFactory newblock = new StatementFactory(args.SelectionData.Text, "drop");
			newblock.ShowAll();
			if (sender as Gtk.Expander != null) {
				// Add to Expander's VBox
				Gtk.Expander expander = (Gtk.Expander)sender;
				Gtk.ScrolledWindow sw = (Gtk.ScrolledWindow)expander.Child;
				Gtk.Viewport vp = (Gtk.Viewport)sw.Child;
				Gtk.VBox vbox = (Gtk.VBox)vp.Child;
				Console.WriteLine("({0}, {1})", args.X, args.Y);
				newblock.DragDataReceived += new Gtk.DragDataReceivedHandler (HandleLabelDragDataReceived);

				vbox.PackStart(newblock, false, false, 0);
			} else if (sender as Gtk.VBox != null) {
				((Gtk.VBox)sender).Add(newblock);
			}
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

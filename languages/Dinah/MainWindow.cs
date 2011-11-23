using System;
using Dinah;

public partial class MainWindow: Gtk.Window
{	
	int menu_position = 0;

	public Gtk.VBox Menu {
		get { return _menu; }
	}
		
	public Gtk.VBox ProgramArea {
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
		category.ModifyBg(Gtk.StateType.Normal, Colors.PaleGoldenrod);
		StatementFactory block = new StatementFactory("Begin", "begin");	
		block.ModifyBg(Gtk.StateType.Normal, Colors.PaleGoldenrod);
		category.Add(block);
		block = new StatementFactory("If", "if");	
		block.ModifyBg(Gtk.StateType.Normal, Colors.PaleGoldenrod);
		category.Add(block);
		block = new StatementFactory("If Then", "ifthen");	
		block.ModifyBg(Gtk.StateType.Normal, Colors.PaleGoldenrod);
		category.Add(block);
		block = new StatementFactory("While", "while");	
		block.ModifyBg(Gtk.StateType.Normal, Colors.PaleGoldenrod);
		category.Add(block);
		block = new StatementFactory("Repeat", "repeat");	
		block.ModifyBg(Gtk.StateType.Normal, Colors.PaleGoldenrod);
		category.Add(block);
		category.ShowAll();
		category.AddToMenu(Menu, menu_position++);
		// End Library
		
		// Begin a Library:
		category = new Category("Variables");
		category.ModifyBg(Gtk.StateType.Normal, Colors.LightGreen);
		block = new StatementFactory("Let", "let");	
		block.ModifyBg(Gtk.StateType.Normal, Colors.LightGreen);

		category.Add(block);
		category.ShowAll();
		category.AddToMenu(Menu, menu_position++);
		// End Library

		// Begin a Library:
		category = new Category("Expressions");
		category.ModifyBg(Gtk.StateType.Normal, Colors.LightBlue);
		block = new ExpressionFactory("[expression]");	
		block.ModifyBg(Gtk.StateType.Normal, Colors.LightBlue);
		category.Add(block);
		category.ShowAll();
		category.AddToMenu(Menu, menu_position++);
		// End Library

		// Begin a Library:
		category = new Category("Shapes");
		category.ModifyBg(Gtk.StateType.Normal, Colors.LightYellow);
		block = new StatementFactory("draw()", "Shapes.draw");	
		block.ModifyBg(Gtk.StateType.Normal, Colors.LightYellow);
		category.Add(block);
		category.ShowAll();
		category.AddToMenu(Menu, menu_position++);
		// End Library

		// BEGIN Program Block
		Statement statement = new LetStatement("X");
		ProgramArea.Add( statement);
		Gtk.Box.BoxChild w7 = ((Gtk.Box.BoxChild)(ProgramArea[statement]));
		//w7.Position = 0;
		w7.Expand = false;
		w7.Fill = false;
		statement.Show();
		// END
		
		// BEGIN Program Block
		statement = new MethodStatement("draw()");
		ProgramArea.Add( statement);
		w7 = ((global::Gtk.Box.BoxChild)(ProgramArea[statement]));
		//w7.Position = 0;
		w7.Expand = false;
		w7.Fill = false;
		statement.Show();
		// END

		// BEGIN Control Block
		statement = new IfControlStatement();
		ProgramArea.Add( statement);
		w7 = ((global::Gtk.Box.BoxChild)(ProgramArea[statement]));
		//w7.Position = 0;
		w7.Expand = false;
		w7.Fill = false;
		statement.Show();
		// END

		// BEGIN Control Block
		statement = new IfThenControlStatement();
		ProgramArea.Add( statement);
		w7 = ((global::Gtk.Box.BoxChild)(ProgramArea[statement]));
		//w7.Position = 0;
		w7.Expand = false;
		w7.Fill = false;
		statement.Show();
		// END

		Gtk.Drag.DestSet(ProgramArea, Gtk.DestDefaults.All, TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
		ProgramArea.DragDataReceived += HandleProgramDragDataReceived;
		ProgramArea.Show();
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

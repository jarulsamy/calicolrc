using System;
using Dinah;

public partial class MainWindow: Gtk.Window
{	
	int menu_position = 0;

	public Gtk.VBox Menu {
		get { return _menu; }
	}
		
	public Gtk.ScrolledWindow ProgramScrolledWindow {
		get { return scrolledwindow2; }
	}
		
	public MainWindow (): base (Gtk.WindowType.Toplevel)
	{
		Build ();
		
		// Begin a Library:
		Category category = new Category("Control");
		category.ModifyBg(Gtk.StateType.Normal, Colors.PaleGoldenrod);
		StatementFactory block = new StatementFactory("If", "if");	
		Program.registerStatement("if", () => new IfControlStatement());
		block.ModifyBg(Gtk.StateType.Normal, Colors.PaleGoldenrod);
		category.Add(block);
		
		block = new StatementFactory("If Else", "ifelse");	
		Program.registerStatement("ifelse", () => new IfThenControlStatement());
		block.ModifyBg(Gtk.StateType.Normal, Colors.PaleGoldenrod);
		category.Add(block);
		
		block = new StatementFactory("While", "while");	
		Program.registerStatement("while", () => new Statement("while", "while"));
		block.ModifyBg(Gtk.StateType.Normal, Colors.PaleGoldenrod);
		category.Add(block);
		
		block = new StatementFactory("Repeat", "repeat");	
		Program.registerStatement("repeat", () => new Statement("repeat", "repeat"));
		block.ModifyBg(Gtk.StateType.Normal, Colors.PaleGoldenrod);
		category.Add(block);
		
		category.ShowAll();
		category.AddToMenu(Menu, menu_position++);
		// End Library
		
		// Begin a Library:
		category = new Category("Variables");
		category.ModifyBg(Gtk.StateType.Normal, Colors.LightGreen);
		block = new StatementFactory("Let", "let");	
		Program.registerStatement("let", () => new LetStatement("X"));
		block.ModifyBg(Gtk.StateType.Normal, Colors.LightGreen);
		category.Add(block);
		category.ShowAll();
		category.AddToMenu(Menu, menu_position++);
		// End Library

		// Begin a Library:
		category = new Category("Expressions");
		category.ModifyBg(Gtk.StateType.Normal, Colors.LightBlue);
		block = new ExpressionFactory("[expression]");	
		Program.registerStatement("expression", () => new Statement("expression", "expression"));

		block.ModifyBg(Gtk.StateType.Normal, Colors.LightBlue);
		category.Add(block);
		category.ShowAll();
		category.AddToMenu(Menu, menu_position++);
		// End Library

		// Begin a Library:
		category = new Category("Shapes");
		category.ModifyBg(Gtk.StateType.Normal, Colors.LightYellow);
		block = new StatementFactory("draw()", "Shapes.draw");	
		Program.registerStatement("Shapes.draw", () => new MethodStatement("draw()"));
		block.ModifyBg(Gtk.StateType.Normal, Colors.LightYellow);
		category.Add(block);
		category.ShowAll();
		category.AddToMenu(Menu, menu_position++);
		// End Library
		
		Program program = new Program();
		ProgramScrolledWindow.AddWithViewport(program);		
	}
}

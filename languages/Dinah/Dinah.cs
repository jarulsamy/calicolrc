using System;
using System.Collections.Generic;

namespace Dinah
{

	public static class Colors {
		public static readonly Gdk.Color White          = makeColor(1.0,    1.0,    1.0);
		public static readonly Gdk.Color Silver         = makeColor(0.75,   0.75,   0.75);
		public static readonly Gdk.Color Gray           = makeColor(0.5,    0.5,    0.5);
		public static readonly Gdk.Color LightGray      = makeColor(0.8242, 0.8242, 0.8242);
		public static readonly Gdk.Color DarkGray       = makeColor(0.6601, 0.6601, 0.6601);
		public static readonly Gdk.Color SlateGray      = makeColor(0.4375, 0.5,    0.5625);
		public static readonly Gdk.Color DarkSlateGray  = makeColor(0.1562, 0.3086, 0.3086);
		public static readonly Gdk.Color LightSlateGray = makeColor(0.4648, 0.5312, 0.5977);
		public static readonly Gdk.Color WhiteSmoke     = makeColor(0.9570, 0.9570, 0.9570);
		public static readonly Gdk.Color Black          = makeColor(0.0,    0.0,    0.0);
		public static readonly Gdk.Color Yellow         = makeColor(1.0,    1.0,    0.0);
		public static readonly Gdk.Color LightYellow    = makeColor(1.0,    1.0,    0.875);
		public static readonly Gdk.Color DarkGoldenrod  = makeColor(0.7187, 0.5234, 0.0430);
		public static readonly Gdk.Color PaleGoldenrod  = makeColor(0.9297, 0.9062, 0.6641);
		public static readonly Gdk.Color Honeydew       = makeColor(0.9375, 1.0,    0.9375);
		public static readonly Gdk.Color LightBlue      = makeColor(0.6758, 0.8437, 0.8984);
		public static readonly Gdk.Color DarkBlue       = makeColor(0.0,    0.0,    0.5430);
		public static readonly Gdk.Color Red            = makeColor(1.0,    0.0,    0.0);
		public static readonly Gdk.Color DarkRed        = makeColor(0.5430, 0.0,    0.0);
		public static readonly Gdk.Color LightPink      = makeColor(1.0,    0.7109, 0.7539);
		public static readonly Gdk.Color DarkGreen      = makeColor(0.0,    0.3910, 0.0);
		public static readonly Gdk.Color LightGreen     = makeColor(0.5625, 0.9297, 0.5625);
		
		public static Gdk.Color makeColor(double r, double g, double b) {
			return new Gdk.Color((byte)(r * 255), (byte)(g * 255), (byte)(b * 255));
		}
	}
	
	public class DinahWidget : Gtk.Frame {
		private global::Gtk.HPaned hpaned1;
		private global::Gtk.VBox vbox7;
		private global::Gtk.ScrolledWindow scrolledwindow1;
		private global::Gtk.VBox vbox6;
		private global::Gtk.VBox _menu;
		private global::Gtk.Button button8;
		private global::Gtk.ScrolledWindow scrolledwindow2;

		int menu_position = 0;
	
		public Gtk.VBox Menu {
			get { return _menu; }
		}
			
		public Gtk.ScrolledWindow ProgramScrolledWindow {
			get { return scrolledwindow2; }
		}
		
		public Gtk.Button Trashcan {
			get { return button8; }
		}
		
		public DinahWidget() : base() {
			this.hpaned1 = new global::Gtk.HPaned ();
			this.hpaned1.CanFocus = true;
			this.hpaned1.Name = "hpaned1";
			this.hpaned1.Position = 143;
			// Container child hpaned1.Gtk.Paned+PanedChild
			this.vbox7 = new global::Gtk.VBox ();
			this.vbox7.Name = "vbox7";
			this.vbox7.Spacing = 6;
			// Container child vbox7.Gtk.Box+BoxChild
			this.scrolledwindow1 = new global::Gtk.ScrolledWindow ();
			this.scrolledwindow1.CanFocus = true;
			this.scrolledwindow1.Name = "scrolledwindow1";
			this.scrolledwindow1.ShadowType = ((global::Gtk.ShadowType)(1));
			// Container child scrolledwindow1.Gtk.Container+ContainerChild
			global::Gtk.Viewport w1 = new global::Gtk.Viewport ();
			w1.ShadowType = ((global::Gtk.ShadowType)(0));
			// Container child GtkViewport.Gtk.Container+ContainerChild
			this.vbox6 = new global::Gtk.VBox ();
			this.vbox6.Name = "vbox6";
			this.vbox6.Spacing = 6;
			// Container child vbox6.Gtk.Box+BoxChild
			this._menu = new global::Gtk.VBox ();
			this._menu.Name = "_menu";
			this._menu.Spacing = 6;
			this.vbox6.Add (this._menu);
			global::Gtk.Box.BoxChild w2 = ((global::Gtk.Box.BoxChild)(this.vbox6 [this._menu]));
			w2.Position = 0;
			w1.Add (this.vbox6);
			this.scrolledwindow1.Add (w1);
			this.vbox7.Add (this.scrolledwindow1);
			global::Gtk.Box.BoxChild w5 = ((global::Gtk.Box.BoxChild)(this.vbox7 [this.scrolledwindow1]));
			w5.Position = 0;
			// Container child vbox7.Gtk.Box+BoxChild
			//"stock_trash_full", "gtk-justify-fill"
			Gtk.Image image = new Gtk.Image("gtk-delete", Gtk.IconSize.Button);
			image.Show();
			this.button8 = new Gtk.Button (image);
			//this.button8.WidthRequest = 30;
			this.button8.HeightRequest = 60;
			this.button8.CanFocus = true;
			this.button8.Name = "button8";
			this.button8.UseUnderline = true;
			this.button8.Show();
			// Container child button8.Gtk.Container+ContainerChild
			//global::Gtk.Alignment w6 = new global::Gtk.Alignment (0.5F, 0.5F, 0F, 0F);
			// Container child GtkAlignment.Gtk.Container+ContainerChild
			//global::Gtk.HBox w7 = new global::Gtk.HBox ();
			//w7.Spacing = 2;
			// Container child GtkHBox.Gtk.Container+ContainerChild
			//global::Gtk.Image w8 = new global::Gtk.Image ();
			//w8.Pixbuf = global::Stetic.IconLoader.LoadIcon (this, "stock_trash_full", global::Gtk.IconSize.Dialog);
			//w7.Add (w8);
			// Container child GtkHBox.Gtk.Container+ContainerChild
			//global::Gtk.Label w10 = new global::Gtk.Label ();
			//w7.Add (w10);
			//w6.Add (w7);
			//this.button8.Add (w6);
			this.vbox7.Add (this.button8);
			global::Gtk.Box.BoxChild w14 = ((global::Gtk.Box.BoxChild)(this.vbox7 [this.button8]));
			w14.Position = 1;
			w14.Expand = false;
			w14.Fill = true;
			this.hpaned1.Add (this.vbox7);
			global::Gtk.Paned.PanedChild w15 = ((global::Gtk.Paned.PanedChild)(this.hpaned1 [this.vbox7]));
			w15.Resize = false;
			// Container child hpaned1.Gtk.Paned+PanedChild
			this.scrolledwindow2 = new global::Gtk.ScrolledWindow ();
			this.scrolledwindow2.CanFocus = true;
			this.scrolledwindow2.Name = "scrolledwindow2";
			this.scrolledwindow2.ShadowType = ((global::Gtk.ShadowType)(1));
			this.hpaned1.Add (this.scrolledwindow2);
			this.Add (this.hpaned1);
			if ((this.Child != null)) {
				this.Child.ShowAll ();
			}
			
		Gtk.Drag.DestSet(Trashcan, Gtk.DestDefaults.All, TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
		Trashcan.DragDataReceived += HandleTrashDragDataReceived;
		
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
		
		void HandleTrashDragDataReceived (object sender, Gtk.DragDataReceivedArgs args)
		{
			string [] data = args.SelectionData.Text.Split(':');
			if (data[0] == "move") {
				Statement statement = Program.lookup[data[1]];
				statement.RemoveFromProgram();
				Program.lookup.Remove(statement.BlockID);
	        	Gtk.Drag.Finish (args.Context, true, false, args.Time);
			}		
		}
		
	}
	
	public class TargetTable {
		public static Gtk.TargetEntry [] target_table = new Gtk.TargetEntry [] {
            new Gtk.TargetEntry ("STRING", 0, (uint) TargetType.String ),
	    };		
	}
	
	public enum TargetType {
            String,
    };
	
	public class EmptyExpression : Gtk.Button {
		public EmptyExpression(string text) : base(text) {
			ModifyBg(Gtk.StateType.Normal, Colors.LightPink);
			Gtk.Drag.DestSet(this, Gtk.DestDefaults.All, TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
			DragDataReceived += HandleDragDataReceived;
		}

		private static void HandleDragDataReceived (object sender, Gtk.DragDataReceivedArgs args)
	    {
			// sender is what you dropped on
			// args.SelectionData.Text says what to make
	        Console.WriteLine ("Empty Expression Received: {0}", args.SelectionData.Text);
	        Console.WriteLine ("Empty Expression From: {0}", sender);
            //Gtk.Drag.Finish (args.Context, true, false, args.Time);			
		}
	}
}


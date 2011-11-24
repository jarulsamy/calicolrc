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
		
	public class Statement : Gtk.Frame {
		public Gtk.HBox hbox;
		public Gtk.Button gripper;
		public Gtk.Alignment alignment;
		public Gtk.EventBox eventbox;
		public string label;
		public string icon_string = "gtk-justify-fill";
		
		public Statement(string label) : base() {
			// Hbox [ Gipper, ...]
			eventbox = new Gtk.EventBox();
			//this.modify
			hbox = new Gtk.HBox(false, 3);
			hbox.Show();
			Gtk.Image image = new Gtk.Image(icon_string, Gtk.IconSize.Button);
			image.Show();
			gripper = new Gtk.Button(image);
			gripper.HeightRequest = 30;
			gripper.Relief = ((Gtk.ReliefStyle)(2));
			gripper.CanFocus = false;
			gripper.FocusOnClick = false;
			gripper.Xalign = 0;
			gripper.Yalign = 0;
			//gripper
			gripper.Show();
			hbox.PackStart(gripper, false, false, 0);
			Gtk.Box.BoxChild w6 = ((global::Gtk.Box.BoxChild)(hbox[gripper]));
			w6.Position = 0;
			w6.Expand = false;
			w6.Fill = false;
			// Label
			if (label != null) {
				Gtk.Label _label = new Gtk.Label(label);
				_label.Show();
				hbox.PackStart(_label, false, false, 0);
			}
			alignment = new global::Gtk.Alignment (0F, 1F, 1F, 1F);
			alignment.Add(hbox);
			alignment.Show();
			eventbox.Show();
			eventbox.Add(alignment);
			Add(eventbox);
			BorderWidth = 3;
			//alignment.LeftPadding = ((uint)(12)); // gripper from left size
			// Setup Dragging:
			Gtk.Drag.SourceSet (this, 
				Gdk.ModifierType.Button1Mask | Gdk.ModifierType.Button3Mask,
            	TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
			// Setup Drop:
			//Gtk.Drag.DestSet(this, Gtk.DestDefaults.All, TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
			//DragDataReceived += HandleDragDataReceived;
		}
		
	    protected static void HandleDragDataReceived (object sender, Gtk.DragDataReceivedArgs args)
	    {
			// sender is what you dropped on
			// args.SelectionData.Text says what to make
			// FIXME: should drop before or after statement
			//Statement statement = Program.makeStatement(args.SelectionData.Text); 
			//((Program)((Gtk.VBox)((Statement)sender).Parent).Parent).AddStatement(statement);
            //Gtk.Drag.Finish (args.Context, true, false, args.Time);
	    }
		
	}
	
	public class LetStatement : Statement {
		public LetStatement(string label) : base("Let") {
			Gtk.Entry variable = new Gtk.Entry("X");
			eventbox.ModifyBg(Gtk.StateType.Normal, Colors.LightGreen);
			ModifyBg(Gtk.StateType.Normal, Colors.DarkGreen);
			variable.Show();
			variable.WidthChars = 5;
			hbox.PackStart(variable, false, false, 0);
			Gtk.Button expression = new Gtk.Button("[expression]");
			expression.ModifyBg(Gtk.StateType.Normal, Colors.LightPink);
			Gtk.Drag.DestSet(expression, Gtk.DestDefaults.All, TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
			expression.DragDataReceived += HandleDragDataReceived;			
			Gtk.Drag.DestSet(this, Gtk.DestDefaults.All, TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
			DragDataReceived += HandleDragDataReceived;
			expression.Show();
			hbox.PackStart(expression, true, true, 0);
		}
	}
	
	public class MethodStatement : Statement {
		public MethodStatement(string label) : base(null) {
			eventbox.ModifyBg(Gtk.StateType.Normal, Colors.LightYellow);
			Gtk.Expander expander = new Gtk.Expander(label);
			//Gtk.Box.BoxChild w30 = ((Gtk.Box.BoxChild)(hbox[expander]));
			Gtk.Alignment alignment = new Gtk.Alignment (0F, .75F, 1F, 0F);
			alignment.Show();
			expander.Show();
			// for each parameter:
			Gtk.VBox parameters = new Gtk.VBox();
			Gtk.HBox parameter = new Gtk.HBox();
			Gtk.Label param = new Gtk.Label("shape:");
			param.Xalign = 1.0f;
			parameter.PackStart(param, true, true, 3);
			Gtk.Button button = new Gtk.Button("[shape]");
			button.ModifyBg(Gtk.StateType.Normal, Colors.LightPink);
			Gtk.Drag.DestSet(button, Gtk.DestDefaults.All, TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
			button.DragDataReceived += HandleDragDataReceived;			
			parameter.PackStart(button, true, true, 0);
			Gtk.Drag.DestSet(button, Gtk.DestDefaults.All, TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
			DragDataReceived += HandleDragDataReceived;
			parameters.Add(parameter);
			parameter.ShowAll();
			parameter = new Gtk.HBox();
			param = new Gtk.Label("window:");
			param.Xalign = 1.0f;
			parameter.PackStart(param, true, true, 3);
			button = new Gtk.Button("[window]");
			button.ModifyBg(Gtk.StateType.Normal, Colors.LightPink);
			Gtk.Drag.DestSet(button, Gtk.DestDefaults.All, TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
			button.DragDataReceived += HandleDragDataReceived;
			parameter.PackStart(button, true, true, 0);
			Gtk.Drag.DestSet(button, Gtk.DestDefaults.All, TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
			button.DragDataReceived += HandleDragDataReceived;
			parameters.Add(parameter);
			parameter.ShowAll();
			parameters.Show();
			expander.Add(parameters);
			alignment.Add(expander);
			hbox.Add(alignment);
		}
	}
	
	public class IfControlStatement : Statement {
		public IfControlStatement() : base(null) {
			eventbox.ModifyBg(Gtk.StateType.Normal, Colors.LightBlue);
			Gtk.Expander expander = new Gtk.Expander("Do:");
			//Gtk.Box.BoxChild w30 = ((Gtk.Box.BoxChild)(hbox[expander]));
			Gtk.Alignment alignment = new Gtk.Alignment (0F, .75F, 1F, 0F);
			alignment.Show();
			expander.Show();
			// for Do block:
			alignment.Add(expander);
			Gtk.VBox vbox = new Gtk.VBox();
			vbox.Show();
			Gtk.Label labelWidget = new Gtk.Label("If");
			labelWidget.Xalign = 0;
			labelWidget.Show();
			Gtk.HBox topLine = new Gtk.HBox();
			topLine.Show();
			topLine.PackStart(labelWidget, false, false, 6);
			Gtk.Entry boolean = new Gtk.Entry("True");
			topLine.PackStart(boolean);
			boolean.Show();
			vbox.PackStart(topLine);
			hbox.PackStart(vbox);
			vbox.Add(alignment);
			Program program = new Program();		
			expander.Add( program);
		}
	}

	public class IfThenControlStatement : Statement {
		public IfThenControlStatement() : base(null) {
			eventbox.ModifyBg(Gtk.StateType.Normal, Colors.LightBlue);
			Gtk.Expander expander = new Gtk.Expander("Do:");
			//Gtk.Box.BoxChild w30 = ((Gtk.Box.BoxChild)(hbox[expander]));
			Gtk.Alignment alignment = new Gtk.Alignment (0F, .75F, 1F, 0F);
			alignment.Show();
			expander.Show();
			// for Do block:
			alignment.Add(expander);
			Gtk.VBox vbox = new Gtk.VBox();
			vbox.Show();
			Gtk.Label labelWidget = new Gtk.Label("If");
			labelWidget.Xalign = 0;
			labelWidget.Show();
			Gtk.HBox topLine = new Gtk.HBox();
			topLine.Show();
			topLine.PackStart(labelWidget, false, false, 6);
			Gtk.Entry boolean = new Gtk.Entry("True");
			topLine.PackStart(boolean);
			boolean.Show();
			vbox.PackStart(topLine);
			hbox.PackStart(vbox);
			vbox.Add(alignment);
			Program program = new Program();
			expander.Add(program);

			// Else:
			Gtk.Expander else_expander = new Gtk.Expander("Else Do:");
			//Gtk.Box.BoxChild w30 = ((Gtk.Box.BoxChild)(hbox[expander]));
			Gtk.Alignment else_alignment = new Gtk.Alignment (0F, .75F, 1F, 0F);
			else_alignment.Show();
			else_expander.Show();

			// for Do block:
			else_alignment.Add(else_expander);
			vbox.Add(else_alignment);
			program = new Program();
			else_expander.Add(program);
		}
	}

}


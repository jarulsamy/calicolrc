
using System;
using System.Collections.Generic;
namespace Dinah
{
	public class Statement : Gtk.Frame {
		public Gtk.HBox hbox;
		public Gtk.Button gripper;
		public Gtk.Alignment alignment;
		public Gtk.EventBox eventbox;
		public string label;
		public string icon_string = "gtk-justify-fill";
		public string BlockType;
		public string BlockID;
		public Program myProgram;

		public void HandleGripperDragDataGet (object sender, Gtk.DragDataGetArgs args)
		{
			// Sender is the gripper
			Console.WriteLine("HandleSourceDragDataGet {0}", sender);
            Byte [] data = System.Text.Encoding.UTF8.GetBytes("move:" + this.BlockID);
            Gdk.Atom [] targets = args.Context.Targets;
			args.SelectionData.Set(targets[0], 8, data, data.Length);
		}

		public Statement(string label, string block_type) : base() {
			// Hbox [ Gipper, ...]
			BlockType = block_type;
			BlockID = Program.makeID();
			Program.lookup[BlockID] = this;
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
			Gtk.Drag.SourceSet (gripper, 
				Gdk.ModifierType.Button1Mask | Gdk.ModifierType.Button3Mask,
            	TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
			gripper.DragDataGet += new Gtk.DragDataGetHandler (HandleGripperDragDataGet);
						
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
			Gtk.Drag.DestSet(this, Gtk.DestDefaults.All, TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
			DragDataReceived += HandleDragDataReceived;
		}

		public void RemoveFromProgram() {
			myProgram.vbox.Remove(this);
			if (myProgram.vbox.Children.Length == 1) // the hidden label
				myProgram.label.Show();
		}
		
	    protected static void HandleDragDataReceived (object sender, Gtk.DragDataReceivedArgs args)
	    {
			// Drop on a Statement 
			// ---------------------------------------------------
			// sender is what you dropped on
			// args.SelectionData.Text says what to make
			if (sender is Statement) {
				string [] data = args.SelectionData.Text.Split(':');
				if (data[0] == "create") {
					Statement statement = Program.makeStatement(data[1]); 
					Program myprogram = ((Statement)sender).myProgram;
					myprogram.AddStatement(statement, (Statement)sender, args.X, args.Y);
	            	Gtk.Drag.Finish (args.Context, true, false, args.Time);
				} else if (data[0] == "move") {
					((Statement)sender).myProgram.MoveStatement(data[1], (Statement)sender, args.X, args.Y);
	            	Gtk.Drag.Finish (args.Context, true, false, args.Time);
				}
			} else {
				// FIXME: can it be an expression here?
				//Expression = 
			}
	    }
		
	}
	
	public class LetStatement : Statement {
		public LetStatement(string label) : base("Let", "let") {
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
			expression.Show();
			hbox.PackStart(expression, true, true, 0);
		}
	}
	
	public class MethodStatement : Statement {
		public MethodStatement(string label) : base(null, "method") {
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
		public IfControlStatement() : base(null, "if") {
			eventbox.ModifyBg(Gtk.StateType.Normal, Colors.PaleGoldenrod);
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
		public IfThenControlStatement() : base(null, "ifelse") {
			eventbox.ModifyBg(Gtk.StateType.Normal, Colors.PaleGoldenrod);
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

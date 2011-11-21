using System;

namespace Dinah
{

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
			Gtk.Drag.DestSet(this, Gtk.DestDefaults.All, TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
			DragDataReceived += HandleDragDataReceived;
		}

		private static void HandleDragDataReceived (object sender, Gtk.DragDataReceivedArgs args)
	    {
			// sender is what you dropped on
			// args.SelectionData.Text says what to make
	        Console.WriteLine ("Received: {0}", args.SelectionData.Text);
	        Console.WriteLine ("From: {0}", sender);
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
			eventbox.ModifyBg(Gtk.StateType.Normal, new Gdk.Color(128, 128, 0));
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
			//gripper
			gripper.Show();
			hbox.Add(gripper);
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

			//this.DragDataReceived += new Gtk.DragDataReceivedHandler (HandleLabelDragDataReceived);
			//DragDataGet += new Gtk.DragDataGetHandler (HandleSourceDragDataGet);
    	    //DragDataDelete += new Gtk.DragDataDeleteHandler (HandleSourceDragDataDelete);
			//DragDataReceived += new Gtk.DragDataReceivedHandler (OnButton1DragDataReceived);
		}
		
	    protected static void HandleDragDataReceived (object sender, Gtk.DragDataReceivedArgs args)
	    {
			// sender is what you dropped on
			// args.SelectionData.Text says what to make
	        Console.WriteLine ("Received: {0}", args.SelectionData.Text);
	        Console.WriteLine ("From: {0}", sender);
			/*
	        if (args.SelectionData.Length >=0 && args.SelectionData.Format == 8) {
				// Gtk.SelectionData
				DBlock newblock = new DBlock(args.SelectionData.Text, "drop");
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
	        */
	        Gtk.Drag.Finish (args.Context, false, false, args.Time);
	    }
		
	}
	
	public class LetStatement : Statement {
		public LetStatement(string label) : base("Let") {
			Gtk.Entry variable = new Gtk.Entry("X");
			variable.Show();
			variable.WidthChars = 5;
			hbox.PackStart(variable, false, false, 0);
			Gtk.Button expression = new Gtk.Button("[expression]");
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
			Gtk.Expander expander = new Gtk.Expander(label);
			//Gtk.Box.BoxChild w30 = ((Gtk.Box.BoxChild)(hbox[expander]));
			Gtk.Alignment alignment = new Gtk.Alignment (0F, .75F, 1F, 0F);
			alignment.Show();
			expander.Show();
			// for each parameter:
			Gtk.VBox parameters = new Gtk.VBox();
			Gtk.HBox parameter = new Gtk.HBox();
			parameter.PackStart(new Gtk.Label("shape"), true, true, 0);
			Gtk.Button button = new Gtk.Button("[shape]");
			Gtk.Drag.DestSet(button, Gtk.DestDefaults.All, TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
			button.DragDataReceived += HandleDragDataReceived;			
			parameter.PackStart(button, true, true, 0);
			Gtk.Drag.DestSet(button, Gtk.DestDefaults.All, TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
			DragDataReceived += HandleDragDataReceived;
			parameters.Add(parameter);
			parameter.ShowAll();
			parameter = new Gtk.HBox();
			parameter.PackStart(new Gtk.Label("window"), true, true, 0);
			button = new Gtk.Button("[window]");
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
	
	public class Category : Gtk.Expander {
		Gtk.VBox vbox;
		
		public Category(string label) : base(label) {
			CanFocus = true;
			Expanded = false;
			// Container child expander1.Gtk.Container+ContainerChild
			vbox = new Gtk.VBox ();
			vbox.Spacing = 3;
			Gtk.Label GtkLabel3 = new Gtk.Label ();
			GtkLabel3.LabelProp = Mono.Unix.Catalog.GetString (label);
			GtkLabel3.UseUnderline = true;
			this.LabelWidget = GtkLabel3;
			vbox.ShowAll();
			this.Add(vbox);
		}
		
		public void Add(StatementFactory block) {
			block.Show();
			vbox.Add(block);
		}
		
		public void AddToMenu(Gtk.VBox vbox, int position) {
			this.Show();
			vbox.Add(this);
			Gtk.Box.BoxChild w30 = ((Gtk.Box.BoxChild)(vbox[this]));
			w30.Position = position;
			w30.Expand = false;
			w30.Fill = false;
		}
	}

	public class StatementFactory : Gtk.Button
	{
		string BlockType;
		string icon_string = "gtk-justify-fill";

		public StatementFactory (string label, string type) : base() 
		{
			BlockType = type;
			CanFocus = true;
			UseUnderline = true;
			Xalign = 0.15F;			
			ModifyBg(Gtk.StateType.Normal, new Gdk.Color(128, 128, 0));
			Gtk.Alignment w2 = new Gtk.Alignment (0.15F, 0.5F, 0F, 0F);
			// Container child GtkAlignment.Gtk.Container+ContainerChild
			Gtk.HBox w3 = new Gtk.HBox ();
			w3.Spacing = 2;
			// Container child GtkHBox.Gtk.Container+ContainerChild
			Gtk.Image w4 = new Gtk.Image (icon_string, Gtk.IconSize.Button);
			w3.Add (w4);
			// Container child GtkHBox.Gtk.Container+ContainerChild
			global::Gtk.Label w6 = new global::Gtk.Label ();
			w6.LabelProp = global::Mono.Unix.Catalog.GetString (label);
			w6.UseUnderline = true;
			w3.Add (w6);
			w2.Add (w3);
			this.Add (w2);	
			
			
			Gtk.Drag.SourceSet (this, 
				Gdk.ModifierType.Button1Mask | Gdk.ModifierType.Button3Mask,
            	TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
			DragDataGet += new Gtk.DragDataGetHandler (HandleSourceDragDataGet);
    	    DragDataDelete += new Gtk.DragDataDeleteHandler (HandleSourceDragDataDelete);
			DragDataReceived += new Gtk.DragDataReceivedHandler (OnButton1DragDataReceived);
		}
		
		// Drag object:
	    private static void HandleSourceDragDataDelete (object sender, Gtk.DragDataDeleteArgs args)
	    {
	        Console.WriteLine ("Delete the data!");
	    }
		
		// Drag object:
	    private static void HandleSourceDragDataGet (object sender, Gtk.DragDataGetArgs args)
	    {
			Console.WriteLine("HandleSourceDragDataGet {0}", sender);
			// sender is object being dropped
			// Set args to be what you want to create
	        //if (args.Info == (uint) TargetType.RootWindow)
	        //    Console.WriteLine ("I was dropped on the rootwin");
	        //else
	        //args.SelectionData.Text = "create:" + ((DBlock)sender).BlockType;
			//UriList list = new UriList (SelectedPhotos ());	
            Byte [] data = System.Text.Encoding.UTF8.GetBytes(((StatementFactory)sender).BlockType);
            Gdk.Atom [] targets = args.Context.Targets;
			args.SelectionData.Set(targets[0], 8, data, data.Length);
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
	
	class ExpressionFactory : StatementFactory {
		public string icon_string = "stock_dialog-info";
		
		public ExpressionFactory(string label) : base(label, "expression") {
		}

	}
	
}


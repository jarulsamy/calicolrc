using System;

namespace Dinah
{
   	public enum TargetType {
            String,
            RootWindow
    };
	
	public class DCollection : Gtk.Expander {
		Gtk.VBox vbox;
		
		public DCollection(string label) : base(null) {
			CanFocus = true;
			Expanded = true;
			// Container child expander1.Gtk.Container+ContainerChild
			vbox = new Gtk.VBox ();
			vbox.Spacing = 6;
			Gtk.Label GtkLabel3 = new Gtk.Label ();
			GtkLabel3.LabelProp = Mono.Unix.Catalog.GetString (label);
			GtkLabel3.UseUnderline = true;
			this.LabelWidget = GtkLabel3;
			this.Add(vbox);
		}
		
		public void Add(DBlock block) {
			vbox.Add(block);
		}
	
		
		public void AddToVBox(Gtk.VBox vbox, int position) {
			vbox.Add(this);
			Gtk.Box.BoxChild w30 = ((Gtk.Box.BoxChild)(vbox[this]));
			w30.Position = position;
			w30.Expand = false;
			w30.Fill = false;
		}
	}

	public class DBlock : Gtk.Button
	{

		public static Gtk.TargetEntry [] target_table = new Gtk.TargetEntry [] {
            new Gtk.TargetEntry ("STRING", 0, (uint) TargetType.String ),
            new Gtk.TargetEntry ("text/plain", 0, (uint) TargetType.String),
            new Gtk.TargetEntry ("application/x-rootwindow-drop", 0, (uint) TargetType.RootWindow)
	    };

		
		public DBlock (string label) : base() 
		{
			CanFocus = true;
			UseUnderline = true;
			Xalign = 0.15F;			
			
			Gtk.Alignment w2 = new Gtk.Alignment (0.15F, 0.5F, 0F, 0F);
			// Container child GtkAlignment.Gtk.Container+ContainerChild
			Gtk.HBox w3 = new Gtk.HBox ();
			w3.Spacing = 2;
			// Container child GtkHBox.Gtk.Container+ContainerChild
			Gtk.Image w4 = new Gtk.Image ();
			//w4.Pixbuf = Stetic.IconLoader.LoadIcon(this, "gtk-sort-ascending", Gtk.IconSize.Menu);
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
            	target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
			DragDataGet += new Gtk.DragDataGetHandler (HandleSourceDragDataGet);
    	    DragDataDelete += new Gtk.DragDataDeleteHandler (HandleSourceDragDataDelete);
		}
		
		// Drag object:
	    private static void HandleSourceDragDataDelete (object sender, Gtk.DragDataDeleteArgs args)
	    {
	        Console.WriteLine ("Delete the data!");
	    }
		
		// Drag object:
	    private static void HandleSourceDragDataGet (object sender, Gtk.DragDataGetArgs args)
	    {
	        if (args.Info == (uint) TargetType.RootWindow)
	            Console.WriteLine ("I was dropped on the rootwin");
	        else
	            args.SelectionData.Text = "I'm data!";
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
}


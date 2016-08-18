
using System;
namespace Dinah
{
	public class Category : Gtk.EventBox {
		Gtk.VBox vbox;
		Gtk.Expander expander;
		
		public Category(string label) : base() {
			expander = new Gtk.Expander(label);
			expander.Show();
			Add(expander);
			expander.CanFocus = true;
			expander.Expanded = false;
			// Container child expander1.Gtk.Container+ContainerChild
			vbox = new Gtk.VBox ();
			vbox.Spacing = 3;
			Gtk.Label GtkLabel3 = new Gtk.Label ();
			GtkLabel3.LabelProp = Mono.Unix.Catalog.GetString (label);
			GtkLabel3.UseUnderline = true;
			expander.LabelWidget = GtkLabel3;
			vbox.ShowAll();
			expander.Add(vbox);
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
		public string BlockType;
		string icon_string = "gtk-justify-fill";

		public StatementFactory (string label, string type) : base() 
		{
			BlockType = type;
			CanFocus = true;
			UseUnderline = true;
			Xalign = 0.0F;			
			Gtk.Alignment w2 = new Gtk.Alignment (0.0F, 0.5F, 0F, 0F);
			// Container child GtkAlignment.Gtk.Container+ContainerChild
			Gtk.HBox w3 = new Gtk.HBox ();
			w3.Spacing = 2;
			// Container child GtkHBox.Gtk.Container+ContainerChild
			Gtk.Image w4 = new Gtk.Image (icon_string, Gtk.IconSize.Button);
			w4.SetPadding(10, 0);
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
		}
		
		// Drag object:
	    private static void HandleSourceDragDataGet (object sender, Gtk.DragDataGetArgs args)
	    {
			Console.WriteLine("HandleSourceDragDataGet {0}", sender);
            Byte [] data = System.Text.Encoding.UTF8.GetBytes("create:" + ((StatementFactory)sender).BlockType);
            Gdk.Atom [] targets = args.Context.Targets;
			args.SelectionData.Set(targets[0], 8, data, data.Length);
	    }
	}
	
	class ExpressionFactory : StatementFactory {
		public string icon_string = "stock_dialog-info";
		
		public ExpressionFactory(string label) : base(label, "expression") {
		}

	}
}

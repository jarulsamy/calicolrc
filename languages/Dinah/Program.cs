
using System;
using System.Collections.Generic;
namespace Dinah
{
	public class Program : Gtk.Frame {
		public Gtk.VBox vbox;
		public Gtk.Label label;
		public static Dictionary<string,Func<Statement>> factories = new Dictionary<string, Func<Statement>>();
		public Program() {
			vbox = new Gtk.VBox();
			vbox.ModifyBg(Gtk.StateType.Normal, Colors.DarkGray);
			label = new Gtk.Label("<i>drop items here</i>");
			label.UseMarkup = true;
			label.Show();
			vbox.PackStart(label, true, true, 0);
			vbox.Show();
			Add(vbox);
			Show();
			HeightRequest = 25;
			Gtk.Drag.DestSet(this, Gtk.DestDefaults.All, TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
			DragDataReceived += HandleProgramDragDataReceived;
		}
		
		public void AddStatement(Statement statement) {
			label.Hide();
			vbox.PackStart(statement, true, true, 0);
			Gtk.Box.BoxChild w7 = ((Gtk.Box.BoxChild)(vbox[statement]));
			w7.Position = 0;
			w7.Expand = false;
			w7.Fill = false;
			statement.Show();
			HeightRequest = -1;
		}

		void HandleProgramDragDataReceived (object sender, Gtk.DragDataReceivedArgs args)
		{
			Statement statement = makeStatement(args.SelectionData.Text); 
			((Program)sender).AddStatement(statement);
            //Gtk.Drag.Finish (args.Context, true, false, args.Time);
		}
		
		public static void registerStatement(string id, Func<Statement> constructor) {
			factories[id] = constructor;
		}
		
		public static Statement makeStatement(string id) {
			Console.WriteLine(id);
			if (factories.ContainsKey(id)) {
				return factories[id]();
			}
			throw new Exception(String.Format("no such id: {0}", id));
		}		
	}
}

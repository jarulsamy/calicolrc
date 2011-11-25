
using System;
using System.Collections.Generic;
namespace Dinah
{
	public class Program : Gtk.Frame {
		public Gtk.VBox vbox;
		public Gtk.Label label;
		public static Dictionary<string,Func<Statement>> factories = new Dictionary<string, Func<Statement>>();
		public static Dictionary<string,Statement> lookup = new Dictionary<string, Statement>();
		static int id_count = 1;
			
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
		
		public static string makeID() {
			string retval = id_count.ToString();
			id_count++;
			return retval;
		}
		
		public void AddStatement(Statement statement) {
			label.Hide();
			vbox.PackStart(statement, true, true, 0);
			Gtk.Box.BoxChild w7 = ((Gtk.Box.BoxChild)(vbox[statement]));
			//w7.Position = 0;
			w7.Expand = false;
			w7.Fill = false;
			statement.Show();
			HeightRequest = -1;
		}

		public void MoveStatement(string id, Statement dropped_on, int x, int y) {
			// drop ID onto statement
			Statement mover = Program.lookup[id];
			Gtk.Box.BoxChild w7 = ((Gtk.Box.BoxChild)(vbox[dropped_on]));
			Gtk.Box.BoxChild w8 = ((Gtk.Box.BoxChild)(vbox[mover]));
			Gdk.Rectangle rect = dropped_on.Allocation;
			if (y < rect.Height/2) {
				if (w8.Position == w7.Position) {
					// then you probably meant next
					w8.Position = w7.Position + 1; // next
				} else {
					w8.Position = w7.Position; //before, will put in front
				}
			} else {
				if (w8.Position == w7.Position) {
					// then you probably meant previous
					w8.Position = w7.Position - 1; // before, will put in front
				} else {
					w8.Position = w7.Position + 1; // next
				}
			}
		}

		public void AddStatement(Statement statement, Program widget, int x, int y) {
			// Currently, the only exposed location is at the bottom of the list
			AddStatement(statement);
		}
		
		public void AddStatement(Statement statement, Statement widget, int x, int y) {
			Gdk.Rectangle rect = widget.Allocation;
			label.Hide();
			vbox.PackStart(statement, true, true, 0);
			Gtk.Box.BoxChild w6 = ((Gtk.Box.BoxChild)(vbox[widget]));
			Gtk.Box.BoxChild w7 = ((Gtk.Box.BoxChild)(vbox[statement]));
			if (y < rect.Height/2)
				w7.Position = w6.Position; // same, will put in front
			else
				w7.Position = w6.Position + 1; // next			
			w7.Expand = false;
			w7.Fill = false;
			statement.Show();
			HeightRequest = -1;
		}

		void HandleProgramDragDataReceived (object sender, Gtk.DragDataReceivedArgs args)
		{
			// Drop on a Program area
			string [] data = args.SelectionData.Text.Split(':');
			if (data[0] == "create") {
				Statement statement = Program.makeStatement(data[1]); 
				((Program)sender).AddStatement(statement, (Program)sender, args.X, args.Y);
            	Gtk.Drag.Finish (args.Context, true, false, args.Time);
			} else if (data[0] == "move") {
				// FIXME: move item here
            	Gtk.Drag.Finish (args.Context, true, false, args.Time);
			}
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

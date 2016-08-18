
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
		public static Engine engine = new Engine();
			
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
			HeightRequest = 40;
			Gtk.Drag.DestSet(this, Gtk.DestDefaults.All, TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
			DragDataReceived += HandleProgramDragDataReceived;
		}
		
		public static List<Statement> getAllStatements() {
			List<Statement> list = new List<Statement>();
			foreach (Statement s in lookup.Values) {
				list.Add(s);
			}
			return list;
		}
		
		public static List<Statement> getStartStatements() {
			List<Statement> list = new List<Statement>();
			foreach (Statement s in lookup.Values) {
				if (s.myProgram.vbox.Children.Length > 0 && s.myProgram.vbox.Children[0] == s)
					list.Add(s);
			}
			return list;
		}

		public List<Statement> getStatements() {
			List<Statement> list = new List<Statement>();
			foreach (Statement s in vbox) {
				list.Add(s);
			}
			return list;
		}

		public static string makeID() {
			string retval = id_count.ToString();
			id_count++;
			return retval;
		}
		
		public void AddStatement(Statement statement) {
			statement.myProgram = this;
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
			if (mover.myProgram == dropped_on.myProgram) {
				Gtk.Box.BoxChild w7 = ((Gtk.Box.BoxChild)(vbox[dropped_on]));
				Gtk.Box.BoxChild w8 = ((Gtk.Box.BoxChild)(vbox[mover]));
				Gdk.Rectangle rect = dropped_on.Allocation;
				if (y < rect.Height/2) {
					w8.Position = w7.Position; //before, will put in front
				} else {
					w8.Position = w7.Position + 1; // next
				}
			} else { // different programs
				// FIXME: check for no more and show label
				mover.RemoveFromProgram();
				dropped_on.myProgram.AddStatement(mover, dropped_on, x, y);
			}
		}

		public void MoveStatementToBottom(Statement mover) {
			Gtk.Box.BoxChild w8 = ((Gtk.Box.BoxChild)(vbox[mover]));
			w8.Position = -1; //last
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
				this.AddStatement(statement, this, args.X, args.Y);
            	Gtk.Drag.Finish (args.Context, true, false, args.Time);
			} else if (data[0] == "move") {
				// FIXME: move item here
				// Move to bottom:
				Statement statement = Program.lookup[data[1]];
				statement.RemoveFromProgram();
				((Program)sender).AddStatement(statement, (Program)sender, args.X, args.Y);
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

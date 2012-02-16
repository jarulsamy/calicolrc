using System;

namespace Common
{
	public static class Utils
	{
		public static double random(double min, double max) {
			Random r = new Random();
			return min + (max-min)*r.NextDouble();
		}
		
		public static double random(double max) {
			return random (0.0, max);
		}
	}
	public static class Dialogs
	{
		public static string Ask(string question) 
		{
			Gtk.MessageDialog _dlg = new Gtk.MessageDialog(
				null,
				Gtk.DialogFlags.Modal | Gtk.DialogFlags.DestroyWithParent, 
				Gtk.MessageType.Question,
				Gtk.ButtonsType.Ok,
				null);
			
			_dlg.Title = "Ask...";
			_dlg.Markup = question;
			_dlg.DefaultResponse = Gtk.ResponseType.Ok;
			
			Gtk.Entry _entry = new Gtk.Entry();
			Gtk.HBox hbox = new Gtk.HBox();
			hbox.PackStart(new Gtk.Label("Ask: "), false, false, 5);
			hbox.PackEnd(_entry);
			_dlg.VBox.PackEnd(hbox, true, true, 0);
			_dlg.ShowAll();
	
			Gtk.ResponseType rsp = (Gtk.ResponseType)_dlg.Run ();
			string answer = _entry.Text;
			_dlg.Destroy();	
			
			return answer;
		}
		
		public static void Tell(string message) 
		{
			Gtk.MessageDialog _dlg = new Gtk.MessageDialog(
				null,
				Gtk.DialogFlags.Modal | Gtk.DialogFlags.DestroyWithParent, 
				Gtk.MessageType.Info,
				Gtk.ButtonsType.Ok,
				null);
			
			_dlg.Title = "Tell...";
			_dlg.Markup = message;
			_dlg.DefaultResponse = Gtk.ResponseType.Ok;
			_dlg.ShowAll();
	
			Gtk.ResponseType rsp = (Gtk.ResponseType)_dlg.Run ();
			_dlg.Destroy();
		}
	}
}


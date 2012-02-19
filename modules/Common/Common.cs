/*
Calico - Scripting Environment

Copyright (c) 2012, Doug Blank <dblank@cs.brynmawr.edu>, Mark Russo <russomf@gmail.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

$Id: $
*/

using System;
using System.Media;
using System.Threading;
using IronPython.Runtime; // List
using System.Collections.Generic; // IList
using System.Collections; // IEnumerator

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
		
		public static void beep() {
			SystemSounds.Beep.Play();
		}

		public static void wait(double seconds) {
    		Thread.Sleep((int)(seconds * 1000));
  		}
	}
	
	public static class Dialogs
	{
  		public static int gui_thread_id = -1;

		public static void set_gui_thread_id(int gui_thread_id) {
		    Dialogs.gui_thread_id = gui_thread_id;
		}

		public delegate void InvokeDelegate();
		public static void Invoke(InvokeDelegate invoke) {
			if (needInvoke())
				Gtk.Application.Invoke(delegate {invoke();});
			else
				invoke();
		}

		public static bool needInvoke() {
			//Console.WriteLine("gui_thread_id: {0}", Myro.gui_thread_id);
			if (Dialogs.gui_thread_id == -1) {
				return false; // in another thread
			} else if (Dialogs.gui_thread_id == Thread.CurrentThread.ManagedThreadId) {
				return false; // you are already in the GUI thread
			} else {
				return true; // need to invoke!
			}
		}
		
	  	public class MessageDialog : Gtk.MessageDialog {
	
			public MessageDialog(Gtk.Window window,
				Gtk.DialogFlags dialogFlags, 
				Gtk.MessageType messageType,
				Gtk.ButtonsType buttonsType,
				string title) : base(window, dialogFlags, messageType, buttonsType, 
					title) {
			  KeyPressEvent += MyKeyPressEventHandler;
			}
		
			[GLib.ConnectBefore]
			public void MyKeyPressEventHandler (object obj, 
				Gtk.KeyPressEventArgs args) {
			  if (args.Event.Key == Gdk.Key.Return) {
				Respond(Gtk.ResponseType.Ok);
				args.RetVal = true;
			  }
			}
		}
		
		public static object ask() {
			return ask("Input: ", "Information Request");
		}
		
		public static object ask(object question) {
			return ask(question, "Information Request");
		}
		
		public static object ask(object question, string title) {
		    ManualResetEvent ev = new ManualResetEvent(false);
		    object retval = null;
		    Gtk.Entry myentry = null;
		    PythonDictionary responses = new PythonDictionary();
		    Invoke(delegate {
		        Gtk.MessageDialog fc = new MessageDialog(null,
		                               0, Gtk.MessageType.Question,
		                               Gtk.ButtonsType.OkCancel,
		                               title);
		        if (question is List) {
		            foreach (string choice in (List)question) {
		                Gtk.HBox hbox = new Gtk.HBox();
		                Gtk.Label label = new Gtk.Label(choice);
		                Gtk.Entry entry = new Gtk.Entry();
		                responses[choice] = entry;
		                hbox.PackStart(label);
		                hbox.PackStart(entry);
		                fc.VBox.PackStart(hbox);
		            }
		        } else {
		            string choice = (string)question;
		            Gtk.HBox hbox = new Gtk.HBox();
		            Gtk.Label label = new Gtk.Label(choice);
		            Gtk.Entry entry = new Gtk.Entry();
		            myentry = entry;
		            hbox.PackStart(label);
		            hbox.PackStart(entry);
		            fc.VBox.PackStart(hbox);
		        }
		        fc.ShowAll();
		        if (fc.Run() == (int)Gtk.ResponseType.Ok) {
		            if (question is List) {
		                foreach (string choice in responses.Keys) {
		                    responses[choice] = ((Gtk.Entry)responses[choice]).Text;
		                }
		                retval = responses;
		            } else {
		                retval = myentry.Text;
		            }
		        }
		        fc.Destroy();
		        ev.Set();
		        });
		    ev.WaitOne();
		    return retval;
		}

		public static void tell(object message) {
			tell(message, "Information");
		}

		public static void tell(object message, string title) {
		    ManualResetEvent ev = new ManualResetEvent(false);

		    Invoke(delegate {
		        Gtk.MessageDialog fc = new MessageDialog(null,
		                               0, Gtk.MessageType.Info,
		                               Gtk.ButtonsType.Ok,
		                               title);
				string choice = (string)message;
				Gtk.HBox hbox = new Gtk.HBox();
				Gtk.Label label = new Gtk.Label(choice);
				hbox.PackStart(label);
				fc.VBox.PackStart(hbox);
		        
		        fc.ShowAll();
		        fc.Run();
		        fc.Destroy();
		        ev.Set();
		        });
		    ev.WaitOne();
		    return;
		}
	}
}


/*
Calico - Scripting Environment

Copyright (c) 2013, Hannah Organick <horganick@brynmawr.edu>
Copyright (c) 2013, Doug Blank <dblank@cs.brynmawr.edu>

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
using System.Threading;
using System.Collections;// IEnumerable
using System.Collections.Generic;
using System.Runtime.InteropServices;

public class Event {
  public string type; // message
  public double time = 0.0;
  public bool wait;
  public ManualResetEvent ev = null;
  public object obj = null;
  public double x;
  public double y;
  public object value;
  public string key;

  public Event (string message, bool wait) {
	this.time = currentTime();
	this.type = message;
	this.wait = wait;
	if (this.wait) {
	  ev = new ManualResetEvent(false);
	}
  }

  public Event (string message) : this(message, false) {
  }

  public Event (Gtk.ButtonReleaseEventArgs args) {
	type = "mouse-release";
	x = args.Event.X;
	y = args.Event.Y;
	time = args.Event.Time;
  }

  public Event (Gtk.KeyPressEventArgs args) {
	type = "key-press";
	time = args.Event.Time;
	key = args.Event.Key.ToString ();
  }
  
  public Event (Gtk.KeyReleaseEventArgs args) {
	type = "key-release";
	time = args.Event.Time;
	key = args.Event.Key.ToString();
  }

  public Event (Gtk.ButtonPressEventArgs args) {
	type = "mouse-press";
	x = args.Event.X;
	y = args.Event.Y;
	time = args.Event.Time;
  }

  public Event (Gtk.MotionNotifyEventArgs args) {
	type = "mouse-motion";
	x = args.Event.X;
	y = args.Event.Y;
	time = args.Event.Time;
  }

  public Event (System.EventArgs args) {
	type = "system-event";
  }

  public Event (string args, double time) {
	type = args;
	this.time = time;
  }

  public Event (string args, object value, double time) {
	type = args;
	this.value = value;
	this.time = time;
  }

  public void setWait (bool wait) {
	this.wait = wait;
	if (this.wait) {
	  ev = new ManualResetEvent(false);
	} else {
	  ev = null;
	}
  }

  public override string ToString () {
	if (time == 0 && x == 0 && y == 0) {
	  return String.Format ("<Event \"{0}\">", type);
	} else if (x == 0 && y == 0) {
	  return String.Format ("<Event \"{0}\" at {1}>", type, time);
	} else {
	  return String.Format ("<Event \"{0}\" ({1},{2}) at {3}>", 
		  type, x, y, time);
	}
  }
  
  public string __repr__ () {
	return ToString ();
  }

  public static double currentTime() {
	System.TimeSpan t = System.DateTime.UtcNow - new System.DateTime (1970, 1, 1);
	return t.TotalSeconds;
  }
}

public static class Events {
  
  public static List<Event> queue = new List<Event>();
  public static Dictionary<string,List<Tuple<Func<object,Event,object>,object>>> handler = 
	  new Dictionary<string,List<Tuple<Func<object,Event,object>,object>>>();
  public static Thread thread = null;
  public static Calico.MainWindow calico = null;
  static Dictionary<int,Tuple<Func<object,Event,object>,object>> assoc = 
	new Dictionary<int,Tuple<Func<object,Event,object>,object>>();
  static int counter = 0;
  
  public static void publish (Event evt) {
      // Event for a particular obj, already set
      lock (queue) {
	  queue.Add(evt);
      }
  }

  public static void publish (Calico.MainWindow calico, string message) {
      calico.SendEvent("all", message);
  }
  
  public static void publishAndWait (Calico.MainWindow calico, string message, string to) {
      calico.SendEvent(to, message);
      // FIXME: wait for what?
  }
  
  public static void publish (string message) {
	lock (queue) {
	  queue.Add(new Event(message));
	}
  }
  
    public static void publish (string message, object obj) {
	Event evt = new Event(message);
	evt.obj = obj;
	lock (queue) {
	  queue.Add(evt);
	}
  }
  
  public static object publishAndWait (string message) {
	Event evt = new Event(message, true);
	lock (queue) {
	  queue.Add(evt);
	}
	evt.ev.WaitOne();
	return evt.value;
  }

  public static object publishAndWait (string message, object obj) {
	Event evt = new Event(message, true);
	evt.obj = obj;
	lock (queue) {
	  queue.Add(evt);
	}
	evt.ev.WaitOne();
	return evt.value;
  }

  public static object publishAndWait (Event evt) {
      // Event for a particular obj, already set
      evt.setWait(true);
      lock (queue) {
	  queue.Add(evt);
      }
      evt.ev.WaitOne();
      return evt.value;
  }

  public static string getID(object obj, string message) {
      if (obj == null) {
	  return message;
      } else {
	  return (obj.GetHashCode().ToString() + "-" + message);
      }
  }

  public static int subscribe (string message, Func<object,Event,object> function) {
      return subscribe(message, function, null, null);
  }
  
  public static int subscribe (Calico.MainWindow calico, string message, Func<object,Event,object> function) {
	return subscribe(message, function, null, calico);
  }
  
  public static int subscribe (string message, Func<object,Event,object> function, 
	  object obj) {
      return subscribe(message, function, obj, null);
  }

  private static int subscribe (string message, Func<object,Event,object> function, 
				object obj, Calico.MainWindow calico) {
      string id = getID(obj, message);
      lock (handler) {
	  Events.calico = calico;
	  if (!handler.ContainsKey(id)) {
	      handler[id] = new List<Tuple<Func<object,Event,object>,object>>();
	  }
	  var tuple = Tuple.Create(function, obj);
	  handler[id].Add(tuple);
	  counter++;
	  assoc[counter] = tuple;
      }
      return counter;
  }

    public static void unsubscribe(int id) {
	foreach(string key in handler.Keys) {
	    List<Tuple<Func<object,Event,object>,object>> list = handler[key];
	    foreach (Tuple<Func<object,Event,object>,object> tuple in list) {
		if (tuple == assoc[id]) {
		    list.Remove(tuple);
		    return;
		}
	    }
	}
    }

  public static void loop() {
	while (true) {
	  lock (queue) {
	      // check with calico if any system events
	        if (Events.calico != null) {
		    List<List<string>> evts = Events.calico.ReceiveEvents();
		    foreach (List<string> evt in evts) {
			lock (queue) {
			    queue.Add(new Event(evt[1]));
			}
		    }
	        }
		foreach(Event evt in queue) {
		  string id = getID(evt.obj, evt.type);
		  if (handler.ContainsKey(id)) {
			Event lastEvent = null;
			List<Thread> threads = new List<Thread>();
			foreach (Tuple<Func<object,Event,object>,object> tuple in handler[id]) {
			  Func<object,Event,object> code = tuple.Item1;
			  object obj = tuple.Item2;
			  Thread codethread = new Thread (new ThreadStart ( delegate {
				      try {
					  evt.value = code(obj, evt);
				      } catch (Exception e) {
					  System.Console.Error.WriteLine("Error in receiving message: '{0}': {1}", evt.type, e.Message);
				      }
				  }));
			  threads.Add(codethread);
			  codethread.IsBackground = true;
			  codethread.Start();
			  lastEvent = evt;
			}
			if (lastEvent != null && lastEvent.wait) {
			    // Wait for them all to finish
			    try {
				foreach (Thread t in threads) {
				    t.Join ();
				}
			    } catch { 
				// error in joining, probably an abort
			    } finally {
				foreach (Thread t in threads) {
				    try {
					t.Abort ();
				    } catch {
				    }
				}
			    }
			    lastEvent.ev.Set();
			}
		  } else {
		      //Console.Error.WriteLine("Event not handled by a subscribe: " + evt);
		  }
		}
		queue.Clear();
	  }
	  Thread.Sleep(100);
	}
  }

  public static void init() {
	lock (queue) {
	  queue.Clear();
	}
	lock (handler) {
	  handler.Clear();
	}
	if (thread == null) {
	  System.Console.WriteLine("Starting event loop...");
	  thread = new Thread (new ThreadStart (loop));
	  thread.IsBackground = true;
	  thread.Start();
	}
  }

  public static void stop() {
	if (thread != null) {
	    thread.Abort();
	    thread = null;
	}
  }
}

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

  public Event (string message) : this(message, true) {
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
  public static Dictionary<string,List<Tuple<Func<object,Event,object>,object>>> handler = new Dictionary<string,List<Tuple<Func<object,Event,object>,object>>>();
  
  public static void publish (string message) {
	lock (queue) {
	  queue.Add(new Event(message));
	}
  }
  
  public static object publishAndWait (string message) {
	Event evt = new Event(message, true);
	lock (queue) {
	  queue.Add(evt);
	}
	evt.ev.WaitOne();
	// FIXME: return value?
	return null;
  }

  public static void subscribe (string message, Func<object,Event,object> function, 
	  object obj) {
	lock (handler) {
	  if (!handler.ContainsKey(message)) {
		handler[message] = new List<Tuple<Func<object,Event,object>,object>>();
	  }
	  handler[message].Add(Tuple.Create(function, obj));
	}
  }

  public static void loop() {
	while (true) {
	  lock (queue) {
		foreach(Event evt in queue) {
		  if (handler.ContainsKey(evt.type)) {
			foreach (Tuple<Func<object,Event,object>,object> tuple in handler[evt.type]) {
			  Func<object,Event,object> code = tuple.Item1;
			  object obj = tuple.Item2;
			  evt.value = code(obj, evt);
			  if (evt.wait) {
				evt.ev.Set();
			  }
			}
		  }
		}
		queue.Clear();
	  }
	}
  }
}
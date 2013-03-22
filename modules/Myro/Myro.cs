/*
Calico - Scripting Environment

Copyright (c) 2011-2012, Doug Blank <dblank@cs.brynmawr.edu>

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
using System.Runtime.InteropServices;

// Marshal
using System.Diagnostics;

// Process
using System.IO;

// DirectoryInfo, FileInfo
using System.IO.Ports;

// SerialPort
using System.Threading;
using IronPython.Runtime;

// List
using System.Collections.Generic;

// IList
using System.Collections;
using System.Reflection;// IEnumerator

using Tao.Sdl;

public static class Extensions
{
        [method: JigsawTab(null)]
	public static T[] Slice<T> (this T[] source, int start, int end)
	{
		if (start == 0 && end == source.Length)
			return source;
		// Handles negative ends.
		if (end < 0) {
			end = source.Length + end;
		}
		int len = end - start;
		// Return new array.
		T[] res = new T[len];
		for (int i = 0; i < len; i++) {
			res [i] = source [i + start];
		}
		return res;
	}

        [method: JigsawTab(null)]
	public static object[] Slice (this IList source, int start, int end)
	{
		// Handles negative ends.
		if (end < 0) {
			end = source.Count + end;
		}
		int len = end - start;
		// Return new array.
		object[] res = new object[len];
		for (int i = 0; i < len; i++) {
			res [i] = source [i + start];
		}
		return res;
	}
}

public static class SerialPortCache
{
	public static Dictionary<string,SerialPort> ports = new Dictionary<string,SerialPort> ();

        [method: JigsawTab(null)]
	public static bool Contains (string port)
	{
		foreach (string key in ports.Keys) {
			if (key == port)
				return true;
		}
		return false;
	}

        [method: JigsawTab(null)]
	public static SerialPort getSerialPort (string port, int baud)
	{
		SerialPort serial;
		if (Contains (port)) {
			serial = ports [port];
			if (serial.IsOpen) {
				if (serial.PortName.Equals (port) && serial.BaudRate == baud) {
					return serial; // ok, return it
				} else {
					// It exists, but wrong port/baud, so close it:
					serial.Close (); 
					ports.Remove (port);
					return SerialPortCache.getSerialPort (port, baud); // call again, now that it is removed
				}
			} else { // closed
				if ((serial.PortName.Equals (port) || port == null) && serial.BaudRate == baud) {
					serial.Open ();
					return serial;
				} else {
					// It exists, closed, and wrong port or baud
					serial.Close (); 
					ports.Remove (port);
					return SerialPortCache.getSerialPort (port, baud); // call again, now that it is removed
				}
			}
		} else { // not in ports
			serial = new SerialPort (port, baud);
			serial.ReadTimeout = 1000; // milliseconds
			serial.WriteTimeout = 1000; // milliseconds
			try {
				serial.Open ();
			} catch {
				Console.Error.WriteLine (String.Format ("ERROR: unable to open port '{0}'", 
						port));
				return null;
			}
			ports [port] = serial;
			return serial;
		}
	}
}

public static class Myro
{
	//public readonly static List __all__ = new List() {"robot"};

	public static Robot robot;
	public static Simulation simulation;
	public static int gui_thread_id = -1;
	static string REVISION = "$Revision: $";
	static string startup_path = null;
	static string os_name = null;
	static string speech_name = "default";
	static bool warn_missing_speak = true;
	static PythonDictionary voices = new PythonDictionary ();
	public readonly static PythonDictionary frequencies = new PythonDictionary ();
	public readonly static Gamepads gamepads = new Gamepads ();
	public readonly static Computer computer = new Computer ();

	[method: JigsawTab(null)]
	public static void initialize_module (string path, string os)
	{
		Myro.startup_path = path;
		Myro.os_name = os;
		voices ["af"] = "Afrikaans Male";
		voices ["af+f1"] = "Afrikaans Female";
		voices ["bs"] = "Bosnian Male";
		voices ["bs+f1"] = "Bosnian Female";
		voices ["ca"] = "Catalan Male";
		voices ["ca+f1"] = "Catalan Female";
		voices ["cs"] = "Czech Male";
		voices ["cs+f1"] = "Czech Female";
		voices ["cy"] = "Welsh Male";
		voices ["cy+f1"] = "Welsh Female";
		voices ["da"] = "Danish Male";
		voices ["da+f1"] = "Danish Female";
		voices ["de"] = "German Male";
		voices ["de+f1"] = "German Female";
		voices ["el"] = "Greek Male";
		voices ["el+f1"] = "Greek Female";
		voices ["en"] = "Default Male";
		voices ["en+f1"] = "Default Female";
		voices ["en-sc"] = "English-Scottish Male";
		voices ["en-sc+f1"] = "English-Scottish Female";
		voices ["en-uk"] = "Englsih-British Male";
		voices ["en-uk+f1"] = "English-British Female";
		voices ["en-uk-north"] = "English-British-Lancashire Male";
		voices ["en-uk-north+f1"] = "English-British-Lancashire Female";
		voices ["en-uk-rp"] = "English-British-RP Male";
		voices ["en-uk-rp+f1"] = "English-British-RP Female";
		voices ["en-uk-wmids"] = "English-British-WMIDS Male";
		voices ["en-uk-wmids+f1"] = "English-British-WMIDS Female";
		voices ["en-us"] = "English-US Male";
		voices ["en-us+f1"] = "English-US Female";
		voices ["en-wi"] = "English-West-Indies Male";
		voices ["en-wi+f1"] = "English-West-Indies Female";
		voices ["eo"] = "Esperanto Male";
		voices ["eo+f1"] = "Esperanto Female";
		voices ["es"] = "Spanish Male";
		voices ["es+f1"] = "Spanish Female";
		voices ["es-la"] = "Spanish-Latin-American Male";
		voices ["es-la+f1"] = "Spanish-Latin-American Female";
		voices ["fi"] = "Finnish Male";
		voices ["fi+f1"] = "Finnish Female";
		voices ["fr"] = "French Male";
		voices ["fr+f1"] = "French Female";
		voices ["fr-be"] = "French-Belgium Male";
		voices ["fr-be+f1"] = "French-Belgium Female";
		voices ["grc"] = "Greek-Ancient Male";
		voices ["grc+f1"] = "Greek-Ancient Female";
		voices ["hi"] = "Hindi Male";
		voices ["hi+f1"] = "Hindi Female";
		voices ["hr"] = "Croatian Male";
		voices ["hr+f1"] = "Croatian Female";
		voices ["hu"] = "Hungarian Male";
		voices ["hu+f1"] = "Hungarian Female";
		voices ["hy"] = "Armenian Male";
		voices ["hy+f1"] = "Armenian Female";
		voices ["hy-west"] = "Armenian-West Male";
		voices ["hy-west+f1"] = "Armenian-West Female";
		voices ["id"] = "Indonesian Male";
		voices ["id+f1"] = "Indonesian Female";
		voices ["is"] = "Icelandic Male";
		voices ["is+f1"] = "Icelandic Female";
		voices ["it"] = "Italian Male";
		voices ["it+f1"] = "Italian Female";
		voices ["jbo"] = "Lojban Male";
		voices ["jbo+f1"] = "Lojban Female";
		voices ["ku"] = "Kurdish Male";
		voices ["ku+f1"] = "Kurdish Female";
		voices ["la"] = "Latin Male";
		voices ["la+f1"] = "Latin Female";
		voices ["lv"] = "Latvian Male";
		voices ["lv+f1"] = "Latvian Female";
		voices ["mk"] = "Macedonian Male";
		voices ["mk+f1"] = "Macedonian Female";
		voices ["nci"] = "Nahautl-Classical Male";
		voices ["nci+f1"] = "Nahautl-ClassicalFemale";
		voices ["nl"] = "Dutch Male";
		voices ["nl+f1"] = "Dutch Female";
		voices ["no"] = "Norwegian Male";
		voices ["no+f1"] = "Norweigian Female";
		voices ["pap"] = "Papiamento Male";
		voices ["pap+f1"] = "Papiamento Female";
		voices ["pl"] = "Polish Male";
		voices ["pl+f1"] = "Polish Female";
		voices ["pt"] = "Brazil Male";
		voices ["pt+f1"] = "Brazil Female";
		voices ["pt-pt"] = "Portugal Male";
		voices ["pt-pt+f1"] = "Portugal Female";
		voices ["ro"] = "Romanian Male";
		voices ["ro+f1"] = "Romanian Female";
		voices ["ru"] = "Russian Male";
		voices ["ru+f1"] = "Russian Female";
		voices ["sk"] = "Slovak Male";
		voices ["sk+f1"] = "Slovak Female";
		voices ["sq"] = "Albanian Male";
		voices ["sq+f1"] = "Albanian Female";
		voices ["sr"] = "Serbian Male";
		voices ["sr+f1"] = "Serbian Female";
		voices ["sv"] = "Swedish Male";
		voices ["sv+f1"] = "Swahili Female";
		voices ["sw"] = "Swahili Male";
		voices ["sw+f1"] = "Swahili Female";
		voices ["ta"] = "Tamil Male";
		voices ["ta+f1"] = "Tamil Female";
		voices ["tr"] = "Turkish Male";
		voices ["tr+f1"] = "Turkish Female";
		voices ["vi"] = "Vietnam Male";
		voices ["vi+f1"] = "Vietnam Female";
		voices ["zh"] = "Mandarin Male";
		voices ["zh+f1"] = "Mandarin Female";
		voices ["zh-yue"] = "Cantonese Male";
		voices ["zh-yue+f1"] = "Cantonese Female";

		frequencies ["rest"] = 0.0;
		frequencies ["pause"] = 0.0;
		frequencies ["a0"] = 27.50;
		frequencies ["a#0"] = 29.14;
		frequencies ["bb0"] = 29.14;
		frequencies ["b0"] = 30.87;
		frequencies ["c1"] = 32.70;
		frequencies ["c#1"] = 34.65;
		frequencies ["db1"] = 34.65;
		frequencies ["d1"] = 36.71;
		frequencies ["d#1"] = 38.89;
		frequencies ["eb1"] = 38.89;
		frequencies ["e1"] = 41.20;
		frequencies ["f1"] = 43.65;
		frequencies ["f#1"] = 46.25;
		frequencies ["gb1"] = 46.25;
		frequencies ["g1"] = 49.00;
		frequencies ["g#1"] = 51.91;
		frequencies ["ab1"] = 51.91;
		frequencies ["a1"] = 55.00;
		frequencies ["a#1"] = 58.27;
		frequencies ["bb1"] = 58.27;
		frequencies ["b1"] = 61.74;
		frequencies ["c2"] = 65.41;
		frequencies ["c#2"] = 69.30;
		frequencies ["db2"] = 69.30;
		frequencies ["d2"] = 73.42;
		frequencies ["d#2"] = 77.78;
		frequencies ["eb2"] = 77.78;
		frequencies ["e2"] = 82.41;
		frequencies ["f2"] = 87.31;
		frequencies ["f#2"] = 92.50;
		frequencies ["gb2"] = 92.50;
		frequencies ["g2"] = 98.00;
		frequencies ["g#2"] = 103.80;
		frequencies ["ab2"] = 103.80;
		frequencies ["a2"] = 110.00;
		frequencies ["a#2"] = 116.50;
		frequencies ["bb2"] = 116.50;
		frequencies ["b2"] = 123.471;
		frequencies ["c3"] = 130.8;
		frequencies ["c#3"] = 138.6;
		frequencies ["db3"] = 138.6;
		frequencies ["d3"] = 146.8;
		frequencies ["d#3"] = 155.6;
		frequencies ["eb3"] = 155.6;
		frequencies ["e3"] = 164.8;
		frequencies ["f3"] = 174.6;
		frequencies ["f#3"] = 185.0;
		frequencies ["gb3"] = 185.0;
		frequencies ["g3"] = 196.0;
		frequencies ["g#3"] = 207.7;
		frequencies ["ab3"] = 207.7;
		frequencies ["a3"] = 220.0;
		frequencies ["a#3"] = 233.1;
		frequencies ["bb3"] = 233.1;
		frequencies ["b3"] = 246.9;
		frequencies ["c4"] = 261.6;
		frequencies ["c#4"] = 277.2;
		frequencies ["db4"] = 277.2;
		frequencies ["d4"] = 293.7;
		frequencies ["d#4"] = 311.1;
		frequencies ["eb4"] = 311.1;
		frequencies ["e4"] = 329.6;
		frequencies ["f4"] = 349.2;
		frequencies ["f#4"] = 370.0;
		frequencies ["gb4"] = 370.0;
		frequencies ["g4"] = 392.0;
		frequencies ["g#4"] = 415.3;
		frequencies ["ab4"] = 415.3;
		frequencies ["a4"] = 440.0;
		frequencies ["a#4"] = 466.2;
		frequencies ["bb4"] = 466.2;
		frequencies ["b4"] = 493.9;
		frequencies ["c5"] = 523.3;
		frequencies ["c#5"] = 554.4;
		frequencies ["db5"] = 554.4;
		frequencies ["d5"] = 587.3;
		frequencies ["d#5"] = 622.3;
		frequencies ["eb5"] = 622.3;
		frequencies ["e5"] = 659.3;
		frequencies ["f5"] = 698.5;
		frequencies ["f#5"] = 740.0;
		frequencies ["gb5"] = 740.0;
		frequencies ["g5"] = 784.0;
		frequencies ["g#5"] = 830.6;
		frequencies ["ab5"] = 830.6;
		frequencies ["a5"] = 880.0;
		frequencies ["a#5"] = 932.3;
		frequencies ["bb5"] = 932.3;
		frequencies ["b5"] = 987.8;
		// -------------------- default octave
		frequencies ["c"] = 523.3;
		frequencies ["c#"] = 554.4;
		frequencies ["db"] = 554.4;
		frequencies ["d"] = 587.3;
		frequencies ["d#"] = 622.3;
		frequencies ["eb"] = 622.3;
		frequencies ["e"] = 659.3;
		frequencies ["f"] = 698.5;
		frequencies ["f#"] = 740.0;
		frequencies ["gb"] = 740.0;
		frequencies ["g"] = 784.0;
		frequencies ["g#"] = 830.6;
		frequencies ["ab"] = 830.6;
		frequencies ["a"] = 880.0;
		frequencies ["a#"] = 932.3;
		frequencies ["bb"] = 932.3;
		frequencies ["b"] = 987.8;
		// --------------------
		frequencies ["c6"] = 1047.0;
		frequencies ["c#6"] = 1109.0;
		frequencies ["db6"] = 1109.0;
		frequencies ["d6"] = 1175.0;
		frequencies ["d#6"] = 1245.0;
		frequencies ["eb6"] = 1245.0;
		frequencies ["e6"] = 1319.0;
		frequencies ["f6"] = 1397.0;
		frequencies ["f#6"] = 1480.0;
		frequencies ["gb6"] = 1480.0;
		frequencies ["g6"] = 1568.0;
		frequencies ["g#6"] = 1661.0;
		frequencies ["ab6"] = 1661.0;
		frequencies ["a6"] = 1760.0;
		frequencies ["a#6"] = 1865.0;
		frequencies ["bb6"] = 1865.0;
		frequencies ["b6"] = 1976.0;
		frequencies ["c7"] = 2093.0;
		frequencies ["c#7"] = 2217.0;
		frequencies ["db7"] = 2217.0;
		frequencies ["d7"] = 2349.0;
		frequencies ["d#7"] = 2489.0;
		frequencies ["eb7"] = 2489.0;
		frequencies ["e7"] = 2637.0;
		frequencies ["f7"] = 2794.0;
		frequencies ["f#7"] = 2960.0;
		frequencies ["gb7"] = 2960.0;
		frequencies ["g7"] = 3136.0;
		frequencies ["g#7"] = 3322.0;
		frequencies ["ab7"] = 3322.0;
		frequencies ["a7"] = 3520.0;
		frequencies ["a#7"] = 3729.0;
		frequencies ["bb7"] = 3729.0;
		frequencies ["b7"] = 3951.0;
		frequencies ["c8"] = 4186.0;
	}

	[method: JigsawTab(null)]
	public static void close_module ()
	{
		if (Myro.computer.audio_initialized || Myro.computer.sound_initialized) {	 
			Tao.Sdl.Sdl.SDL_AudioQuit ();
		}
	}
		
	[method: JigsawTab(null)]
	public static void set_gui_thread_id (int gui_thread_id)
	{
		Myro.gui_thread_id = gui_thread_id;
	}

	public delegate void InvokeDelegate ();

	[method: JigsawTab(null)]
	public static void Invoke (InvokeDelegate invoke)
	{
	  if (needInvoke ()) {
		Gtk.Application.Invoke (delegate {
			  invoke ();});
	  } else {
		invoke ();
	  }
	}

	[method: JigsawTab(null)]
	public static bool needInvoke ()
	{
	  return (Thread.CurrentThread.ManagedThreadId != 1);
	}
	
	[method: JigsawTab(null)]
	static void invoke_function (Func<object,object> function, object args)
	{
		try {
			Gtk.Application.Invoke (delegate {
				function (args);
			});
		} catch (Exception e) {
			Console.Error.WriteLine ("Error in function");
			Console.Error.WriteLine (e.Message);
		}        
	}
	
	[method: JigsawTab(null)]
	static public object [] slice (IList list, int start, int end)
	{
		return list.Slice (start, end);
	}

	[method: JigsawTab("Movement")]
	public static void gamepad (PythonDictionary dict)
	{
		// everytime something (up/down) happens on the keys being
		// watched, then args are sent to the associated function(s)
		PythonDictionary results;
		List keys = new List ();
		foreach (string key in dict.Keys) {
			keys.append (key);
		}
		while (true) {
			results = (PythonDictionary)getGamepad (keys);
			foreach (string key in results.Keys) {
				invoke_function ((Func<object,object>)dict [key], results [key]);
			}
		}
	}

	[method: JigsawTab("Movement")]
	public static void gamepad ()
	{
		// sample robot controller
		PythonDictionary results = (PythonDictionary)getGamepadNow ();
		List button = (List)results ["button"];
		string name = "Scribby";
		try {
			name = getName ();
		} catch {
			// ignore
		}
		List phrases = Graphics.PyList (
		   String.Format ("Hello. My name is {0}.", name),
		                 "Ouch! I'm a sensitive robot.", 
				 "I'm hungry. Do you have any batteries?");
    
		Console.WriteLine ("        Pad   Action");
		Console.WriteLine ("     ------   -------");
		Console.WriteLine (" Left/Right   turnLeft() and turnRight()");
		Console.WriteLine ("    Up/Down   forward() and backward()");
		Console.WriteLine ("");
    
		if (button.Count > 0) {
			Console.WriteLine ("     Button   Action");
			Console.WriteLine ("     ------   -------");
			Console.WriteLine ("          1   stop()");
			if (button.Count > 1) 
				Console.WriteLine ("          2   takePicture()");
			if (button.Count > 2) 
				Console.WriteLine ("          3   beep(.25, 523)");
			if (button.Count > 3) 
				Console.WriteLine ("          4   beep(.25, 587)");
			if (button.Count > 4) 
				Console.WriteLine ("          5   beep(.25, 659)");
			if (button.Count > 5) 
				Console.WriteLine (String.Format ("          6   speak('{0}')", 
					phrases [0]));
			if (button.Count > 6) 
				Console.WriteLine (String.Format ("          7   speak('{0}')",
					phrases [1]));
			if (button.Count > 7) 
				Console.WriteLine (String.Format ("          8   speak('{0}')",
					phrases [2]));
			Console.WriteLine ("");
		}
    
		Console.WriteLine ("Gamepad is now running... Press button 1 to stop.");
    
		List lastMove = Graphics.PyList (0, 0);
		List axis, freqs;
		bool doneSpeaking = true;
		PythonDictionary retval = (PythonDictionary)getGamepadNow ();
		button = (List)retval ["button"];
		int length = button.Count;
		bool tryToMove = true;
		while (true) {
			retval = (PythonDictionary)getGamepad ();  // changed to blocking, JWS
			button = (List)retval ["button"];
			axis = (List)retval ["axis"];
			freqs = Graphics.PyList (null, null);
			if (length > 0 && (int)button [0] == 1) {
				try {
					stop ();
				} catch {
				}
				break;
			}
			if (length > 1 && (int)button [1] == 1) {
				speak ("Say cheese!", 1);
				try {
					Graphics.Picture pic = takePicture ();
					show (pic);
				} catch {
				}
			}
			if (length > 2 && (int)button [2] == 1) {
				freqs [0] = 523;
			}
			if (length > 3 && (int)button [3] == 1) {
				if (freqs [0] == null)
					freqs [0] = 587;
				else
					freqs [1] = 587;
			}
			if (length > 4 && (int)button [4] == 1) {
				if (freqs [0] == null)
					freqs [0] = 659;
				else
					freqs [1] = 659;
			}
			// speak
			if (length > 5 && (int)button [5] == 1) {
				if (doneSpeaking) {
					speak ((string)phrases [0], 1);
					doneSpeaking = false;
				} 
			} else if (length > 6 && (int)button [6] == 1) {
				if (doneSpeaking) {
					speak ((string)phrases [1], 1);
					doneSpeaking = false;
				}
			} else if (length > 7 && (int)button [7] == 1) {
				if (doneSpeaking) {
					speak ((string)phrases [2], 1);
					doneSpeaking = false;
				}
			} else {
				doneSpeaking = true;
			}
      
			if (tryToMove && 
	  axis [0] != lastMove [0] &&
	  axis [1] != lastMove [1]) {
				try {
					move (-(double)axis [1], -(double)axis [0]);
					lastMove = Graphics.PyList (axis [0], axis [1]);
				} catch {
					tryToMove = false;
				}
			}
			if (freqs [0] != null || freqs [1] != null) {
				if (freqs [1] == null) {
					try {
						beep (.25, (int)freqs [0]);
					} catch {
						Myro.computer.beep (.25, (int)freqs [0]);
					}
				} else if (freqs [0] == null) {
					try {
						beep (.25, (int)freqs [1]);
					} catch {
						Myro.computer.beep (.25, (int)freqs [1]);
					}
				} else {
					try {
						beep (.25, (int)freqs [0], (int)freqs [1]);
					} catch {
						Myro.computer.beep (.25, (int)freqs [0], (int)freqs [1]);
					}
				}
			}
		}
	}
		
	[method: JigsawTab(null)]
	public static int bool_to_int (bool value)
	{
		if (value)
			return 1;
		return 0;
	}

	[method: JigsawTab(null)]
	public static bool int_to_bool (int value)
	{
		if (value == 0)
			return false;
		return true;
	}

	[method: JigsawTab(null)]
	public static bool byte_to_bool (byte value)
	{
		if (value == 0)
			return false;
		return true;
	}

	[method: JigsawTab(null)]
	public static int byte_to_int (byte value)
	{
		return (int)value;
	}

	[method: JigsawTab("Senses")]
	public static void senses ()
	{
		new Senses ();
	}

	class Senses
	{
		Gtk.Window win;
		PythonDictionary dict_entry;
		uint idle = 0;

		public Senses ()
		{
		    ManualResetEvent ev = new ManualResetEvent(false);
		    Invoke (delegate {
			    win = new Gtk.Window ("Senses");
			    Gtk.VBox vbox = new Gtk.VBox ();
			    //win.AllowGrow = true;
			    //win.AllowShrink = true;
			    //win.SetDefaultSize(width, height);
			    win.DeleteEvent += OnDelete;
			    win.Add (vbox);
			    Gtk.HBox hbox;
			    PythonDictionary items = new PythonDictionary ();
			    items ["Line:"] = 2;
			    items ["Stall:"] = 1;
			    items ["Bright:"] = 3;
			    items ["Obstacle:"] = 3;
			    items ["IR:"] = 2;
			    items ["Light:"] = 3;
			    items ["Battery:"] = 1;
			    dict_entry = new PythonDictionary ();
			    List entries = new List ();
			    foreach (string key in items.Keys) {
				hbox = new Gtk.HBox ();
				Gtk.Label label = new Gtk.Label (key);
				label.WidthRequest = 100;
				hbox.PackStart (label, false, false, 0);
				entries = new List ();
				for (int i= 0; i < (int)items[key]; i++) {
				    Gtk.Entry entry = new Gtk.Entry ();
				    entries.append (entry);
				    hbox.PackStart (entry);
				    dict_entry [key] = entries;
				}
				vbox.PackStart (hbox);
			    }
			    win.ShowAll ();
			    ev.Set();
			});
		    idle = GLib.Idle.Add (new GLib.IdleHandler (update_entries));
		    ev.WaitOne();
		}
    
		private void OnDelete (object obj, Gtk.DeleteEventArgs args)
		{
			if (idle != 0) {
				GLib.Source.Remove (idle);
			}
			idle = 0;
		}
    
		bool update_entries ()
		{
			Invoke (delegate {
				try {	  
					List results = (List)getLight ();
					for (int i=0; i < results.Count; i++) {
						((Gtk.Entry)((List)dict_entry ["Light:"]) [i]).Text = results [i].ToString ();
					}
					results = (List)getBright ();
					for (int i=0; i < results.Count; i++) {
						((Gtk.Entry)((List)dict_entry ["Bright:"]) [i]).Text = results [i].ToString ();
					}
					results = (List)getObstacle ();
					for (int i=0; i < results.Count; i++) {
						((Gtk.Entry)((List)dict_entry ["Obstacle:"]) [i]).Text = results [i].ToString ();
					}
					results = (List)getIR ();
					for (int i=0; i < results.Count; i++) {
						((Gtk.Entry)((List)dict_entry ["IR:"]) [i]).Text = results [i].ToString ();
					}
					results = (List)getLine ();
					for (int i=0; i < results.Count; i++) {
						((Gtk.Entry)((List)dict_entry ["Line:"]) [i]).Text = results [i].ToString ();
					}
					((Gtk.Entry)((List)dict_entry ["Battery:"]) [0]).Text = getBattery ().ToString ();
					((Gtk.Entry)((List)dict_entry ["Stall:"]) [0]).Text = getStall ().ToString ();	  
				} catch {
					// pass
				}
			});
			wait (.1);
			return true; // continue
		}
	}

	public class Gamepads
	{
   
		IntPtr[] handles;

		public Gamepads ()
		{
			try {
				Sdl.SDL_Init (Sdl.SDL_INIT_JOYSTICK);
				handles = new IntPtr [Sdl.SDL_NumJoysticks ()];
				for (int i=0; i < Sdl.SDL_NumJoysticks(); i++) {
					handles [i] = Sdl.SDL_JoystickOpen (i);
				}
			} catch {
				Console.Error.WriteLine ("WARNING: SDL is not installed.");
			}
		}

		public List getHatStates (int index)
		{
			List retval = new List ();
			int num = Sdl.SDL_JoystickNumHats (handles [index]);
			for (int button = 0; button < num; button++) {
				retval.append (Sdl.SDL_JoystickGetHat (handles [index], button));
			}
			return retval;
		}

		public List getBallStates (int index)
		{
			List retval = new List ();
			int num = Sdl.SDL_JoystickNumBalls (handles [index]);
			for (int button = 0; button < num; button++) {
				int x, y;
				Sdl.SDL_JoystickGetBall (handles [index], button, out x, out y);
				retval.append (Graphics.PyTuple (x, y));
			}
			return retval;
		}

		public List getButtonStates (int index)
		{
			List retval = new List ();
			int num = Sdl.SDL_JoystickNumButtons (handles [index]);
			for (int button = 0; button < num; button++) {
				retval.append ((int)Sdl.SDL_JoystickGetButton (handles [index], button));
			}
			return retval;
		}

		public List getAxisStates (int index)
		{
			List retval = new List ();
			int num = Sdl.SDL_JoystickNumAxes (handles [index]);
			for (int button = 0; button < num; button++) {
				retval.append (Math.Round (Sdl.SDL_JoystickGetAxis (handles [index], button) / 32767.0, 2));
			}
			return retval;
		}

		public PythonDictionary getGamepadNow (int index, List whats)
		{
			PythonDictionary retval = new PythonDictionary ();
			foreach (string what in whats) {
				retval [what] = getGamepadNow (index, what);
			}
			return retval;
		}

		public object getGamepadNow (int index, string what)
		{
			if (what == "all") {
				return getGamepadNow (index, 
                             Graphics.PyList ("name", "axis", "ball", 
                                             "button", "hat", "count"));
			}
			if (what == "button") {
				return gamepads.getButtonStates (index);
			} else if (what == "name") {
				return Sdl.SDL_JoystickName (index);
			} else if (what == "axis") {
				return gamepads.getAxisStates (index);
			} else if (what == "robot") {
				List xy = gamepads.getAxisStates (index);
				return Graphics.PyList (-System.Convert.ToDouble (xy [1]), 
			       -System.Convert.ToDouble (xy [0]));
			} else if (what == "ball") {
				return gamepads.getBallStates (index);
			} else if (what == "hat") {
				return gamepads.getHatStates (index);
			} else if (what == "count") {
				return Sdl.SDL_NumJoysticks ();
			} else {
				throw new Exception (String.Format ("unknown gamepad component: '{0}'", what));
			}
		}

	        [method: JigsawTab(null)]
		public static bool Same (object o1, object o2)
		{
			if (o1 is PythonDictionary) {
				PythonDictionary d1 = (PythonDictionary)o1;
				PythonDictionary d2 = (PythonDictionary)o2;
				foreach (string key in d1.Keys) {
					if (! Same (d1 [key], d2 [key]))
						return false;
				}
				foreach (string key in d2.Keys) {
					if (! Same (d1 [key], d2 [key]))
						return false;
				}
				return true;
			} else if (o1 is List) {
				List l1 = (List)o1;
				List l2 = (List)o2;
				if (l1.Count != l2.Count)
					return false;
				for (int i = 0; i < l1.Count; i++) {
					if (! Same (l1 [i], l2 [i]))
						return false;
				}
				return true;
			} else {
				return o1.Equals (o2);
			}
		}

		public object getGamepad (List indices, string what)
		{
			List initials = new List ();
			foreach (int index in indices) {
				initials.append (getGamepadNow (index, what));
			}
			Sdl.SDL_JoystickUpdate ();
			List currents = new List ();
			foreach (int index in indices) {
				currents.append (getGamepadNow (index, what));
			}
			while (Same(initials, currents)) {
				wait (.01);
				Sdl.SDL_JoystickUpdate ();
				currents = new List ();
				foreach (int index in indices) {
					currents.append (getGamepadNow (index, what));
				}
			}
			return currents;
		}

		public object getGamepad (int index, string what)
		{
			object initial = getGamepadNow (index, what);
			Sdl.SDL_JoystickUpdate ();
			object current = getGamepadNow (index, what);
			while (Same(initial, current)) {
				wait (.01);
				Sdl.SDL_JoystickUpdate ();
				current = getGamepadNow (index, what);
			}
			return current;
		}

		public object getGamepad (List indices, List whats)
		{
			List initials = new List ();
			foreach (int index in indices) {
				initials.append (getGamepadNow (index, whats));
			}
			Sdl.SDL_JoystickUpdate ();
			List currents = new List ();
			foreach (int index in indices) {
				currents.append (getGamepadNow (index, whats));
			}
			while (Same(initials, currents)) {
				wait (.01);
				Sdl.SDL_JoystickUpdate ();
				currents = new List ();
				foreach (int index in indices) {
					currents.append (getGamepadNow (index, whats));
				}
			}
			return currents;
		}

		public object getGamepad (int index, List whats)
		{
			object initial = getGamepadNow (index, whats);
			Sdl.SDL_JoystickUpdate ();
			object current = getGamepadNow (index, whats);
			while (Same(initial, current)) {
				wait (.01);
				Sdl.SDL_JoystickUpdate ();
				current = getGamepadNow (index, whats);
			}
			return current;
		}
	}

	[method: JigsawTab("Senses")]
	public static object getGamepadNow ()
	{
		Sdl.SDL_JoystickUpdate ();
		return gamepads.getGamepadNow (0, "all");
	}

	[method: JigsawTab("Senses")]
	public static object getGamepadNow (int index)
	{
		Sdl.SDL_JoystickUpdate ();
		return gamepads.getGamepadNow (index, "all");
	}

	[method: JigsawTab("Senses")]
	public static object getGamepadNow (string what)
	{
		Sdl.SDL_JoystickUpdate ();
		return gamepads.getGamepadNow (0, what);
	}

	[method: JigsawTab("Senses")]
	public static object getGamepadNow (int index, string what)
	{
		Sdl.SDL_JoystickUpdate ();
		return gamepads.getGamepadNow (index, what);
	}

	[method: JigsawTab("Senses")]
	public static object getGamepadNow (int index, List whats)
	{
		Sdl.SDL_JoystickUpdate ();
		return gamepads.getGamepadNow (index, whats);
	}
  
	[method: JigsawTab("Senses")]
	public static object getGamepadNow (IList iterable)
	{
		Sdl.SDL_JoystickUpdate ();
		List retval = new List ();
		foreach (string what in iterable)
			retval.append (gamepads.getGamepadNow (0, what));
		return retval;
	}
  
	[method: JigsawTab("Senses")]
	public static object getGamepadNow (IList iterable, string what)
	{
		Sdl.SDL_JoystickUpdate ();
		List retval = new List ();
		foreach (int index in iterable)
			retval.append (gamepads.getGamepadNow (index, what));
		return retval;
	}
  
	[method: JigsawTab("Senses")]
	public static object getGamepadNow (IList iterable, List whats)
	{
		Sdl.SDL_JoystickUpdate ();
		List retval = new List ();
		foreach (int index in iterable)
			retval.append (gamepads.getGamepadNow (index, whats));
		return retval;
	}
  
	[method: JigsawTab("Senses")]
	public static object getGamepad ()
	{
		Sdl.SDL_JoystickUpdate ();
		return gamepads.getGamepad (0, "all");
	}

	[method: JigsawTab("Senses")]
	public static object getGamepad (int index)
	{
		Sdl.SDL_JoystickUpdate ();
		return gamepads.getGamepad (index, "all");
	}
  
	[method: JigsawTab("Senses")]
	public static object getGamepad (string what)
	{
		Sdl.SDL_JoystickUpdate ();
		return gamepads.getGamepad (0, what);
	}

	[method: JigsawTab("Senses")]
	public static object getGamepad (int index, string what)
	{
		Sdl.SDL_JoystickUpdate ();
		return gamepads.getGamepad (index, what);
	}

	[method: JigsawTab("Senses")]
	public static object getGamepad (int index, List whats)
	{
		Sdl.SDL_JoystickUpdate ();
		return gamepads.getGamepad (index, whats);
	}
  
	[method: JigsawTab("Senses")]
	public static object getGamepad (List whats)
	{
		Sdl.SDL_JoystickUpdate ();
		return gamepads.getGamepad (0, whats);
	}
  
	[method: JigsawTab("Senses")]
	public static object getGamepad (List indices, string what)
	{
		Sdl.SDL_JoystickUpdate ();
		return gamepads.getGamepad (indices, what);
	}
  
	[method: JigsawTab("Senses")]
	public static object getGamepad (List indices, List whats)
	{
		Sdl.SDL_JoystickUpdate ();
		return gamepads.getGamepad (indices, whats);
	}
  
	public class MyTexView : Gtk.TextView
	{
		public string MyString;
	}

	// Functional Interface

	[method: JigsawTab("Robot")]
	public static Robot getRobot ()
	{
		return robot;
	}

	[method: JigsawTab("Robot")]
	public static void init ()
	{
		initialize (null);
	}

	[method: JigsawTab(null)]
	public static void initialize ()
	{
		initialize (null);
	}

	[method: JigsawTab(null)]
	public static void init (string robot_type, string port)
	{
	}

	[method: JigsawTab("Robot")]
	public static void init (string port)
	{
		initialize (port, 38400);
	}

	[method: JigsawTab(null)]
	public static void init (string port, int baud)
	{
		initialize (port, baud);
	}

	[method: JigsawTab(null)]
	public static void initialize (string port, int baud=38400)
    {
	  // assumes a single robot will be used
	  if (port == null) {
		object retval = ask("Please enter port");
		if (retval == null)
		  return;
		else
		  port = retval.ToString();
	  }
	  if (port.StartsWith ("sim")) {
		if (simulation == null || ! simulation.window.Visible) {
		  simulation = new Simulation ();
		  //Thread.Sleep ((int)(5 * 1000));
		} 
		else {
		  simulation.setup ();
		}
		// defaults to SimScribbler in this interface
		if (robot != null && robot.GetType().Name == "SimScribbler") {
		  // don't add another!
		} else {
		  robot = makeRobot ("SimScribbler", simulation); 
		}
	  } else {
		if (Myro.robot != null)
		  Myro.robot.reinit (port, baud);
		else {
		  // defaults to Scribbler in this interface
		  Myro.robot = makeRobot ("Scribbler", port, baud);
		}
	  }
	}
  
	[method: JigsawTab(null)]
	public static Type[] getTypesOfArgs (object [] objects)
	{
		Type [] retval = new Type[objects.Length];
		int count = 0;
		foreach (object obj in objects) {
			retval [count] = obj.GetType ();
			count++;
		}
		return retval;
	}

	[method: JigsawTab("Robot")]
	public static Robot makeRobot (string robot_type, params object [] args)
	{
	    string path = System.IO.Path.GetDirectoryName (System.Reflection.Assembly.GetExecutingAssembly ().GetName ().CodeBase).Substring (5);
	    if (path.StartsWith ("\\")) {
		path = path.Substring (1);
	    }
	    DirectoryInfo d = new DirectoryInfo (System.IO.Path.Combine (path, "Myro", "Robots"));		
	    foreach (FileInfo f in d.GetFiles("*.dll")) {
		//System.Console.WriteLine ("Loading {0}...", f.FullName);
		Assembly assembly = Assembly.LoadFrom (f.FullName);
		if (assembly != null) {
		    foreach (Type type in assembly.GetTypes()) {
			Type [] types = getTypesOfArgs (args);
			ConstructorInfo constructor = type.GetConstructor (types);
			if (constructor != null && type.Name == robot_type) {
			    Robot robot;
			    try {
				robot = (Robot)constructor.Invoke (args);
			    } catch (Exception e) {
				//System.Console.WriteLine ("Failure; skipping robot '{0}': {1}", f.Name, e.Message);
				continue;
			    }
			    Myro.robot = robot;
			    return robot;
			}
		    }
		}
	    }
	    throw new Exception(String.Format("Unable to make robot of type '{0}'; did you give proper arguments?",
					      robot_type));
	}

	public class Simulation
	{
		public Graphics.WindowClass window;
		public Thread thread;
		public List<Robot> robots = new List<Robot> ();
		public List<Graphics.Shape> lights = new List<Graphics.Shape> ();
		public Graphics.Color groundColor = new Graphics.Color (24, 155, 28);
		public double extra_simulation_time = 0.0;
        public bool run = true;
	    
		public Simulation (string title, int width, int height, Graphics.Color color)
		{
			window = makeWindow (title, width, height);
			groundColor = color;
			window.setBackground (groundColor);
			window.mode = "physics";
			window.gravity = Graphics.Vector (0, 0); // turn off gravity
		}

		public Simulation () : this(640, 480, true)
		{
		}

		public Simulation (int width, int height) : this(width, height, true)
		{
		}

		public Simulation (int width, int height, bool load_default)
		{
			window = makeWindow ("Myro Simulation", width, height);
			window.setBackground (groundColor);
			window.mode = "physics";
			window.gravity = Graphics.Vector (0, 0); // turn off gravity
		
			if (load_default) {
			    // Non-physical things here:
			    addLight (new Graphics.Point (width - 100, height - 100), 
				      50, new Graphics.Color ("yellow"));
			    addWall (new Graphics.Point (0, 0), 
				     new Graphics.Point (5, height));
			    addWall (new Graphics.Point (5, 0), 
				     new Graphics.Point (width - 5, 5));
			    addWall (new Graphics.Point (width - 5, 0), 
				     new Graphics.Point (width, height));
			    addWall (new Graphics.Point (0, height - 5), 
				     new Graphics.Point (width - 5, height));
			    Graphics.Rectangle pyramid = new Graphics.Rectangle (
										 new Graphics.Point (100, 100), 
										 new Graphics.Point (150, 150));
			    pyramid.color = makeColor ("orange");
			    pyramid.rotate (45);
			    pyramid.bodyType = "static";
			    addShape (pyramid);
			    Graphics.Circle ball = new Graphics.Circle (
									new Graphics.Point (200, height - 150), 
									25);
			    ball.color = makeColor ("blue");
			    addShape (ball);      
			}
			setup();
		}
	    
		public void addLight (IList list, int radius, Graphics.Color color)
		{
			// Non-physical things here:
			window.mode = "auto";
			Graphics.Circle light = new Graphics.Circle (new Graphics.Point (list [0], list [1]), radius);
			light.gradient = new Graphics.Gradient ("radial", 
							new Graphics.Point (0, 0), 
							10, 
							new Graphics.Color ("yellow"),
							new Graphics.Point (0, 0), 
							radius, 
							groundColor);
			light.outline = groundColor;
			lights.Add (light);
			light.draw (window);
			window.mode = "physics";
		}

		public void addWall (IList ul, IList lr)
		{
			addWall (ul, lr, new Graphics.Color ("purple"));
		}

		public void addWall (IList ul, IList lr, Graphics.Color color)
		{
			Graphics.Rectangle wall = new Graphics.Rectangle (new Graphics.Point (ul [0], ul [1]), 
								  new Graphics.Point (lr [0], lr [1]));
			wall.fill = color;
			wall.bodyType = "static";
			wall.draw (window);
		}
    
		public void addShape (Graphics.Shape shape)
		{
			shape.draw (window);
		}
    
		public void setup ()
		{
			if (thread == null || !thread.IsAlive) {
				thread = new Thread (new ThreadStart (loop));
				thread.IsBackground = true;
				thread.Start ();
			}      
		}

	  public void step() {
	      if (window.isRealized()) {
		  foreach (Robot robot in robots) {
		      robot.draw_simulation ();
		  }
		  window.step (.1);
	      }
	  }

	  public void stop() {
		run = false;
	  }

	  public void setPose(int position, int x, int y, double theta) {
		robots[position].setPose(x, y, theta);
		window.refresh ();
	  }

	  public void setOption(int position, string option, object value) {
		robots[position].setOption(option, value);
		window.refresh ();
	  }

	  public void loop ()
	  {
		  run = true;
		  while (window.IsRealized && run) {
			foreach (Robot robot in robots) {
			  robot.draw_simulation ();
			}
			if (!window.IsRealized)
			  return;
			window.step (.1);
			wait (extra_simulation_time);
		  }
		}
	}

	[method: JigsawTab("Robot")]
	public static void uninit ()
	{
		if (Myro.robot != null)
			Myro.robot.uninit ();
	}

	[method: JigsawTab(null)]
	public static string repeat (string s, int times)
	{
		// repeat(" ", 10) => "          "
		string retval = "";
		for (int i = 0; i < times; i++) {
			retval += s;
		}
		return retval;
	}
  
	[method: JigsawTab(null)]
	public static string pad (string s, int length)
	{
		// pad("hi", 3) => "hi "
		if (length <= s.Length) { // trim
			return s.Substring (0, length);
		} else {
			return (s + repeat (" ", length)).Substring (0, length);
		}
	}
  
	[method: JigsawTab("Graphics")]
	public static List rgb2yuv (int R, int G, int B)
	{
		int Y = (int)(0.299 * R + 0.587 * G + 0.114 * B);
		int U = (int)(-0.14713 * R - 0.28886 * G + 0.436 * B + 128);
		int V = (int)(0.615 * R - 0.51499 * G - 0.10001 * B + 128);
		return Graphics.PyList (
        Math.Max (Math.Min (Y, 255), 0),
        Math.Max (Math.Min (U, 255), 0),
        Math.Max (Math.Min (V, 255), 0));
	}

	[method: JigsawTab("Movement")]
	public static void forward (double power)
	{
		robot.forward (power);
	}
  
	[method: JigsawTab("Movement")]
	public static void forward (double power, double time)
	{
		robot.forward (power, time);
	}
  
	[method: JigsawTab("Movement")]
	public static void translate (double power)
	{
		robot.translate (power);
	}
  
	[method: JigsawTab("Movement")]
	public static void translate (double power, double time)
	{
		robot.translate (power, time);
	}
  
	[method: JigsawTab("Movement")]
	public static void rotate (double power)
	{
		robot.rotate (power);
	}

	[method: JigsawTab("Movement")]
	public static void rotate (double power, double time)
	{
		robot.rotate (power, time);
	}

	[method: JigsawTab("Movement")]
	public static void backward (double power)
	{
		robot.backward (power);
	}

	[method: JigsawTab("Movement")]
	public static void backward (double power, double time)
	{
		robot.backward (power, time);
	}

	[method: JigsawTab("Movement")]
	public static void stop ()
	{
		robot.stop ();
	}
  
	[method: JigsawTab("Actions")]
	public static void penDown ()
	{
		robot.penDown ();
	}
  
	[method: JigsawTab("Actions")]
	public static void penDown (string color)
	{
		robot.penDown (color);
	}
  
	[method: JigsawTab("Actions")]
	public static Graphics.Line penUp ()
	{
		return robot.penUp ();
	}
  
	[method: JigsawTab("Movement")]
	public static void move (double translate, double rotate)
	{
		robot.move (translate, rotate);
	}
  
	[method: JigsawTab("Movement")]
	public static void turnLeft (double power)
	{
		robot.turnLeft (power);
	}

	[method: JigsawTab("Movement")]
	public static void turnLeft (double power, double time)
	{
		robot.turnLeft (power, time);
	}

	[method: JigsawTab("Movement")]
	public static void turnRight (double power)
	{
		robot.turnRight (power);
	}

	[method: JigsawTab("Movement")]
	public static void turnRight (double power, double time)
	{
		robot.turnRight (power, time);
	}

	[method: JigsawTab("Movement")]
	public static void motors (double left, double right)
	{
		robot.motors (left, right);
	}

	[method: JigsawTab("Robot")]
	public static void reboot ()
	{
		robot.reboot ();
	}

	[method: JigsawTab("Audio")]
	public static void stopBeep ()
	{
		if (robot != null)
			robot.beep (0.001, 0);
		else {
			Myro.computer.stopBeep ();
		}
	}

	[method: JigsawTab("Audio")]
	public static void beep (double duration, double frequency)
	{
		if (robot != null)
			robot.beep (duration, frequency);
		else {
			Myro.computer.beep (duration, frequency);
		}
	}

	[method: JigsawTab("Audio")]
	public static void beep (double duration, double frequency, double frequency2)
	{
		if (robot != null)
			robot.beep (duration, frequency, frequency2);
		else {
			Myro.computer.beep (duration, frequency, frequency2);
		}
	}

	[method: JigsawTab("Audio")]
	public static void beep (int duration, int frequency)
	{
		if (robot != null)
			robot.beep (duration, frequency);
		else {
			Myro.computer.beep (duration, frequency);
		}
	}

	[method: JigsawTab("Audio")]
	public static void beep (int duration, int frequency, int frequency2)
	{
		if (robot != null)
			robot.beep (duration, frequency, frequency2);
		else {
			Myro.computer.beep (duration, frequency, frequency2);
		}
	}

	[method: JigsawTab("Audio")]
	public static void beep (double duration, int frequency)
	{
		if (robot != null)
			robot.beep (duration, frequency);
		else {
			Myro.computer.beep (duration, frequency);
		}
	}

	[method: JigsawTab("Audio")]
	public static void beep (double duration, int frequency, int frequency2)
	{
		if (robot != null)
			robot.beep (duration, frequency, frequency2);
		else {
			Myro.computer.beep (duration, frequency, frequency2);
		}
	}

	[method: JigsawTab("Audio")]
	public static void play (string filename)
	{
		Myro.computer.play (filename);
	}
	
	[method: JigsawTab("Audio")]
	public static void playUntilDone (string filename)
	{
		Myro.computer.playUntilDone (filename);
	}

	[method: JigsawTab("Audio")]
	public static void playUntilDone (SdlDotNet.Audio.Sound sound)
	{
		Myro.computer.playUntilDone (sound);
	}

	[method: JigsawTab("Audio")]
	public static SdlDotNet.Audio.Channel play (string filename, int loop, double seconds)
	{
		return Myro.computer.play (filename, loop, seconds);
	}

	[method: JigsawTab("Audio")]
	public static SdlDotNet.Audio.Channel play (string filename, int loop)
	{
		return Myro.computer.play (filename, loop);
	}

	[method: JigsawTab("Audio")]
	public static SdlDotNet.Audio.Channel play (string filename, bool loop_forever)
	{
		return Myro.computer.play (filename, loop_forever);
	}
	
	[method: JigsawTab("Audio")]
	public static object makeSound (string filename)
	{
		return Myro.computer.makeSound (filename);
	}

	[method: JigsawTab("Audio")]
	public static void setPhases (double phase1, double phase2)
	{
		Myro.computer.setPhases (phase1, phase2);
	}

	[method: JigsawTab("Audio")]
	public static void play (double duration, Func<int[],int,object> function)
	{
		Myro.computer.play (duration, function);
	}

	/*
  public static void play(double duration, Func<int[],double,double,object> function) {
    // play a function
    //audioManager.play(duration, function);
  }

  public static void play(double duration, Func<int,object> function) {
    // play a function
    audioManager.play(duration, function);
  }
  */

	[method: JigsawTab("Picture")]
	public static void show (Graphics.Picture picture, 
      string title="Myro Camera")
	{
		Graphics.WindowClass win = Graphics.makeWindowFast (title,
        (int)(picture.width * picture.scaleFactor), (int)(picture.height * picture.scaleFactor));
		picture.draw (win);
		picture.setX (picture.width * picture.scaleFactor / 2);
		picture.setY (picture.height * picture.scaleFactor / 2);
	}

	[method: JigsawTab("Misc")]
	public static void setOption (string key, object value)
	{
		robot.setOption (key, value);
	}

	[method: JigsawTab("Robot")]
	public static void reinit (string port, int baud)
	{
		robot.reinit (port, baud);
	}

	[method: JigsawTab("Robot")]
	public static void setup ()
	{
		robot.setup ();
	}

	[method: JigsawTab("Senses")]
	public static string getName ()
	{
		return robot.getName ();
	}

	[method: JigsawTab("Senses")]
	public static List getIRMessage ()
	{
		return robot.getIRMessage ();
	}

	[method: JigsawTab("Actions")]
	public static void sendIRMessage (string data)
	{
	}

	[method: JigsawTab(null)]
	public static void setCommunicate ()
	{
	}

	[method: JigsawTab("Senses")]
	public static List getBlob ()
	{
		return robot.getBlob ();
	}

	[method: JigsawTab("Senses")]
	public static object getData (params int [] position)
	{
		return robot.getData (position);
	}

	[method: JigsawTab(null)]
	public static void setData (int position, int value)
	{
		robot.setData (position, value);
	}

	[method: JigsawTab("Senses")]
	public static PythonDictionary getAll ()
	{
		return robot.getAll ();
	}

	[method: JigsawTab("Senses")]
	public static PythonDictionary getInfo ()
	{
		return robot.getInfo ();
	}

	[method: JigsawTab("Senses")]
	public static object getObstacle (params object [] position)
	{
		if (position == null || position.Length == 0)
			return robot.getObstacle ();
		else
			return robot.getObstacle (position);
	}

	[method: JigsawTab("Senses")]
	public static object getDistance (params object [] position)
	{
		if (position == null || position.Length == 0)
			return robot.getDistance ();
		else
			return robot.getDistance (position);
	}

	[method: JigsawTab("Senses")]
	public static object getLight (params object [] position)
	{
		if (position == null || position.Length == 0)
			return robot.getLight ();
		else
			return robot.getLight (position);
	}

	[method: JigsawTab("Senses")]
	public static object getIR (params object [] position)
	{
		if (position == null || position.Length == 0)
			return robot.getIR ();
		else
			return robot.getIR (position);
	}

	[method: JigsawTab("Senses")]
	public static object getBright ()
	{
		return robot.getBright ();
	}

	[method: JigsawTab("Senses")]
	public static object getBright (string window)
	{
		return robot.getBright (window);
	}

	[method: JigsawTab("Senses")]
	public static object getBright (int window)
	{
		return robot.getBright (window);
	}

	[method: JigsawTab("Senses")]
	public static object getLine (params object [] position)
	{
		if (position == null || position.Length == 0)
			return robot.getLine ();
		else
			return robot.getLine (position);
	}

	[method: JigsawTab("Senses")]
	public static object get (string sensor="all")
	{
		return robot.get (sensor);
	}

	[method: JigsawTab("Senses")]
	public static object get (string sensor="all", params object [] position)
	{
		return robot.get (sensor, position);
	}

	[method: JigsawTab("Senses")]
	public static string getPassword ()
	{
		return robot.getPassword ();
	}

	[method: JigsawTab("Senses")]
	public static double getBattery ()
	{
		return robot.getBattery ();
	}

	[method: JigsawTab("Senses")]
	public static PythonDictionary getConfig ()
	{
		return robot.getConfig ();
	}

	[method: JigsawTab("Senses")]
	public static int getStall ()
	{
		return robot.getStall ();
	}

	[method: JigsawTab("Actions")]
	public static void setLED (string position, object value)
	{
		robot.setLED (position, value);
	}

	[method: JigsawTab("Actions")]
	public static void setLEDFront (object value)
	{
		robot.setLEDFront (value);
	}

	[method: JigsawTab("Actions")]
	public static void setLEDBack (double value)
	{
		robot.setLEDBack (value);
	}

	[method: JigsawTab("Actions")]
	public static void setEchoMode (int value)
	{
		robot.setEchoMode (value);
	}

	[method: JigsawTab("Robot")]
	public static void setName (string name)
	{
		robot.setName (name);
	}

	[method: JigsawTab("Actions")]
	public static void setIRPower (int power)
	{
		robot.setIRPower (power);
	}

	[method: JigsawTab("Picture")]
	public static void setWhiteBalance (object value)
	{
		robot.setWhiteBalance (value);
	}

	[method: JigsawTab("Robot")]
	public static void setForwardness (object value)
	{
		robot.setForwardness (value);
	}

	[method: JigsawTab("Senses")]
	public static object getForwardness ()
	{
		return robot.getForwardness ();
	}

	[method: JigsawTab("Audio")]
	public static void setVolume (object volume)
	{
		robot.setVolume (volume);
	}

	[method: JigsawTab("Robot")]
	public static void setPassword (string password)
	{
		robot.setPassword (password);
	}

    [method: JigsawTab("Picture")]
    public static void darkenCamera(int level)
    {
        robot.darkenCamera(level);
    }
    
    [method: JigsawTab("Picture")]
    public static void manualCamera(int gain=0x00, int brightness=0x80, int exposure=0x41)
    {
        robot.manualCamera(gain, brightness, exposure);
    }

    [method: JigsawTab("Picture")]
    public static void autoCamera()
    {
        robot.autoCamera();
    }
	
    // fluke2
    [method: JigsawTab("Picture")]
    public static void setPicSize(string size)
    {
        robot.setPicSize(size);
    }

    [method: JigsawTab("Actions")]
    public static void servo(int id, int value)
    {
        robot.servo(id, value);
    }
    
    [method: JigsawTab(null)]
    public static void enablePanNetworking()
    {
        robot.enablePanNetworking();
    }
    
    [method: JigsawTab("Actions")]
    public static string getFlukeLog()
    {
        return robot.getFlukeLog();
    }

	// s2
	[method: JigsawTab("Senses")]
	public static object getEncoders (bool zero = false)
	{
		return robot.getEncoders (zero);
	}

	[method: JigsawTab("Senses")]
	public static object getMicrophone ()
	{
		return robot.getMicrophone ();
	}
  
	[method: JigsawTab("Senses")]
	public static object getPosition ()
	{
		return robot.getPosition ();
	}
  
	[method: JigsawTab("Senses")]
	public static int getAngle ()
	{
		return robot.getAngle ();
	}
  
       [method: JigsawTab("Movement")]
	public static void turnTo (int angle, string units = "deg")
	{
		robot.turnTo (angle, units);
	}

       [method: JigsawTab("Movement")]
	public static void turnBy (int angle, string units = "deg")
	{
		robot.turnBy (angle, units);
	}

       [method: JigsawTab("Movement")]
	public static void setPosition (int x, int y)
	{
		robot.setPosition (x, y);
	}

       [method: JigsawTab("Movement")]
	public static void setAngle (uint a)
	{
		robot.setAngle (a);
	}

       [method: JigsawTab("Movement")]
	public static void setBeginPath (int speed=7)
	{
		robot.setBeginPath (speed);
	}

       [method: JigsawTab("Movement")]
	public static void moveTo (int x, int y, string units = "mm")
	{
		robot.moveTo (x, y, units);
	}

       [method: JigsawTab("Movement")]
	public static void moveBy (int x, int y, string units = "mm")
	{
		robot.moveBy (x, y, units);
	}

       [method: JigsawTab("Movement")]
	public static void arc (int degrees, int radius)
	{
		robot.arc (degrees, radius);
	}

       [method: JigsawTab("Movement")]
	public static void arcTo (int x, int y, int radius, string units = "mm")
	{
		robot.arcTo (x, y, radius, units);
	}

       [method: JigsawTab("Movement")]
	public static void arcBy (int x, int y, int radius, string units = "mm")
	{
		robot.arcBy (x, y, radius, units);
	}
  
       [method: JigsawTab("Movement")]
	public static void setEndPath ()
	{
		robot.setEndPath ();
	}
  
       [method: JigsawTab("Audio")]
	public static void setS2Volume (int level)
	{
		robot.setS2Volume (level);
	}
 	
	[method: JigsawTab("Misc")]
	public static string flipCoin ()
	{
		if (Random.random () < .5) {
			return "heads";
		} else {
			return "tails";
		}
	}

        [method: JigsawTab(null)]
	public static int randint (int start, int end)
	{
		return (int)(Random.random () * (end - start + 1) + start);
	}

	[method: JigsawTab("Misc")]
	public static bool heads ()
	{
		return (flipCoin () == "heads");
	}

	[method: JigsawTab("Misc")]
	public static bool tails ()
	{
		return (flipCoin () == "tails");
	}

	[method: JigsawTab("Misc")]
	public static double randomNumber ()
	{
		return Random.random ();
	}

	[method: JigsawTab("Misc")]
	public static double random ()
	{
		return Random.random ();
	}

	[method: JigsawTab("Misc")]
	public static List getFilenames (string path)
	{
		List<string > retval = new List<string> ();
		List filenames = new List ();
		string directory = "";
		string pattern;
		string [] parts = path.Split (Path.DirectorySeparatorChar);
		pattern = parts [parts.Length - 1]; // last
		for (int i = 0; i < parts.Length - 1; i++) {
			if (parts [i] == "") {
				directory = (directory + Path.DirectorySeparatorChar);
			} else {
				directory = Path.Combine (directory, parts [i]);
			}
		}
		if (directory == "") {
			directory = ".";
		}
		directory = Path.GetFullPath (directory);
		DirectoryInfo di = new DirectoryInfo (directory);
		FileInfo[] rgFiles = di.GetFiles (pattern);
		foreach (FileInfo fi in rgFiles) {
			retval.Add (Path.Combine (directory, fi.Name));
		}
		retval.Sort ();
		foreach (string filename in retval) {
			filenames.append (filename);
		}
		return filenames;
	}

	[method: JigsawTab("Misc")]
	public static string pickAFile ()
	{
	    string pickAFileResponse = null;
	    ManualResetEvent ev = new ManualResetEvent(false);
	    Invoke (delegate {
		    Gtk.FileChooserDialog fc = new Gtk.FileChooserDialog (
                            "Select a file",
			    null,
			    Gtk.FileChooserAction.Open,
			    "Cancel", 
			    Gtk.ResponseType.Cancel,
			    "Select File", 
			    Gtk.ResponseType.Accept);
		    if (fc.Run() == (int)Gtk.ResponseType.Accept) {
			pickAFileResponse = fc.Filename;
		    }
		    fc.Destroy();
		    ev.Set();
		});
	    ev.WaitOne ();
	    return pickAFileResponse;
	}

	[method: JigsawTab("Misc")]
	public static string pickAFolder ()
	{
	    string pickAFolderResponse = null;
	    ManualResetEvent ev = new ManualResetEvent(false);
	    Invoke (delegate {
		    Gtk.FileChooserDialog fc = new Gtk.FileChooserDialog (
		        "Select a folder",
			null,
			Gtk.FileChooserAction.SelectFolder,
			"Cancel", Gtk.ResponseType.Cancel,
			"Select Folder", Gtk.ResponseType.Accept);
		    if (fc.Run() == (int)Gtk.ResponseType.Accept) {
			pickAFolderResponse = fc.Filename;
		    }
		    fc.Destroy();
		    ev.Set();
		});
	    ev.WaitOne ();
	    return pickAFolderResponse;
	}

	[method: JigsawTab("Graphics")]
	public static Graphics.Color pickAColor ()
	{
	    Graphics.Color pickAColorResponse = null;
	    ManualResetEvent ev = new ManualResetEvent(false);
	    Invoke (delegate {
		    Gtk.ColorSelectionDialog fc = new Gtk.ColorSelectionDialog ("Select a color");
		    if (fc.Run() == (int)Gtk.ResponseType.Accept) {
			pickAColorResponse = new Graphics.Color (
  			   (int)Math.Round (((double)((int)fc.ColorSelection.CurrentColor.Red)) / Math.Pow (2, 16) * 255.0),
			   (int)Math.Round (((double)((int)fc.ColorSelection.CurrentColor.Green)) / Math.Pow (2, 16) * 255.0),
			   (int)Math.Round (((double)((int)fc.ColorSelection.CurrentColor.Blue)) / Math.Pow (2, 16) * 255.0));
		    }
		    fc.Destroy();
		    ev.Set();
		});
	    ev.WaitOne ();
	    return pickAColorResponse;
	}

	[method: JigsawTab("Graphics")]
	public static string pickAFont ()
	{
	    string pickAFontResponse = null;
	    ManualResetEvent ev = new ManualResetEvent(false);
	    Invoke (delegate {
		    Gtk.FontSelectionDialog fc = new Gtk.FontSelectionDialog ("Select a font");
		    if (fc.Run() == (int)Gtk.ResponseType.Accept) {
			pickAFontResponse = fc.FontName;
		    }
		    fc.Destroy();
		    ev.Set();
		});
	    ev.WaitOne ();
	    return pickAFontResponse;
	}

	[method: JigsawTab("Senses")]
	public static object ask (object question, string title)
	{
	    ManualResetEvent ev = new ManualResetEvent(false);
	    object askResponse = null;
	    Gtk.Entry myentry = null;
	    PythonDictionary responses = new PythonDictionary ();
	    Invoke (delegate {
			  Gtk.MessageDialog fc = new MessageDialog (null,
				  0, Gtk.MessageType.Question,
				  Gtk.ButtonsType.OkCancel,
				  title);
			  if (question is List) {
				foreach (string choice in (List)question) {
				  Gtk.HBox hbox = new Gtk.HBox ();
				  Gtk.Label label = new Gtk.Label (choice.Replace("_", "__") + ":");
				  Gtk.Entry entry = new Gtk.Entry ();
				  responses [choice] = entry;
				  hbox.PackStart (label);
				  hbox.PackStart (entry);
				  fc.VBox.PackStart (hbox);
				}
			  } else 	if (question is IDictionary) {
				foreach (Object choice in ((IDictionary)question).Keys) {
				  Gtk.HBox hbox = new Gtk.HBox ();
				  Gtk.Label label = new Gtk.Label (choice.ToString().Replace("_", "__") + ":");
				  Gtk.Entry entry = new Gtk.Entry (((IDictionary)question)[choice].ToString());
				  responses [choice.ToString()] = entry;
				  hbox.PackStart (label);
				  hbox.PackStart (entry);
				  fc.VBox.PackStart (hbox);
				}
				
			  } else {
				string choice = (string)question;
				Gtk.HBox hbox = new Gtk.HBox ();
				Gtk.Label label = new Gtk.Label (choice + ":");
				Gtk.Entry entry = new Gtk.Entry ();
				myentry = entry;
				hbox.PackStart (label);
				hbox.PackStart (entry);
				fc.VBox.PackStart (hbox);
			  }
			  fc.ShowAll();
			  if (fc.Run() == (int)Gtk.ResponseType.Ok) {
				if (question is List) {
				  foreach (string choice in responses.Keys) {
					responses [choice] = ((Gtk.Entry)responses [choice]).Text;
				  }
				  askResponse = responses;
				} else if (question is IDictionary) {
				  foreach (string choice in responses.Keys) {
					responses [choice] = ((Gtk.Entry)responses [choice]).Text;
				  }
				  askResponse = responses;
				} else {
				  askResponse = myentry.Text;
				}
			  }
			  fc.Destroy();
			  ev.Set();
			}); 
	    ev.WaitOne ();
	    return askResponse;
	}

        [method: JigsawTab("Graphics")]
	public static List getColorNames ()
	{
		return Graphics.getColorNames ();
	}

	[method: JigsawTab("Graphics")]
	public static Graphics.WindowClass Window (
      string title="Calico Graphics",
      int width=300, 
      int height=300)
	{
		return Graphics.makeWindow (title, width, height);
	}

	[method: JigsawTab("Graphics")]
	public static Graphics.WindowClass Window (int width, int height)
	{
		return makeWindow ("Calico Graphics", width, height);
	}

	[method: JigsawTab("Graphics")]
	public static bool yesno (string question)
	{
	    bool yesnoResponse = false;
	    ManualResetEvent ev = new ManualResetEvent(false);
	    Invoke (delegate {
		    Gtk.MessageDialog fc = new Gtk.MessageDialog (
				null,
				0, Gtk.MessageType.Question,
				Gtk.ButtonsType.YesNo,
				question);
		    if (fc.Run() == (int)Gtk.ResponseType.Yes) {
			yesnoResponse = true;
		    }
		    fc.Destroy();
		    ev.Set();
		});
	    ev.WaitOne ();
	    return yesnoResponse;
	}

	[method: JigsawTab("Graphics")]
	public static void inform (string question)
	{
            ManualResetEvent ev = new ManualResetEvent(false);
	    Invoke (delegate {
		    Gtk.MessageDialog fc = new Gtk.MessageDialog (
		          null,
			  0, Gtk.MessageType.Info,
			  Gtk.ButtonsType.Ok,
			  question);
		    fc.Run();
		    fc.Destroy();
		    ev.Set();
		});
	    ev.WaitOne ();
	}

	[method: JigsawTab("Senses")]
	public static string askQuestion (string question, IList choices)
	{
	    string askResponse = null;
	    ManualResetEvent ev = new ManualResetEvent(false);
	    Invoke (delegate {
		    Gtk.Dialog fc = new Gtk.Dialog ("Information Request", null, 0);
		    fc.VBox.PackStart (new Gtk.Label (question));
		    foreach (string choice in choices) {
			Gtk.Button button = new Gtk.Button (choice);
			button.Clicked += (obj, a) => {
			    Invoke (delegate {			    
				    askResponse = ((Gtk.Button)obj).Label;
				    fc.Destroy ();
				    ev.Set();
				});
			}
			if (choices.Count < 5) {
			    fc.AddActionWidget (button, Gtk.ResponseType.Ok);
			} else {
			    fc.VBox.PackStart (button, true, true, 5);
			}
		    }
		    fc.ShowAll();
		    fc.Run();
		});
	    ev.WaitOne ();
	    return askResponse;
	}

        public static void DialogHandler (object obj, 
					  System.EventArgs args, 
					  Gtk.Dialog dialog)
        {
	}

        [method: JigsawTab(null)]
	public static string to_s (object obj)
	{
		return obj.ToString ();
	}

        [method: JigsawTab(null)]
	public static List to_l (IList obj)
	{
		List list = new List ();
		foreach (object choice in obj)
			list.append (choice.ToString ());
		return list;
	}

	[method: JigsawTab("Movement")]
	public static void joystick ()
	{
		new Joystick ();
	}

	class Joystick
	{
		Graphics.WindowClass window;
		Graphics.Line arrow = new Graphics.Line (new Graphics.Point (200, 200), 
                        new Graphics.Point (200, 200));
		double x, y;
		double r, t;
		string state = "up";
		double center_x, center_y;
		int radius = 175;

		public Joystick ()
		{
			window = makeWindow ("Myro Joystick", 400, 400);
			window.ButtonPressEvent += onMouseDown;
			window.ButtonReleaseEvent += onMouseUp;
			window.MotionNotifyEvent += onMouseMove;
			Graphics.Circle circle = new Graphics.Circle (
                new Graphics.Point (200, 200), radius);
			circle.fill = new Graphics.Color ("white");
			circle.draw (window);
			circle = new Graphics.Circle (
                new Graphics.Point (200, 200), 10);
			circle.fill = new Graphics.Color ("black");
			circle.draw (window);
			Graphics.Text text = new Graphics.Text (
            new Graphics.Point (window.width / 2, 
                       12), "Forward");
			text.draw (window);
			text = new Graphics.Text (
            new Graphics.Point (window.width / 2, 
                       window.height - 12), "Backward");
			text.draw (window);
			text = new Graphics.Text (
            new Graphics.Point (12, 
                       window.height / 2), "Left");
			text.rotate (90);
			text.draw (window);
			text = new Graphics.Text (
            new Graphics.Point (window.width - 12, 
                       window.height / 2), "Right");
			text.rotate (-90);
			text.draw (window);
			arrow.border = 5;
			arrow.draw (window);
		}

		void onMouseUp (object obj, Gtk.ButtonReleaseEventArgs args)
		{
			state = "up";
			arrow.points [1].x = 0;
			arrow.points [1].y = 0;
			window.QueueDraw ();
			if (getRobot () != null) {
				getRobot ().stop ();
				wait(.1);
			}
		}

		void onMouseDown (object obj, Gtk.ButtonPressEventArgs args)
		{
			state = "down";
			x = args.Event.X;
			y = args.Event.Y;
			center_x = window.width / 2;
			center_y = window.height / 2;
			r = (center_x - x) / (center_x - radius);
			t = (center_y - y) / (center_y - radius);
			r = Math.Min (Math.Max (r, -1), 1);
			t = Math.Min (Math.Max (t, -1), 1);
			arrow.points [1].x = x - center_x;
			arrow.points [1].y = y - center_y;
			window.QueueDraw ();
			if (getRobot () != null) {
				getRobot ().move (t, r);
				wait(.1);
			}
		}

		void onMouseMove (object obj, Gtk.MotionNotifyEventArgs args)
		{
			if (state == "down") {
				x = args.Event.X;
				y = args.Event.Y;
				center_x = window.width / 2;
				center_y = window.height / 2;
				r = (center_x - x) / (center_x - radius);
				t = (center_y - y) / (center_y - radius);
				r = Math.Min (Math.Max (r, -1), 1);
				t = Math.Min (Math.Max (t, -1), 1);
				arrow.points [1].x = x - center_x;
				arrow.points [1].y = y - center_y;
				window.QueueDraw ();
				if (getRobot () != null) {
					getRobot ().move (t, r);
					wait(.1);
				}
			}
		}
	}

	[method: JigsawTab("Senses")]
	public static string askQuestion (string question)
	{
		return askQuestion (question, Graphics.PyList ("Yes", "No"));
	}

	[method: JigsawTab("Senses")]
	public static string input (object question)
	{
		return ask (question, "Input").ToString ();
	}

	[method: JigsawTab("Senses")]
	public static object ask ()
	{
		return ask ("Input: ", "Information Request");
	}

	[method: JigsawTab("Senses")]
	public static object ask (object question)
	{
		return ask (question, "Information Request");
	}

	public class MessageDialog : Gtk.MessageDialog
	{

		public MessageDialog (Gtk.Window window,
		Gtk.DialogFlags dialogFlags, 
		Gtk.MessageType messageType,
		Gtk.ButtonsType buttonsType,
		string title) : base(window, dialogFlags, messageType, buttonsType, 
			title)
		{
			KeyPressEvent += MyKeyPressEventHandler;
		}
	
		[GLib.ConnectBefore]
		public void MyKeyPressEventHandler (object obj, 
		Gtk.KeyPressEventArgs args)
		{
			if (args.Event.Key == Gdk.Key.Return) {
				Respond (Gtk.ResponseType.Ok);
				args.RetVal = true;
			}
		}
	}

	[method: JigsawTab("Misc")]
	public static object pickOne (params object [] items)
	{
		if (items.Length == 1) {
			if (items [0] is int) {
				return (int)(Random.random () * (int)items [0]);
			} else if (items [0] is IList<object>) {
				int pos = (int)(Random.random () * ((IList<object>)items [0]).Count);
				return ((IList<object>)items [0]) [pos];
			} else {
				throw new Exception ("pickOne: unknown item type");
			}
		} else {
			int pos = (int)(Random.random () * items.Length);
			return items [pos];
		}
	}

	[method: JigsawTab("Misc")]
	public static void wait (double seconds)
	{
		if (seconds < .1)
			Thread.Sleep ((int)(seconds * 1000));
		else {
			double start = currentTime ();
			while (seconds > currentTime () - start) {
				while (Gtk.Application.EventsPending ())
					Gtk.Application.RunIteration ();
				Thread.Sleep (100); 
			}
		}
	}

	[method: JigsawTab("Misc")]
	public static double currentTime ()
	{
		System.TimeSpan t = System.DateTime.UtcNow - new System.DateTime (1970, 1, 1);
		return t.TotalSeconds;
	}

	[method: JigsawTab("Misc")]
	public static bool odd (int n)
	{
		return ((n % 2) == 1);
	}

	[method: JigsawTab("Misc")]
	public static bool even (int n)
	{
		return ((n % 2) == 0);
	}

	[method: JigsawTab("Picture")]
	public static Graphics.Picture takePicture (string mode="jpeg")
	{
		return robot.takePicture (mode);
	}

	[method: JigsawTab("Misc")]
	public static void setSeed (int value)
	{
		Random.seed = value;
	}

	[method: JigsawTab("Misc")]
	public static int getSeed ()
	{
		return Random.seed;
	}

	public class Randomizer
	{
		int _seed;
		Random _random = new Random ();

		public Randomizer () : this(0)
		{
		}
    
		public Randomizer (int seed)
		{
			if (seed != 0)
				this.seed = seed;
		}

		public int seed {
			get { 
				return _seed; 
			}
			set { 
				_seed = value; 
				_random = new Random (_seed);
			}
		}

		public double random ()
		{
			return _random.NextDouble ();
		}

	}
    
	// singleton
	public readonly static Randomizer Random = new Randomizer (); 
  
	public class Robot
	{
		public double _lastTranslate = 0;
		public double _lastRotate = 0;

		public virtual void setOption (string key, object value)
		{
		}
    
	    public virtual void setPose (int x, int y, double theta)
	    {
		}
    
		public virtual void beep (double duration, double frequency, double frequency2)
		{
			// Override in subclassed robots
			Myro.computer.beep (duration, frequency, frequency2);
		}
    
		public virtual void beep (double duration, double frequency)
		{
			// Override in subclassed robots
			Myro.computer.beep (duration, frequency);
		}
    
		public virtual void stopBeep ()
		{
			// Override in subclassed robots
			Myro.computer.stopBeep ();
		}
    
		public virtual void reboot ()
		{
		}
    
		public virtual Graphics.Picture takePicture (string mode="jpeg")
		{
			// Override in subclassed robots
			return null;
		}
    
		public virtual void reinit (string port, int baud)
		{
		}

		public virtual void uninit ()
		{
		}

		public virtual void setup ()
		{
		}
    
		public virtual string getName ()
		{
			return null;
		}
    
		public virtual List getIRMessage ()
		{
			return null;
		}
    
		public virtual void setCommunicate ()
		{
		}
    
		public virtual void sendIRMessage (string data)
		{
		}
    
		public virtual List getBlob ()
		{
			return null;
		}
    
		public virtual object getData (params int [] position)
		{
			return null;
		}
    
		public virtual void setData (int position, int value)
		{
		}
    
		public virtual PythonDictionary getAll ()
		{
			return null;
		}
    
		public virtual PythonDictionary getInfo ()
		{
			return null;
		}
    
		public virtual object getObstacle (params object [] position)
		{
			return null;
		}

		public virtual object getDistance (params object [] position)
		{
			return null;
		}
    
		public virtual object getLight (params object [] position)
		{
			return null;
		}
    
		public virtual object getIR (params object [] position)
		{
			return null;
		}
    
		public virtual object getBright ()
		{
			return null;
		}
    
		public virtual object getBright (string window)
		{
			return null;
		}
    
		public virtual object getBright (int window)
		{
			return null;
		}
    
		public virtual object getLine (params object [] position)
		{
			return null;
		}
    
		public virtual object get (string sensor="all")
		{
			return null;
		}
    
		public virtual object get (string sensor="all", params object [] position)
		{
			return null;
		}
    
		public virtual string getPassword ()
		{
			return null;
		}
    
		public virtual PythonDictionary getConfig ()
		{
			return null;
		}
    
		public virtual int getStall ()
		{
			return 0; 
		}

        public virtual void darkenCamera(int level)
        {
        }
        
        public virtual void manualCamera(int gain=0x00, int brightness=0x80, int exposure=0x41)
        {
        }

        public virtual void autoCamera()
        {
        }        
        
        // fluke2
        public virtual void setPicSize(string size)
        {
        }
        
        public virtual void servo(int id, int value)
        {
        }
        
        public virtual void enablePanNetworking()
        {
        }
        
        public virtual string getFlukeLog()
        {
            return "";
        }
		
		// s2
		public virtual object getEncoders (bool zero = false)
		{
			return null;
		}

		public virtual int getMicrophone ()
		{
			return 0;
		}

		public virtual object getPosition ()
		{
			return null;
		}

		public virtual int getAngle ()
		{
			return 0;
		}

		public virtual void setAngle (uint angle)
		{
      
		}
    
		public virtual void setBeginPath (int speed=7)
		{
      
		}

		public virtual void turnTo (int angle, string units = "deg")
		{
      
		}

		public virtual void turnBy (int angle, string units = "deg")
		{
      
		}

		public virtual void moveBy (int x, int y, string units = "mm")
		{
      
		}

		public virtual void moveTo (int x, int y, string units = "mm")
		{
      
		}

		public virtual void  arc (int degrees, int radius)
		{
      
		}

		public virtual void  arcTo (int x, int y, int radius, string units = "mm")
		{
      
		}

		public virtual void  arcBy (int x, int y, int radius, string units = "mm")
		{
			
		}
		public virtual void setEndPath ()
		{
		}

		public virtual void setS2Volume (int level)
		{
		}

		public virtual void setPosition (int x, int y)
		{
		}
		
		public virtual double getBattery ()
		{
			return 0.0;
		}
    
		public virtual void setLED (string position, object value)
		{
		}
    
		public virtual void setLEDFront (object value)
		{
		}
    
		public virtual void setLEDBack (double value)
		{
		}
    
		public virtual void setEchoMode (int value)
		{
		}
    
		public virtual void setName (string name)
		{
		}
    
		public virtual void setIRPower (int power)
		{
		}
    
		public virtual void setWhiteBalance (object value)
		{
		}
    
		public virtual void setForwardness (object value)
		{
		}
    
		public virtual object getForwardness ()
		{
		  return null;
		}
    
		public virtual void setVolume (object volume)
		{
		}
    
		public virtual void setPassword (string password)
		{
		}
    
		public virtual void adjustSpeed ()
		{
		}

		public virtual bool isConnected ()
		{
			return true;
		}

		public virtual void flush ()
		{
		}

		public void penDown ()
		{
			penDown ("black");
		}

		public virtual void penDown (string color)
		{
			ask (String.Format ("Please put a {0} pen in the robot.", color));
		}

		public virtual Graphics.Line penUp ()
		{
			return null;
		}

		public void move (double translate, double rotate)
		{
			_lastTranslate = translate;
			_lastRotate = rotate;
			adjustSpeed ();
		}
    
		public void playSong (List song)
		{
			playSong (song, 1.0);
		}
    
		public void playSong (List song, double speed)
		{
			foreach (IList tup in song) {
				if (tup.Count == 2) {
					double f = System.Convert.ToDouble (tup [0]); 
					double d = System.Convert.ToDouble (tup [1]);
					beep (d, f);
				} else if (tup.Count == 3) {
					double f1 = System.Convert.ToDouble (tup [0]); 
					double f2 = System.Convert.ToDouble (tup [1]); 
					double d = System.Convert.ToDouble (tup [2]);
					beep (d * speed, f1, f2);
				}
			}
		}
    
		public void stop ()
		{
			if (! isConnected ()) 
				return;
			move (0, 0);
		}
    
		public void forward (double speed, double interval)
		{
			move (speed, 0);
			wait (interval);
			stop ();
		}
    
		public void forward (double speed)
		{
			move (speed, 0);
		}
    
		public void translate (double speed)
		{
			_lastTranslate = speed;
			adjustSpeed ();
		}
    
		public void translate (double speed, double interval)
		{
			_lastTranslate = speed;
			adjustSpeed ();
			wait (interval);
			_lastTranslate = 0;
			adjustSpeed ();
		}
    
		public void rotate (double speed)
		{
			_lastRotate = speed;
			adjustSpeed ();
		}
    
		public void rotate (double speed, double interval)
		{
			_lastRotate = speed;
			adjustSpeed ();
			wait (interval);
			_lastRotate = 0;
			adjustSpeed ();
		}
    
		public void backward (double speed)
		{
			move (-speed, 0);
		}
    
		public void backward (double speed, double interval)
		{
			move (-speed, 0);
			wait (interval);
			stop ();
		}

		public void turnLeft (double speed)
		{
			move (0, speed);
		}

		public void turnLeft (double speed, double interval)
		{
			move (0, speed);
			wait (interval);
			stop ();
		}

		public void turnRight (double speed)
		{
			move (0, -speed);
		}
    
		public void turnRight (double speed, double interval)
		{
			move (0, -speed);
			wait (interval);
			stop ();
		}
    
		public void motors (double left, double right)
		{
			double trans = (right + left) / 2.0;
			double rotate = (right - left) / 2.0;
			move (trans, rotate);
		}

		public virtual void draw_simulation ()
		{
			throw new NotImplementedException ();
		}
	}

        [method: JigsawTab(null)]
	public static bool Contains (object item, params object[] items)
	{
		return ((IList<object>)items).Contains (item);
	}
		
	public class Computer
	{
		public bool audio_initialized = false;
		public bool sound_initialized = false;
		const int playbackFreq = 44100;
		const short samples = 2048;
		const double pi2 = 360 * Math.PI / 180.0;
		const double slice = 1.0 / playbackFreq * pi2;
		byte[] buffer8 = new byte[samples];
		double volume = 1.0;
		double[] frequencies = new double [2];
		double[] phases = new double [2] {0.0, 0.0};
		SdlDotNet.Audio.AudioStream stream = null;
		SdlDotNet.Audio.AudioCallback audioCallback = null;
		Func<int[],int,object> audio_function = null;
		int audio_index = 0;

		public Computer ()
		{
		}
		
		public void playSong (List song)
		{
			playSong (song, 1.0);
		}
    
		public void playSong (List song, double speed)
		{
			foreach (IList tup in song) {
				if (tup.Count == 2) {
					double f = System.Convert.ToDouble (tup [0]); 
					double d = System.Convert.ToDouble (tup [1]);
					beep (d, f);
				} else if (tup.Count == 3) {
					double f1 = System.Convert.ToDouble (tup [0]); 
					double f2 = System.Convert.ToDouble (tup [1]); 
					double d = System.Convert.ToDouble (tup [2]);
					beep (d * speed, f1, f2);
				}
			}
		}
    
		public SdlDotNet.Audio.Sound makeSound (string filename)
		{
			if (! sound_initialized)
				initialize_sound ();
			SdlDotNet.Audio.Sound sound = new SdlDotNet.Audio.Sound (filename);
			return sound;
		}

		public void playUntilDone (string filename)
		{
			if (! sound_initialized)
				initialize_sound ();
			SdlDotNet.Audio.Sound sound = new SdlDotNet.Audio.Sound (filename);
			SdlDotNet.Audio.Channel channel = sound.Play ();
			while (channel.IsPlaying())
				wait (.1);
		}

		public void playUntilDone (SdlDotNet.Audio.Sound sound)
		{
			SdlDotNet.Audio.Channel channel = sound.Play ();
			while (channel.IsPlaying())
				wait (.1);
		}

		public SdlDotNet.Audio.Channel play (string filename)
		{
			if (! sound_initialized)
				initialize_sound ();
			SdlDotNet.Audio.Sound sound = new SdlDotNet.Audio.Sound (filename);
			return sound.Play ();
		}

		public SdlDotNet.Audio.Channel play (string filename, int loop, double seconds)
		{
			if (! sound_initialized)
				initialize_sound ();
			SdlDotNet.Audio.Sound sound = new SdlDotNet.Audio.Sound (filename);
			return sound.Play (loop, (int)(seconds * 100));
		}

		public SdlDotNet.Audio.Channel play (string filename, int loop)
		{
			if (! sound_initialized)
				initialize_sound ();
			SdlDotNet.Audio.Sound sound = new SdlDotNet.Audio.Sound (filename);
			return sound.Play (loop);
		}

		public SdlDotNet.Audio.Channel play (string filename, bool loop_forever)
		{
			if (! sound_initialized)
				initialize_sound ();
			SdlDotNet.Audio.Sound sound = new SdlDotNet.Audio.Sound (filename);
			return sound.Play (loop_forever);
		}

		public void setPhases (double phase1, double phase2)
		{
			phases [0] = phase1;
			phases [1] = phase2;
		}

		public void play (double duration, Func<int[],int,object> function)
		{
			if (! audio_initialized) {
				initialize_tone ();
			}
			audio_function = function;
			Tao.Sdl.Sdl.SDL_PauseAudio (0); // start
			if (duration > 0) {
				//Tao.Sdl.Sdl.SDL_Delay((int)(duration * 1000));
				wait (duration);
				Tao.Sdl.Sdl.SDL_PauseAudio (1); // pause
			}
		}

		public void beep (double duration, double frequency)
		{
			beep (duration, frequency, -1);
		}

		public void initialize_tone ()
		{
			ManualResetEvent ev = new ManualResetEvent (false);
			try {
			    audioCallback = new SdlDotNet.Audio.AudioCallback (Callback);
			    stream = new SdlDotNet.Audio.AudioStream (playbackFreq,
								      SdlDotNet.Audio.AudioFormat.Unsigned8,
								      SdlDotNet.Audio.SoundChannel.Mono,
								      samples,
								      audioCallback,
								      null);
			} catch {
			    throw new Exception("Unable to initialize tone");
			}
			Invoke (delegate {
				// BUG: OpenAudio (or lower) apparently requires a *visible* screen
				try {
				    SdlDotNet.Graphics.Video.SetVideoMode (250, 1);
				    SdlDotNet.Graphics.Video.WindowCaption = "Calico Audio";
				    SdlDotNet.Audio.Mixer.OpenAudio (stream);
				    ev.Set ();
				} catch {
				    throw new Exception("Unable to initialize OpenAudio");
				}
			    });
			ev.WaitOne ();
			audio_initialized = true;
		}

		public void initialize_sound ()
		{
			ManualResetEvent ev = new ManualResetEvent (false);
			Invoke (delegate {
				// BUG: OpenAudio (or lower) apparently requires a *visible* screen
				try {
				    SdlDotNet.Graphics.Video.SetVideoMode (250, 1);
				    SdlDotNet.Graphics.Video.WindowCaption = "Calico Audio";
				    SdlDotNet.Audio.Mixer.Open ();
				    SdlDotNet.Audio.Mixer.ChannelsAllocated = 1000;
				    ev.Set ();
				} catch {
				    throw new Exception("Unable to initialize sound");
				}
			});
			ev.WaitOne ();
			sound_initialized = true;
		}

		public void beep (double duration, double frequency1, double frequency2)
		{
			if (! audio_initialized) {
				initialize_tone ();
			}
			audio_function = null;
			frequencies [0] = frequency1;
			frequencies [1] = frequency2;
			phases [1] = phases [0]; // set the phases to be the same
			Tao.Sdl.Sdl.SDL_PauseAudio (0); // start
			if (duration > 0) {
				//Tao.Sdl.Sdl.SDL_Delay((int)(duration * 1000));
				wait (duration);
				Tao.Sdl.Sdl.SDL_PauseAudio (1); // pause
			}
		}

		public void stopBeep ()
		{
			if (! audio_initialized) {
				initialize_tone ();
			}
			Tao.Sdl.Sdl.SDL_PauseAudio (1); // pause
		}

		public void Callback (IntPtr userData, IntPtr stream, int len)
		{
			if (audio_function != null) {
				int [] buffer = new int[len];
				try {
					audio_function (buffer, audio_index);
					audio_index += len;
				} catch (Exception e) {
					Console.Error.WriteLine ("Error in audio function");
					Console.Error.WriteLine (e.Message);
					return;
				}
				byte [] mybuffer8 = new byte[len];
				for (int i = 0; i < len; i++) {
					mybuffer8 [i] = (byte)buffer [i];
				}
				System.Runtime.InteropServices.Marshal.Copy (mybuffer8, 0, stream, len);
			} else {
				for (int buf_pos = 0; buf_pos < len; buf_pos++) {
					double sum = 0.0;
					int count = 0;
					if (frequencies [0] != -1) {
						sum += (byte)(127 + Math.Cos (phases [0]) * volume * 127);
						count++;
					}
					if (frequencies [1] != -1) {
						sum += (byte)(127 + Math.Cos (phases [1]) * volume * 127);
						count++;
					}
					if (count > 0) {
						buffer8 [buf_pos] = (byte)(sum / count);
						phases [0] += frequencies [0] * slice;
						phases [1] += frequencies [1] * slice;
						if (phases [0] > pi2) {
							phases [0] -= pi2;
						}
						if (phases [1] > pi2) {
							phases [1] -= pi2;
						}
					}
				}
				System.Runtime.InteropServices.Marshal.Copy (buffer8, 0, stream, len);
			}
		}
	}	
	
	[method: JigsawTab("Misc")]
	public static IEnumerable timer (double seconds)
	{
		double start = currentTime ();
		while (currentTime() - start < seconds) {
			yield return (currentTime() - start);
		}
	}
	
        [method: JigsawTab(null)]
	public static Func<object> freeze (Func<object> function)
	{
		return () => function.Invoke ();
	}

        [method: JigsawTab(null)]
	public static Func<object> freeze (Func<object,object> function, object arg1)
	{
		return () => function.Invoke (arg1);
	}
	
        [method: JigsawTab(null)]
	public static Func<object> freeze (Func<object,object,object> function, object arg1, object arg2)
	{
		return () => function.Invoke (arg1, arg2);
	}
	
        [method: JigsawTab(null)]
	public static Func<object> freeze (Func<object,object,object,object> function, object arg1, object arg2, object arg3)
	{
		return () => function.Invoke (arg1, arg2, arg3);
	}
	
        [method: JigsawTab(null)]
	public static Func<object> freeze (Func<object,object,object,object,object> function, object arg1, object arg2, object arg3, object arg4)
	{
		return () => function.Invoke (arg1, arg2, arg3, arg4);
	}
	
        [method: JigsawTab(null)]
	public static Func<object> freeze (Func<object,object,object,object,object,object> function, object arg1, object arg2, object arg3, object arg4, object arg5)
	{
		return () => function.Invoke (arg1, arg2, arg3, arg4, arg5);
	}
	
        [method: JigsawTab(null)]
	public static Func<object> freeze (Func<object,object,object,object,object,object,object> function, object arg1, object arg2, object arg3, object arg4, object arg5, object arg6)
	{
		return () => function.Invoke (arg1, arg2, arg3, arg4, arg5, arg6);
	}
	
        [method: JigsawTab(null)]
	public static Func<object> freeze (Func<object,object,object,object,object,object,object,object> function, object arg1, object arg2, object arg3, object arg4, object arg5, object arg6,
									object arg7)
	{
		return () => function.Invoke (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
	}
	
        [method: JigsawTab(null)]
	public static Func<object> freeze (Func<object,object,object,object,object,object,object,object,object> function, object arg1, object arg2, object arg3, object arg4, object arg5, object arg6,
									object arg7, object arg8)
	{
		return () => function.Invoke (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
	}
	
        [method: JigsawTab(null)]
	public static Func<object> freeze (Func<object,object,object,object,object,object,object,object,object,object> function, object arg1, object arg2, object arg3, object arg4, object arg5, object arg6,
									object arg7, object arg8, object arg9)
	{
		return () => function.Invoke (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
	}
	
        [method: JigsawTab(null)]
	public static Func<object> freeze (Func<object,object,object,object,object,object,object,object,object,object,object> function, object arg1, object arg2, object arg3, object arg4, object arg5, object arg6,
									object arg7, object arg8, object arg9, object arg10)
	{
		return () => function.Invoke (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
	}
	
        [method: JigsawTab(null)]
	public static object thaw (Func<object> thunk)
	{
		return thunk.Invoke ();
	}

	// Graphics.cs

	[method: JigsawTab("Graphics")]
	public static Graphics.Color makeColor (IList rgb)
	{
		return Graphics.makeColor ((int)rgb [0], (int)rgb [1], (int)rgb [2]);
	}

	[method: JigsawTab("Graphics")]
	public static Graphics.Color makeColor (string color)
	{
		return Graphics.makeColor (color);
	}

	[method: JigsawTab("Graphics")]
	public static Graphics.Color Color (IList rgb)
	{
		return Graphics.makeColor ((int)rgb [0], (int)rgb [1], (int)rgb [2]);
	}

	[method: JigsawTab("Graphics")]
	public static Graphics.Color Color (string color)
	{
		return Graphics.makeColor (color);
	}

	/*
  public static List toList(IEnumerable enumumerator) {
    List retval = new List();
    object item = null;
    while (enumumerator.MoveNext()) {
      item = enumumerator.Current;
      retval.append(item);
    }
    return retval;
  }
  */

	[method: JigsawTab("Picture")]
	public static Graphics.Color getColor (Graphics.Picture picture, int x, int y)
	{
		return picture.getColor (x, y);
	}
  
	[method: JigsawTab("Picture")]
	public static IEnumerable getPixels (Graphics.Picture picture)
	{
		return Graphics.getPixels (picture);
	}

	[method: JigsawTab("Picture")]
	public static Graphics.Pixel getPixel (Graphics.Picture picture, int col, int row)
	{
		return picture.getPixel (col, row);
	}

	[method: JigsawTab("Picture")]
	public static void setPixel (Graphics.Picture picture, int col, int row, Graphics.Color color)
	{
		picture.setPixel (col, row, color);
	}

	[method: JigsawTab("Picture")]
	public static void setPixel (Graphics.Picture picture, int col, int row, Graphics.Pixel pixel)
	{
		picture.setPixel (col, row, pixel);
	}

	[method: JigsawTab("Graphics")]
	public static Graphics.Color makeColor (int r, int g, int b)
	{
		return Graphics.makeColor (r, g, b);
	}

	[method: JigsawTab("Graphics")]
	public static Graphics.Color makeColor (int r, int g, int b, int a)
	{
		return Graphics.makeColor (r, g, b, a);
	}

	[method: JigsawTab("Graphics")]
	public static Graphics.Color makeColor (double r, double g, double b)
	{
		return Graphics.makeColor (r, g, b);
	}

	[method: JigsawTab("Graphics")]
	public static Graphics.Color makeColor (double r, double g, double b, double a)
	{
		return Graphics.makeColor (r, g, b, a);
	}

	[method: JigsawTab("Graphics")]
	public static Graphics.Color getColor (Graphics.Pixel pixel)
	{
		return pixel.getColor ();
	}

	[method: JigsawTab("Graphics")]
	public static PythonTuple getRGB (Graphics.Pixel pixel)
	{
		return pixel.getRGB ();
	}

	[method: JigsawTab("Graphics")]
	public static PythonTuple getRGBA (Graphics.Pixel pixel)
	{
		return pixel.getRGBA ();
	}

	[method: JigsawTab("Graphics")]
	public static int getX (Graphics.Pixel pixel)
	{
		return pixel.x;
	}

	[method: JigsawTab("Graphics")]
	public static int getY (Graphics.Pixel pixel)
	{
		return pixel.y;
	}

	[method: JigsawTab("Graphics")]
	public static int getGray (Graphics.Pixel pixel)
	{
		return pixel.getGray ();
	}

	[method: JigsawTab("Graphics")]
	public static int getRed (Graphics.Pixel pixel)
	{
		return pixel.getRed ();
	}

	[method: JigsawTab("Graphics")]
	public static int getGreen (Graphics.Pixel pixel)
	{
		return pixel.getGreen ();
	}

	[method: JigsawTab("Graphics")]
	public static int getBlue (Graphics.Pixel pixel)
	{
		return pixel.getBlue ();
	}

	[method: JigsawTab("Graphics")]
	public static int getAlpha (Graphics.Pixel pixel)
	{
		return pixel.getAlpha ();
	}

	[method: JigsawTab("Misc")]
	public static object get (IList tuple, int position)
	{
		return tuple [position];
	}

	[method: JigsawTab("Misc")]
	public static PythonTuple makeTuple (params object [] items)
	{
		// make a tuple from an array
		return new PythonTuple (items);
	}

	[method: JigsawTab("Graphics")]
	public static void setColor (Graphics.Pixel pixel, Graphics.Color c)
	{
		pixel.setColor (c);
	}

	[method: JigsawTab("Graphics")]
	public static void setRGB (Graphics.Pixel pixel, IList rgb)
	{
		pixel.setRGB (
			System.Convert.ToByte ((int)rgb [0]), 
			System.Convert.ToByte ((int)rgb [1]), 
			System.Convert.ToByte ((int)rgb [2]));
	}

	[method: JigsawTab("Graphics")]
	public static void setRGB (Graphics.Pixel pixel, byte red, byte green, byte blue)
	{
		pixel.setRGB (red, green, blue);
	}

	[method: JigsawTab("Graphics")]
	public static void setRGB (Graphics.Pixel pixel, int red, int green, int blue)
	{
		pixel.setRGB (
			System.Convert.ToByte ((int)red), 
			System.Convert.ToByte ((int)green), 
			System.Convert.ToByte ((int)blue));
	}

	[method: JigsawTab("Graphics")]
	public static void setRGB (Graphics.Pixel pixel, float red, float green, float blue)
	{
		pixel.setRGB (
			System.Convert.ToByte ((int)red), 
			System.Convert.ToByte ((int)green), 
			System.Convert.ToByte ((int)green));
	}

	[method: JigsawTab("Graphics")]
	public static void setRGBA (Graphics.Pixel pixel, byte red, byte green, byte blue, byte alpha)
	{
		pixel.setRGBA (red, green, blue, alpha);
	}

	[method: JigsawTab("Graphics")]
	public static void setGray (Graphics.Pixel pixel, byte value)
	{
		pixel.setGray (value);
	}

	[method: JigsawTab("Graphics")]
	public static void setRed (Graphics.Pixel pixel, byte value)
	{
		pixel.setRed (value);
	}

	[method: JigsawTab("Graphics")]
	public static void setGreen (Graphics.Pixel pixel, byte value)
	{
		pixel.setGreen (value);
	}

	[method: JigsawTab("Graphics")]
	public static void setBlue (Graphics.Pixel pixel, byte value)
	{
		pixel.setBlue (value);
	}

	[method: JigsawTab("Graphics")]
	public static void setAlpha (Graphics.Pixel pixel, byte value)
	{
		pixel.setAlpha (value);
	}

	[method: JigsawTab("Picture")]
	public static void savePicture (Graphics.Picture picture, string filename)
	{
		picture.savePicture (filename);
	}

	[method: JigsawTab("Picture")]
	public static void savePicture (List list, string filename, short delay, bool repeat)
	{
		Graphics.savePicture (list, filename, delay, repeat);
	}

	[method: JigsawTab("Picture")]
	public static void savePicture (List list, string filename)
	{
		Graphics.savePicture (list, filename, 0, true);
	}

        [method: JigsawTab(null)]
	public static object Vector (int x, int y)
	{
		return Graphics.Vector ((float)x, (float)y);
	}

        [method: JigsawTab(null)]
	public static object Vector (double x, double y)
	{
		return Graphics.Vector ((float)x, (float)y);
	}

	[method: JigsawTab("Picture")]
	public static Graphics.Picture makePicture (int x, int y)
	{
		return new Graphics.Picture (x, y);
	}

	[method: JigsawTab("Picture")]
	public static Graphics.Picture makePicture (int x, int y, Graphics.Color c)
	{
		return new Graphics.Picture (x, y, c);
	}

	[method: JigsawTab("Picture")]
	public static Graphics.Picture makePicture (string filename)
	{
		return new Graphics.Picture (filename);
	}

	[method: JigsawTab("Picture")]
	public static Graphics.Picture makePicture (Graphics.Picture picture)
	{
		return new Graphics.Picture (picture);
	}

	[method: JigsawTab("Picture")]
	public static Graphics.Picture makePicture (Graphics.WindowClass window)
	{ //, string filename) {
		return Graphics.makePicture (window);
	}

	[method: JigsawTab("Picture")]
	public static Graphics.Picture copyPicture (Graphics.Picture picture)
	{ //, string filename) {
		return Graphics.copyPicture (picture);
	}

        [method: JigsawTab(null)]
	public static Dictionary<object,object> makeDictionary ()
	{
		return new Dictionary<object,object> ();
	}

        [method: JigsawTab(null)]
	public static object getDictionaryItem (IDictionary dict, object key)
	{
		return dict [key];
	}

        [method: JigsawTab(null)]
	public static object setDictionaryItem (IDictionary dict, object key, object value)
	{
		return dict [key] = value;
	}

        [method: JigsawTab(null)]
	public static object [] getDictionaryKeys (IDictionary dict)
	{
		return (object[])dict.Keys;
	}

	[method: JigsawTab("Graphics")]
	public static Graphics.WindowClass makeWindow (string title="Calico Graphics",
      int width=300,
      int height=300)
	{
		return Graphics.makeWindow (title, width, height);
	}

	[method: JigsawTab("Graphics")]
	public static Graphics.WindowClass getWindow ()
	{
		return Graphics.getWindow ();
	}

	[method: JigsawTab("Robot")]
	public static Simulation getSimulation ()
	{
		return Myro.simulation;
	}

	[method: JigsawTab("Graphics")]
	public static PythonTuple getMouse ()
	{
		return Graphics.getMouse ();
	}

	[method: JigsawTab("Graphics")]
	public static PythonTuple getMouseNow ()
	{
		return Graphics.getMouseNow ();
	}

	[method: JigsawTab("Graphics")]
	public static string getMouseState ()
	{
		return Graphics.getMouseState ();
	}

	[method: JigsawTab("Graphics")]
	public static string getKeyState ()
	{
		return Graphics.getKeyState ();
	}

	[method: JigsawTab("Graphics")]
	public static string getKeyPressed ()
	{
		return Graphics.getKeyPressed ();
	}

	[method: JigsawTab("Graphics")]
	public static void run ()
	{
		Graphics.run ();
	}

	[method: JigsawTab("Graphics")]
	public static void run (Func<object> function)
	{
		Graphics.run (function);
	}

	// Callbacks:

	[method: JigsawTab("Graphics")]
	public static void onMouseUp (Func<object,Graphics.Event,object> function)
	{
		Graphics.onMouseUp (function);
	}

	[method: JigsawTab("Graphics")]
	public static void onMouseDown (Func<object,Graphics.Event,object> function)
	{
		Graphics.onMouseDown (function);
	}

	[method: JigsawTab("Graphics")]
	public static void onMouseMovement (Func<object,Graphics.Event,object> function)
	{
		Graphics.onMouseMovement (function);
	}

	[method: JigsawTab("Graphics")]
	public static void onKeyPress (Func<object,Graphics.Event,object> function)
	{
		Graphics.onKeyPress (function);
	}

	[method: JigsawTab("Graphics")]
	public static void onKeyRelease (Func<object,Graphics.Event,object> function)
	{
		Graphics.onKeyRelease (function);
	}

	[method: JigsawTab("Graphics")]
	public static Graphics.WindowClass getWindow (string title)
	{
		return Graphics.getWindow (title);
	}

	[method: JigsawTab("Graphics")]
	public static int getHeight (Graphics.WindowClass window)
	{
		return window.getHeight ();
	}

	[method: JigsawTab("Graphics")]
	public static int getHeight (Graphics.Picture picture)
	{
		return picture.getHeight ();
	}

	[method: JigsawTab("Graphics")]
	public static int getWidth (Graphics.WindowClass window)
	{
		return window.getWidth ();
	}

	[method: JigsawTab("Graphics")]
	public static int getWidth (Graphics.Picture picture)
	{
		return picture.getWidth ();
	}

	[method: JigsawTab("Graphics")]
	public static Graphics.Picture getRegion (Graphics.Picture picture, IList iterable, int width, int height, 
                    double degrees)
	{
		return picture.getRegion (new Graphics.Point (iterable [0], iterable [1]), width, height, degrees);
	}
  
	[method: JigsawTab("Graphics")]
	public static Graphics.Picture getRegion (Graphics.Picture picture, Graphics.Point p, int width, int height, double degrees)
	{
		return picture.getRegion (p, width, height, degrees);
	}

	[method: JigsawTab("Graphics")]
	public static void setRegion (Graphics.Picture picture, IList iterable, 
                   int width, int height, 
                   double degrees, Graphics.Color color)
	{
		picture.setRegion (new Graphics.Point (iterable [0], iterable [1]), width, 
              height, degrees, color);
	}
  
	[method: JigsawTab("Graphics")]
	public static void setRegion (Graphics.Picture picture, Graphics.Point p, 
                   int width, int height, double degrees,
                   Graphics.Color color)
	{
		picture.setRegion (p, width, height, degrees, color);
	}

	static Action functionInvoke (Func<object> func, List list, int position)
	{
		// Take a function, return list, and position
		// Return an Action that when called will
		// call the function, and put the result in the
		// list in the given position.
		return () => {
			try {  
				list [position] = func ();
			} catch (Exception e) {
				list [position] = e.Message;
			}
		};
	}

	static Action functionInvokeWithArg (Func<object,object> func, 
                       object arg,
                       List list, 
                       int position)
	{
		// Take a function, arsg, return list, and position
		// Return an Action that when called will
		// call the function, and put the result in the
		// list in the given position.
		return () => {
			try {  
				list [position] = func (arg);
			} catch (Exception e) {
				list [position] = e.Message;
			}
		}; 
	}
	
	static Action functionInvokeWithArgs (object ofunc, 
                       object [] args,
                       List list, 
                       int position)
	{
		// Take a function, arsg, return list, and position
		// Return an Action that when called will
		// call the function, and put the result in the
		// list in the given position.
		return () => {
			try {  
				list [position] = callFunc (ofunc, args.Length, args);
			} catch (Exception e) {
				list [position] = e.Message;
			}
		}; 
	}

        [method: JigsawTab(null)]
	public static object callFunc (object func, int len, object [] args)
	{
		if (len == 1) 
			return IronPython.Runtime.Converter.Convert<Func<object,object>> (func) (args [0]);
		else if (len == 2) 
			return IronPython.Runtime.Converter.Convert<Func<object,object,object>> (func) (args [0], args [1]);
		else if (len == 3) 
			return IronPython.Runtime.Converter.Convert<Func<object,object,object,object>> (func) (args [0], args [1], args [2]);
		else if (len == 4) 
			return IronPython.Runtime.Converter.Convert<Func<object,object,object,object,object>> (func) (args [0], args [1], args [2], args [3]);
		else if (len == 5) 
			return IronPython.Runtime.Converter.Convert<Func<object,object,object,object,object,object>> (func) (args [0], args [1], args [2], args [3], args [4]);
		else if (len == 6) 
			return IronPython.Runtime.Converter.Convert<Func<object,object,object,object,object,object,object>> (func) (args [0], args [1], args [2], args [3], args [4], args [5]);
		else if (len == 7) 
			return IronPython.Runtime.Converter.Convert<Func<object,object,object,object,object,object,object,object>> (func) (args [0], args [1], args [2], args [3], args [4], args [5], args [6]);
		else if (len == 8) 
			return IronPython.Runtime.Converter.Convert<Func<object,object,object,object,object,object,object,object,object>> (func) (args [0], args [1], args [2], args [3], args [4], args [5], args [6], args [7]);
		else if (len == 9) 
			return IronPython.Runtime.Converter.Convert<Func<object,object,object,object,object,object,object,object,object,object>> (func) (args [0], args [1], args [2], args [3], args [4], args [5], args [6], args [7], args [8]);
		else if (len == 10) 
			return IronPython.Runtime.Converter.Convert<Func<object,object,object,object,object,object,object,object,object,object,object>> (func) (args [0], args [1], args [2], args [3], args [4], args [5], args [6], args [7], args [8], args [9]);
		else
			throw new Exception ("invalid arguments in callFun");
	}
		
	// doTogether(f1, f2, ...)
	// doTogether([f1, f2, ...])
	// doTogether([f1, f2, ...], arg)
	// doTogether(f1, [a1, a2, ...])
	// doTogether([f1 a1 ...], [f2 a2 ...], ...)
	
	[method: JigsawTab(null)]
	public static List doTogether (params Func<object> [] functions)
	{
		List retval = new List ();
		List threads = new List ();
		int position = 0; 
		// For each function, make a return list, and thread list
		foreach (Func<object> function in functions) {
			retval.append (null);
			Thread thread = new Thread (
           			new ThreadStart (functionInvoke (function, retval, position))
				);
			thread.IsBackground = true;
			threads.append (thread);
			position++;
		}
		// Start each thread
		foreach (Thread t in threads) {
			t.Start ();
		}
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
		// return
		return retval;
	}
	
	[method: JigsawTab(null)]
	public static List doTogether (IList<dynamic> functions)
	{
		List retval = new List ();
		List threads = new List ();
		int position = 0; 
		// For each function, make a return list, and thread list
		foreach (dynamic function in functions) {
			Func<object > func = IronPython.Runtime.Converter.Convert<Func<object>> (function);
			retval.append (null);
			Thread thread = new Thread (
           			new ThreadStart (functionInvoke (func, retval, position))
				);
			thread.IsBackground = true;
			threads.append (thread);
			position++;
		}
		// Start each thread
		foreach (Thread t in threads) {
			t.Start ();
		}
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
		// return
		return retval;
	}
	
	[method: JigsawTab(null)]
	public static List doTogether (IList farg1, IList farg2, params IList [] fargs)
	{
		List retval = new List ();
		List threads = new List ();
		int position = 0; 
		// Second:
		//Func<object,object[]> func = IronPython.Runtime.Converter.Convert<Func<object,object[]>>(farg1[0]);
		retval.append (null);
		Thread thread = new Thread (
           			new ThreadStart (functionInvokeWithArgs (farg1 [0], farg1.Slice (1, farg1.Count), retval, position))
			);
		threads.append (thread);
		position++;
		// Second:
		//func = IronPython.Runtime.Converter.Convert<Func<object,object[]>>(farg2[0]);
		retval.append (null);
		thread = new Thread (
           			new ThreadStart (functionInvokeWithArgs (farg2 [0], farg2.Slice (1, farg2.Count), retval, position))
			);
		thread.IsBackground = true;
		threads.append (thread);
		position++;
		// For each function, make a return list, and thread list
		foreach (IList list in fargs) {
			//func = IronPython.Runtime.Converter.Convert<Func<object,object[]>>(list[0]);
			retval.append (null);
			thread = new Thread (
           			new ThreadStart (functionInvokeWithArgs (list [0], list.Slice (1, list.Count), retval, position))
				);
			thread.IsBackground = true;
			threads.append (thread);
			position++;
		}
		// Start each thread
		foreach (Thread t in threads) {
			t.Start ();
		}
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
		// return
		return retval;
	}

	[method: JigsawTab(null)]
	public static List doTogether (IList<dynamic> functions, object arg)
	{
		List retval = new List ();
		List threads = new List ();
		int position = 0; 
		// For each function, make a return list, and thread list
		foreach (dynamic function in functions) {
			Func<object,object > func = IronPython.Runtime.Converter.Convert<Func<object,object>> (function);
			retval.append (null);
			Thread thread = new Thread (
           			new ThreadStart (functionInvokeWithArg (func, arg, retval, position))
				);
			thread.IsBackground = true;
			threads.append (thread);
			position++;
		}
		// Start each thread
		foreach (Thread t in threads) {
			t.Start ();
		}
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
		// return
		return retval;
	}
	
	[method: JigsawTab(null)]
	public static List doTogether (Func<object,object> function, IList<object> args)
	{
		List retval = new List ();
		List threads = new List ();
		int position = 0; 
		// For each function, make a return list, and thread list
		foreach (object arg in args) { 
			retval.append (null);
			Thread thread = new Thread (
           			new ThreadStart (functionInvokeWithArg (function, arg, retval, position))
				);
			thread.IsBackground = true;
			threads.append (thread);
			position++;
		}
		// Start each thread
		foreach (Thread t in threads) {
			t.Start ();
		}
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
		// return
		return retval;
	}

	/*
  public class StringComparer: IComparer<string> {
    public int Compare(string x, string y) {
      if (x == null) {
	if (y == null) {
	  return 0;
	} else {
	  return -1;
	}
      } else {
	if (y == null) {
	  return 1;
	} else {
	  int retval = x.Length.CompareTo(y.Length);
	  if (retval != 0) {
	    return retval;
	  } else {
	    return x.CompareTo(y);
	  }
	}
      }
    }
  }
  */

	static int getPivotPoint (List input, int begPoint, int endPoint)
	{
		int pivot = begPoint / 2;
		int m = begPoint + 1;
		int n = endPoint;
		while ((m < endPoint) && 
	   (((string)input[pivot]).CompareTo((string)input[m]) >= 0)) {
			m++;
		}
    
		while ((n > begPoint) && 
	   (((string)input[pivot]).CompareTo((string)input[n]) <= 0)) {
			n--;
		}
		while (m < n) {
			string temp = (string)input [m];
			input [m] = input [n];
			input [n] = temp;
			while ((m < endPoint) && 
	     (((string)input[pivot]).CompareTo((string)input[m]) >= 0)) {
				m++;
			}
			while ((n > begPoint) && 
	     (((string)input[pivot]).CompareTo((string)input[n]) <= 0)) {
				n--;
			}
		}
		if (pivot != n) {
			string temp2 = (string)input [n];
			input [n] = input [pivot];
			input [pivot] = temp2;
      
		}
		return n;
	}

	static void QuickSort (List input, int beg, int end)
	{
		// In-place string sort
		if (end == beg) {
			return;
		} else {
			int pivot = getPivotPoint (input, beg, end);
			if (pivot > beg)
				QuickSort (input, beg, pivot - 1);
			if (pivot < end)
				QuickSort (input, pivot + 1, end);
		}
		return;
	}
  
	static List Sort (List list)
	{
		// in-place String sort
		QuickSort (list, 0, list.Count - 1);
		return list;
	}

	[method: JigsawTab("Senses")]
	public static List getVoiceNames ()
	{
		return Sort (voices.values ());
	}

	[method: JigsawTab("Senses")]
	public static List getVoices ()
	{
		return voices.keys ();
	}

	[method: JigsawTab("Actions")]
	public static void setVoice (string name)
	{
		speech_name = name;
	}

	[method: JigsawTab("Actions")]
	public static void setVoiceName (string name)
	{
		foreach (string key in voices.keys()) {
			if ((string)voices [key] == name) {
				speech_name = key;
				return;
			}
		}
		throw new Exception ("voice name not found: " + name);
	}

	[method: JigsawTab("Senses")]
	public static string getVoice ()
	{
		return speech_name;
	}

	[method: JigsawTab("Senses")]
	public static string getVoiceName ()
	{
		return getVoiceName (speech_name);
	}
  
	[method: JigsawTab("Senses")]
	public static string getVoiceName (string name)
	{
		foreach (string key in voices.keys()) {
			if (key == name) {
				return (string)voices [key];
			}
		}
		return name;
	}

	[method: JigsawTab("Audio")]
	public static void speak (string text)
	{
		speak (text, 0); // not async, wait for exit
	}

	[method: JigsawTab("Audio")]
	public static void speak (string text, bool async)
	{
		speak (text, bool_to_int (async)); // not async, wait for exit
	}

	[method: JigsawTab("Audio")]
	public static void speak (string text, double async)
	{
		speak (text, (int)async); // not async, wait for exit
	}

        [method: JigsawTab("Audio")]
	public static void speak (string text, int async)
	{
		Console.WriteLine (text);
		Process myProcess = new Process ();

		// create a temporary file with the text to be spoken
		var textpath = System.IO.Path.GetTempFileName ();
		using (TextWriter writer = File.CreateText(textpath)) {
			writer.WriteLine (text);
		}


		try {
			myProcess.StartInfo.UseShellExecute = false;

			if (os_name == "Windows") {
				string file = startup_path;
				file = Path.Combine (file, "windows");
				file = Path.Combine (file, "eSpeak");
				myProcess.StartInfo.FileName = Path.Combine (file, "espeak.exe");
			} else if (os_name == "Mac") {
				string file = startup_path;
				file = Path.Combine (file, "mac");
				file = Path.Combine (file, "eSpeak");
				myProcess.StartInfo.FileName = Path.Combine (file, "speak");
				Environment.SetEnvironmentVariable ("ESPEAK_DATA_PATH", file);
			} else {
				if (File.Exists ("/usr/bin/speak")) {
					// assumes espeak is in /usr/bin/ on macs
					myProcess.StartInfo.FileName = "speak";
				} else if (File.Exists ("/usr/local/bin/speak")) {
					// or look for espeak is in /usr/local/bin/ on macs
					myProcess.StartInfo.FileName = "speak";
				} else {
					// assumes in path
					myProcess.StartInfo.FileName = "espeak";
				}
			}
			myProcess.StartInfo.CreateNoWindow = true;
			myProcess.StartInfo.Arguments = ("-v \"" + speech_name + "\" -f " + textpath);
			myProcess.Start ();

			if (async == 0)
				myProcess.WaitForExit ();
#pragma warning disable 0168
		} catch (Exception e) {
#pragma warning restore 0168
			if (warn_missing_speak) {
				Console.WriteLine ("WARNING: missing speak command");
				warn_missing_speak = false; // just once
			}
		}
	}

	[method: JigsawTab("Audio")]
	public static string getNoteFromFrequency (int frequency)
	{
		return getNoteFromFrequency (System.Convert.ToDouble (frequency));
	}

	[method: JigsawTab("Audio")]
	public static string getNoteFromFrequency (double frequency)
	{
		// Return closest note name based on a given frequency. 
		double diff = 100000;
		string diffNote = "a";
		foreach (string key in frequencies.keys()) {
			if (Math.Abs (((double)frequencies [key]) - frequency) < diff) {
				diff = (double)Math.Abs (((double)frequencies [key]) - frequency);
				diffNote = key;
			}
		}
		return diffNote.Substring (0, 1).ToUpper () + 
      diffNote.Substring (1, diffNote.Length - 1);
	}

	[method: JigsawTab("Audio")]
	public static void playSong (List song)
	{
		playSong (song, 1.0);
	}

	[method: JigsawTab("Audio")]
	public static void playSong (List song, double speed)
	{
		if (robot != null)
			robot.playSong (song, speed);
		else
			computer.playSong (song, speed);
	}

	[method: JigsawTab("Audio")]
	public static void saveSong (List song, string filename, int append)
	{
		saveSong (song, filename, int_to_bool (append));
	}

	[method: JigsawTab("Audio")]
	public static void saveSong (List song, string filename, bool append)
	{
		//  Writes a song list to a file. 
		System.IO.StreamWriter fp = new System.IO.StreamWriter (filename, append); 
		foreach (IList tup in song) {
			if (tup.Count == 2) {
				double f = System.Convert.ToDouble (tup [0]); 
				double d = System.Convert.ToDouble (tup [1]);
				fp.WriteLine ("{0} {1}", getNoteFromFrequency (f), d);
			} else if (tup.Count == 3) {
				double f1 = System.Convert.ToDouble (tup [0]); 
				double f2 = System.Convert.ToDouble (tup [1]); 
				double d = System.Convert.ToDouble (tup [2]);
				fp.WriteLine ("{0} {1} {2}", getNoteFromFrequency (f1),
             getNoteFromFrequency (f2), d);
				fp.Close ();
			}
		}
	}

	[method: JigsawTab("Audio")]
	public static List readSong (string filename)
	{
		// Read a song file. Returns a song list 
		List song = new List ();
		System.IO.StreamReader songFile = new System.IO.StreamReader (filename);
		int lineNumber = 1;
		string line;
		while ((line = songFile.ReadLine()) != null) {
			Array notes = line.Split (';');
			foreach (string n in notes) {
				parseSongLine (song, n, lineNumber, filename);
			}
			lineNumber += 1;
		}
		songFile.Close ();
		return song;
	}

	[method: JigsawTab("Audio")]
	public static string song2text (List song)
	{
		// Given a song list, return a text string form 
		string text = "";
		foreach (IList tup in song) {
			if (tup.Count == 2) {
				double f = System.Convert.ToDouble (tup [0]); 
				double d = System.Convert.ToDouble (tup [1]);
				text += String.Format ("{0} {1}; ", getNoteFromFrequency (f), d);
			} else if (tup.Count == 3) {
				double f1 = System.Convert.ToDouble (tup [0]); 
				double f2 = System.Convert.ToDouble (tup [1]); 
				double d = System.Convert.ToDouble (tup [2]);
				text += String.Format ("{0} {1} {2}; ", 
                  getNoteFromFrequency (f1),
                  getNoteFromFrequency (f2), d);
			}
		}
		return text;
	}

	[method: JigsawTab("Audio")]
	public static List makeSong (string text)
	{
		// Given a text string format of a song, return a song list 
		List song = new List ();
		text = text.Replace ('\n', ';');
		Array songData = text.Split (';');
		int lineNumber = 1;
		foreach (string line in songData) {
			parseSongLine (song, line, lineNumber, "string");
			lineNumber += 1;
		}
		return song;
	}

	[method: JigsawTab("Audio")]
	static void parseSongLine (List song, string line, 
                int lineNumber, string filename)
	{
		line = line.Trim ();
		Array lineList = line.Split (' ');
		// FIXME: remove duplicate spaces
		if (lineList.Length <= 1) {
			// blank line, skip
		} else if (((string)lineList.GetValue (0)) [0] == '#') {
			// first word, first char is #, then skip comment
		} else if (lineList.Length == 2) {
			Array name1_dur = line.Split (' ');
			string name1 = (string)name1_dur.GetValue (0);
			string dur = (string)name1_dur.GetValue (1);
			song.append (Graphics.PyTuple (getFrequency (name1, lineNumber, line),
                   getDuration (dur, lineNumber, line)));
		} else if (lineList.Length == 3) {
			Array name1_name2_dur = line.Split (' ');
			string name1 = (string)name1_name2_dur.GetValue (0);
			string name2 = (string)name1_name2_dur.GetValue (1);
			string dur = (string)name1_name2_dur.GetValue (2);
			song.append (Graphics.PyTuple (getFrequency (name1, lineNumber, line),
                    getFrequency (name2, lineNumber, line),
                    getDuration (dur, lineNumber, line)));
		} else {
			throw new Exception (String.Format ("song format error in '{0}' at line {1}: {2}", filename, lineNumber, line));
		}
	}

#pragma warning disable 0168
	[method: JigsawTab("Audio")]
	static double getFrequency (string s, int line, string text)
	{
		//Takes a string that is a note name, or a frequency. Returns
		try {
			return (double)frequencies [s.ToLower ()];
		} catch (Exception e1) {
			try {
				return Double.Parse (s);
			} catch (Exception e2) {
				throw new Exception (String.Format ("invalid note name/frequency '{0}' on line {1}: {2}", s, line, text));
			}
		}
	}

#pragma warning restore 0168

	[method: JigsawTab("Audio")]
	static double getDuration (string v, int line, string text)
	{
		// Takes a string that is a fraction, or a number. Returns whole
		// note portion as float. 
		if (v.Contains ("/")) {
			try {
				Array numerator_denominator = v.Split ('/');
				double numerator = Double.Parse ((string)numerator_denominator.GetValue (0));
				double denominator = Double.Parse ((string)numerator_denominator.GetValue (1));
				return numerator / denominator;
#pragma warning disable 0168
			} catch (Exception e) {
#pragma warning restore 0168
				throw new Exception (
       String.Format ("invalid duration value '{0}' on line {1}: {2}",
             v, line, text));
			}
		} else {
			return Double.Parse (v);
		}
	}	
}

/*
Calico - Scripting Environment

Copyright (c) 2011, Doug Blank <dblank@cs.brynmawr.edu>

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
using System.Diagnostics; // Process
using System.IO; // DirectoryInfo, FileInfo
using System.IO.Ports; // SerialPort
using System.Threading;
using IronPython.Runtime; // List
using IronRuby.Builtins; // RubyArray
using System.Collections.Generic; // IList
using System.Collections; // IEnumerator

using Tao.Sdl;

public static class Extensions {
  public static T[] Slice<T>(this T[] source, int start, int end) {
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
          res[i] = source[i + start];
        }
        return res;
  }
}

public static class Myro {
  //public readonly static List __all__ = new List() {"robot"};

  public static Robot robot;
  public static Simulation simulation;
  public readonly static Computer computer = new Computer();
  static string dialogResponse = null;
  static string REVISION = "$Revision: $";
  static string startup_path = null;
  static string os_name = null;
  static string speech_name = "default";
  static bool warn_missing_speak = true;
  static PythonDictionary voices = new PythonDictionary();
  public readonly static PythonDictionary frequencies = new PythonDictionary();

  public readonly static Gamepads gamepads = new Gamepads();

  static void invoke_function(Func<object,object> function, object args) {
    try {
      Gtk.Application.Invoke( delegate {
	  function(args);
	});
    } catch (Exception e) {
      Console.Error.WriteLine("Error in function");
      Console.Error.WriteLine(e.Message);
    }        
  }

  public static void gamepad(PythonDictionary dict) {
    // everytime something (up/down) happens on the keys being
    // watched, then args are sent to the associated function(s)
    PythonDictionary results;
    List keys = new List();
    foreach (string key in dict.Keys) {
      keys.append(key);
    }
    while (true) {
      results = (PythonDictionary)getGamepad(keys);
      foreach (string key in results.Keys) {
	invoke_function((Func<object,object>)dict[key], results[key]);
      }
    }
  }

  public static void gamepad() {
    // sample robot controller
    PythonDictionary results = (PythonDictionary)getGamepadNow();
    List button = (List)results["button"];
    string name = "Scribby";
    try {
      name = getName();
    } catch {
      // ignore
    }
    List phrases = Graphics.PyList(
		   String.Format("Hello. My name is {0}.", name),
		                 "Ouch! I'm a sensitive robot.", 
				 "I'm hungry. Do you have any batteries?");
    
    Console.WriteLine("        Pad   Action");
    Console.WriteLine("     ------   -------");
    Console.WriteLine(" Left/Right   turnLeft() and turnRight()");
    Console.WriteLine("    Up/Down   forward() and backward()");
    Console.WriteLine("");
    
    if (button.Count > 0) {
      Console.WriteLine("     Button   Action");
      Console.WriteLine("     ------   -------");
      Console.WriteLine("          1   stop()");
      if (button.Count > 1) 
	Console.WriteLine("          2   takePicture()");
      if (button.Count > 2) 
	Console.WriteLine("          3   beep(.25, 523)");
      if (button.Count > 3) 
	Console.WriteLine("          4   beep(.25, 587)");
      if (button.Count > 4) 
	Console.WriteLine("          5   beep(.25, 659)");
      if (button.Count > 5) 
	Console.WriteLine(String.Format("          6   speak('{0}')", 
					phrases[0]));
      if (button.Count > 6) 
	Console.WriteLine(String.Format("          7   speak('{0}')",
					phrases[1]));
      if (button.Count > 7) 
	Console.WriteLine(String.Format("          8   speak('{0}')",
					phrases[2]));
      Console.WriteLine("");
    }
    
    Console.WriteLine("Gamepad is now running... Press button 1 to stop.");
    
    List lastMove = Graphics.PyList(0, 0);
    List axis, freqs;
    bool doneSpeaking = true;
    PythonDictionary retval = (PythonDictionary)getGamepadNow();
    button = (List)retval["button"];
    int length = button.Count;
    bool tryToMove = true;
    while (true) {
      retval = (PythonDictionary)getGamepad();  // changed to blocking, JWS
      button = (List)retval["button"];
      axis = (List)retval["axis"];
      freqs = Graphics.PyList(null, null);
      if (length > 0 && (int)button[0] == 1) {
	try {
	  stop();
	} catch {
	}
	break;
      }
      if (length > 1 && (int)button[1] == 1) {
	speak("Say cheese!", 1);
	try {
	  Graphics.Picture pic = takePicture();
	  show(pic);
	} catch {
	}
      }
      if (length > 2 && (int)button[2] == 1) {
	freqs[0] = 523;
      }
      if (length > 3 && (int)button[3] == 1) {
	if (freqs[0] == null)
	  freqs[0] = 587;
	else
	  freqs[1] = 587;
      }
      if (length > 4 && (int)button[4] == 1) {
	if (freqs[0] == null)
	  freqs[0] = 659;
	else
	  freqs[1] = 659;
      }
      // speak
      if (length > 5 && (int)button[5] == 1) {
	if (doneSpeaking) {
	  speak((string)phrases[0], 1);
	  doneSpeaking = false;
	} 
      } else if (length > 6 && (int)button[6] == 1) {
	if (doneSpeaking) {
	  speak((string)phrases[1], 1);
	  doneSpeaking = false;
	}
      } else if (length > 7 && (int)button[7] == 1) {
	if (doneSpeaking) {
	  speak((string)phrases[2], 1);
	  doneSpeaking = false;
	}
      } else {
	doneSpeaking = true;
      }
      
      if (tryToMove && 
	  axis[0] != lastMove[0] &&
	  axis[1] != lastMove[1]) {
	try {
	  move(-(double)axis[1], -(double)axis[0]);
	  lastMove = Graphics.PyList(axis[0], axis[1]);
	} catch {
	  tryToMove = false;
	}
      }
      if (freqs[0] != null || freqs[1] != null) {
	if (freqs[1] == null) {
	  try {
	    beep(.25, (int)freqs[0]);
	  } catch {
	    computer.beep(.25, (int)freqs[0]);
	  }
	} else if (freqs[0] == null) {
	  try {
	    beep(.25, (int)freqs[1]);
	  } catch {
	    computer.beep(.25, (int)freqs[1]);
	  }
	} else {
	  try {
	    beep(.25, (int)freqs[0], (int)freqs[1]);
	  } catch {
	    computer.beep(.25, (int)freqs[0], (int)freqs[1]);
	  }
	}
      }
    }
  }

  public static void senses()
  {
    new Senses();
  }

  class Senses {
    Gtk.Window win;
    PythonDictionary dict_entry;
    uint idle = 0;

    public Senses() {
      win = new Gtk.Window("Senses");
      Gtk.VBox vbox = new Gtk.VBox();
      //win.AllowGrow = true;
      //win.AllowShrink = true;
      //win.SetDefaultSize(width, height);
      win.DeleteEvent += OnDelete;
      win.Add(vbox);
      Gtk.HBox hbox;
      PythonDictionary items = new PythonDictionary();
      items["Line:"] = 2;
      items["Stall:"] = 1;
      items["Bright:"] = 3;
      items["Obstacle:"] = 3;
      items["IR:"] = 2;
      items["Light:"] = 3;
      items["Battery:"] = 1;
      dict_entry = new PythonDictionary();
      List entries = new List();
      foreach (string key in items.Keys) {
	hbox = new Gtk.HBox();
	Gtk.Label label = new Gtk.Label(key);
	label.WidthRequest = 100;
	hbox.PackStart(label, false, false, 0);
	entries = new List();
	for (int i= 0; i < (int)items[key]; i++) {
	  Gtk.Entry entry = new Gtk.Entry();
	  entries.append(entry);
	  hbox.PackStart(entry);
	  dict_entry[key] = entries;
	}
	vbox.PackStart(hbox);
      }
      Gtk.Application.Invoke(delegate {
	  win.ShowAll();
	});
      idle = GLib.Idle.Add(new GLib.IdleHandler(update_entries));
    }
    
    private void OnDelete(object obj, Gtk.DeleteEventArgs args)  {
      if (idle != 0) {
	GLib.Source.Remove(idle);
      }
      idle = 0;
    }
    
    bool update_entries() {
      Gtk.Application.Invoke(delegate {
	  try {	  
	    List results = (List)getLight();
	    for (int i=0; i < results.Count; i++) {
	      ((Gtk.Entry)((List)dict_entry["Light:"])[i]).Text = results[i].ToString();
	    }
	    results = (List)getBright();
	    for (int i=0; i < results.Count; i++) {
	      ((Gtk.Entry)((List)dict_entry["Bright:"])[i]).Text = results[i].ToString();
	    }
	    results = (List)getObstacle();
	    for (int i=0; i < results.Count; i++) {
	      ((Gtk.Entry)((List)dict_entry["Obstacle:"])[i]).Text = results[i].ToString();
	    }
	    results = (List)getIR();
	    for (int i=0; i < results.Count; i++) {
	      ((Gtk.Entry)((List)dict_entry["IR:"])[i]).Text = results[i].ToString();
	    }
	    results = (List)getLine();
	    for (int i=0; i < results.Count; i++) {
	      ((Gtk.Entry)((List)dict_entry["Line:"])[i]).Text = results[i].ToString();
	    }
	    ((Gtk.Entry)((List)dict_entry["Battery:"])[0]).Text = getBattery().ToString();
	    ((Gtk.Entry)((List)dict_entry["Stall:"])[0]).Text = getStall().ToString();	  
	  } catch {
	    // pass
	  }
	});
      wait(.1);
      return true; // continue
    }
  }

  public class Gamepads {
   
    IntPtr [] handles;

    public Gamepads() {
      try {
        Sdl.SDL_Init(Sdl.SDL_INIT_JOYSTICK);
        handles = new IntPtr [Sdl.SDL_NumJoysticks()];
        for (int i=0; i < Sdl.SDL_NumJoysticks(); i++) {
          handles[i] = Sdl.SDL_JoystickOpen(i);
        }
      } catch {
        Console.Error.WriteLine("WARNING: SDL is not installed.");
      }
    }

    public List getHatStates(int index) {
      List retval = new List();
      int num = Sdl.SDL_JoystickNumHats(handles[index]);
      for (int button = 0; button < num; button++) {
        retval.append(Sdl.SDL_JoystickGetHat(handles[index], button));
      }
      return retval;
    }

    public List getBallStates(int index) {
      List retval = new List();
      int num = Sdl.SDL_JoystickNumBalls(handles[index]);
      for (int button = 0; button < num; button++) {
        int x, y;
        Sdl.SDL_JoystickGetBall(handles[index], button, out x, out y);
        retval.append(Graphics.PyTuple(x, y));
      }
      return retval;
    }

    public List getButtonStates(int index) {
      List retval = new List();
      int num = Sdl.SDL_JoystickNumButtons(handles[index]);
      for (int button = 0; button < num; button++) {
        retval.append((int)Sdl.SDL_JoystickGetButton(handles[index], button));
      }
      return retval;
    }

    public List getAxisStates(int index) {
      List retval = new List();
      int num = Sdl.SDL_JoystickNumAxes(handles[index]);
      for (int button = 0; button < num; button++) {
        retval.append(Math.Round(Sdl.SDL_JoystickGetAxis(handles[index], button)/32767.0, 2));
      }
      return retval;
    }

    public PythonDictionary getGamepadNow(int index, List whats) {
      PythonDictionary retval =  new PythonDictionary();
      foreach (string what in whats) {
	retval[what] = getGamepadNow(index, what);
      }
      return retval;
    }

    public object getGamepadNow(int index, string what) {
      if (what == "all") {
        return getGamepadNow(index, 
                             Graphics.PyList("name", "axis", "ball", 
                                             "button", "hat", "count"));
      } if (what == "button") {
        return gamepads.getButtonStates(index);
      } else if (what == "name") {
        return Sdl.SDL_JoystickName(index);
      } else if (what == "axis") {
        return gamepads.getAxisStates(index);
      } else if (what == "robot") {
        List xy = gamepads.getAxisStates(index);
        return Graphics.PyList(-System.Convert.ToDouble(xy[1]), 
			       -System.Convert.ToDouble(xy[0]));
      } else if (what == "ball") {
        return gamepads.getBallStates(index);
      } else if (what == "hat") {
        return gamepads.getHatStates(index);
      } else if (what == "count") {
        return Sdl.SDL_NumJoysticks();
      } else {
        throw new Exception(String.Format("unknown gamepad component: '{0}'", what));
      }
    }

    public static bool Same(object o1, object o2) {
      if (o1 is PythonDictionary) {
        PythonDictionary d1 = (PythonDictionary)o1;
        PythonDictionary d2 = (PythonDictionary)o2;
        foreach (string key in d1.Keys) {
          if (! Same(d1[key], d2[key]))
            return false;
        }
        foreach (string key in d2.Keys) {
          if (! Same(d1[key], d2[key]))
            return false;
        }
        return true;
      } else if (o1 is List) {
        List l1 = (List)o1;
        List l2 = (List)o2;
        if (l1.Count != l2.Count)
          return false;
        for (int i = 0; i < l1.Count; i++) {
          if (! Same(l1[i], l2[i]))
            return false;
        }
        return true;
      } else {
        return o1.Equals(o2);
      }
    }

    public object getGamepad(List indices, string what) {
      List initials = new List();
      foreach (int index in indices) {
	initials.append(getGamepadNow(index, what));
      }
      Sdl.SDL_JoystickUpdate();
      List currents = new List();
      foreach (int index in indices) {
	currents.append(getGamepadNow(index, what));
      }
      while (Same(initials, currents)) {
	wait(.01);
        Sdl.SDL_JoystickUpdate();
	currents = new List();
	foreach (int index in indices) {
	  currents.append(getGamepadNow(index, what));
	}
      }
      return currents;
    }

    public object getGamepad(int index, string what) {
      object initial = getGamepadNow(index, what);
      Sdl.SDL_JoystickUpdate();
      object current = getGamepadNow(index, what);
      while (Same(initial, current)) {
	wait(.01);
        Sdl.SDL_JoystickUpdate();
        current = getGamepadNow(index, what);
      }
      return current;
    }

    public object getGamepad(List indices, List whats) {
      List initials = new List();
      foreach (int index in indices) {
	initials.append(getGamepadNow(index, whats));
      }
      Sdl.SDL_JoystickUpdate();
      List currents = new List();
      foreach (int index in indices) {
	currents.append(getGamepadNow(index, whats));
      }
      while (Same(initials, currents)) {
	wait(.01);
        Sdl.SDL_JoystickUpdate();
	currents = new List();
	foreach (int index in indices) {
	  currents.append(getGamepadNow(index, whats));
	}
      }
      return currents;
    }

    public object getGamepad(int index, List whats) {
      object initial = getGamepadNow(index, whats);
      Sdl.SDL_JoystickUpdate();
      object current = getGamepadNow(index, whats);
      while (Same(initial, current)) {
	wait(.01);
        Sdl.SDL_JoystickUpdate();
        current = getGamepadNow(index, whats);
      }
      return current;
    }
  }

  public static object getGamepadNow() {
    Sdl.SDL_JoystickUpdate();
    return gamepads.getGamepadNow(0, "all");
  }

  public static object getGamepadNow(int index) {
    Sdl.SDL_JoystickUpdate();
    return gamepads.getGamepadNow(index, "all");
  }

  public static object getGamepadNow(string what) {
    Sdl.SDL_JoystickUpdate();
    return gamepads.getGamepadNow(0, what);
  }

  public static object getGamepadNow(int index, string what) {
    Sdl.SDL_JoystickUpdate();
    return gamepads.getGamepadNow(index, what);
  }

  public static object getGamepadNow(int index, List whats) {
    Sdl.SDL_JoystickUpdate();
    return gamepads.getGamepadNow(index, whats);
  }
  
  public static object getGamepadNow(IList iterable) {
    Sdl.SDL_JoystickUpdate();
    List retval = new List();
    foreach (string what in iterable)
      retval.append(gamepads.getGamepadNow(0, what));
    return retval;
  }
  
  public static object getGamepadNow(IList iterable, string what) {
    Sdl.SDL_JoystickUpdate();
    List retval = new List();
    foreach (int index in iterable)
      retval.append(gamepads.getGamepadNow(index, what));
    return retval;
  }
  
  public static object getGamepadNow(IList iterable, List whats) {
    Sdl.SDL_JoystickUpdate();
    List retval = new List();
    foreach (int index in iterable)
      retval.append(gamepads.getGamepadNow(index, whats));
    return retval;
  }
  
  public static object getGamepad() {
    Sdl.SDL_JoystickUpdate();
    return gamepads.getGamepad(0, "all");
  }

  public static object getGamepad(int index) {
    Sdl.SDL_JoystickUpdate();
    return gamepads.getGamepad(index, "all");
  }
  
  public static object getGamepad(string what) {
    Sdl.SDL_JoystickUpdate();
    return gamepads.getGamepad(0, what);
  }

  public static object getGamepad(int index, string what) {
    Sdl.SDL_JoystickUpdate();
    return gamepads.getGamepad(index, what);
  }

  public static object getGamepad(int index, List whats) {
    Sdl.SDL_JoystickUpdate();
    return gamepads.getGamepad(index, whats);
  }
  
  public static object getGamepad(List whats) {
    Sdl.SDL_JoystickUpdate();
    return gamepads.getGamepad(0, whats);
  }
  
  public static object getGamepad(List indices, string what) {
    Sdl.SDL_JoystickUpdate();
    return gamepads.getGamepad(indices, what);
  }
  
  public static object getGamepad(List indices, List whats) {
    Sdl.SDL_JoystickUpdate();
    return gamepads.getGamepad(indices, whats);
  }
  
  public class MyTexView : Gtk.TextView  {
        public string MyString;
  }

  // Functional Interface

  public static Robot getRobot() {
    return robot;
  }

  public static void init() {
    initialize(null);
  }

  public static void initialize() {
    initialize(null);
  }

  public static void init(string port) {
    initialize(port, 38400);
  }

  public static void init(string port, int baud) {
    initialize(port, baud);
  }

  public static void initialize(string port, int baud=38400) {
        bool need_port = true;
        if (port != null && (port.StartsWith("COM") || port.StartsWith("com"))) {
            port = @"\\.\" + port;             // "comment
        }
        if (Myro.robot is Scribbler) {
          if (((Scribbler)(Myro.robot)).serial is SerialPort) {
                SerialPort serial = (((Scribbler)(Myro.robot)).serial as SerialPort);
                if (serial.IsOpen) {
                  if (port == null) 
                        need_port = false;
                  else if (serial.PortName.Equals(port) && serial.BaudRate == baud) {
                        need_port = false;
                  } else {
                        // It exists, but wrong port/baud, so close it:
                        serial.Close(); // and need_port
          }
                } else { // already closed
		  if ((serial.PortName.Equals(port) || port == null) && serial.BaudRate == baud) {
            need_port = false;
            serial.Open();
          } else {
            need_port = true;
          }
        }
          } // not a serial port
        } else if (Myro.robot is SimScribbler) {
	  // do nothing if simulator
	  need_port = false;
	}
        if (need_port) {
          if (port == null) {
	    port = (string)ask("Port");
          }
          if (port != null) {
	    if (port.StartsWith("sim")) {
	      if (simulation == null) {
		simulation = new Simulation();
		Thread.Sleep((int)(1 * 1000));
	      }
	      robot = new SimScribbler(simulation);
	    } else {
	      robot = new Scribbler(port, baud);
	    }
	  } else {
	    Console.WriteLine("init() cancelled");
	  }
        } else {
          robot.setup();
        }
  }

  public class Simulation {
    public Graphics.WindowClass window;
    public Thread thread;
    public List<Robot> robots = new List<Robot>();

    public Simulation() : this(640, 480) {
    }

    public Simulation(int width, int height) {
      window = makeWindow("Myro Simulation", width, height);
      window.mode = "physics";
      window.gravity = Graphics.Vector(0,0); // turn off gravity
      
      Graphics.Rectangle wall = new Graphics.Rectangle(new Graphics.Point(0, 0), 
						       new Graphics.Point(5, height));
      wall.bodyType = "static";
      wall.draw(window);

      wall = new Graphics.Rectangle(new Graphics.Point(5, 0), 
				    new Graphics.Point(width - 5, 5));
      wall.bodyType = "static";
      wall.draw(window);

      wall = new Graphics.Rectangle(new Graphics.Point(width - 5, 0), 
				    new Graphics.Point(width, height));
      wall.bodyType = "static";
      wall.draw(window);

      wall = new Graphics.Rectangle(new Graphics.Point(0, height - 5), 
				    new Graphics.Point(width - 5, height));
      wall.bodyType = "static";
      wall.draw(window);

      thread = new Thread(new ThreadStart(loop));
      thread.IsBackground = true;
      thread.Start();
    }
    
    public void loop() {
      while (true) {
	foreach(SimScribbler robot in robots) {
	  lock(robot) {
	    robot.frame.body.LinearVelocity = Graphics.VectorRotate(
                  Graphics.Vector(robot.velocity, 0), 
		  robot.frame.body.Rotation);
	  }
	}
	window.step(.1);
      }
    }
  }


  public static void uninit() {
    if (Myro.robot is Scribbler) {
      if (((Scribbler)(Myro.robot)).serial is SerialPort) {
        SerialPort serial = (((Scribbler)(Myro.robot)).serial as SerialPort);
        lock(serial) {
          serial.Close(); // and need_port
        }
      } // not a serial port
    } // not a scribbler
  }

  public static string repeat(string s, int times) {
    // repeat(" ", 10) => "          "
    string retval = "";
    for(int i = 0; i < times; i++) {
      retval += s;
    }
    return retval;
  }
  
  public static string pad(string s, int length) {
    // pad("hi", 3) => "hi "
    if (length <= s.Length) { // trim
      return s.Substring(0, length);
    } else {
      return (s + repeat(" ", length)).Substring(0, length);
    }
  }
  
  public static List rgb2yuv(int R, int G, int B) {
    int Y = (int)(0.299 * R + 0.587 * G + 0.114 * B);
    int U = (int)(-0.14713 * R - 0.28886 * G + 0.436 * B + 128);
    int V = (int)( 0.615 * R - 0.51499* G - 0.10001 * B + 128);
    return Graphics.PyList(
        Math.Max(Math.Min(Y,255),0),
        Math.Max(Math.Min(U,255),0),
        Math.Max(Math.Min(V,255),0));
  }

  public static void forward(double power) {
    robot.forward(power);
  }
  
  public static void forward(double power, double time) {
    robot.forward(power, time);
  }
  
  public static void translate(double power) {
    robot.translate(power);
  }
  
  public static void translate(double power, double time) {
    robot.translate(power, time);
  }
  
  public static void rotate(double power) {
    robot.rotate(power);
  }

  public static void rotate(double power, double time) {
    robot.rotate(power, time);
  }

  public static void backward(double power) {
    robot.backward(power);
  }

  public static void backward(double power, double time) {
    robot.backward(power, time);
  }

  public static void stop() {
    robot.stop();
  }
  
  public static void penDown() {
    robot.penDown();
  }
  
  public static void penDown(string color) {
    robot.penDown(color);
  }
  
  public static Graphics.Line penUp() {
    return robot.penUp();
  }
  
  public static void move(double translate, double rotate) {
    robot.move(translate, rotate);
  }
  
  public static void turnLeft(double power) {
    robot.turnLeft(power);
  }

  public static void turnLeft(double power, double time) {
    robot.turnLeft(power, time);
  }

  public static void turnRight(double power) {
    robot.turnRight(power);
  }

  public static void turnRight(double power, double time) {
    robot.turnRight(power, time);
  }

  public static void motors(double left, double right) {
    robot.motors(left, right);
  }

  public static void reboot() {
    robot.reboot();
  }

  public static void beep(double duration, double frequency) {
    robot.beep(duration, frequency);
  }

  public static void beep(double duration, double frequency, double frequency2) {
    robot.beep(duration, frequency, frequency2);
  }

  public static void beep(int duration, int frequency) {
    robot.beep(duration, frequency);
  }

  public static void beep(int duration, int frequency, int frequency2) {
    robot.beep(duration, frequency, frequency2);
  }

  public static void beep(double duration, int frequency) {
    robot.beep(duration, frequency);
  }

  public static void beep(double duration, int frequency, int frequency2) {
    robot.beep(duration, frequency, frequency2);
  }

  public static void show(Graphics.Picture picture, 
      string title="Myro Camera") {
    Graphics.WindowClass win = Graphics.makeWindowFast(title,
        picture.width, picture.height);
    picture.draw(win);
  }

  public static void setup() {
    robot.setup();
  }

  public static string getName() {
    return robot.getName();
  }

  public static List getIRMessage() {
    return robot.getIRMessage();
  }

  public static void sendIRMessage(string data) {
  }

  public static void setCommunicate() {
  }

  public static List getBlob() {
    return robot.getBlob();
  }

  public static object getData(params int [] position) {
    return robot.getData(position);
  }

  public static void setData(int position, int value) {
    robot.setData(position, value);
  }

  public static PythonDictionary getAll() {
    return robot.getAll();
  }

  public static PythonDictionary getInfo() {
    return robot.getInfo();
  }

  public static object getObstacle(params object [] position) {
    if (position == null || position.Length == 0)
      return robot.getObstacle();
    else
      return robot.getObstacle(position);
  }

  public static object getLight(params object [] position) {
    if (position == null || position.Length == 0)
      return robot.getLight();
    else
      return robot.getLight(position);
  }

  public static object getIR(params object [] position) {
    if (position == null || position.Length == 0)
      return robot.getIR();
    else
      return robot.getIR(position);
  }

  public static object getBright(string window=null) {
    return robot.getBright(window);
  }

  public static object getBright(int window) {
    return robot.getBright(window);
  }

  public static object getLine(params object [] position) {
    if (position == null || position.Length == 0)
      return robot.getLine();
    else
      return robot.getLine(position);
  }

  public static object get(string sensor="all") {
    return robot.get(sensor);
  }

  public static object get(string sensor="all", params object [] position) {
    return robot.get(sensor, position);
  }

  public static string getPassword() {
    return robot.getPassword();
  }

  public static double getBattery() {
    return robot.getBattery();
  }

  public static PythonDictionary getConfig() {
    return robot.getConfig();
  }

  public static int getStall() {
    return robot.getStall();
  }

  public static void setLED(string position, object value) {
    robot.setLED(position, value);
  }

  public static void setLEDFront(object value) {
    robot.setLEDFront(value);
  }

  public static void setLEDBack(double value) {
    robot.setLEDBack(value);
  }

  public static void setEchoMode(int value) {
    robot.setEchoMode(value);
  }

  public static void setName(string name) {
    robot.setName(name);
  }

  public static void setIRPower(int power) {
    robot.setIRPower(power);
  }

  public static void setWhiteBalance(object value) {
    robot.setWhiteBalance(value);
  }

  public static void setForwardness(object value) {
    robot.setForwardness(value);
  }

  public static void setVolume(object volume) {
    robot.setVolume(volume);
  }

  public static void setPassword(string password) {
    robot.setPassword(password);
  }

  public static string flipCoin() {
    if (Random.random() < .5) {
      return "heads";
    } else {
      return "tails";
    }
  }

  public static int randint(int start, int end) {
    return (int)(Random.random() * (end - start + 1) + start);
  }

  public static bool heads() {
    return (flipCoin() == "heads");
  }

  public static bool tails() {
    return (flipCoin() == "tails");
  }

  public static double randomNumber() {
    return Random.random();
  }

  public static double random() {
    return Random.random();
  }

  public static List getFilenames(string path) {
    List<string> retval = new List<string>();
    List filenames = new List();
    string directory = "";
    string pattern;
    string [] parts = path.Split(Path.DirectorySeparatorChar);
    pattern = parts[parts.Length - 1]; // last
    for (int i = 0; i < parts.Length - 1; i++) {
        if (parts[i] == "") {
            directory = (directory + Path.DirectorySeparatorChar);
        } else {
            directory = Path.Combine(directory, parts[i]);
        }
    }
    if (directory == "") {
        directory = ".";
    }
    directory = Path.GetFullPath(directory);
    DirectoryInfo di = new DirectoryInfo(directory);
    FileInfo[] rgFiles = di.GetFiles(pattern);
    foreach(FileInfo fi in rgFiles) {
        retval.Add(Path.Combine(directory, fi.Name));
    }
    retval.Sort();
    foreach(string filename in retval) {
        filenames.append(filename);
    }
    return filenames;
  }

  public static string pickAFile() {
    ManualResetEvent ev = new ManualResetEvent(false);
    string retval = null;
    Gtk.Application.Invoke(delegate {
        Gtk.FileChooserDialog fc = new Gtk.FileChooserDialog("Select a file",
                               null,
                               Gtk.FileChooserAction.Open,
                               "Cancel", Gtk.ResponseType.Cancel,
                               "Open", Gtk.ResponseType.Accept);
        fc.ShowAll();
        if (fc.Run() == (int)(Gtk.ResponseType.Accept)) {
           retval = fc.Filename;
        }
        fc.Destroy();
        ev.Set();
        });
    ev.WaitOne();
    return retval;
  }

  public static string pickAFolder() {
    ManualResetEvent ev = new ManualResetEvent(false);
    string retval = null;
    Gtk.Application.Invoke(delegate {
        Gtk.FileChooserDialog fc = new Gtk.FileChooserDialog("Select a file",
                               null,
                               Gtk.FileChooserAction.SelectFolder,
                               "Cancel", Gtk.ResponseType.Cancel,
                               "Open", Gtk.ResponseType.Accept);
        fc.ShowAll();
        if (fc.Run() == (int)(Gtk.ResponseType.Accept)) {
           retval = fc.Filename;
        }
        fc.Destroy();
        ev.Set();
        });
    ev.WaitOne();
    return retval;
  }

  public static List getColorNames() {
    return Graphics.getColorNames();
  }

  public static Graphics.WindowClass Window(
      string title="Calico Graphics",
      int width=300, 
      int height=300) {
    return Graphics.makeWindow(title, width, height);
  }


  public static Graphics.WindowClass Window(int width,
                                            int height) {
    return makeWindow("Calico Graphics", width, height);
  }

  public static Graphics.Color pickAColor() {
    ManualResetEvent ev = new ManualResetEvent(false);
    Graphics.Color retval = null;
    Gtk.Application.Invoke(delegate {
        Gtk.ColorSelectionDialog fc = new Gtk.ColorSelectionDialog("Select a color");
        fc.ShowAll();
        if (fc.Run() == (int)(Gtk.ResponseType.Ok)) {
      retval = new Graphics.Color(
               (int)Math.Round(((double)((int)fc.ColorSelection.CurrentColor.Red))/Math.Pow(2,16) * 255.0),
           (int)Math.Round(((double)((int)fc.ColorSelection.CurrentColor.Green))/Math.Pow(2, 16) * 255.0),
           (int)Math.Round(((double)((int)fc.ColorSelection.CurrentColor.Blue))/Math.Pow(2, 16) * 255.0));
        }
        fc.Destroy();
        ev.Set();
        });
    ev.WaitOne();
    return retval;
  }

  public static string pickAFont() {
    ManualResetEvent ev = new ManualResetEvent(false);
    string retval = null;
    Gtk.Application.Invoke(delegate {
        Gtk.FontSelectionDialog fc = new Gtk.FontSelectionDialog("Select a font");
        fc.ShowAll();
        if (fc.Run() == (int)(Gtk.ResponseType.Ok)) {
           retval = fc.FontName;
        }
        fc.Destroy();
        ev.Set();
        });
    ev.WaitOne();
    return retval;
  }

  public static bool yesno(string question) {
    ManualResetEvent ev = new ManualResetEvent(false);
    bool retval = false;
    Gtk.Application.Invoke(delegate {
        Gtk.MessageDialog fc = new Gtk.MessageDialog(null,
                               0, Gtk.MessageType.Question,
                               Gtk.ButtonsType.YesNo,
                               question);
        fc.ShowAll();
        if (fc.Run() == (int)(Gtk.ResponseType.Yes)) {
           retval = true;
        }
        fc.Destroy();
        ev.Set();
        });
    ev.WaitOne();
    return retval;
  }

  public static string to_s(object obj) {
    return obj.ToString();
  }

  public static List to_l(RubyArray obj) {
    List list = new List();
    foreach (object choice in obj)
        list.append(choice.ToString());
    return list;
  }

  public static void joystick() {
    new Joystick();
  }

  class Joystick {
    Graphics.WindowClass window;
    Graphics.Line arrow = new Graphics.Line(new Graphics.Point(200, 200), 
                        new Graphics.Point(200, 200));
    double x, y;
    double r, t;
    string state = "up";
    double center_x, center_y;
    int radius = 175;

    public Joystick() {
      window = makeWindow("Myro Joystick", 400, 400);
      window.ButtonPressEvent  += onMouseDown;
      window.ButtonReleaseEvent  += onMouseUp;
      window.MotionNotifyEvent += onMouseMove;
      Graphics.Circle circle = new Graphics.Circle(
                new Graphics.Point(200, 200), radius);
      circle.fill = new Graphics.Color("white");
      circle.draw(window);
      circle = new Graphics.Circle(
                new Graphics.Point(200, 200), 10);
      circle.fill = new Graphics.Color("black");
      circle.draw(window);
      Graphics.Text text = new Graphics.Text(
            new Graphics.Point(window.width/2, 
                       12), "Forward");
      text.draw(window);
      text = new Graphics.Text(
            new Graphics.Point(window.width/2, 
                       window.height - 12), "Backward");
      text.draw(window);
      text = new Graphics.Text(
            new Graphics.Point(12, 
                       window.height/2), "Left");
      text.rotate(90);
      text.draw(window);
      text = new Graphics.Text(
            new Graphics.Point(window.width - 12, 
                       window.height/2), "Right");
      text.rotate(-90);
      text.draw(window);
      arrow.border = 5;
      arrow.draw(window);
    }

    void onMouseUp(object obj, Gtk.ButtonReleaseEventArgs args) {
      state = "up";
      arrow.points[1].x = 0;
      arrow.points[1].y = 0;
      window.QueueDraw();
      if (getRobot() != null) {
    getRobot().stop();
      }
    }

    void onMouseDown(object obj, Gtk.ButtonPressEventArgs args) {
      state = "down";
      x = args.Event.X;
      y = args.Event.Y;
      center_x = window.width/2;
      center_y = window.height/2;
      r = -(center_x - x) / (center_x - radius);
      t = (center_y - y) / (center_y - radius);
      r = Math.Min(Math.Max(r, -1), 1);
      t = Math.Min(Math.Max(t, -1), 1);
      arrow.points[1].x = x - center_x;
      arrow.points[1].y = y - center_y;
      window.QueueDraw();
      if (getRobot() != null) {
    getRobot().move(t, r);
      }
    }

    void onMouseMove(object obj, Gtk.MotionNotifyEventArgs args) {
      if (state == "down") {
    x = args.Event.X;
    y = args.Event.Y;
    center_x = window.width/2;
    center_y = window.height/2;
    r = -(center_x - x) / (center_x - radius);
    t = (center_y - y) / (center_y - radius);
    r = Math.Min(Math.Max(r, -1), 1);
    t = Math.Min(Math.Max(t, -1), 1);
    arrow.points[1].x = x - center_x;
    arrow.points[1].y = y - center_y;
    window.QueueDraw();
    if (getRobot() != null) {
      getRobot().move(t, r);
    }
      }
    }
  }

  public static string askQuestion(string question) {
    return askQuestion(question, Graphics.PyList("Yes", "No"));
  }

  public static string askQuestion(string question, RubyArray choices) {
    return askQuestion(question, to_l(choices));
  }

  public static string askQuestion(string question, List choices) {
    ManualResetEvent ev = new ManualResetEvent(false);
    dialogResponse = null;
    Gtk.Application.Invoke(delegate {
        Gtk.Dialog fc = new Gtk.Dialog("Information Request", null, 0);
        fc.VBox.PackStart(new Gtk.Label(question));
        foreach (string choice in choices) {
            Gtk.Button button = new Gtk.Button(choice);
            button.Clicked += new System.EventHandler(DialogHandler);
            fc.AddActionWidget(button, Gtk.ResponseType.Ok);
        }
        fc.ShowAll();
        fc.Run();
        fc.Destroy();
        ev.Set();
        });
    ev.WaitOne();
    return dialogResponse;
  }

  public static void DialogHandler(object obj, System.EventArgs args) {
    dialogResponse = ((Gtk.Button)obj).Label;
  }

  public static string input(object question) {
    return ask(question, "Input").ToString();
  }

  public static object ask() {
    return ask("Input: ", "Information Request");
  }

  public static object ask(object question) {
    return ask(question, "Information Request");
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

  public static object ask(object question, string title) {
    ManualResetEvent ev = new ManualResetEvent(false);
    object retval = null;
    Gtk.Entry myentry = null;
    PythonDictionary responses = new PythonDictionary();
    Gtk.Application.Invoke(delegate {
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

  public static object pickOne(params object [] items) {
    if (items.Length == 1) {
      if (items[0] is int) {
        return (int)(Random.random() * (int)items[0]);
      } else if (items[0] is IList<object>) {
        int pos = (int)(Random.random() * ((IList<object>)items[0]).Count);
        return ((IList<object>)items[0])[pos];
      } else {
        throw new Exception("pickOne: unknown item type");
      }
    } else {
      int pos = (int)(Random.random() * items.Length);
      return items[pos];
    }
  }

  public static void wait(double seconds) {
    Thread.Sleep((int)(seconds * 1000));
  }

  public static double currentTime() {
    System.TimeSpan t = System.DateTime.UtcNow - new System.DateTime(1970,1,1);
    return t.TotalSeconds;
  }

  public static bool odd(int n) {
    return ((n % 2) == 1);
  }

  public static bool even(int n) {
    return ((n % 2) == 0);
  }

  public static Graphics.Picture takePicture(string mode="jpeg") {
    return robot.takePicture(mode);
  }

  public static void setSeed(int value) {
    Random.seed = value;
  }

  public static int getSeed() {
    return Random.seed;
  }

  public class Randomizer {
    int _seed; 
    Random _random = new Random();

    public Randomizer() : this(0) {
	}
    
    public Randomizer(int seed) {
      if (seed != 0)
        this.seed = seed;
    }

    public int seed {
      get { 
        return _seed; 
      }
      set { 
        _seed = value; 
        _random = new Random(_seed);
      }
    }

    public double random() {
      return _random.NextDouble();
    }

  }
    
  // singleton
  public readonly static Randomizer Random = new Randomizer(); 
  
  public class Robot {
    internal double _lastTranslate = 0;
    internal double _lastRotate = 0;
    
    public virtual void beep(double duration, double frequency, double frequency2) {
      // Override in subclassed robots
      Console.WriteLine(String.Format("computer.beep({0},{1},{2})",
				      duration, frequency, frequency2));
    }
    
    public virtual void beep(double duration, double frequency) {
      // Override in subclassed robots
      Console.WriteLine(String.Format("computer.beep({0},{1})",
				      duration, frequency));
    }
    
    public virtual void reboot() {
    }
    
    public virtual Graphics.Picture takePicture(string mode="jpeg") {
      // Override in subclassed robots
      return null;
    }
    
    public virtual void setup() {
    }
    
    public virtual string getName() {
      return null;
    }
    
    public virtual List getIRMessage() {
      return null;
    }
    
    public virtual void setCommunicate() {
    }
    
    public virtual void sendIRMessage(string data) {
    }
    
    public virtual List getBlob() {
      return null;
    }
    
    public virtual object getData(params int [] position) {
      return null;
    }
    
    public virtual void setData(int position, int value) {
    }
    
    public virtual PythonDictionary getAll() {
      return null;
    }
    
    public virtual PythonDictionary getInfo() {
      return null;
    }
    
    public virtual object getObstacle(params object [] position) {
      return null;
    }
    
    public virtual object getLight(params object [] position) {
      return null;
    }
    
    public virtual object getIR(params object [] position) {
      return null;
    }
    
    public virtual object getBright(string window = null) {
      return null;
    }
    
    public virtual object getBright(int window) {
      return null;
    }
    
    public virtual object getLine(params object [] position) {
      return null;
    }
    
    public virtual object get(string sensor="all") {
      return null;
    }
    
    public virtual object get(string sensor="all", params object [] position) {
      return null;
    }
    
    public virtual string getPassword() {
      return null;
    }
    
    public virtual PythonDictionary getConfig() {
      return null;
    }
    
    public virtual int getStall() {
      return 0; 
    }
    
    public virtual double getBattery() {
      return 0.0;
    }
    
    public virtual void setLED(string position, object value) {
    }
    
    public virtual void setLEDFront(object value) {
    }
    
    public virtual void setLEDBack(double value) {
    }
    
    public virtual void setEchoMode(int value) {
    }
    
    public virtual void setName(string name) {
    }
    
    public virtual void setIRPower(int power) {
    }
    
    public virtual void setWhiteBalance(object value) {
    }
    
    public virtual void setForwardness(object value) {
    }
    
    public virtual void setVolume(object volume) {
    }
    
    public virtual void setPassword(string password) {
    }
    
    public virtual void adjustSpeed() {
    }

    public virtual bool isConnected()
    {
      return true;
    }

    public virtual void flush()
    {
    }

    public void penDown() {
      penDown("black");
    }

    public virtual void penDown(string color)
    {
      ask(String.Format("Please put a {0} pen in the robot.", color));
    }

    public virtual Graphics.Line penUp()
    {
      return null;
    }

    public void move(double translate, double rotate) {
      _lastTranslate = translate;
      _lastRotate = rotate;
      adjustSpeed();
    }
    
    public void playSong(List song) {
      playSong(song, 1.0);
    }
    
    public void playSong(List song, double speed) {
      foreach(IList tup in song) {
	if (tup.Count == 2) {
	  double f = System.Convert.ToDouble(tup[0]); 
	  double d = System.Convert.ToDouble(tup[1]);
	  beep(d, f);
	} else if (tup.Count == 3) {
	  double f1 = System.Convert.ToDouble(tup[0]); 
	  double f2 = System.Convert.ToDouble(tup[1]); 
	  double d = System.Convert.ToDouble(tup[2]);
	  beep(d * speed, f1, f2);
	}
      }
    }
    
    public void stop() {
      if (! isConnected()) 
	return;
      move(0, 0);
    }
    
    public void forward(double speed, double interval) {
      move(speed, 0);
      Thread.Sleep((int)(interval * 1000)); 
      stop();
    }
    
    public void forward(double speed) {
      move(speed, 0);
    }
    
    public void translate(double speed) {
      move(speed, 0);
    }
    
    public void translate(double speed, double interval) {
      move(speed, 0);
      Thread.Sleep((int)(interval * 1000)); 
      stop();
    }
    
    public void rotate(double speed) {
      move(0, speed);
    }
    
    public void rotate(double speed, double interval) {
      move(0, speed);
      Thread.Sleep((int)(interval * 1000)); 
      stop();
    }
    
    public void backward(double speed) {
      move(-speed, 0);
    }
    
    public void backward(double speed, double interval) {
      move(-speed, 0);
      Thread.Sleep((int)(interval * 1000)); 
      stop();
    }

    public void turnLeft(double speed) {
      move(0, speed);
    }

    public void turnLeft(double speed, double interval) {
      move(0, speed);
      Thread.Sleep((int)(interval * 1000)); 
      stop();
    }

    public void turnRight(double speed) {
      move(0, -speed);
    }
    
    public void turnRight(double speed, double interval) {
      move(0, -speed);
      Thread.Sleep((int)(interval * 1000)); 
      stop();
    }
    
    public void motors(double left, double right) {
      double trans = (right + left) / 2.0;
            double rotate = (right - left) / 2.0;
      move(trans, rotate);
    }
  }

  public static bool Contains(object item, params object[] items) {
    return ((IList<object>)items).Contains(item);
  }

  public class Computer: Robot {
    
  }

  public class SimScribbler : Robot {
    public Graphics.Rectangle frame;
    public Simulation simulation;
    public double velocity = 0;
    public double rate = 8.0;

    public SimScribbler(Simulation simulation) {
      this.simulation = simulation;
      frame = new Graphics.Rectangle(new Graphics.Point(320 - 23, 240 - 23),
				     new Graphics.Point(320 + 23, 240 + 23));
      frame.pen.minDistance = 10; // minimum distance from last point
      // Draw a body:
      Graphics.Polygon body = new Graphics.Polygon();

      double [] sx = new double[] {0.05, 0.05, 0.07, 0.07, 0.09, 0.09, 0.07, 
				   0.07, 0.05, 0.05, -0.05, -0.05, -0.07, 
				   -0.08, -0.09, -0.09, -0.08, -0.07, -0.05, 
				   -0.05};
      double [] sy = new double[] {0.06, 0.08, 0.07, 0.06, 0.06, -0.06, -0.06, 
				   -0.07, -0.08, -0.06, -0.06, -0.08, -0.07, 
				   -0.06, -0.05, 0.05, 0.06, 0.07, 0.08, 0.06};
      for (int i =0; i < sx.Length; i++) {
        body.append(new Graphics.Point(sx[i] * 250, sy[i] * 250));
      }
      body.fill = Color("red");
      body.draw(frame);
      // Draw wheels:
      Graphics.Rectangle wheel1 = new Graphics.Rectangle(new Graphics.Point(-10, -23),
							 new Graphics.Point(10, -17));
      wheel1.color = Color("black");
      wheel1.draw(frame);
      Graphics.Rectangle wheel2 = new Graphics.Rectangle(new Graphics.Point(-10, 23),
							 new Graphics.Point(10, 17));
      wheel2.color = Color("black");
      wheel2.draw(frame);
      
      // Details
      Graphics.Circle hole = new Graphics.Circle(new Graphics.Point(0,0), 2);
      hole.fill = Color("black");
      hole.draw(frame);
      
      Graphics.Rectangle fluke = new Graphics.Rectangle(new Graphics.Point(17, -10),
							new Graphics.Point(23, 10));
      fluke.color = Color("green");
      fluke.draw(frame);
      
      // Just the fill, to see outline of bounding box:
      frame.fill = null;
      // FIXME: something not closing correctly in render when :
      //frame.color = null;
      frame.outline = Color("lightgrey");
      // set collision
      frame.draw(simulation.window);
      frame.body.OnCollision += SetStall;
      this.simulation.robots.Add(this);
    }

    bool SetStall(FarseerPhysics.Dynamics.Fixture fixture1,
		  FarseerPhysics.Dynamics.Fixture ficture2,
		  FarseerPhysics.Dynamics.Contacts.Contact contact) {
      return true;
    }
	
    public override void adjustSpeed() {
      lock(this) {
	velocity = _lastTranslate * rate;
	frame.body.AngularVelocity = (float)(-_lastRotate * rate);
      }
    }
    
    public override void flush() { 
      //
    }   

    public override void penDown(string color) { 
      frame.outline = new Graphics.Color(color);
      frame.penDown();
    }   

    public override Graphics.Line penUp() { 
      return frame.penUp();
    }   

  }

  public class Scribbler: Robot {
    public SerialPort serial = null;
    double [] _fudge  = new double[4];
    double [] _oldFudge = new double[4];
    public string dongle;
    public int volume;
    public string startsong;
    public byte [] color_header = null;
    public byte [] gray_header = null;
    public byte emitters = 0x1 | 0x2 | 0x4;
    
    private byte [] _lastSensors;
    
    //static byte SOFT_RESET=33;
    static byte GET_ALL=65 ;
    //static byte GET_ALL_BINARY=66  ;
    //static byte GET_LIGHT_LEFT=67  ;
    //static byte GET_LIGHT_CENTER=68  ;
    //static byte GET_LIGHT_RIGHT=69  ;
    static byte GET_LIGHT_ALL=70  ;
    //static byte GET_IR_LEFT=71  ;
    //static byte GET_IR_RIGHT=72  ;
    static byte GET_IR_ALL=73  ;
    //static byte GET_LINE_LEFT=74  ;
    //static byte GET_LINE_RIGHT=75  ;
    static byte GET_LINE_ALL=76  ;
    //static byte GET_STATE=77  ;
    static byte GET_NAME1=78;
    static byte GET_NAME2=64;
    //static byte GET_STALL=79  ;
    static byte GET_INFO=80  ;
    static byte GET_DATA=81  ;

    static byte GET_PASS1=50;
    static byte GET_PASS2=51;

    static byte GET_RLE=82 ; // a segmented and run-length encoded image
    static byte GET_IMAGE=83 ; // the entire 256 x 192 image in YUYV format
    static byte GET_WINDOW=84 ; // the windowed image (followed by which window)
    static byte GET_DONGLE_L_IR=85 ; // number of returned pulses when
                                    // left emitter is turned on
    static byte GET_DONGLE_C_IR=86 ; // number of returned pulses when
                                    // center emitter is turned on
    static byte GET_DONGLE_R_IR=87 ; // number of returned pulses when
                                    // right emitter is turned on
    static byte GET_WINDOW_LIGHT=88   ; // average intensity in the
                                       // user defined region
    static byte GET_BATTERY=89 ; // battery voltage
    static byte GET_SERIAL_MEM=90 ; // with the address returns the
                                   // value in serial memory
    //static byte GET_SCRIB_PROGRAM=91 ; // with offset, returns the
                                      // scribbler program buffer
    //static byte GET_CAM_PARAM=92; // with address, returns the camera parameter at that address

    static byte GET_BLOB=95;

    static byte SET_PASS1=55;
    static byte SET_PASS2=56;
    static byte SET_SINGLE_DATA=96;
    static byte SET_DATA=97;
    static byte SET_ECHO_MODE=98;
    static byte SET_LED_LEFT_ON=99 ;
    static byte SET_LED_LEFT_OFF=100;
    static byte SET_LED_CENTER_ON=101;
    static byte SET_LED_CENTER_OFF=102;
    static byte SET_LED_RIGHT_ON=103;
    static byte SET_LED_RIGHT_OFF=104;
    static byte SET_LED_ALL_ON=105;
    static byte SET_LED_ALL_OFF=106;
    //static byte SET_LED_ALL=107 ;
    //static byte SET_MOTORS_OFF=108;
    static byte SET_MOTORS=109 ;
    static byte SET_NAME1=110 ;
    static byte SET_NAME2=119;           // set name2 byte
    static byte SET_LOUD=111;
    static byte SET_QUIET=112;
    static byte SET_SPEAKER=113;
    static byte SET_SPEAKER_2=114;

    static byte SET_DONGLE_LED_ON=116;   // turn binary dongle led on
    static byte SET_DONGLE_LED_OFF=117;  // turn binary dongle led off
    static byte SET_RLE=118;             // set rle parameters 
    static byte SET_DONGLE_IR=120;       // set dongle IR power
    //static byte SET_SERIAL_MEM=121;      // set serial memory byte
    //static byte SET_SCRIB_PROGRAM=122;   // set scribbler program memory byte
    //static byte SET_START_PROGRAM=123;   // initiate scribbler
                                        // programming process 
    static byte SET_RESET_SCRIBBLER=124; // hard reset scribbler
    //static byte SET_SERIAL_ERASE=125;    // erase serial memory
    static byte SET_DIMMER_LED=126;      // set dimmer led
    static byte SET_WINDOW=127;          // set user defined window
    static byte SET_FORWARDNESS=128;     // set direction of scribbler
    static byte SET_WHITE_BALANCE=129;   // turn on white balance on camera 
    static byte SET_NO_WHITE_BALANCE=130; // diable white balance on
                                         // camera (default)
    static byte SET_CAM_PARAM=131;       // with address and value, 
                                        // sets the camera parameter
                                        // at that address

    static byte GET_JPEG_GRAY_HEADER=135;
    static byte GET_JPEG_GRAY_SCAN=136;
    static byte GET_JPEG_COLOR_HEADER=137;
    static byte GET_JPEG_COLOR_SCAN=138;

    //static byte SET_PASS_N_BYTES=139;
    //static byte GET_PASS_N_BYTES=140;
    //static byte GET_PASS_BYTES_UNTIL=141;

    //static byte GET_VERSION=142;

    static byte GET_IR_MESSAGE = 150;
    static byte SEND_IR_MESSAGE = 151;
    static byte SET_IR_EMITTERS = 152;

    static byte PACKET_LENGTH     =  9;
        
    // #### Camera Addresses ####
    //static byte CAM_PID=0x0A;
    //static byte CAM_PID_DEFAULT=0x76;
    //static int    CAM_VER=0x0B;
    //static byte CAM_VER_DEFAULT=0x48;
    //static byte CAM_BRT=0x06;
    //static byte CAM_BRT_DEFAULT=0x80;
    //static byte CAM_EXP=0x10;
    //static byte CAM_EXP_DEFAULT=0x41;
    static byte CAM_COMA=0x12;
    static byte CAM_COMA_DEFAULT=0x14;
    static byte CAM_COMA_WHITE_BALANCE_ON= (byte)(CAM_COMA_DEFAULT |  (1 << 2));
    //static byte CAM_COMA_WHITE_BALANCE_OFF=(byte)(CAM_COMA_DEFAULT & ~(1 << 2));
    static byte CAM_COMB=0x13;
    static byte CAM_COMB_DEFAULT=0xA3;
    static byte CAM_COMB_GAIN_CONTROL_ON= (byte)(CAM_COMB_DEFAULT |  (1 << 1));
    //static byte CAM_COMB_GAIN_CONTROL_OFF=(byte)(CAM_COMB_DEFAULT & ~(1 << 1));
    static byte CAM_COMB_EXPOSURE_CONTROL_ON= (byte)(CAM_COMB_DEFAULT |  (1 << 0));
    //static byte CAM_COMB_EXPOSURE_CONTROL_OFF=(byte)(CAM_COMB_DEFAULT & ~(1 << 0));

    public Scribbler(SerialPort serial) {
      setup();
    }

    public Scribbler(string port):  this(port, 38400) {
    }  
    
    public Scribbler(string port, int baud) {

      if (port.StartsWith("COM") || port.StartsWith("com")){
       port = @"\\.\" + port;             // "comment
      }

      serial = new SerialPort(port, baud);
      lock(serial) {
        serial.ReadTimeout = 1000; // milliseconds
        serial.WriteTimeout = 1000; // milliseconds
        try {
          serial.Open();
        } catch {
          Console.Error.WriteLine(String.Format("ERROR: unable to open port '{0}'", 
						port));
	  serial = null;
        }
      }
      if (serial != null)
	setup();
    }
    
    public override void setup() {
      PythonDictionary info = null;
      try {
        info = getInfo();
      } catch {
        Console.WriteLine("ERROR: unable to talk to Scribbler");
      }
      if (info.Contains("fluke")) {
        Console.WriteLine("You are using:\n   Fluke, version {0}", info["fluke"]);
    if (info.Contains("robot") && info.Contains("robot-version")) {
      Console.WriteLine("   {0}, version {1}",
                (string)info["robot"], (string)info["robot-version"]);
    }
        dongle = (string)info["fluke"];
      } else if (info.Contains("dongle")) {
        dongle = (string)info["dongle"];
    Console.WriteLine("You are using:\n   Fluke, version {0}", info["dongle"]);
      } else {
    dongle = null;
    Console.WriteLine("You are using:\n   Scribbler without Fluke, version 0.0.0");
      }
      flush();
      // only ask to beep if there is a robot attached.
      if (info.Contains("robot")){
      setEchoMode(0);
      wait(.25);
      flush();
      setEchoMode(0);
      stop();
      set("led", "all", "off");
      beep(.03, 784);
      beep(.03, 880);
      beep(.03, 698);
      beep(.03, 349);
      beep(.03, 523);
      Console.WriteLine("Hello, my name is '{0}'!", getName());
      }
      if (dongle != null) {
        set_cam_param(Scribbler.CAM_COMA, Scribbler.CAM_COMA_WHITE_BALANCE_ON);
        set_cam_param(Scribbler.CAM_COMB,
              Scribbler.CAM_COMB_GAIN_CONTROL_ON | Scribbler.CAM_COMB_EXPOSURE_CONTROL_ON);
        // Config grayscale on window 0, 1, 2
        conf_gray_window(0, 2,   0, 128, 191, 1, 1);
        conf_gray_window(1, 64,  0, 190, 191, 1, 1);
        conf_gray_window(2, 128, 0, 254, 191, 1, 1);
        set_ir_power(135);
        conf_rle(90, 4, 0, 255, 51, 136, 190, 255);
      }
      if (info.Contains("robot")) {
        loadFudge();
      }
    }
    
    // Sets the fudge values (in memory, and on the flash memory on the robot)
    public void setFudge(double f1, double f2, double f3, double f4) {
        _fudge[0] = f1;
        _fudge[1] = f2;
        _fudge[2] = f3;
        _fudge[3] = f4;

        // Save the fudge data (in integer 0..255 form) to the flash memory
        // f1-f4 are float values 0..2, convert to byte values
        // But to make things quick, only save the ones that have changed!
        // 0..255 and save.

        if (_oldFudge[0] != _fudge[0]) {
            if (dongle == null) {
                setSingleData(0,  (int)(_fudge[0] * 127.0) );
            } else {
                setSingleData(3,  (int)(_fudge[0] * 127.0) );
            }
            _oldFudge[0] = _fudge[0];
        }
        if (_oldFudge[1] != _fudge[1]) {
            if (dongle == null) {
                setSingleData(1,  (int)(_fudge[1] * 127.0) );
            } else {
                setSingleData(2,  (int)(_fudge[1] * 127.0) );
            }
            _oldFudge[1] = _fudge[1];
        }

        if (_oldFudge[2] != _fudge[2]) {
            if (dongle == null) {
                setSingleData(2,  (int)(_fudge[2] * 127.0) );
            } else {
                setSingleData(1,  (int)(_fudge[2] * 127.0) );
            }
            _oldFudge[2] = _fudge[2];
        }

        if (_oldFudge[3] != _fudge[3]) {
            if (dongle == null) {
                setSingleData(3,  (int)(_fudge[3] * 127.0) );
            } else {
                setSingleData(0,  (int)(_fudge[3] * 127.0) );
            }
            _oldFudge[3] = _fudge[3];
        }
    }

    public void loadFudge() {
       for (int i=0; i < 4; i++) {
         _fudge[i] = (int)get("data",i);
         if (_fudge[i] == 0) {
             _fudge[i] = 127;
         }
         _fudge[i] = _fudge[i] / 127.0;
       }
       if (dongle != null) {
         double t1 = _fudge[3];
         double t2 = _fudge[2];
         double t3 = _fudge[1];
         double t4 = _fudge[0];
         _fudge[0] = t1;
         _fudge[1] = t2;
         _fudge[2] = t3;
         _fudge[3] = t4;
       }
    }

    //Gets the fudge values (from memory, so we don't get penalized by a slow
    // serial link)
    public PythonTuple getFudge() {
        return Graphics.PyTuple(_fudge[0], _fudge[1], _fudge[2], _fudge[3]);
    }

    // ------------------------------------------------------------
    // Data structures:
    public PythonDictionary dict(params object [] list) {
      // make a dictionary from a list
      PythonDictionary retval = new PythonDictionary();
      for (int i = 0; i < list.Length; i += 2) {
        retval[list[i]] = list[i+1];
      }
      return retval;
    }
    
    public List list(params object [] items) {
      // make a list from an array
      List retval = new List();
      for (int i = 0; i < items.Length; i++) {
        retval.append(items[i]);
      }
      return retval;
    }
    // ------------------------------------------------------------

    List bytes2ints(byte [] bytes) {
      List ints = new List();
      for (int i = 0; i < bytes.Length; i++) {
        ints.append((int)bytes[i]);
      }
      return ints;
    }

    byte [] GetBytes(byte value, int bytes) {
      byte [] retval = null;
      lock (this) { // lock robot
	write_packet(value);
	read(Scribbler.PACKET_LENGTH); // read the echo
	retval = read(bytes);
      }
      return retval;
    }

    List GetWord(byte value, int bytes) {
      List retval = new List();
      byte [] retvalBytes;
      lock (this) { // lock robot
	write_packet(value);
	read(Scribbler.PACKET_LENGTH); // read the echo
	retvalBytes = read(bytes);
      }
      for (int p = 0; p < retvalBytes.Length; p += 2) {
        retval.append(retvalBytes[p] << 8 | retvalBytes[p + 1]);
      }
      return retval;
    }

    public override object get(string sensor="all") {
      return get(sensor, null);
    }

    public override object get(string sensor="all", params object [] position) {
      object retval = null;
      sensor = sensor.ToLower();
      if (sensor == "config") {
        if (dongle == null) {
          return dict("ir", 2, "line", 2, "stall", 1, "light", 3);
        } else {
          return dict("ir", 2, "line", 2, "stall", 1, "light", 3,
              "battery", 1, "obstacle", 3, "bright", 3);
        }
      } else if (sensor == "stall") {
        // lastSensors are the single byte sensors
        _lastSensors = GetBytes(Scribbler.GET_ALL, 11); 
        // returned as bytes
        return byte_to_int(_lastSensors[10]);
      } else if (sensor == "forwardness") {
        if (read_mem(0, 0) != 0xDF) {
          retval = "fluke-forward";
        } else {
          retval = "scribbler-forward";
        }
        return retval;
      } else if (sensor == "startsong") {
        //TODO: need to get this from flash memory
        return "close-encounters";
      } else if (sensor == "version") {
        //TODO: just return this version for now; get from flash
        return REVISION.Split()[1];
      } else if (sensor == "info") {
        return getInfo();
      } else if (sensor == "name") {
        string s = "";
        byte [] c1 = GetBytes(Scribbler.GET_NAME1, 8);
        byte [] c2 = GetBytes(Scribbler.GET_NAME2, 8);
        foreach (char c in c1)
          if ((int)c >= (int)'0' & (int)c <= 'z')
            s += c;
        foreach (char c in c2)
          if ((int)c >= (int)'0' & (int)c <= 'z')
            s += c;
        //c = string.join([chr(x) for x in c if "0" <= chr(x) <= "z"], '').strip();
        return s;
      } else if (sensor == "password") {
        string s = "";
        byte [] c1 = GetBytes(Scribbler.GET_PASS1, 8);
        byte [] c2 = GetBytes(Scribbler.GET_PASS2, 8);
        foreach (char c in c1)
          if ((int)c >= (int)'0' & (int)c <= 'z')
            s += c;
        foreach (char c in c2)
          if ((int)c >= (int)'0' & (int)c <= 'z')
            s += c;
        return s;
      } else if (sensor == "volume") {
        return volume;
      } else if (sensor == "battery") {
        return getBattery();
      } else if (sensor == "blob") {
        return getBlob();
      } else {
        if (position == null) {
          if (sensor == "light") {
            return GetWord(Scribbler.GET_LIGHT_ALL, 6);
          } else if (sensor == "line") {
            return bytes2ints(GetBytes(Scribbler.GET_LINE_ALL, 2));
          } else if (sensor == "ir") {
            return bytes2ints(GetBytes(Scribbler.GET_IR_ALL, 2));
          } else if (sensor == "obstacle") {
            return list(getObstacle1("left"), 
                getObstacle1("center"), 
                getObstacle1("right"));
          } else if (sensor == "bright") {
            return list(getBright("left"), 
                getBright("middle"), 
                getBright("right"));
          } else if (sensor == "data") {
            return getData();
          } else if (sensor == "all") {
            _lastSensors = GetBytes(Scribbler.GET_ALL, 11); 
            // returned as bytes
            // single bit sensors
            if (dongle == null) {
              return dict(
                  "light", list(
                      (int)(_lastSensors[2] << 8 | _lastSensors[3]), 
                      (int)(_lastSensors[4] << 8 | _lastSensors[5]), 
                      (int)(_lastSensors[6] << 8 |_lastSensors[7])),
                  "ir", list((int)_lastSensors[0], (int)_lastSensors[1]), 
                  "line", list((int)_lastSensors[8],(int)_lastSensors[9]), 
                  "stall",byte_to_int(_lastSensors[10]));
            } else {
              return dict(
                  "light", list(
                      (int)(_lastSensors[2] << 8 |_lastSensors[3]), 
                      (int)(_lastSensors[4] << 8 |_lastSensors[5]), 
                      (int)(_lastSensors[6] << 8 |_lastSensors[7])),
                  "ir", list((int)_lastSensors[0],(int)_lastSensors[1]), 
                  "line", list((int)_lastSensors[8],(int)_lastSensors[9]), 
                  "stall", byte_to_int(_lastSensors[10]),
                  "obstacle", list(
                      getObstacle1("left"), 
                      getObstacle1("center"), 
                      getObstacle1("right")),
                  "bright", list(
                      getBright("left"), 
                      getBright("middle"), 
                      getBright("right")),
                  "blob", getBlob(),
                  "battery", getBattery()
                          );
            }
          } else {
            throw new Exception(String.Format("invalid sensor name: '{0}'", 
                    sensor));
          }
        }
        List retvals = list();
        foreach (object pos in position) {
          //System.Console.WriteLine("pos = {0}", pos);
          if (sensor == "light") {
            List values = GetWord(Scribbler.GET_LIGHT_ALL, 6);
            if (Contains(pos, 0, "left")) {
              retvals.append((int)values[0]);
            } else if (Contains(pos, 1, "middle", "center")) {
              retvals.append((int)values[1]);
            } else if (Contains(pos, 2, "right")) {
              retvals.append((int)values[2]);
            } else if (pos == null | (string)pos == "all") {
              retvals.append(values);
            } 
          } else if (sensor == "ir") {
            byte [] values = GetBytes(Scribbler.GET_IR_ALL, 2);
            if (Contains(pos, 0, "left")) {
              retvals.append((int)values[0]);
            } else if (Contains(pos, 1, "right")) {
              retvals.append((int)values[1]);
            } else if (pos == null | (string)pos == "all") {
              retvals.append(values);
            }
          } else if (sensor == "line") {
            byte [] values = GetBytes(Scribbler.GET_LINE_ALL, 2);
            if (Contains(pos, 0, "left")) {
              retvals.append((int)values[0]);
            } else if (Contains(pos, 1, "right")) {
              retvals.append((int)values[1]);
            }
          } else if (sensor == "data") {
            retvals.append(getData((int)pos));
          } else if (sensor == "obstacle") {
            retvals.append(getObstacle1(pos));
          } else if (sensor == "bright") {
            retvals.append(getBright((string)pos));
          } else {
            throw new Exception(String.Format("invalid sensor name: '{0}'",
                    sensor));
          }
        }
        if (retvals.__len__() == 0) {
          return null;
        } else if (retvals.__len__() == 1) {
          return retvals[0];
        } else {
          return retvals;
        }
      }
    }

    public object getData(int position) {
      int [] ints = new int[1];
      ints[0] = position;
      return getData(ints);
    }

    public object getData(object [] position) {
      int [] ints = new int[position.Length];
      for (int i=0; i < position.Length; i++) {
        ints[i] = (int)position[i];
      }
      return getData(ints);
    }
    
    public override object getData(params int [] position) {
      if (position == null || position.Length == 0) {
        position = new int[] {0, 1, 2, 3, 4, 5, 6, 7 };
      } 
      List retval = list();
      foreach (int p in position)
        retval.append((int)GetBytes(Scribbler.GET_DATA, 8)[p]);
      if (retval.Count == 1) 
        return (int)retval[0];
      else
        return retval;
    }

    public override void setLED(string position, object value) {
      set("led", position, value);
    }

    public override void setLEDFront(object value) {
      if (isTrue(value)) {
        write(Scribbler.SET_DONGLE_LED_ON);
      } else {
        write(Scribbler.SET_DONGLE_LED_OFF);
      }
    }

    public override void setLEDBack(double value) {
      if (value > 1) {
        value = 1;
      } else if (value <= 0) {
        value = 0;
      } else {
        value = (int)(value * (255 - 170) + 170); // scale
      }
      write(Scribbler.SET_DIMMER_LED);
      write((byte)value);
    }
    public override void setVolume(object volume) {
        set("volume", volume, null);
    }

    public override void setName(string name) {
      name = pad(name, 16);
      string name1 = name.Substring(0, 8);
      string name2 = name.Substring(8, 8);
      set(Scribbler.SET_NAME1, name1);
      set(Scribbler.SET_NAME2, name2);
    }
    
    public override void setWhiteBalance(object value) {
      if (isTrue(value)) {
        write(Scribbler.SET_WHITE_BALANCE);
      } else {
        write(Scribbler.SET_NO_WHITE_BALANCE);
      }
    }

    public override void setIRPower(int power) {
      write(Scribbler.SET_DONGLE_IR);
      write((byte)power);
    }

    public override void setEchoMode(int value) {
      if (isTrue(value)) {
        set(Scribbler.SET_ECHO_MODE, 1);
      } else {
        set(Scribbler.SET_ECHO_MODE, 0);
      }
    }

    byte [] _get(byte value, int bytes, string mode) {
      lock (this) { // lock robot
	write(value);
	read(Scribbler.PACKET_LENGTH); // read the echo
	if (mode == "byte") {
	  return read(bytes);
	} else if (mode == "word") {
	  byte [] retvalBytes = read(bytes);
	  byte [] retval = new byte[retvalBytes.Length/2];
	  for (int p=0; p < retvalBytes.Length; p+=2) {
	    retval[p/2] = (byte)(retvalBytes[p] << 8 | retvalBytes[p + 1]);
	  }
	  return retval;
	}
	return null;
      }
    }

    public override void setData(int position, int value) {
      byte [] data = GetBytes(Scribbler.GET_DATA, 8);
      data[position] = (byte)value;
      set(Scribbler.SET_DATA, data);
    }

    public override void setPassword(string password) {
      password = pad(password, 16);
      string pass1 = password.Substring(0, 8);
      string pass2 = password.Substring(8, 8);
      set(Scribbler.SET_PASS1, pass1);
      set(Scribbler.SET_PASS2, pass2);
    }

    public override void setForwardness(object direction) {
      byte val = 0;
      if (Contains(direction, "fluke-forward", 1)) {
        val = 1;
      } else if (Contains(direction, "scribbler-forward", 0)) {
        val = 0;
      } else {
        throw new Exception("unknown direction: should be 'fluke-forward' or 'scribbler-forward'");
      }
      write(Scribbler.SET_FORWARDNESS);
      write(val);
    }

    public bool isTrue(object value) {
      if (value as string != null) {
        return ((string)value == "on" ||
            (string)value == "1");
      } else if (value as int? != null) {
        return ((int)value == 1);
      } else 
        return false;
    }

    public void set(byte value, string s) {
      byte [] buffer = new byte[s.Length + 1];
      buffer[0] = value;
      for (int i = 0; i < s.Length; i++) {
        buffer[i + 1] = (byte)s[i];
      }
      set(buffer);
    }

    public void set(byte value, byte [] bytes) {
      byte [] buffer = new byte[bytes.Length + 1];
      buffer[0] = value;
      for (int i = 0; i < bytes.Length; i++) {
        buffer[i + 1] = (byte)bytes[i];
      }
      set(buffer);
    }

    public void set(params byte [] values) {
      lock (this) { // lock robot
	write_packet(values);
	read(Scribbler.PACKET_LENGTH); // read echo
	_lastSensors = read(11); // single bit sensors
      }
    }
  
    public void set(string item, object position, object value) {
      if (item == "led") {
    if ((string)position == "center") {
      if (isTrue(value))
        set(Scribbler.SET_LED_CENTER_ON);
      else
        set(Scribbler.SET_LED_CENTER_OFF);
    } else if ((string)position == "left") {
      if (isTrue(value)) 
        set(Scribbler.SET_LED_LEFT_ON);
      else             
        set(Scribbler.SET_LED_LEFT_OFF);
    } else if ((string)position == "right") {
      if (isTrue(value)) 
        set(Scribbler.SET_LED_RIGHT_ON);
      else
        set(Scribbler.SET_LED_RIGHT_OFF);
    } else if ((string)position == "front") {
      setLEDFront(value);
    } else if ((string)position == "back") {
      setLEDBack(System.Convert.ToDouble(value));
    } else if ((string)position == "all") {
      if (isTrue(value)) 
        set(Scribbler.SET_LED_ALL_ON);
      else
        set(Scribbler.SET_LED_ALL_OFF);
    } else {
      throw new Exception(String.Format("no such LED: '{0}'", position));
    }
      } else if (item == "name") {
    setName((string)position);
      } else if (item == "whitebalance") {
    setWhiteBalance((string)position);
      } else if (item == "irpower") {
    setIRPower((int)position);
      } else if (item == "volume") {
    if (isTrue(position)){
      volume = 1;
      set(Scribbler.SET_LOUD);
    } else {
      volume = 0;
      set(Scribbler.SET_QUIET);
    }
      } else if (item == "startsong") {
    startsong = (string)position;
      } else if (item == "echomode") {
        setEchoMode((int)position);
      } else if (item == "data") {
        setData((int)position, (byte)value);
      } else if (item == "password") {
        setPassword((string)position);
      } else if (item == "forwardness") {
        setForwardness((string)position);
      } else {
        throw new Exception(String.Format("invalid set item name: '{0}'", item));
      }
    }

    public override object getObstacle(params object [] position) {
      if (position == null || position.Length == 0) {
        return get("obstacle");
      } else {
        return get("obstacle", position);
      }
    }

    int getObstacle1(object position) {
      if (position as string != null) {
        string value = (string)position;
        if (value == "left") {
          write(Scribbler.GET_DONGLE_L_IR);
        } else if (value == "middle" || value == "center") {
          write(Scribbler.GET_DONGLE_C_IR);
        } else if (value == "right") {
          write(Scribbler.GET_DONGLE_R_IR);
        } else {
          throw new Exception();
        }
      } else {
        int value = (int)position;
        if (value == 0) {
          write(Scribbler.GET_DONGLE_L_IR);
        } else if (value == 1) {
          write(Scribbler.GET_DONGLE_C_IR);
        } else if (value == 2) {
          write(Scribbler.GET_DONGLE_R_IR);
        } else {
          throw new Exception();
        }
      }
      return read_2byte();
    }

    public override object getLight(params object [] position) {
      if (position == null || position.Length == 0) {
        return get("light");
      } else {
        return get("light", position);
      }
    }

    public override object getIR(params object [] position) {
      if (position == null || position.Length == 0) {
        return get("ir");
      } else {
        return get("ir", position);
      }
    }

    public override object getLine(params object [] position) {
      if (position == null || position.Length == 0) {
        return get("line");
      } else {
        return get("line", position);
      }
    }

    public override object getBright(int window) {
      write(Scribbler.GET_WINDOW_LIGHT);
      write((byte)window);
      return read_3byte(); // (63.0 * 192.0 * 255.0)
    }

    public override object getBright(string window=null) {
      byte byte_window = 0;
      if (window == null || (string)window == "all") {
        return get("bright");
      } else if (window as string != null) {
        if ((string)window == "left") {
          byte_window = 0;
        } else if ((string)window == "middle" || (string)window == "center") {
          byte_window = 1;
        } else if ((string)window == "right") {
          byte_window = 2;
        } else {
          throw new Exception();
        }
      }
      write(Scribbler.GET_WINDOW_LIGHT);
      write(byte_window);
      return read_3byte(); // (63.0 * 192.0 * 255.0)
    }

    public override List getBlob() {
      write(Scribbler.GET_BLOB);
      int numpixs = read_2byte();
      int xloc = (int)read_byte();
      int yloc = (int)read_byte();
      return list(numpixs, xloc, yloc);
    }

    public int read_2byte() {
      byte hbyte = read_byte();
      byte lbyte = read_byte();
      return (int)((hbyte << 8) | lbyte);
    }

    public int read_3byte() {
      byte hbyte = read_byte();
      byte mbyte = read_byte();
      byte lbyte = read_byte();
      return (int)((hbyte << 16)| (mbyte << 8) | lbyte);
    }

        public string ReadLine() {
          // Replaces serial.ReadLine() as it doesn't stop on \n
          string retval = "";
          byte b = read_byte();
          int counter = 0;
          while (b != 10 && counter < 255) { // '\n' newline
                retval += (char)b;
                b = read_byte();
                counter++;
          }
          return (retval + "\n");
        }

    public override PythonDictionary getInfo() {
      PythonDictionary retDict = new PythonDictionary();
      //int old = serial.ReadTimeout; // milliseconds
      string retval;
      // serial.setTimeout(4)
      //lock(serial)
      //  serial.ReadTimeout = 4000; // milliseconds
      flush();
      // have to do this twice since sometime the first echo isn't
      // echoed correctly (spaces) from the scribbler
      lock (this) { // lock robot
	write_packet(Scribbler.GET_INFO, 32, 32, 32, 32, 32, 32, 32, 32);
	lock(serial) {
	  try {
	    retval = ReadLine();
	  } catch {
	    //serial.ReadTimeout = old;
	    return retDict;
	  }
	}
	//#print "Got", retval
	Thread.Sleep(100); 
	//time.sleep(.1)
	write_packet(Scribbler.GET_INFO, 32, 32, 32, 32, 32, 32, 32, 32);
	lock(serial) {
	  try {
	    retval = ReadLine();
	  } catch {
	    //serial.ReadTimeout = old;
	    return retDict;
	  }
	}
	if (retval.Length == 0) {
	  lock(serial) 
	    //serial.ReadTimeout = old;
	    return retDict;
	}
      }
      if (retval[0] == 'P' | retval[0] == 'p') {
        retval = retval.Substring(1);
      }
      
      if (retval[0] == 'P' | retval[0] == 'p') {
        retval = retval.Substring(1);
      }
      
      foreach (string pair in retval.Split(',')) {
        if (pair.Contains(":")) {
          string [] split_pair = pair.Split(':');
          string it = split_pair[0]; string value = split_pair[1];
          retDict[it.ToLower().Trim()] = value.Trim();
        }
      }
      //lock(serial)
      //serial.ReadTimeout = old;
      return retDict;
    }
    
    public override void adjustSpeed() {
      double left  = Math.Min(Math.Max(_lastTranslate - _lastRotate, -1), 1);
      double right  = Math.Min(Math.Max(_lastTranslate + _lastRotate, -1), 1);
      byte leftPower = (byte)((left + 1.0) * 100.0);
      byte rightPower = (byte)((right + 1.0) * 100.0);
      set(Scribbler.SET_MOTORS, rightPower, leftPower);
    }

    public override void reboot() {
      write(Scribbler.SET_RESET_SCRIBBLER);
    }

    public override void beep(double duration, double frequency) {
      lock (this) { // lock robot
	set_speaker((int)frequency, (int)(duration * 1000));
	Thread.Sleep((int)(duration * 1000));
	read(Scribbler.PACKET_LENGTH + 11);
      }
    }

    public override void beep(double duration, double frequency, double frequency2) {
      set_speaker_2((int)frequency, (int)frequency2, (int)(duration * 1000));
      Thread.Sleep((int)(duration * 1000));
      read(Scribbler.PACKET_LENGTH + 11);
    }

    public void set_speaker(int frequency, int duration) {
      write_packet(Scribbler.SET_SPEAKER, 
		   (byte)(duration >> 8),
		   (byte)(duration % 256),
		   (byte)(frequency >> 8),
		   (byte)(frequency % 256));
    }
        
    public void set_speaker_2(int freq1, int freq2, int duration) {
      write_packet(Scribbler.SET_SPEAKER_2, 
		   (byte)(duration >> 8),
		   (byte)(duration % 256),
		   (byte)(freq1 >> 8),
		   (byte)(freq1 % 256),
		   (byte)(freq2 >> 8),
		   (byte)(freq2 % 256));
    }

    public override string getName() {
      return (string)get("name");
    }

    public override PythonDictionary getAll() {
      return (PythonDictionary)get("all");
    }

    public override string getPassword() {
      return (string)get("password");
    }

    public override double getBattery() {
      write(Scribbler.GET_BATTERY);
      double retval = Math.Round(read_2byte() / 20.9813, 2);
      return retval;
    }

    public override int getStall() {
      return (int)get("stall");
    }

    public override PythonDictionary getConfig() {
      return (PythonDictionary)get("config");
    }

    public byte read_byte() {
      byte [] bytes = new byte[1];
      lock(serial)
        serial.Read(bytes, 0, 1);
      return bytes[0];
    }

    public void read(int bytes, byte [] buffer, int start) {
      int tries = 0;
      int count = 0;
      while (count < bytes && tries < 4) { // 4 seconds
        byte [] retval = try_read(bytes-count);
        buffer_copy(retval, buffer, start, retval.Length);
        count += retval.Length;
        start += retval.Length;
        if (retval.Length == 0)
          tries++;
      }
    }

    public byte [] read(int bytes) {
      int count = 0;
      int tries = 0;
      byte [] buffer = new byte[bytes];
      while (count < bytes && tries < 4) { // 4 seconds
        byte [] retval = try_read(bytes-count);
        buffer_copy(retval, buffer, count, retval.Length);
        count += retval.Length;
        if (retval.Length == 0)
          tries++;
      }
      return buffer.Slice(0, count);
    }

    static void buffer_copy(byte [] from_buf, byte [] to_buf, int start, int length) {
        int count = 0;
        for (int i=start; i < start + length; i++) {
            to_buf[i] = from_buf[count];
            count++;
        }
    }

    static byte [] buffer_add(byte [] buf1, byte [] buf2) {
      byte[] buffer = new byte[buf1.Length + buf2.Length];
      buffer_copy(buf1, buffer, 0, buf1.Length);
      buffer_copy(buf2, buffer, buf1.Length, buf2.Length);
      return buffer;
    }

    public byte [] try_read(int bytes) {
      byte[] buffer = new byte[bytes];
      int len = 0;
      try {
        lock(serial)
          len = serial.Read(buffer, 0, bytes);
      } catch {
        Thread.Sleep(10); 
      }
      //if (len != bytes)
      //  Console.WriteLine("read: Wrong number of bytes read");
      if (dongle == null) {
        // HACK! THIS SEEMS TO NEED TO BE HERE!
        Thread.Sleep(10); 
      }
      return buffer.Slice(0, len);
    }

    public int read_mem(int page, int offset) {
      write(Scribbler.GET_SERIAL_MEM);
      write_2byte(page);
      write_2byte(offset);
      return read_byte();
    }

    public void write(byte b) {
      byte [] buffer = new byte[1];
      buffer[0] = b;
      lock(serial)
        serial.Write(buffer, 0, 1);
    }

    public void write(byte [] b) {
      lock(serial)
        serial.Write(b, 0, b.Length);
    }

    public void write_2byte(int value) {
      write((byte)((value >> 8) & 0xFF));
      write((byte)(value & 0xFF));
    }

    public void write_packet(params byte [] data) {
      byte [] buffer = new byte [Scribbler.PACKET_LENGTH]; 
      lock(serial) {
        try {
          serial.Write(data, 0, data.Length);
        } catch {
          Console.WriteLine("ERROR: in write");
        }
        if (Scribbler.PACKET_LENGTH - data.Length > 0) {
          try {
	    serial.Write(buffer, 0, Scribbler.PACKET_LENGTH - data.Length);
          } catch {
	    Console.WriteLine("ERROR: in write");
          }
        }
      } 
    }

    public override bool isConnected()
    {
      if (serial == null) 
	return false;
      return serial.IsOpen;
    }

    public override void flush() { 
      if (! isConnected()) 
	return;
      byte [] bytes = new byte[1];
      lock(serial) {
        serial.DiscardInBuffer();
        serial.DiscardOutBuffer();
	while (true) {
	  try {
	    serial.Read(bytes, 0, 1);
	  } catch {
	    // timeout, default is one second
	    // no data, so we're done
	    break;
	  }
	  serial.DiscardInBuffer();
	  serial.DiscardOutBuffer();
	}
      }
    }

    public override Graphics.Picture takePicture(string mode="jpeg") {
      int width = 256;
      int height = 192;
      Graphics.Picture p = null;
      if (mode == "color") {
        byte [] a = grab_array_yuv();
        if (a.Length == (width * height * 3))
          p = new Graphics.Picture(width, height, a);
      } else if (mode == "jpeg") {
        byte [] buffer = grab_jpeg_color(1);
        System.IO.MemoryStream ms = new System.IO.MemoryStream(buffer);
        System.Drawing.Bitmap bitmap = (System.Drawing.Bitmap)System.Drawing.Bitmap.FromStream(ms);
        p = new Graphics.Picture(bitmap, 256, 192);
      } else if (mode == "jpeg-fast") {
        byte [] buffer = grab_jpeg_color(0);
        System.IO.MemoryStream ms = new System.IO.MemoryStream(buffer);
        System.Drawing.Bitmap bitmap = (System.Drawing.Bitmap)System.Drawing.Bitmap.FromStream(ms);
        p = new Graphics.Picture(bitmap, 256, 192);
      } else if (mode == "gray" || mode == "grey") {
        byte [] buffer = grab_jpeg_gray(1);
        System.IO.MemoryStream ms = new System.IO.MemoryStream(buffer);
        System.Drawing.Bitmap bitmap = (System.Drawing.Bitmap)System.Drawing.Bitmap.FromStream(ms);
        p = new Graphics.Picture(bitmap, 256, 192);
      } else if (mode == "grayjpeg") {
        byte [] buffer = grab_jpeg_gray(1);
        System.IO.MemoryStream ms = new System.IO.MemoryStream(buffer);
        System.Drawing.Bitmap bitmap = (System.Drawing.Bitmap)System.Drawing.Bitmap.FromStream(ms);
        p = new Graphics.Picture(bitmap, 256, 192);
      } else if (mode == "grayjpeg-fast") {
        byte [] buffer = grab_jpeg_gray(0);
        System.IO.MemoryStream ms = new System.IO.MemoryStream(buffer);
        System.Drawing.Bitmap bitmap = (System.Drawing.Bitmap)System.Drawing.Bitmap.FromStream(ms);
        p = new Graphics.Picture(bitmap, 256, 192);
      } else if (mode == "grayraw" || mode == "greyraw") {
        conf_window(0, 1, 0, 255, 191, 2, 2);
        byte [] a = grab_gray_array();
        conf_gray_window(0, 2, 0,    128, 191, 1, 1);
        p = new Graphics.Picture(width, height, a, 1);
      } else if (mode == "blob") {
        byte [] a = grab_blob_array(); 
        p = new Graphics.Picture(width, height, a);
      }
      return p;
    }
  
  public byte [] grab_array_yuv() { // YUV color
    int width = 256;
    int height = 192;
    int size = width * height;
    byte [] buffer = new byte [size * 3];
    int vy, vu, y1v, y1u, uy, uv, y2u, y2v;
    int Y = 0, U = 0, V = 0;
    byte [] line;
    lock (this) { // lock robot
      write(Scribbler.GET_IMAGE);
      line = read(size); //BufferedRead(self.ser, size,
    }
    //start = 0);
    //create the image from the YUV layer
    for (int i=0; i < height; i++) {
      for (int j=0; j < width; j++) {
    if (j >= 3) {
      // go to the left for other values
      vy = -1; vu = -2; y1v = -1; y1u = -3; uy = -1; uv = -2; y2u = -1; y2v = -3;
    } else {
      // go to the right for other values
      vy = 1; vu = 2; y1v = 3; y1u = 1; uy = 1; uv = 2; y2u = 3; y2v = 1;
    }
    //   0123 0123 0123
    if ((j % 4) == 0) { //3 #2   VYUY VYUY VYUY
      V = line[i * width + j];
      Y = line[i * width + j + vy];
      U = line[i * width + j + vu];
    } else if ((j % 4) == 1) { //0 #3
      Y = line[i * width + j];
      V = line[i * width + j + y1v];
      U = line[i * width + j + y1u];
    } else if ((j % 4) == 2) { //1 #0
      U = line[i * width + j];
      Y = line[i * width + j + uy];
      V = line[i * width + j + uv];
    } else if ((j % 4) == 3) { //2 #1
      Y = line[i * width + j];
      U = line[i * width + j + y2u];
      V = line[i * width + j + y2v];
    }
    U = U - 128;
    V = V - 128;
    // Y = Y;
    buffer[(i * width + j) * 3 + 0] = (byte)Math.Max(Math.Min(Y + 1.13983 * V, 255), 0);
    buffer[(i * width + j) * 3 + 1] = (byte)Math.Max(Math.Min(Y - 0.39466*U-0.58060*V, 255), 0);
    buffer[(i * width + j) * 3 + 2] = (byte)Math.Max(Math.Min(Y + 2.03211*U, 255), 0);
      }
    }            
    return buffer;
  }
    
  public byte [] read_jpeg_header() {
    byte [] buf = read(2);
    int len = buf[0] + buf[1] * 256;
    return read(len);
  }
  
  public byte [] grab_jpeg_color(int mode) { // new color,
    // compressed (0=fast,
    // 1=reg)
    byte [] jpeg;
    lock (this) { // lock robot
      if (color_header == null) {
	write(Scribbler.GET_JPEG_COLOR_HEADER);
	color_header = read_jpeg_header();
      }
      write(GET_JPEG_COLOR_SCAN);
      write((byte)mode);
      jpeg = buffer_add(color_header, read_jpeg_scan());
    }
    return jpeg;
  }
  
    public byte [] grab_jpeg_gray(int mode) { // new gray, compressed
                                              // (0=fast, 1=reg)
      byte [] jpeg;
      lock (this) { // lock robot
	if (gray_header == null) {
	  write(Scribbler.GET_JPEG_GRAY_HEADER);
	  gray_header = read_jpeg_header();
	}
	write(Scribbler.GET_JPEG_GRAY_SCAN);
	write((byte)mode);
	jpeg = buffer_add(gray_header, read_jpeg_scan());
      }
      return jpeg;
    }
    
    public byte [] read_jpeg_scan() {
      byte [] bytes = new byte[100000];
      byte last_byte = 0;
      int count = 0;
      lock(serial) {
    while (true) {
      serial.Read(bytes, count, 1);
      if ((last_byte == 0xff) && (bytes[count] == 0xd9)) {
        // End-of-image marker
        break;
      }
      last_byte = bytes[count];
      count++;
    }
      }
      read_uint32();   // Start
      read_uint32();   // Read
      read_uint32();   // Compress
      return bytes.Slice(0, count);
    }
  
    public int read_uint32() {
        byte [] buf = read(4);
        return buf[0] + buf[1] * 256 + buf[2] * 65536 + buf[3] * 16777216;
    }

    public byte [] grab_blob_array() { // blob, RLE
      int width = 256;
      int height = 192;
      byte [] blobs = new byte[height * width * 3];  // RGB
      byte [] buffer;
      lock (this) { // lock robot
	write(Scribbler.GET_RLE);
	int size=(int)read_byte();
	size = (size << 8) | read_byte();
	buffer = read(size);
      }
      int px = 0;
      int counter = 0;
      int val = 128;
      bool inside = true;
      for (int i=0; i < height; i++) {
	for (int j=0; j < width; j+=4) {
	  if (counter < 1 && px < buffer.Length) {
	    counter = buffer[px];
	    px += 1;
	    counter = (counter << 8) | buffer[px];
            px += 1;
            if (inside) {
              val = 0;
              inside = false;
            } else {
              val = 255;
              inside = true;
            }
          }
          blobs[i * width * 3 + j + 0] = (byte)val;
          blobs[i * width * 3 + j + 1] = (byte)val;
          blobs[i * width * 3 + j + 2] = (byte)val;
          blobs[i * width * 3 + j + 3] = (byte)val;
	  
          counter -= 1;
        }
      }
      return blobs;
    }
      
    public byte [] grab_gray_array() {
      byte [] line;
      int width = 128;
      int height = 96;
      int size= width * height; 
      lock (this) { // lock robot
	write(Scribbler.GET_WINDOW);
	write((byte)0);
	line = read(size);
      }
      return quadrupleSize(line, width);
    }
  
    public byte [] quadrupleSize(byte [] line, int width) {
      byte [] retval = new byte[line.Length * 4]; 
      int col = 0;
      int row = 0;
      for (int i=0; i< line.Length; i++) {
        byte c = line[i];
        retval[row       * 2 * width + col]   = c;
        retval[row       * 2 * width + col+1] = c;
        retval[(row + 1) * 2 * width + col]   = c;
        retval[(row + 1) * 2 * width + col+1] = c;
        col += 2;
        if (col == width * 2) {
          col = 0;
          row += 2;
        }
      }
      return retval;
    }

    public void conf_gray_window(int window, int lx, int ly, int ux, int uy, 
        int xstep, int ystep) {
      // Y's are on odd pixels
      if ((lx % 2) == 0) {
        lx += 1;
      }
      if ((xstep % 2) == 1) {
        xstep += 1;
      }
      conf_window(window, lx, ly, ux, uy, xstep, ystep);
    }
  
    public void conf_window(int window, int X_LOW, int Y_LOW, 
        int X_HIGH, int Y_HIGH, int X_STEP, int Y_STEP) {
      write(Scribbler.SET_WINDOW);
      write((byte)window);
      write((byte)X_LOW);
      write((byte)Y_LOW);
      write((byte)X_HIGH);
      write((byte)Y_HIGH);
      write((byte)X_STEP);
      write((byte)Y_STEP);
    }

    public void setSingleData(int position, int value) {
      byte [] data = new byte[1]; 
      data[0] = (byte)position;
      data[1] = (byte)value;
      set(Scribbler.SET_SINGLE_DATA, data);
    }
    
    public void set_cam_param(int addr, int b) {
      write(Scribbler.SET_CAM_PARAM);
      write((byte)addr);
      write((byte)b);
      wait(.15); // camera needs time to reconfigure
    }
    
    public void set_ir_power(int power) {
      write(Scribbler.SET_DONGLE_IR);
      write((byte)power);
    }
    
    public override List getIRMessage() {
      lock (this) { // lock robot
	write(Scribbler.GET_IR_MESSAGE);
	int size = read_2byte();
	return bytes2ints(read(size));
      }
    }
    
    public override void sendIRMessage(string message) {
      byte [] data = new byte[message.Length];
      for (int i = 0; i < message.Length; i ++) {
        data[i] = (byte) message[i];
      }
      write(Scribbler.SEND_IR_MESSAGE);
      write((byte)data.Length);
      write(data);
    }
    
    public override void setCommunicate() {
      write(Scribbler.SET_IR_EMITTERS);
      write(emitters);
    }
    
    public PythonTuple set_blob_yuv(Graphics.Picture picture, int x1, int y1, int x2, int y2) {
      int [] xs = new int[2]; //[x1,x2];
      int [] ys = new int[2]; //[y1,y2];
      xs[0] = Math.Min(x1, x2);
      xs[1] = Math.Max(x1, x2);
      ys[0] = Math.Min(y1, y2);
      ys[1] = Math.Max(y1, y2);
    
      //set up variables to hold counts and accumulations:
      double totalY = 0.0;
      double totalU = 0.0;
      double totalV = 0.0;
       
      List ySamples = new List();
      List uSamples = new List();
      List vSamples = new List();
        
      for (int i=xs[0]; i < xs[1]; i++) {
        for (int j=ys[0]; j < ys[1]; j++) {
          PythonTuple rgb = picture.getPixel(i,j).getRGB();
          List yuv = rgb2yuv((int)rgb[0], (int)rgb[1], (int)rgb[2]);
          totalY = totalY + (int)yuv[0];
          totalU = totalU + (int)yuv[1];
          totalV = totalV + (int)yuv[2];
          ySamples.append((int)yuv[0]);
          uSamples.append((int)yuv[1]);
          vSamples.append((int)yuv[2]);
        }
      }
      
      int count = ySamples.Count;
      double yMean = totalY / count;
      double uMean = totalU / count;
      double vMean = totalV / count;
      
      // The standard deviation of a random variable with a normal 
      // distribution is the root-mean-square (RMS) deviation of its 
      // values from their mean.
      double sY = 0.0;
      double sU = 0.0;
      double sV = 0.0;
      
      for (int i=0; i < count; i ++) {
        sY = sY + ((int)ySamples[i] - yMean) * ((int)ySamples[i] - yMean);
        sU = sU + ((int)uSamples[i] - uMean) * ((int)uSamples[i] - uMean);
        sV = sV + ((int)vSamples[i] - vMean) * ((int)vSamples[i] - vMean);
      }

      sY = Math.Sqrt( sY / count);
      sU = Math.Sqrt( sU / count);
      sV = Math.Sqrt( sV / count);

      // Select the U/V bounding box based upon stdMod stdDev
      // from the mean, with approripate
      // min/max values to fit in an 8 bit register.
      //
      double stdMod = 3.0;

      int minU = (int)Math.Max(0, (uMean - sU*stdMod)    );
      int maxU = (int)Math.Min(255, (uMean + sU*stdMod)  );
      int minV = (int)Math.Max(0, (vMean - sV*stdMod) );
      int maxV = (int)Math.Min(255, (vMean + sV*stdMod) );
      
      // Note that we use the default values for
      // several parameters, most importantly the Y value
      // defaults to a range of 0-254
      conf_rle(minU, maxU, minV, maxV);

      // Return a tupal of parameters suitable for the configureBlob
      // function, to be shown to the user.
      
      return Graphics.PyTuple(0, 254, minU, maxU, minV, maxV);
    }
    
    public void conf_rle(int delay = 90, int smooth_thresh = 4,
        int y_low=0, int y_high=254,
        int u_low=51, int u_high=136,
        int v_low=190, int v_high=254) {
      write(Scribbler.SET_RLE);
      write((byte)delay);
      write((byte)smooth_thresh);
      write((byte)y_low);
      write((byte)y_high);
      write((byte)u_low);
      write((byte)u_high);
      write((byte)v_low);
      write((byte)v_high);
    }
  }

  public static IEnumerable timer(double seconds) {
    double start = currentTime();
    while (currentTime() - start < seconds) {
      yield return (currentTime() - start);
    }
  }

  // Graphics.cs

  public static Graphics.Color makeColor(PythonTuple rgb) {
    return Graphics.makeColor((int)rgb[0], (int)rgb[1], (int)rgb[2]);
  }

  public static Graphics.Color makeColor(string color) {
    return Graphics.makeColor(color);
  }

  public static Graphics.Color Color(PythonTuple rgb) {
    return Graphics.makeColor((int)rgb[0], (int)rgb[1], (int)rgb[2]);
  }

  public static Graphics.Color Color(string color) {
    return Graphics.makeColor(color);
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

  public static Graphics.Color getColor(Graphics.Picture picture, int x, int y) {
    return picture.getColor(x, y);
  }
  
  public static IEnumerable getPixels(Graphics.Picture picture) {
    return Graphics.getPixels(picture);
  }

  public static Graphics.Pixel getPixel(Graphics.Picture picture, int col, int row) {
    return picture.getPixel(col, row);
  }

  public static void setPixel(Graphics.Picture picture, int col, int row, Graphics.Color color) {
    picture.setPixel(col, row, color);
  }

  public static void setPixel(Graphics.Picture picture, int col, int row, Graphics.Pixel pixel) {
    picture.setPixel(col, row, pixel);
  }

  public static Graphics.Color makeColor(int r, int g, int b) {
    return Graphics.makeColor(r, g, b);
  }

  public static Graphics.Color makeColor(int r, int g, int b, int a) {
    return Graphics.makeColor(r, g, b, a);
  }

  public static Graphics.Color makeColor(double r, double g, double b) {
    return Graphics.makeColor(r, g, b);
  }

  public static Graphics.Color makeColor(double r, double g, double b, double a) {
    return Graphics.makeColor(r, g, b, a);
  }

  public static Graphics.Color getColor(Graphics.Pixel pixel) {
    return pixel.getColor();
  }
  public static PythonTuple getRGB(Graphics.Pixel pixel) {
    return pixel.getRGB();
  }
  public static PythonTuple getRGBA(Graphics.Pixel pixel) {
    return pixel.getRGBA();
  }
  public static int getX(Graphics.Pixel pixel) {
    return pixel.x;
  }
  public static int getY(Graphics.Pixel pixel) {
    return pixel.y;
  }
  public static int getGray(Graphics.Pixel pixel) {
    return pixel.getGray();
  }
  public static int getRed(Graphics.Pixel pixel) {
    return pixel.getRed();
  }
  public static int getGreen(Graphics.Pixel pixel) {
    return pixel.getGreen();
  }
  public static int getBlue(Graphics.Pixel pixel) {
    return pixel.getBlue();
  }
  public static int getAlpha(Graphics.Pixel pixel) {
    return pixel.getAlpha();
  }
  public static object get(PythonTuple tuple, int position) {
    return tuple[position];
  }
  public static PythonTuple makeTuple(params object [] items) {
    // make a tuple from an array
    return new PythonTuple(items);
  }
  public static void setColor(Graphics.Pixel pixel, Graphics.Color c) {
    pixel.setColor(c);
  }
  public static void setRGB(Graphics.Pixel pixel, PythonTuple rgb) {
    pixel.setRGB((byte)rgb[0], (byte)rgb[1], (byte)rgb[2]);
  }
  public static void setRGB(Graphics.Pixel pixel, byte red, byte green, byte blue) {
    pixel.setRGB(red, green, blue);
  }
  public static void setRGB(Graphics.Pixel pixel, int red, int green, int blue) {
    pixel.setRGB((byte)red, (byte)green, (byte)blue);
  }
  public static void setRGB(Graphics.Pixel pixel, float red, float green, float blue) {
    pixel.setRGB((byte)red, (byte)green, (byte)blue);
  }
  public static void setRGBA(Graphics.Pixel pixel, byte red, byte green, byte blue, byte alpha) {
    pixel.setRGBA(red, green, blue, alpha);
  }
  public static void setGray(Graphics.Pixel pixel, byte value) {
    pixel.setGray(value);
  }
  public static void setRed(Graphics.Pixel pixel, byte value) {
    pixel.setRed(value);
  }
  public static void setGreen(Graphics.Pixel pixel, byte value) {
    pixel.setGreen(value);
  }
  public static void setBlue(Graphics.Pixel pixel, byte value) {
    pixel.setBlue(value);
  }
  public static void setAlpha(Graphics.Pixel pixel, byte value) {
    pixel.setAlpha(value);
  }

  public static void savePicture(Graphics.Picture picture, string filename) {
    picture.savePicture(filename);
  }
  public static void savePicture(List list, string filename, short delay, bool repeat) {
    Graphics.savePicture(list, filename, delay, repeat);
  }
  public static object Vector(int x, int y) {
    return Graphics.Vector((float)x, (float)y);
  }
  public static object Vector(double x, double y) {
    return Graphics.Vector((float)x, (float)y);
  }
  public static Graphics.Picture makePicture(int x, int y) {
    return new Graphics.Picture(x, y);
  }
  public static Graphics.Picture makePicture(int x, int y, Graphics.Color c) {
    return new Graphics.Picture(x, y, c);
  }
  public static Graphics.Picture makePicture(string filename) {
    return new Graphics.Picture(filename);
  }
  public static Graphics.Picture makePicture(Graphics.Picture picture) {
    return new Graphics.Picture(picture);
  }

  public static Graphics.Picture makePicture(Graphics.WindowClass window) { //, string filename) {
    return Graphics.makePicture(window);
  }

  public static Graphics.Picture copyPicture(Graphics.Picture picture) { //, string filename) {
    return Graphics.copyPicture(picture);
  }

  public static Graphics.WindowClass makeWindow(string title="Calico Graphics",
      int width=300,
      int height=300) {
    return Graphics.makeWindow(title, width, height);
  }

  public static Graphics.WindowClass getWindow() {
    return Graphics.getWindow();
  }

  public static Simulation getSimulation() {
    return Myro.simulation;
  }

  public static PythonTuple getMouse() {
    return Graphics.getMouse();
  }

  public static PythonTuple getMouseNow() {
    return Graphics.getMouseNow();
  }

  public static string getMouseState() {
    return Graphics.getMouseState();
  }

  public static string getKeyState() {
    return Graphics.getKeyState();
  }  

  public static string getKeyPressed() {
    return Graphics.getKeyPressed();
  }  

  public static void run() {
    Graphics.run();
  }

  public static void run(Func<object> function) {
    Graphics.run(function);
  }

  // Callbacks:

  public static void onMouseUp(Func<object,Graphics.Event,object> function) {
    Graphics.onMouseUp(function);
  }

  public static void onMouseDown(Func<object,Graphics.Event,object> function) {
    Graphics.onMouseDown(function);
  }

  public static void onMouseMovement(Func<object,Graphics.Event,object> function) {
    Graphics.onMouseMovement(function);
  }

  public static void onKeyPress(Func<object,Graphics.Event,object> function) {
    Graphics.onKeyPress(function);
  }

  public static void onKeyRelease(Func<object,Graphics.Event,object> function) {
    Graphics.onKeyRelease(function);
  }

  public static Graphics.WindowClass getWindow(string title) {
    return Graphics.getWindow(title);
  }

  public static int getHeight(Graphics.WindowClass window) {
    return window.getHeight();
  }

  public static int getHeight(Graphics.Picture picture) {
    return picture.getHeight();
  }

  public static int getWidth(Graphics.WindowClass window) {
    return window.getWidth();
  }

  public static int getWidth(Graphics.Picture picture) {
    return picture.getWidth();
  }

  public static Graphics.Picture getRegion(Graphics.Picture picture, IList iterable, int width, int height, 
                    double degrees) {
    return picture.getRegion(new Graphics.Point(iterable[0], iterable[1]), width, height, degrees);
  }
  
  public static Graphics.Picture getRegion(Graphics.Picture picture, Graphics.Point p, int width, int height, double degrees) {
    return picture.getRegion(p, width, height, degrees);
  }

  public static void setRegion(Graphics.Picture picture, IList iterable, 
                   int width, int height, 
                   double degrees, Graphics.Color color) {
    picture.setRegion(new Graphics.Point(iterable[0], iterable[1]), width, 
              height, degrees, color);
  }
  
  public static void setRegion(Graphics.Picture picture, Graphics.Point p, 
                   int width, int height, double degrees,
                   Graphics.Color color) {
    picture.setRegion(p, width, height, degrees, color);
  }

  static Action functionInvoke(Func<object> func, List list, 
                      int position) {
    // Take a function, return list, and position
    // Call the function, and put the result in the
    // list in the given position.
    return () => {  
          list[position] = func.Invoke();
    }; 
  }

  static Action functionInvokeWithArgs(Func<object,Array> func, 
                       object [] args,
                       List list, 
                      int position) {
    // Take a function, return list, and position
    // Call the function, and put the result in the
    // list in the given position.
    return () => {  
          list[position] = func.Invoke(args);
    }; 
  }

  public static List doTogether(params IList [] objects) {
    List retval = new List();
    List threads = new List();
    int position = 0; 
    // For each function, make a return list, and thread list
    foreach (IList list in objects) {
      // FIXME: what signature? Not: object [], IList, or Array
      Func<object,Array> function = (Func<object,Array>)list[0];
      object [] args = ((object [])list).Slice(1, list.Count);
      retval.append(null);
      Thread thread = new Thread( 
           new ThreadStart(
                functionInvokeWithArgs(function, args, retval, position)));
      thread.IsBackground = true;
      threads.append(thread);
      position++;
    }
    // Start each thread
    foreach (Thread t in threads) {
      t.Start();
    }
    // Wait for them all to finish
    foreach (Thread t in threads) {
      t.Join();
    }
    // return
    return retval;
  }

  public static List doTogether(params Func<object> [] functions) {
    List retval = new List();
    List threads = new List();
    int position = 0; 
    // For each function, make a return list, and thread list
    foreach (Func<object> function in functions) {
      retval.append(null);
      Thread thread = new Thread( 
           new ThreadStart(functionInvoke(function, retval, position)));
      thread.IsBackground = true;
      threads.append(thread);
      position++;
    }
    // Start each thread
    foreach (Thread t in threads) {
      t.Start();
    }
    // Wait for them all to finish
    try {
       foreach (Thread t in threads) {
         t.Join();
       }
    } catch { 
      // error in joining, probably an abort
    } finally {
       foreach (Thread t in threads) {
         try {
            t.Abort();
         } catch {}
       }
    }
    // return
    return retval;
  }

  public static void initialize_module(string startup_path, 
                       string os_name) {
    Myro.startup_path = startup_path;
    Myro.os_name = os_name;
    voices["af"] = "Afrikaans Male";
    voices["af+f1"] = "Afrikaans Female";
    voices["bs"] = "Bosnian Male";
    voices["bs+f1"] = "Bosnian Female";
    voices["ca"] = "Catalan Male";
    voices["ca+f1"] = "Catalan Female";
    voices["cs"] = "Czech Male";
    voices["cs+f1"] = "Czech Female";
    voices["cy"] = "Welsh Male";
    voices["cy+f1"] = "Welsh Female";
    voices["da"] = "Danish Male";
    voices["da+f1"] = "Danish Female";
    voices["de"] = "German Male";
    voices["de+f1"] = "German Female";
    voices["el"] = "Greek Male";
    voices["el+f1"] = "Greek Female";
    voices["en"] = "Default Male";
    voices["en+f1"] = "Default Female";
    voices["en-sc"] = "English-Scottish Male";
    voices["en-sc+f1"] = "English-Scottish Female";
    voices["en-uk"] = "Englsih-British Male";
    voices["en-uk+f1"] = "English-British Female";
    voices["en-uk-north"] = "English-British-Lancashire Male";
    voices["en-uk-north+f1"] = "English-British-Lancashire Female";
    voices["en-uk-rp"] = "English-British-RP Male";
    voices["en-uk-rp+f1"] = "English-British-RP Female";
    voices["en-uk-wmids"] = "English-British-WMIDS Male";
    voices["en-uk-wmids+f1"] = "English-British-WMIDS Female";
    voices["en-us"] = "English-US Male";
    voices["en-us+f1"] = "English-US Female";
    voices["en-wi"] = "English-West-Indies Male";
    voices["en-wi+f1"] = "English-West-Indies Female";
    voices["eo"] = "Esperanto Male";
    voices["eo+f1"] = "Esperanto Female";
    voices["es"] = "Spanish Male";
    voices["es+f1"] = "Spanish Female";
    voices["es-la"] = "Spanish-Latin-American Male";
    voices["es-la+f1"] = "Spanish-Latin-American Female";
    voices["fi"] = "Finnish Male";
    voices["fi+f1"] = "Finnish Female";
    voices["fr"] = "French Male";
    voices["fr+f1"] = "French Female";
    voices["fr-be"] = "French-Belgium Male";
    voices["fr-be+f1"] = "French-Belgium Female";
    voices["grc"] = "Greek-Ancient Male";
    voices["grc+f1"] = "Greek-Ancient Female";
    voices["hi"] = "Hindi Male";
    voices["hi+f1"] = "Hindi Female";
    voices["hr"] = "Croatian Male";
    voices["hr+f1"] = "Croatian Female";
    voices["hu"] = "Hungarian Male";
    voices["hu+f1"] = "Hungarian Female";
    voices["hy"] = "Armenian Male";
    voices["hy+f1"] = "Armenian Female";
    voices["hy-west"] = "Armenian-West Male";
    voices["hy-west+f1"] = "Armenian-West Female";
    voices["id"] = "Indonesian Male";
    voices["id+f1"] = "Indonesian Female";
    voices["is"] = "Icelandic Male";
    voices["is+f1"] = "Icelandic Female";
    voices["it"] = "Italian Male";
    voices["it+f1"] = "Italian Female";
    voices["jbo"] = "Lojban Male";
    voices["jbo+f1"] = "Lojban Female";
    voices["ku"] = "Kurdish Male";
    voices["ku+f1"] = "Kurdish Female";
    voices["la"] = "Latin Male";
    voices["la+f1"] = "Latin Female";
    voices["lv"] = "Latvian Male";
    voices["lv+f1"] = "Latvian Female";
    voices["mk"] = "Macedonian Male";
    voices["mk+f1"] = "Macedonian Female";
    voices["nci"] = "Nahautl-Classical Male";
    voices["nci+f1"] = "Nahautl-ClassicalFemale";
    voices["nl"] = "Dutch Male";
    voices["nl+f1"] = "Dutch Female";
    voices["no"] = "Norwegian Male";
    voices["no+f1"] = "Norweigian Female";
    voices["pap"] = "Papiamento Male";
    voices["pap+f1"] = "Papiamento Female";
    voices["pl"] = "Polish Male";
    voices["pl+f1"] = "Polish Female";
    voices["pt"] = "Brazil Male";
    voices["pt+f1"] = "Brazil Female";
    voices["pt-pt"] = "Portugal Male";
    voices["pt-pt+f1"] = "Portugal Female";
    voices["ro"] = "Romanian Male";
    voices["ro+f1"] = "Romanian Female";
    voices["ru"] = "Russian Male";
    voices["ru+f1"] = "Russian Female";
    voices["sk"] = "Slovak Male";
    voices["sk+f1"] = "Slovak Female";
    voices["sq"] = "Albanian Male";
    voices["sq+f1"] = "Albanian Female";
    voices["sr"] = "Serbian Male";
    voices["sr+f1"] = "Serbian Female";
    voices["sv"] = "Swedish Male";
    voices["sv+f1"] = "Swahili Female";
    voices["sw"] = "Swahili Male";
    voices["sw+f1"] = "Swahili Female";
    voices["ta"] = "Tamil Male";
    voices["ta+f1"] = "Tamil Female";
    voices["tr"] = "Turkish Male";
    voices["tr+f1"] = "Turkish Female";
    voices["vi"] = "Vietnam Male";
    voices["vi+f1"] = "Vietnam Female";
    voices["zh"] = "Mandarin Male";
    voices["zh+f1"] = "Mandarin Female";
    voices["zh-yue"] = "Cantonese Male";
    voices["zh-yue+f1"] = "Cantonese Female";

    frequencies["rest"] = 0.0;
    frequencies["pause"] = 0.0;
    frequencies["a0"] = 27.50;
    frequencies["a#0"] = 29.14;
    frequencies["bb0"] = 29.14;
    frequencies["b0"] = 30.87;
    frequencies["c1"] = 32.70;
    frequencies["c#1"] = 34.65;
    frequencies["db1"] = 34.65;
    frequencies["d1"] = 36.71;
    frequencies["d#1"] = 38.89;
    frequencies["eb1"] = 38.89;
    frequencies["e1"] = 41.20;
    frequencies["f1"] = 43.65;
    frequencies["f#1"] = 46.25;
    frequencies["gb1"] = 46.25;
    frequencies["g1"] = 49.00;
    frequencies["g#1"] = 51.91;
    frequencies["ab1"] = 51.91;
    frequencies["a1"] = 55.00;
    frequencies["a#1"] = 58.27;
    frequencies["bb1"] = 58.27;
    frequencies["b1"] = 61.74;
    frequencies["c2"] = 65.41;
    frequencies["c#2"] = 69.30;
    frequencies["db2"] = 69.30;
    frequencies["d2"] = 73.42;
    frequencies["d#2"] = 77.78;
    frequencies["eb2"] = 77.78;
    frequencies["e2"] = 82.41;
    frequencies["f2"] = 87.31;
    frequencies["f#2"] = 92.50;
    frequencies["gb2"] = 92.50;
    frequencies["g2"] = 98.00;
    frequencies["g#2"] = 103.80;
    frequencies["ab2"] = 103.80;
    frequencies["a2"] = 110.00;
    frequencies["a#2"] = 116.50;
    frequencies["bb2"] = 116.50;
    frequencies["b2"] = 123.471;
    frequencies["c3"] = 130.8;
    frequencies["c#3"] = 138.6;
    frequencies["db3"] = 138.6;
    frequencies["d3"] = 146.8;
    frequencies["d#3"] = 155.6;
    frequencies["eb3"] = 155.6;
    frequencies["e3"] = 164.8;
    frequencies["f3"] = 174.6;
    frequencies["f#3"] = 185.0;
    frequencies["gb3"] = 185.0;
    frequencies["g3"] = 196.0;
    frequencies["g#3"] = 207.7;
    frequencies["ab3"] = 207.7;
    frequencies["a3"] = 220.0;
    frequencies["a#3"] = 233.1;
    frequencies["bb3"] = 233.1;
    frequencies["b3"] = 246.9;
    frequencies["c4"] = 261.6;
    frequencies["c#4"] = 277.2;
    frequencies["db4"] = 277.2;
    frequencies["d4"] = 293.7;
    frequencies["d#4"] = 311.1;
    frequencies["eb4"] = 311.1;
    frequencies["e4"] = 329.6;
    frequencies["f4"] = 349.2;
    frequencies["f#4"] = 370.0;
    frequencies["gb4"] = 370.0;
    frequencies["g4"] = 392.0;
    frequencies["g#4"] = 415.3;
    frequencies["ab4"] = 415.3;
    frequencies["a4"] = 440.0;
    frequencies["a#4"] = 466.2;
    frequencies["bb4"] = 466.2;
    frequencies["b4"] = 493.9;
    frequencies["c5"] = 523.3;
    frequencies["c#5"] = 554.4;
    frequencies["db5"] = 554.4;
    frequencies["d5"] = 587.3;
    frequencies["d#5"] = 622.3;
    frequencies["eb5"] = 622.3;
    frequencies["e5"] = 659.3;
    frequencies["f5"] = 698.5;
    frequencies["f#5"] = 740.0;
    frequencies["gb5"] = 740.0;
    frequencies["g5"] = 784.0;
    frequencies["g#5"] = 830.6;
    frequencies["ab5"] = 830.6;
    frequencies["a5"] = 880.0;
    frequencies["a#5"] = 932.3;
    frequencies["bb5"] = 932.3;
    frequencies["b5"] = 987.8;
    // -------------------- default octave
    frequencies["c"] = 523.3;
    frequencies["c#"] = 554.4;
    frequencies["db"] = 554.4;
    frequencies["d"] = 587.3;
    frequencies["d#"] = 622.3;
    frequencies["eb"] = 622.3;
    frequencies["e"] = 659.3;
    frequencies["f"] = 698.5;
    frequencies["f#"] = 740.0;
    frequencies["gb"] = 740.0;
    frequencies["g"] = 784.0;
    frequencies["g#"] = 830.6;
    frequencies["ab"] = 830.6;
    frequencies["a"] = 880.0;
    frequencies["a#"] = 932.3;
    frequencies["bb"] = 932.3;
    frequencies["b"] = 987.8;
    // --------------------
    frequencies["c6"] = 1047.0;
    frequencies["c#6"] = 1109.0;
    frequencies["db6"] = 1109.0;
    frequencies["d6"] = 1175.0;
    frequencies["d#6"] = 1245.0;
    frequencies["eb6"] = 1245.0;
    frequencies["e6"] = 1319.0;
    frequencies["f6"] = 1397.0;
    frequencies["f#6"] = 1480.0;
    frequencies["gb6"] = 1480.0;
    frequencies["g6"] = 1568.0;
    frequencies["g#6"] = 1661.0;
    frequencies["ab6"] = 1661.0;
    frequencies["a6"] = 1760.0;
    frequencies["a#6"] = 1865.0;
    frequencies["bb6"] = 1865.0;
    frequencies["b6"] = 1976.0;
    frequencies["c7"] = 2093.0;
    frequencies["c#7"] = 2217.0;
    frequencies["db7"] = 2217.0;
    frequencies["d7"] = 2349.0;
    frequencies["d#7"] = 2489.0;
    frequencies["eb7"] = 2489.0;
    frequencies["e7"] = 2637.0;
    frequencies["f7"] = 2794.0;
    frequencies["f#7"] = 2960.0;
    frequencies["gb7"] = 2960.0;
    frequencies["g7"] = 3136.0;
    frequencies["g#7"] = 3322.0;
    frequencies["ab7"] = 3322.0;
    frequencies["a7"] = 3520.0;
    frequencies["a#7"] = 3729.0;
    frequencies["bb7"] = 3729.0;
    frequencies["b7"] = 3951.0;
    frequencies["c8"] = 4186.0;
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

  static int getPivotPoint(List input, int begPoint, int endPoint) {
    int pivot = begPoint/2;
    int m = begPoint+1;
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
      string temp = (string)input[m];
      input[m] = input[n];
      input[n] = temp;
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
      string temp2 = (string)input[n];
      input[n] = input[pivot];
      input[pivot] = temp2;
      
    }
    return n;
  }

  static void QuickSort(List input, int beg, int end) {
    // In-place string sort
    if (end == beg) {
      return;
    } else {
      int pivot = getPivotPoint(input, beg, end);
      if (pivot > beg)
	QuickSort(input, beg, pivot-1);
      if (pivot < end)
	QuickSort(input, pivot+1, end);
    }
    return;
  }
  
  static List Sort(List list) {
    // in-place String sort
    QuickSort(list, 0, list.Count - 1);
    return list;
  }

  public static List getVoiceNames() {
    return Sort(voices.values());
  }

  public static List getVoices() {
    return voices.keys();
  }

  public static void setVoice(string name) {
    speech_name = name;
  }

  public static void setVoiceName(string name) {
    foreach (string key in voices.keys()) {
      if ((string)voices[key] == name) {
    speech_name = key;
    return;
      }
    }
    throw new Exception("voice name not found: " + name);
  }

  public static string getVoice() {
    return speech_name;
  }

  public static string getVoiceName() {
    return getVoiceName(speech_name);
  }
  
  public static string getVoiceName(string name) {
    foreach (string key in voices.keys()) {
      if (key == name) {
    return (string)voices[key];
      }
    }
    return name;
  }

  public static void speak(string text) {
    speak(text, 0); // not async, wait for exit
  }

  public static void speak(string text, bool async) {
    speak(text, bool_to_int(async)); // not async, wait for exit
  }

  static int bool_to_int(bool value) {
    if (value) return 1;
    return 0;
  }

  static bool int_to_bool(int value) {
    if (value == 0) return false;
    return true;
  }
  static bool byte_to_bool(byte value) {
    if (value == 0) return false;
    return true;
  }

  static int byte_to_int(byte value) {
    return (int)value;
  }

  public static void speak(string text, double async) {
    speak(text, (int)async); // not async, wait for exit
  }

  public static void speak(string text, int async) {
    Console.WriteLine(text);
    Process myProcess = new Process();

    // create a temporary file with the text to be spoken
    var textpath = System.IO.Path.GetTempFileName();
    using (TextWriter writer = File.CreateText(textpath)){
        writer.WriteLine(text);
    }


    try {
      myProcess.StartInfo.UseShellExecute = false;

      if (os_name == "nt") {
        string file = startup_path;
        file = Path.Combine(file, "bin");
        file = Path.Combine(file, "windows");
        file = Path.Combine(file, "eSpeak");
        myProcess.StartInfo.FileName = Path.Combine(file, "espeak.exe");
      } else {
        if (File.Exists("/usr/bin/speak")){
        // assumes espeak is in /usr/bin/ on macs
            myProcess.StartInfo.FileName = "speak";
        }
        else if (File.Exists("/usr/local/bin/speak")){
	// or look for espeak is in /usr/local/bin/ on macs
            myProcess.StartInfo.FileName = "speak";
        }
        else{
        // assumes in path
            myProcess.StartInfo.FileName = "espeak";
        }
      }
      myProcess.StartInfo.CreateNoWindow = true;
      myProcess.StartInfo.Arguments = ("-v \"" + speech_name + "\" -f " + textpath);
      myProcess.Start();

      if (async == 0)
        myProcess.WaitForExit();
#pragma warning disable 0168
    } catch (Exception e) {
#pragma warning restore 0168
      if (warn_missing_speak) {
        Console.WriteLine("WARNING: missing speak command");
        warn_missing_speak = false; // just once
      }
    }
  }

  public static string getNoteFromFrequency(int frequency) {
    return getNoteFromFrequency(System.Convert.ToDouble(frequency));
  }

  public static string getNoteFromFrequency(double frequency) {
    // Return closest note name based on a given frequency. 
    double diff = 100000;
    string diffNote = "a";
    foreach(string key in frequencies.keys()) {
      if (Math.Abs(((double)frequencies[key]) - frequency) < diff) {
    diff = (double) Math.Abs(((double)frequencies[key]) - frequency);
    diffNote = key;
      }
    }
    return diffNote.Substring(0, 1).ToUpper() + 
      diffNote.Substring(1, diffNote.Length - 1);
  }

  public static void playSong(List song) {
    robot.playSong(song);
  }

  public static void saveSong(List song, string filename, int append) {
    saveSong(song, filename, int_to_bool(append));
  }

  public static void saveSong(List song, string filename, bool append) {
    //  Writes a song list to a file. 
    System.IO.StreamWriter fp = new System.IO.StreamWriter(filename, append); 
    foreach (IList tup in song) {
      if (tup.Count == 2) {
	double f = System.Convert.ToDouble(tup[0]); 
	double d = System.Convert.ToDouble(tup[1]);
    fp.WriteLine("{0} {1}", getNoteFromFrequency(f), d);
      } else if (tup.Count == 3) {
	double f1 = System.Convert.ToDouble(tup[0]); 
	double f2 = System.Convert.ToDouble(tup[1]); 
	double d = System.Convert.ToDouble(tup[2]);
    fp.WriteLine("{0} {1} {2}", getNoteFromFrequency(f1),
             getNoteFromFrequency(f2), d);
    fp.Close();
      }
    }
  }

  public static List readSong(string filename) {
    // Read a song file. Returns a song list 
    List song = new List();
    System.IO.StreamReader songFile = new System.IO.StreamReader(filename);
    int lineNumber = 1;
    string line;
    while ((line = songFile.ReadLine()) != null) {
      Array notes = line.Split(';');
      foreach (string n in notes) {
    parseSongLine(song, n, lineNumber, filename);
      }
      lineNumber += 1;
    }
    songFile.Close();
    return song;
  }

  public static string song2text(List song) {
    // Given a song list, return a text string form 
    string text = "";
    foreach(IList tup in song) {
      if (tup.Count == 2) {
	double f = System.Convert.ToDouble(tup[0]); 
	double d = System.Convert.ToDouble(tup[1]);
    text += String.Format("{0} {1}; ", getNoteFromFrequency(f), d);
      } else if (tup.Count == 3) {
	double f1 = System.Convert.ToDouble(tup[0]); 
	double f2 = System.Convert.ToDouble(tup[1]); 
	double d = System.Convert.ToDouble(tup[2]);
    text += String.Format("{0} {1} {2}; ", 
                  getNoteFromFrequency(f1),
                  getNoteFromFrequency(f2), d);
      }
    }
    return text;
  }

  public static List makeSong(string text) {
    // Given a text string format of a song, return a song list 
    List song = new List();
    text = text.Replace('\n', ';');
    Array songData = text.Split(';');
    int lineNumber = 1;
    foreach (string line in songData) {
      parseSongLine(song, line, lineNumber, "string");
      lineNumber += 1;
    }
    return song;
  }

  static void parseSongLine(List song, string line, 
                int lineNumber, string filename) {
    line = line.Trim();
    Array lineList = line.Split(' ');
    // FIXME: remove duplicate spaces
    if (lineList.Length <= 1) {
      // blank line, skip
    } else if (((string)lineList.GetValue(0))[0] == '#') {
      // first word, first char is #, then skip comment
    } else if (lineList.Length == 2) {
      Array name1_dur = line.Split(' ');
      string name1 = (string)name1_dur.GetValue(0);
      string dur = (string)name1_dur.GetValue(1);
      song.append(Graphics.PyTuple(getFrequency(name1, lineNumber, line),
                   getDuration(dur, lineNumber, line)));
    } else if (lineList.Length == 3) {
      Array name1_name2_dur = line.Split(' ');
      string name1 = (string)name1_name2_dur.GetValue(0);
      string name2 = (string)name1_name2_dur.GetValue(1);
      string dur = (string)name1_name2_dur.GetValue(2);
      song.append( Graphics.PyTuple(getFrequency(name1, lineNumber, line),
                    getFrequency(name2, lineNumber, line),
                    getDuration(dur, lineNumber, line)) );
    } else {
      throw new Exception(String.Format("song format error in '{0}' at line {1}: {2}", filename, lineNumber, line));
    }
  }

#pragma warning disable 0168
  static double getFrequency(string s, int line, string text) {
    //Takes a string that is a note name, or a frequency. Returns
    try {
      return (double)frequencies[s.ToLower()];
    } catch (Exception e1) {
      try {
    return Double.Parse(s);
      } catch (Exception e2) {
    throw new Exception(String.Format("invalid note name/frequency '{0}' on line {1}: {2}", s, line, text));
      }
    }
  }
#pragma warning restore 0168

  static double getDuration(string v, int line, string text) {
    // Takes a string that is a fraction, or a number. Returns whole
    // note portion as float. 
    if (v.Contains("/")) {
      try {
    Array numerator_denominator = v.Split('/');
    double numerator = Double.Parse((string)numerator_denominator.GetValue(0));
    double denominator = Double.Parse((string)numerator_denominator.GetValue(1));
    return numerator/denominator;
#pragma warning disable 0168
      } catch (Exception e) {
#pragma warning restore 0168
    throw new Exception(
       String.Format("invalid duration value '{0}' on line {1}: {2}",
             v, line, text));
      }
    } else {
      return Double.Parse(v);
    }
  }
}
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
using System.IO.Ports;
using System.Threading;
using IronPython.Runtime; // List
using IronRuby.Builtins; // RubyArray
using System.Collections.Generic; // IList
using System.Collections; // IEnumerator

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
  public static Robot robot;
  static string dialogResponse = null;
  static string REVISION = "$Revision: $";
  static string startup_path = null;
  static string os_name = null;
  static string speech_name = "default";
  static bool warn_missing_speak = true;
  static PythonDictionary voices = new PythonDictionary();

  public class MyTexView : Gtk.TextView  {
	public string MyString;
  }

  // Functional Interface

  public static void init() {
    initialize(null);
  }

  public static void initialize() {
    initialize(null);
  }

  public static void init(string port, int baud=38400) {
    initialize(port, baud);
  }

  public static void initialize(string port, int baud=38400) {
	bool need_port = true;
	if (Myro.robot is Scribbler) {
	  if (((Scribbler)(Myro.robot)).serial is SerialPort) {
		SerialPort serial = (((Scribbler)(Myro.robot)).serial as SerialPort);
		if (serial.IsOpen) {
		  if (port == null) 
			need_port = false;
		  else if (serial.PortName == port && serial.BaudRate == baud) {
			need_port = false;
		  } else {
			// It exists, but wrong port/baud, so close it:
			serial.Close(); // and need_port
          }
		} else { // already closed
           if ((serial.PortName == port || port == null) && serial.BaudRate == baud) {
            need_port = false;
            serial.Open();
          } else {
            need_port = true;
          }
        }
	  } // not a serial port
	} // not a scribbler
	if (need_port) {
	  if (port == null) {
		port = (string)ask("Port");
	  }
	  robot = new Scribbler(port, baud);
	} else {
	  ((Scribbler)robot).setup();
	}
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

  public static void forward(double power=1) {
	robot.forward(power);
  }
  
  public static void forward(double power, double time) {
	robot.forward(power, time);
  }
  
  public static void translate(double power=1) {
	robot.translate(power);
  }
  
  public static void translate(double power, double time) {
	robot.translate(power, time);
  }
  
  public static void rotate(double power=1) {
	robot.rotate(power);
  }

  public static void rotate(double power, double time) {
	robot.rotate(power, time);
  }

  public static void backward(double power=1) {
	robot.backward(power);
  }

  public static void backward(double power, double time) {
	robot.backward(power, time);
  }

  public static void stop() {
	robot.stop();
  }
  
  public static void move(double translate, double rotate) {
	robot.move(translate, rotate);
  }
  
  public static void turnLeft(double power=1) {
	robot.turnLeft(power);
  }

  public static void turnLeft(double power, double time) {
	robot.turnLeft(power, time);
  }

  public static void turnRight(double power=1) {
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
	Graphics._Window win = Graphics.makeWindow(title,
		picture.width, picture.height);
	picture.draw(win);
  }

  public static void setup() {
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

  public static PythonTuple pickAColor() {
    ManualResetEvent ev = new ManualResetEvent(false);
    PythonTuple retval = null;
    Gtk.Application.Invoke(delegate {
        Gtk.ColorSelectionDialog fc = new Gtk.ColorSelectionDialog("Select a color");
        fc.ShowAll();
        if (fc.Run() == (int)(Gtk.ResponseType.Ok)) {
           retval = Graphics.PyTuple((int)Math.Round(((double)((int)fc.ColorSelection.CurrentColor.Red))/Math.Pow(2,16) * 255.0),
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

  public static object ask(object question) {
    return ask(question, "Information Request");
  }

  public static object ask(object question, string title) {
    ManualResetEvent ev = new ManualResetEvent(false);
    object retval = null;
    Gtk.Entry myentry = null;
    PythonDictionary responses = new PythonDictionary();
    Gtk.Application.Invoke(delegate {
        Gtk.MessageDialog fc = new Gtk.MessageDialog(null,
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

  public class Randomizer {
	int _seed; 
	Random _random = new Random();
	
	public Randomizer(int seed=0) {
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
  public static Randomizer Random = new Randomizer(); 

  public class Robot {

	public Object myLock = new Object();

    public virtual void beep(double duration, double frequency, double frequency2) {
	  // Override in subclassed robots
	}

    public virtual void beep(double duration, double frequency) {
	  // Override in subclassed robots
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

    public virtual void move(double translate, double rotate) {
	  // Override in subclassed robots
	}

	public void stop() {
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

/*
def beep(seconds, freq, async=False):
    if ser.inWaiting():
        ser.write("CS")
        ser.read(ser.inWaiting())
    mseconds = seconds * 1000
    ser.write("B" +
              chr(int(mseconds / 255)) +
              chr(int(mseconds % 255)) +
              chr(int(freq / 255)) +
              chr(int(freq % 255)))
    ser.read(5)
    if not async:
        time.sleep(seconds)

def motors(left, right):
    if ser.inWaiting():
        ser.write("CS")
        ser.read(ser.inWaiting())
    ser.write("V" +
              chr(int(left * 20.0)) +
              chr(int(right * 20.0)))
    ser.read(3)

*/

  [Serializable()]
  public class Scribbler: Robot {
	public SerialPort serial;
    double [] _fudge  = new double[4];
    double [] _oldFudge = new double[4];
	public string dongle;
	public int volume;
	public string startsong;
    public byte [] color_header = null;
    public byte [] gray_header = null;
	public byte emitters = 0x1 | 0x2 | 0x4;

	private double _lastTranslate;
	private double _lastRotate;
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
	//static int	CAM_VER=0x0B;
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

	public Scribbler(string port, int baud) {
	  serial = new SerialPort(port, baud);
	  serial.ReadTimeout = 1000; // milliseconds
	  serial.WriteTimeout = 1000; // milliseconds
	  try {
		serial.Open();
	  } catch {
		Console.WriteLine(String.Format("ERROR: unable to open '{0}'", 
				port));
		return;
	  }
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

    byte [] GetBytes(byte value, int bytes=1) {
	  byte [] retval = null;
	  lock(myLock) {
		write_packet(value);
		read(Scribbler.PACKET_LENGTH); // read the echo
		retval = read(bytes);
	  }
	  return retval;
	}

    List GetWord(byte value, int bytes=1) {
	  List retval = new List();
	  lock(myLock) {
		write_packet(value);
		read(Scribbler.PACKET_LENGTH); // read the echo
		byte [] retvalBytes = read(bytes);
		for (int p = 0; p < retvalBytes.Length; p += 2) {
		  retval.append(retvalBytes[p] << 8 | retvalBytes[p + 1]);
		}
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
		return (int)(_lastSensors[10]);
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
				  "stall",(int)_lastSensors[10]);
			} else {
			  return dict(
				  "light", list(
					  (int)(_lastSensors[2] << 8 |_lastSensors[3]), 
					  (int)(_lastSensors[4] << 8 |_lastSensors[5]), 
					  (int)(_lastSensors[6] << 8 |_lastSensors[7])),
				  "ir", list((int)_lastSensors[0],(int)_lastSensors[1]), 
				  "line", list((int)_lastSensors[8],(int)_lastSensors[9]), 
				  "stall", (int)_lastSensors[10],
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
	  if (Contains(direction, "fluke-forward", 1)) {
		direction = 1;
	  } else if (Contains(direction, "scribbler-forward", 0)) {
		direction = 0;
	  } else {
		throw new Exception("unknown direction: should be 'fluke-forward' or 'scribbler-forward'");
	  }
	  write(Scribbler.SET_FORWARDNESS);
	  write((byte)direction);
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
	  lock(myLock) {
		write_packet(values);
		read(Scribbler.PACKET_LENGTH); // read echo
		_lastSensors = read(11); // single bit sensors
		/* 
		if (requestStop) {
		  requestStop = false;
		  stop();
		  lock.Release();
		} 
		*/
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
		  setLEDBack((double)value);
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

	public override PythonDictionary getInfo() {
	  PythonDictionary retDict = new PythonDictionary();
	  int old = serial.ReadTimeout; // milliseconds
	  string retval;
	  // serial.setTimeout(4)
	  serial.ReadTimeout = 4000; // milliseconds
      
	  flush();
	  // have to do this twice since sometime the first echo isn't
	  // echoed correctly (spaces) from the scribbler
	  write_packet(Scribbler.GET_INFO, 32, 32, 32, 32, 32, 32, 32, 32);
	  try {
		retval = serial.ReadLine();
	  } catch {
		serial.ReadTimeout = old;
		return retDict;
	  }
	  //#print "Got", retval

	  Thread.Sleep(100); 
	  //time.sleep(.1)
        
	  write_packet(Scribbler.GET_INFO, 32, 32, 32, 32, 32, 32, 32, 32);
	  try {
		retval = serial.ReadLine();
	  } catch {
		serial.ReadTimeout = old;
		return retDict;
	  }
	  //#print "Got", retval
        
	  //# remove echoes
	  if (retval.Length == 0) {
		serial.ReadTimeout = old;
		return retDict;
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
	  serial.ReadTimeout = old;
	  return retDict;
	}
	
    public override void move(double translate, double rotate) {
	  _lastTranslate = translate;
	  _lastRotate = rotate;
	  adjustSpeed();
	}

    public void adjustSpeed() {
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
	  lock(myLock) {
		set_speaker((int)frequency, (int)(duration * 1000));
		Thread.Sleep((int)(duration * 1000));
		read(Scribbler.PACKET_LENGTH + 11);
	  }
	}

    public override void beep(double duration, double frequency, double frequency2) {
	  lock(myLock) {
		set_speaker_2((int)frequency, (int)frequency2, (int)(duration * 1000));
		Thread.Sleep((int)(duration * 1000));
		read(Scribbler.PACKET_LENGTH + 11);
	  }
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
	  double retval = read_2byte() / 20.9813;
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
	  serial.Write(buffer, 0, 1);
	}

	public void write(byte [] b) {
	  serial.Write(b, 0, b.Length);
	}

	public void write_2byte(int value) {
	  write((byte)((value >> 8) & 0xFF));
	  write((byte)(value & 0xFF));
	}

	public void write_packet(params byte [] data) {
	  byte [] buffer = new byte [Scribbler.PACKET_LENGTH]; 
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
  
	public void flush() {
	  serial.DiscardInBuffer();
	  serial.DiscardOutBuffer();
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
      lock(myLock) {
		write(Scribbler.GET_IMAGE);
		byte [] line = read(size); //BufferedRead(self.ser, size,
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
	  if (color_header == null) {
		write(Scribbler.GET_JPEG_COLOR_HEADER);
		color_header = read_jpeg_header();
	  }
	  write(GET_JPEG_COLOR_SCAN);
	  write((byte)mode);
	  byte [] jpeg = buffer_add(color_header, read_jpeg_scan());
	  return jpeg;
	}
	
	public byte [] grab_jpeg_gray(int mode) { // new gray, compressed
											  // (0=fast, 1=reg)
	  if (gray_header == null) {
		write(Scribbler.GET_JPEG_GRAY_HEADER);
		gray_header = read_jpeg_header();
	  }
	  write(Scribbler.GET_JPEG_GRAY_SCAN);
	  write((byte)mode);
	  byte [] jpeg = buffer_add(gray_header, read_jpeg_scan());
	  return jpeg;
	}
	
    public byte [] read_jpeg_scan() {
	  byte [] bytes = new byte[100000];
	  byte last_byte = 0;
	  int count = 0;
	  while (true) {
		serial.Read(bytes, count, 1);
		if ((last_byte == 0xff) && (bytes[count] == 0xd9)) {
		  // End-of-image marker
		  break;
		}
		last_byte = bytes[count];
		count++;
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
	  write(Scribbler.GET_RLE);
	  int size=(int)read_byte();
	  size = (size << 8) | read_byte();
	  byte [] buffer = read(size);
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
		  blobs[i * width * 3 + j + 0] = (byte)val; //red
		  blobs[i * width * 3 + j + 1] = (byte)val; //green
		  blobs[i * width * 3 + j + 2] = (byte)val; //blue
		  counter -= 1;
		}
	  }
	  return blobs;
	}
  	
	public byte [] grab_gray_array() {
	  int width = 128;
	  int height = 96;
	  int size= width * height; 
	  write(Scribbler.GET_WINDOW);
	  write((byte)0);
	  byte [] line = read(size);
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
	  write(Scribbler.GET_IR_MESSAGE);
	  int size = read_2byte();
	  return bytes2ints(read(size));
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

  public static Cairo.Color color_map(string name) {
    return Graphics.color_map(name);
  }

  public static Cairo.Color color_rgb(int r, int g, int b) {
    return Graphics.color_rgb(r, g, b);
  }

  public static Cairo.Color makeColor(int r, int g, int b) {
    return Graphics.color_rgb(r, g, b);
  }

  public static Cairo.Color color_rgb(PythonTuple rgb) {
    return Graphics.color_rgb((int)rgb[0], (int)rgb[1], (int)rgb[2]);
  }

  public static Cairo.Color makeColor(PythonTuple rgb) {
    return Graphics.color_rgb((int)rgb[0], (int)rgb[1], (int)rgb[2]);
  }

  public static List<string> color_names() {
    return Graphics.color_names();
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

  public static IEnumerable getPixels(Graphics.Picture picture) {
    return Graphics.getPixels(picture);
  }

  public static Graphics.Pixel getPixel(Graphics.Picture picture, int col, int row) {
    return picture.getPixel(col, row);
  }

  public static void setPixel(Graphics.Picture picture, int col, int row, Cairo.Color color) {
    picture.setPixel(col, row, color);
  }

  public static PythonTuple getRGB(Graphics.Pixel pixel) {
    return pixel.getRGB();
  }
  public static PythonTuple getRGBA(Graphics.Pixel pixel) {
    return pixel.getRGBA();
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

  public static Graphics.Picture makePicture(int x, int y) {
    return new Graphics.Picture(x, y);
  }
  public static Graphics.Picture makePicture(string filename) {
    return new Graphics.Picture(filename);
  }

  public static Graphics.Picture makePicture(Graphics._Window window) { //, string filename) {
    return Graphics.makePicture(window);
  }

  public static Graphics.Picture copyPicture(Graphics.Picture picture) { //, string filename) {
    return Graphics.copyPicture(picture);
  }

  public static Graphics._Window makeWindow(string title="Calico Graphics",
      int width=300,
      int height=300) {
    return Graphics.makeWindow(title, width, height);
  }

  public static Graphics._Window getWindow(string title) {
    return Graphics.getWindow(title);
  }

  public static int getHeight(Graphics._Window window) {
    return window.getHeight();
  }

  public static int getHeight(Graphics.Picture picture) {
    return picture.getHeight();
  }

  public static int getWidth(Graphics._Window window) {
    return window.getWidth();
  }

  public static int getWidth(Graphics.Picture picture) {
    return picture.getWidth();
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

  public static List doTogether(params Func<object> [] functions) {
    List retval = new List();
    List threads = new List();
    int position = 0; 
    // For each function, make a return list, and thread list
    foreach (Func<object> function in functions) {
      retval.append(null);
      threads.append(new Thread( 
           new ThreadStart(functionInvoke(function, retval, position))));
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
  }

  public static List getVoiceNames() {
    return voices.values();
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

  public static void speak(string text, double async) {
    speak(text, (int)async); // not async, wait for exit
  }

  public static void speak(string text, int async) {
    Console.WriteLine(text);
    Process myProcess = new Process();
    try {
      myProcess.StartInfo.UseShellExecute = false;
      if (os_name == "nt") {
	string file = startup_path;
	file = Path.Combine(file, "bin");
	file = Path.Combine(file, "windows");
	file = Path.Combine(file, "eSpeak");
	myProcess.StartInfo.FileName = Path.Combine(file, "espeak.exe");
      } else {
	// assumes in path
	myProcess.StartInfo.FileName = "espeak";
      }
      myProcess.StartInfo.CreateNoWindow = true;
      myProcess.StartInfo.Arguments = ("-v \"" + speech_name + "\" \"" +
				       text +
				       "\"");
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

}
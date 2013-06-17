using System;
using IronPython.Runtime;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using HidSharp;

/*
Command/Response Format:

All commands to the Finch are 9 bytes long:

     Byte 0:  is always 0x00.
     Byte 1:  is an Ascii character
              determines the command for the Finch to execute
  Bytes 2-7:  parametric data in binary
     Byte 8:  sequence number

All responses are 11 bytes long on Linux/ 9 bytes on Windows:

  Bytes 0-6:  sensor data in binary
     Byte 10:  sequence number (from byte 8 of the command)


Ascii Command Codes:

 - 'O' - control full-color LEDs (Orbs)
         Byte 2 = Red Intensity (0-255)
         Byte 3 = Green Intensity (0-255)
         Byte 4 = Blue Intensity (0-255)

 - 'M' - control the velocity of the motors
         Byte 2 = Left Wheel Direction (0:forward, 1:reverse)
         Byte 3 = Left Wheel Speed (0-255)
         Byte 4 = Right Wheel Direction (0:forward, 1:reverse)
         Byte 5 = Right Wheel Speed (0-255)
         *All zeroes will immediately turn the motors off

 - 'B' - sets the buzzer to chirp for a period of time
         Byte 2 = Duration in msec (MSB)
         Byte 3 = Duration in msec (LSB)
         Byte 4 = Frequency in Hz (MSB)
         Byte 5 = Frequency in Hz (LSB)
         *All zeroes will immediately turn the buzzer off

 - 'T' - gets the temperature
         returns
         Byte 2 = Temperature Value (0-255)
                   Celsius = (value-127) / 2.4 + 25.0

 - 'L' - gets the values from the two light sensors
         returns
         Byte 2 = Left Light Sensor (0-255)
         Byte 3 = Right Light Sensor (0-255)

 - 'A' - gets the accelerometer values of the X, Y, Z axis, and the tap/shake byte.
         returns
         Byte 2 = 153 (decimal)
         Byte 3 = X-axis (0-63)
         Byte 4 = Y-axis (0-63)
         Byte 5 = Z-axis (0-63)
                  if value is 0x00 to 0x1f (positive)
                     g-force = value * 1.5/32.0
                  if value is 0x20 to 0x3f (negative)
                     g-force = (value-64) * 1.5/32.0
         Byte 6 = Tap/Shaken flag (0-255)
                  If bit 7 (0x80) is a 1, then the Finch has been shaken since the last read
                  If bit 5 (0x20) is a 0, then the Finch has been tapped since the last read

 - 'I' - gets the values of the two obstacle sensors
         returns
         Byte 2 = Left Sensor (0:none or 1:obstacle present)
         Byte 3 =  Right Sensor (0:none or 1:obstacle present)

 - 'X' - set all motors and LEDs to off

 - 'R' - turns off the motor and
         has the Finch go back into color-cycling mode

 - 'z' - send a counter value showing the number of times command-z has been sent
         used for "keep-alive" or communication tests only
         returns
         Byte 2 = Counter (0-255)

 */

public class Finch: Myro.Robot
{
	public static HidStream stream = null;
	public static HidDevice robot = null;
	private HidDeviceLoader loader = null;
	private static Thread keepAliveThread = null;
	private int red = 0; // stores red LED setting
	private int green = 0; // stores green LED setting 
	private int blue = 0; // stores blue LED setting
	private string color = "#000000"; // stores the color of the LED
	private byte changeByte = 1; //counter
	private string name = "Finchy";
	public bool debug = false;
	public byte READSIZE = 9;
	public byte WRITESIZE = 9;
	public bool loop = true;
	public bool STARTBYTE = false;
	public static string OS = "Unknown";
	
	public Finch ()
	{
		if (System.IO.Directory.Exists("/Applications")) {
			OS = "Mac";
			READSIZE = 8;
			WRITESIZE = 8;
		} else if (System.Environment.OSVersion.Platform.ToString ().Contains ("Unix")) {
			READSIZE = 11;
			OS = "Linux";
		} else {
			OS = "Windows";
		}
		open ();
		flush ();
		setLED ("front", color); // Set the LED to the current values (doesn't change the LED color)
	}

	/// <summary>
	/// Open's connection to the Finch.
	/// </summary>
	public void open ()
	{
		try {
			loader = new HidDeviceLoader ();
			robot = loader.GetDeviceOrDefault (0x2354, 0x1111);
			stream = robot.Open ();
		} catch {
			Console.Error.WriteLine ("Could not find the finch");
		}
		if (robot != null) {
			if (keepAliveThread == null) {
				startKeepAlive ();
			} else {
				System.Console.WriteLine ("Restarting keep alive thread...");
				stopKeepAlive ();
				startKeepAlive ();
			}
		}
	}

	public void stopKeepAlive ()
	{
		loop = false;
		keepAliveThread.Abort ();
		keepAliveThread = null;
	}

	public void startKeepAlive ()
	{
		loop = true;
		keepAliveThread = new Thread (new ThreadStart (keepAlive)); // Start the thread that keeps Finch out of idle mode while program is running
		keepAliveThread.Start ();
	}

	public override string getName ()
	{
		return this.name;
	}
    
	public override void setName (string name)
	{
		this.name = name;
	}
    
	public override void setLEDFront (object value)
	{
		setLED ("front", value);
	}

	public override void setLEDBack (object value)
	{
		setLED ("back", value);
	}

	/// <summary>
	/// Sets the color of the LED
	/// </summary>
	/// <param name="position">Type "finch" for this parameter, it does not do anything but is necessary for inheritance</param>
	/// <param name="value">A string containing '#' followed by the hex value of the color.
	/// Example: "#00FF00"</param>
	public override void setLED (string position, object value)
	{
		if (robot != null) {
			if (position == "front" || position == "center" || position == "middle" || position == "all") {
				try {
					color = (string)value;
					red = Int32.Parse (color.Substring (1, 2), System.Globalization.NumberStyles.HexNumber);
					green = Int32.Parse (color.Substring (3, 2), System.Globalization.NumberStyles.HexNumber);
					blue = Int32.Parse (color.Substring (5, 2), System.Globalization.NumberStyles.HexNumber);
					byte[] report = makePacket((byte)'O', (byte)red, (byte)green, (byte)blue);
					WriteBytes (report);
				} catch {
					//Do nothing
				}
			} else {
				throw new Exception (String.Format ("no such LED: '{0}'", position));
			}
		}
	}

	/// <summary>
	/// Sets the speed of the two motors
	/// </summary>
	public override void adjustSpeed ()
	{
		int left = (int)(_lastTranslate * 255 - _lastRotate * 255);
		int right = (int)(_lastTranslate * 255 + _lastRotate * 255);

		if (robot != null) {
			int dir_left = 0;
			int dir_right = 0;
			if (left < 0) {
				dir_left = 1;
			}
			if (right < 0) {
				dir_right = 1;
			}
			int left1 = Math.Min (Math.Abs (left), 255);
			int right1 = Math.Min (Math.Abs (right), 255);
			byte[] report = makePacket((byte)'M', (byte)dir_left, (byte)left1, (byte)dir_right, (byte)right1);
			WriteBytes (report);
		}
	}

	/// <summary>
	/// Sets the robot into idle mode. Use this function rather than halt in most cases.
	/// </summary>
	public void idle ()
	{
		if (robot != null) {

			byte[] report = makePacket((byte)'R');
			WriteBytes (report);
			stream = null;
			robot = null;
			loader = null;
			keepAliveThread.Abort ();
		}
	}

	/// <summary>
	/// Turns off all motors, LEDs and disconnects.
	/// </summary>
	public void halt ()
	{
		if (robot != null) {
			byte[] report = makePacket((byte)'X');
			WriteBytes (report);
			stream = null;
			robot = null;
			loader = null;
			keepAliveThread.Abort ();
		}
	}

	public void wait (int ms)
	{
		System.Threading.Thread.Sleep (ms);
	}

	/// <summary>
	/// Beeps for a certain duration and frequency
	/// </summary>
	/// <param name ="duration">The duration of the beep in seconds</param>
	/// <param name="frequency">The frequency of the beep in Hz</param>
	public override void beep (double duration, double frequency)
	{
		if (robot != null) {
			int milliseconds = (int)(duration * 1000);
			int freq = (int)frequency;
			byte[] report = makePacket((byte)'B', (byte)((milliseconds & 0xff00) / 256), (byte)(milliseconds & 0x00ff), 
				  (byte)((freq & 0xff00) / 256), (byte)(freq & 0x00ff));
			WriteBytes (report);
			wait ((int)(milliseconds * 1.05));
		}
	}

	/// <summary>
	/// Get left and right light sensors
	/// </summary>
	/// <param name="position">"left" to return the left light sensor, "right" to return the right light sensor, and "both" to return both</param>
	/// <returns>An int, or a two element array containing the the left and right light sensor values (0 to 255)</returns>
	public override object getLight (params object[] position)
	{ // position can be: 0, "left"; 2, "right"; "all" "both"

	    // Linux:
	    // WriteBytes: [0, 76, 0, 0, 0, 0, 0, 0, 84]
	    // ReadBytes: [0, 76, 52, 62, 57, 19, 1, 1, 0, 0, 84]

	    // Windows:
	    // WriteBytes: [0, 76, 0, 0, 0, 0, 0, 0, 6]
	    // ReadBytes: [0, 51, 59, 63, 21, 1, 1, 0, 6]
		
		// Mac:
		// WriteBytes: [76, 0, 0, 0, 0, 0, 0, 67]
		// ReadBytes: [76, 13, 12, 0, 0, 0, 0, 0]

		
		if (robot != null) {
			List list = new List ();

			// Add the "changeByte" into the report to force every returning report to be slightly different - otherwise read won't work
			byte[] report = makePacket((byte)'L');
			byte[] readData = WriteBytesRead (report);

			// Collect and format data - note that the first element in readData is always value 0, so we're off by one when returning
			int[] returnData = new int[2];
			int START = 1;
			if (OS == "Linux") 
			    START = 2;

			returnData [0] = readData [START];
			returnData [1] = readData [START + 1];

			if (position.Length == 0)
				position = new object [] {"all"};

			foreach (object item in position) {
				if (item is string) {
					string temp = (string)item;
					if (temp == "left") {
						list.append (returnData [0]);
					} else if (temp == "right") {
						list.append (returnData [1]);
					} else if (temp == "both" || temp == "all") {
						list.append (returnData [0]);
						list.append (returnData [1]);
					} else {
						throw new Exception (String.Format ("no such light: '{0}'", item));
					}
				} else if (item is int) {
					int temp = (int)item;
					if (temp == 0) {
						list.append (returnData [0]);
					} else if (temp == 1) {
						list.append (returnData [1]);
					} else {
						throw new Exception (String.Format ("no such light: '{0}'", item));
					}
				}
			}
			if (list.Count == 0) {
				return null;
			} else if (list.Count == 1) {
				return list [0];
			} else {
				return list;
			}
		}
		return null;
	}


	public static byte [] makePacket(params byte [] report) {
		byte [] array = null;
		if (OS == "Mac") {
			array = new byte[report.Length];
			for(int i=0; i < report.Length; i++) {
				array[i] = report[i];
			}
		} else {
			array = new byte[report.Length + 1];
			array[0] = 0;
			for(int i=0; i < report.Length; i++) {
				array[i+1] = report[i];
			}
		}
		return array;
	}

	/// <summary>
	/// Returns the accelerations experienced by Finch's accelerometer. Values are -1.5g to 1.5g.
	/// </summary>
	/// <param name="position">"x" to return the x acceleration, "y" to return the y accerleration, "z" to return the z acceleration, and "all" to return all three.</param>
	/// <returns>A double, or an array of 3 doubles holding X, Y, and Z acceleration, null if the read failed.</returns>
	public override object getAcceleration (params object[] position)
	{

	    // Linux:
	    // WriteBytes: [0, 65, 0, 0, 0, 0, 0, 0, 150]
	    // ReadBytes: [0, 65, 153, 61, 63, 21, 133, 1, 0, 0, 150]

	    // Windows:
	    // WriteBytes: [0, 65, 0, 0, 0, 0, 0, 0, 4]
	    // ReadBytes: [0, 153, 63, 63, 21, 1, 1, 0, 4]
		
		// Mac:
		// WriteBytes: [65, 0, 0, 0, 0, 0, 0, 69]
		// ReadBytes: [65, 153, 61, 62, 20, 1, 1, 0]

		
		if (robot != null) {
			List list = new List ();
			byte[] report = makePacket((byte)'A');
			byte[] readData = WriteBytesRead (report);
			double[] returnData = new double[5];
			// Convert to g's. Acceleration data starts at element 2 of the array
			int START = 2; // position in readData
			if (OS == "Linux") {
			    START = 3;
			}
			for (int i = 0; i < 5; i++) {
				if (readData [i + START] > 31)
					returnData [i] = ((double)readData [i + START] - 64) * 1.5 / 32;
				else
					returnData [i] = ((double)readData [i + START]) * 1.5 / 32;
			}

			if (position.Length == 0)
				position = new object [] {"all"};

			foreach (object item in position) {
				if (item is string) {
					string temp = (string)item;
					if (temp == "x") {
						list.append (returnData [0]);
					} else if (temp == "y") {
						list.append (returnData [1]);
					} else if (temp == "z") {
						list.append (returnData [2]);
					} else if (temp == "shake") {
						list.append (returnData [3]);
					} else if (temp == "tap") {
						list.append (returnData [4]);
					} else if (temp == "all") {
						list.append (returnData [0]);
						list.append (returnData [1]);
						list.append (returnData [2]);
						list.append (returnData [3]);
						list.append (returnData [4]);
					} else {
						throw new Exception (String.Format ("no such acceleration: '{0}'", item));
					}
				} else {
					throw new Exception (String.Format ("no such acceleration: '{0}'", item));
				}
			}
			if (list.Count == 0) {
				return null;
			} else if (list.Count == 1) {
				return list [0];
			} else {
				return list;
			}
		}
		return null;
	}

	public override void flush ()
	{
		if (debug)
			System.Console.WriteLine ("stream.flush");
		lock (stream) {
			stream.Flush ();
			try {
				while (true) {
					stream.Read ();
				}
			} catch { // catch a timeout
				// Ok, done
			}
		}
	}

	public void WriteBytes (params byte [] bytes)
	{
		byte[] tbuffer = new byte[WRITESIZE];
		for (int i=0; i < bytes.Length; i++) {
			tbuffer [i] = bytes [i];
		}
		tbuffer [WRITESIZE - 1] = changeByte;
		changeByte++;
		if (debug)
			System.Console.WriteLine ("WriteBytes: " + arrayToString (tbuffer));
		lock (stream) {
			stream.Write (tbuffer);
		}
	}

	public byte [] WriteBytesRead (params byte [] bytes)
	{
		byte[] tbuffer = new byte[WRITESIZE];
		for (int i=0; i < bytes.Length; i++) {
			tbuffer [i] = bytes [i];
		}
		byte lastChangeByte = changeByte;
		tbuffer [WRITESIZE - 1] = changeByte;
		changeByte++;
		if (debug)
			System.Console.WriteLine ("WriteBytes: " + arrayToString (tbuffer));
		lock (stream) {
			stream.Write (tbuffer);
		}
		byte [] readData = ReadBytes (READSIZE);
		if (OS == "Mac") {
			while (readData[0] != bytes[0] && bytes[0] != (byte)'z') {
				readData = ReadBytes (READSIZE);
			}
		} else {
			while (readData[READSIZE - 1] != lastChangeByte && bytes[1] != (byte)'z') {
				readData = ReadBytes (READSIZE);
			}
		}
		return readData;
	}

	public byte [] ReadBytes (int size)
	{
		byte [] retval = new byte[size];
		int r = 0;
		while (r < size) {
			byte [] buffer = null;
			lock (stream) {
				buffer = stream.Read ();
			}
			for (int b = 0; b < buffer.Length; b++) {
				if (r < size)
					retval [r++] = buffer [b];
				else
					System.Console.Error.WriteLine ("Leftover byte on buffer: " + buffer [b]);
			}
		}
		if (debug)
			System.Console.WriteLine ("ReadBytes: " + arrayToString (retval));
		return retval;
	}

	private string arrayToString (byte [] array)
	{
		string retval = "";
		foreach (byte b in array) {
			if (retval != "")
				retval += ", ";
			retval += b.ToString ();
		}
		return "[" + retval + "]";
	}

	/// <summary>
	/// Gets the temperature measured by the Finch's small temperature sensor.
	/// </summary>
	/// <returns>A double, which is the ambient temperature in Celcius</returns>
	public override object getTemperature ()
	{
	    // Linux:
	    // WriteBytes: [0, 84, 0, 0, 0, 0, 0, 0, 196]
	    // ReadBytes: [0, 84, 125, 61, 63, 21, 133, 1, 0, 0, 196]

	    // Window:
	    // WriteBytes: [0, 84, 0, 0, 0, 0, 0, 0, 123]
	    // ReadBytes: [0, 126, 59, 62, 20, 1, 1, 0, 123]
		
		// Mac:
		// WriteBytes: [84, 0, 0, 0, 0, 0, 0, 70]
		// ReadBytes: [84, 126, 61, 62, 20, 1, 1, 0]
		
		if (robot != null) {
			byte[] report = makePacket((byte)'T');
			byte[] readData = WriteBytesRead (report);
			// Converts data to Celcius
			double returnData;
			if (OS == "Linux") {
			    returnData = ((double)readData [2] - 127) / 2.4 + 25;
			} else {
			    returnData = ((double)readData [1] - 127) / 2.4 + 25;
			}
			return returnData;
		}
		return 0;
	}

	/// <summary>
	/// Gets a two element boolean array representing the left (element 0) and right (element 1) obstacle sensors or a bool. True if an obstacle is detected, false otherwise. 
	/// </summary>
	/// <param name="position">"left" to return the left obstacle sensor, "right" to return the right obstacle sensor, "both" to return both</param>
	/// <returns>A bool or an array of bools containing Finch obstacle data</returns>
	public override object getObstacle (params object[] position)
	{
	    // Linux:
	    // WriteBytes: [0, 73, 0, 0, 0, 0, 0, 0, 237]
	    // ReadBytes: [0, 73, 0, 0, 63, 21, 133, 1, 0, 0, 237]

	    // Windows:
	    // WriteBytes: [0, 73, 0, 0, 0, 0, 0, 0, 5]
	    // ReadBytes: [0, 0, 0, 63, 21, 1, 1, 0, 5]
		
		// Mac:
		// WriteBytes: [73, 0, 0, 0, 0, 0, 0, 68]
		// ReadBytes: [73, 1, 1, 0, 0, 0, 0, 0]

		
		if (robot != null) {
			List list = new List ();

			byte[] report = makePacket((byte)'I');
			byte[] readData = WriteBytesRead (report);

			int START = 1;
			if (OS == "Linux")
			    START = 2;

			bool[] returnData = new bool[2];
			if (readData [START] == 1)
				returnData [0] = true;
			else
				returnData [0] = false;
			if (readData [START + 1] == 1)
				returnData [1] = true;
			else
				returnData [1] = false;

			if (position.Length == 0)
				position = new object [] {"all"};

			foreach (object item in position) {
				if (item is string) {
					string temp = (string)item;
					if (temp == "left") {
						list.append (returnData [0]);
					} else if (temp == "right") {
						list.append (returnData [1]);
					} else if (temp == "both" || temp == "all") {
						list.append (returnData [0]);
						list.append (returnData [1]);
					} else {
						throw new Exception (String.Format ("no such obstacle: '{0}'", item));
					}
				} else if (item is int) {
					int temp = (int)item;
					if (temp == 0) {
						list.append (returnData [0]);
					} else if (temp == 1) {
						list.append (returnData [1]);
					} else {
						throw new Exception (String.Format ("no such obstacle: '{0}'", item));
					}
				}
			}
			if (list.Count == 0) {
				return null;
			} else if (list.Count == 1) {
				return list [0];
			} else {
				return list;
			}
		}
		return null;
	}

	private void keepAlive ()
	{
		while (loop) {
			if (robot != null) {
				try {
					byte[] report = makePacket((byte)'z');
					WriteBytesRead (report);
				} catch (Exception e) {
					System.Console.Error.WriteLine ("warning in keepAlive, continuing...: " + e.Message);
				}
				wait (2000); // do this again in 2 seconds
			} else {
				System.Console.Error.WriteLine ("Looking for the Finch...");
			}
		}
		System.Console.WriteLine ("Done!");
	}
}


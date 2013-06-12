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

All responses are 8 bytes long:

  Bytes 0-6:  sensor data in binary
     Byte 7:  sequence number (from byte 8 of the command)


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
         Byte 0 = Temperature Value (0-255)
                   Celsius = (value-127) / 2.4 + 25.0

 - 'L' - gets the values from the two light sensors
         returns
         Byte 0 = Left Light Sensor (0-255)
         Byte 1 = Right Light Sensor (0-255)

 - 'A' - gets the accelerometer values of the X, Y, Z axis, and the tap/shake byte.
         returns
         Byte 0 = 153 (decimal)
         Byte 1 = X-axis (0-63)
         Byte 2 = Y-axis (0-63)
         Byte 3 = Z-axis (0-63)
                  if value is 0x00 to 0x1f (positive)
                     g-force = value * 1.5/32.0
                  if value is 0x20 to 0x3f (negative)
                     g-force = (value-64) * 1.5/32.0
         Byte 4 = Tap/Shaken flag (0-255)
                  If bit 7 (0x80) is a 1, then the Finch has been shaken since the last read
                  If bit 5 (0x20) is a 0, then the Finch has been tapped since the last read

 - 'I' - gets the values of the two obstacle sensors
         returns
         Byte 0 = Left Sensor (0:none or 1:obstacle present)
         Byte 1 =  Right Sensor (0:none or 1:obstacle present)

 - 'X' - set all motors and LEDs to off

 - 'R' - turns off the motor and
         has the Finch go back into color-cycling mode

 - 'z' - send a counter value showing the number of times command-z has been sent
         used for "keep-alive" or communication tests only
         returns
         Byte 0 = Counter (0-255)

 */

public class Finch: Myro.Robot
{
        private HidStream stream = null;
        private HidDevice robot = null;
        private HidDeviceLoader loader = null;
        private Thread keepAliveThread = null;
        private int red = 0; // stores red LED setting
        private int green = 0; // stores green LED setting 
        private int blue = 0; // stores blue LED setting
        private string color = "#000000"; // stores the color of the LED
        private byte changeByte = 1; //counter


	    public Finch() {
	        open();
		if (stream != null) {
		    stream.Flush();
		}
	    }

        /// <summary>
        /// Open's connection to the Finch.
        /// </summary>
        public void open()
        {
            try
            {
                loader = new HidDeviceLoader();
                robot = loader.GetDeviceOrDefault(0x2354, 0x1111);
                stream = robot.Open();
            }
            catch 
            {
                Console.Error.WriteLine("Could not find the finch");
            }
            if (robot != null && keepAliveThread == null)
            {
                keepAliveThread = new Thread(new ThreadStart(keepAlive)); // Start the thread that keeps Finch out of idle mode while program is running
                keepAliveThread.Start();
            }
        }

        /// <summary>
        /// Sets the color of the LED
        /// </summary>
        /// <param name="position">Type "finch" for this parameter, it does not do anything but is necessary for inheritance</param>
        /// <param name="value">A string containing '#' followed by the hex value of the color.
        /// Example: "#00FF00"</param>
        public override void setLED(string position, object value)
        {
            if (robot != null)
            {
		if (position == "front" || position == "center" || position == "middle" || position == "all") {
		    try {
			color = (string)value;
			red = Int32.Parse(color.Substring(1, 2), System.Globalization.NumberStyles.HexNumber);
			green = Int32.Parse(color.Substring(3, 2), System.Globalization.NumberStyles.HexNumber);
			blue = Int32.Parse(color.Substring(5, 2), System.Globalization.NumberStyles.HexNumber);
			byte[] report = { (byte)0, (byte)'O', (byte)red, (byte)green, (byte)blue };
			stream.Write(report);
		    } catch (Exception e) {
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
	        int left  = (int)(_lastTranslate * 255 - _lastRotate * 255);
	        int right = (int)(_lastTranslate * 255 + _lastRotate * 255);

            if (robot != null)
            {
                int dir_left = 0;
                int dir_right = 0;
                if (left < 0)
                {
                    dir_left = 1;
                }
                if (right < 0)
                {
                    dir_right = 1;
                }
                int left1 = Math.Min(Math.Abs(left), 255);
                int right1 = Math.Min(Math.Abs(right), 255);
                byte[] report = { (byte)0, (byte)'M', (byte)dir_left, (byte)left1, (byte)dir_right, (byte)right1 };
                stream.Write(report);
            }
        }

        /// <summary>
        /// Sets the robot into idle mode. Use this function rather than halt in most cases.
        /// </summary>
        public void idle()
        {
            if (robot != null)
            {

                byte[] report = { (byte)0, (byte)'R' };
                stream.Write(report);
                stream = null;
                robot = null;
                loader = null;
                keepAliveThread.Abort();
            }
        }

        /// <summary>
        /// Turns off all motors, LEDs and disconnects.
        /// </summary>
        public void halt()
        {
            if (robot != null)
            {
                byte[] report = { (byte)0, (byte)'X' };
                stream.Write(report);
                stream = null;
                robot = null;
                loader = null;
                keepAliveThread.Abort();
            }
        }

        public void wait(int ms)
        {
            System.Threading.Thread.Sleep(ms);
        }

        /// <summary>
        /// Beeps for a certain duration and frequency
        /// </summary>
        /// <param name ="duration">The duration of the beep in seconds</param>
        /// <param name="frequency">The frequency of the beep in Hz</param>
        public override void beep(double duration, double frequency)
        {
            if (robot != null)
            {
                int milliseconds = (int)duration * 1000;
                int freq = (int)frequency;
                byte[] report = { (byte)0, (byte)'B', (byte)((milliseconds&0xff00)/256), (byte)(milliseconds&0x00ff), 
                                (byte)((freq&0xff00)/256), (byte)(freq&0x00ff)};
                stream.Write(report);
                wait((int)(milliseconds * 1.05));
            }
        }

        /// <summary>
        /// Get left and right light sensors
        /// </summary>
        /// <param name="position">"left" to return the left light sensor, "right" to return the right light sensor, and "both" to return both</param>
        /// <returns>An int, or a two element array containing the the left and right light sensor values (0 to 255)</returns>
        public override object getLight(params object[] position)
        { // position can be: 0, "left"; 1, "middle", "center"; 2, "right"; "all"
            string temp = "";
            try
            {
                if (position != null)
                {
                    temp = (string)position[0];
                }
            }
            catch (Exception e)
            {
                //Do Nothing
            }

            if (robot != null)
            {
                // Add the "changeByte" into the report to force every returning report to be slightly different - otherwise read won't work
                byte[] report = { (byte)0, (byte)'L', 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, changeByte };
                stream.Write(report);
                byte[] readData = stream.Read();
                // Keep reading until you get back to the report that matches to the one you wrote (hopefully this loop isn't triggered, but just in case
                while (readData[8] != changeByte)
                {
                    stream.Write(report);
                    readData = stream.Read();
                }

                changeByte++;

                // Collect and format data - note that the first element in readData is always value 0, so we're off by one when returning
                int[] returnData = new int[2];
                returnData[0] = readData[1];
                returnData[1] = readData[2];
                if (temp == "left")
                {
                    return returnData[0];
                }
                else if(temp == "right")
                {
                    return returnData[1];
                }
                else if (temp == "both")
                {
                    return returnData;
                }
            }
            return null;
        }



        /// <summary>
        /// Returns the accelerations experienced by Finch's accelerometer. Values are -1.5g to 1.5g.
        /// </summary>
        /// <param name="position">"x" to return the x acceleration, "y" to return the y accerleration, "z" to return the z acceleration, and "all" to return all three.</param>
        /// <returns>A double, or an array of 3 doubles holding X, Y, and Z acceleration, null if the read failed.</returns>
        public override object getAcceleration(params object[] position)
        {
            if (robot != null)
            {
                string temp = "";
                try
                {
                    if (position != null)
                    {
                        temp = (string)position[0];
                    }
                }
                catch (Exception e)
                {
                    //Do Nothing
                }

                byte[] report = { (byte)0, (byte)'A', 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, changeByte };
                stream.Write(report);
                byte[] readData = stream.Read();
                // Keep reading until you get back to the report that matches to the one you wrote (hopefully this loop isn't triggered, but just in case
                while (readData[8] != changeByte)
                {
                    stream.Write(report);
                    readData = stream.Read();
                }
                changeByte++;
                double[] returnData = new double[3];
                // Convert to g's. Acceleration data starts at element 2 of the array
                for (int i = 2; i < 5; i++)
                {
                    if (readData[i] > 31)
                        returnData[i - 2] = ((double)readData[i] - 64) * 1.5 / 32;
                    else
                        returnData[i - 2] = ((double)readData[i]) * 1.5 / 32;
                }
                if (temp == "x")
                {
                    return returnData[0];
                }
                else if (temp == "y")
                {
                    return returnData[1];
                }
                else if (temp == "z")
                {
                    return returnData[2];
                }
                else if (temp == "all")
                {
                    return returnData;
                }
            }
            return null;
        }

	public override void flush() {
	    stream.Flush();
	}

	public byte [] ReadBytes(int size) {
	    byte [] retval = new byte[size];
	    stream.Read(retval);
	    return retval;
	}

	public void WriteBytes(params byte [] bytes) {
	    foreach(byte b in bytes) {
		System.Console.Write(b);
		System.Console.Write(", ");
	    }
	    System.Console.Write("\n");
	    stream.Write(bytes);
	}

        /// <summary>
        /// Gets the temperature measured by the Finch's small temperature sensor.
        /// </summary>
        /// <returns>A double, which is the ambient temperature in Celcius</returns>
        public override object getTemperature()
        {
            if (robot != null)
            {
                byte[] report = {(byte)0, (byte)'T', 
				 0x00, 0x00, 
				 0x00, 0x00, 
				 0x00, changeByte };
                WriteBytes(report);
                byte[] readData = ReadBytes(9);
		changeByte++;
		return readData;
                // Keep reading until you get back to the report that matches to the one you wrote (hopefully this loop isn't triggered, but just in case
		/*
		while (readData[8] != changeByte)
		{
		    stream.Write(report);
		    readData = ReadBytes(11);
		    System.Console.WriteLine(readData.Length);
		}
		System.Console.WriteLine("changeByte: " + changeByte + " read: " + readData[8]);
		changeByte++;
		System.Console.WriteLine(readData.Length);
                // Converts data to Celcius
                double returnData = ((double)readData[1] - 127) / 2.4 + 25;
                return returnData;
		*/
            }
            return 0;
        }

        /// <summary>
        /// Gets a two element boolean array representing the left (element 0) and right (element 1) obstacle sensors or a bool. True if an obstacle is detected, false otherwise. 
        /// </summary>
        /// <param name="position">"left" to return the left obstacle sensor, "right" to return the right obstacle sensor, "both" to return both</param>
        /// <returns>A bool or an array of bools containing Finch obstacle data</returns>
        public override object getObstacle(params object[] position)
        {
            if (robot != null)
            {
		List list = new List();

                byte[] report = { (byte)0, (byte)'I', 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, changeByte };
                stream.Write(report);
                byte[] readData = stream.Read();
                // Keep reading until you get back to the report that matches to the one you wrote (hopefully this loop isn't triggered, but just in case
                while (readData[8] != changeByte)
                {
                    stream.Write(report);
                    readData = stream.Read();
                }
                changeByte++;

                bool[] returnData = new bool[2];
                if (readData[1] == 1)
                    returnData[0] = true;
                else
                    returnData[0] = false;
                if (readData[2] == 1)
                    returnData[1] = true;
                else
                    returnData[1] = false;

		foreach (object item in position) {
		    if (item is string) {
			string temp = (string)item;
			if (temp == "left") {
			    list.append(returnData[0]);
			} else if (temp == "right") {
			    list.append(returnData[1]);
			} else if (temp == "both") {
			    list.append(returnData);
			} else {
			    throw new Exception (String.Format ("no such light: '{0}'", item));
			}
		    } else if (item is int) {
			int temp = (int)item;
			if (temp == 0) {
			    list.append(returnData[0]);
			} else if (temp == 1) {
			    list.append(returnData[1]);
			} else {
			    throw new Exception (String.Format ("no such light: '{0}'", item));
			}
		    }
		}
		if (list.Count == 0) {
		    return null;
		} else if (list.Count == 1) {
		    return list[0];
		} else {
		    return list;
		}
	    }
	    return null;
	}


        private void keepAlive()
        {
            while (true)
            {
                if (robot != null)
                {
		    try {
			setLED("front", color); // Set the LED to the current values (doesn't change the LED color)
		    } catch (Exception e) {
			System.Console.Error.WriteLine("error in setLED: " + e.Message);
		    }
                    wait(2000); // do this again in 2 seconds
                }
            }
        }

}


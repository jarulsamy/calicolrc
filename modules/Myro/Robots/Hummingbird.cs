using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using HidSharp;

public class Hummingbird : Finch
{

    public override void initialize_birdbrain() {
	deviceID = 0x2222;
	robotType = "Hummingbird";
    }

    public override void post_initialize_birdbrain() {
    }
	
    /// <summary>
    /// Sets the color of an LED port
    /// </summary>
    /// <param name="position">"t1" for tri colored port 1, "t2" for tri colored port 2, "s1" for single colored port 1,
    /// "s2" for single colored port 2, "s3" for single colored port 3, "s4" for single colored port 4,</param>
    /// <param name="value">A string containing '#' followed by the hex value of the color for a tri colored port, or an int
    /// containing the intensity. Example(tri): "#00FF00", Example(single): 200</param>
    public override void setLED(string position, object value)
    {
        if (robot != null)
        {
            try
            {
                if (position == "t1")
                {
                    color = value.ToString();
                    int red = Int32.Parse(color.Substring(1, 2), System.Globalization.NumberStyles.HexNumber);
                    int green = Int32.Parse(color.Substring(3, 2), System.Globalization.NumberStyles.HexNumber);
                    int blue = Int32.Parse(color.Substring(5, 2), System.Globalization.NumberStyles.HexNumber);
                    byte[] report = makePacket((byte)'O', (byte)48, (byte)red, (byte)green, (byte)blue );
                    WriteBytes(report);
                }
                else if (position == "t2")
                {
                    string color = value.ToString();
                    int red = Int32.Parse(color.Substring(1, 2), System.Globalization.NumberStyles.HexNumber);
                    int green = Int32.Parse(color.Substring(3, 2), System.Globalization.NumberStyles.HexNumber);
                    int blue = Int32.Parse(color.Substring(5, 2), System.Globalization.NumberStyles.HexNumber);
                    byte[] report = makePacket((byte)'O', (byte)49, (byte)red, (byte)green, (byte)blue );
                    WriteBytes(report);
                }
                else if (position == "s1")
                {
                    int intensity = (int)value;
                    byte[] report = makePacket((byte)'L', (byte)48, (byte)intensity );
                    WriteBytes(report);
                }
                else if (position == "s2")
                {
                    int intensity = (int)value;
                    byte[] report = makePacket((byte)'L', (byte)49, (byte)intensity );
                    WriteBytes(report);
                }
                else if (position == "s3")
                {
                    int intensity = (int)value;
                    byte[] report = makePacket((byte)'L', (byte)50, (byte)intensity );
                    WriteBytes(report);
                }
                else if (position == "s4")
                {
                    int intensity = (int)value;
                    byte[] report = makePacket((byte)'L', (byte)51, (byte)intensity );
                    WriteBytes(report);
                }
            }
            catch 
            {
                Console.Error.WriteLine("Bad input");
            }
        }
    }

    /// <summary>
    /// Sets the speed of the two motors
    /// </summary>
    public override void adjustSpeed()
    {
        int left = (int)(_lastTranslate * 255 - _lastRotate * 255);
        int right = (int)(_lastTranslate * 255 + _lastRotate * 255);

        if (robot != null)
        {
            int dir_left = 48;
            int dir_right = 48;
            if (left < 0)
            {
                dir_left = 49;
            }
            if (right < 0)
            {
                dir_right = 49;
            }
            int left1 = Math.Min(Math.Abs(left), 255);
            int right1 = Math.Min(Math.Abs(right), 255);
            byte[] report = makePacket((byte)'M', (byte)48, (byte)dir_left, (byte)left1);
            WriteBytes(report);
            byte[] report1 = makePacket((byte)'M', (byte)49, (byte)dir_right, (byte)right1 );
            WriteBytes(report1);
        }
    }

    public override void keepAliveFunction() {
	setLED("t1", color); // Set the LED to the current values (doesn't change the LED color)
    }

    /// <summary>
    /// Sets the intensity of a vibration motor
    /// </summary>
    /// <param name="volume">"v1" followed by the intensity for vibration port 1, "v2" followed by the intensity for vibration port 2
    /// Example: "v1255" (sets vibration port 1 to 255), "v20" (sets vibration port 2 to 0), "v225" (sets vibration port 2 to 25)</param>
    public override void setVolume(object volume)
    {
        if (robot != null)
        {
	    try	{
		string vib = volume.ToString();
		int port = 48;
		if(vib.Substring(0,2) == "v2")
		    {
			port++;
		    }
		int intensity = Convert.ToInt32(vib.Substring(2, vib.Length - 2));
		byte[] report = makePacket((byte)'V', (byte)port, (byte)intensity );
		WriteBytes(report);
	    } catch {
		Console.Error.WriteLine("Bad input");
	    }
	}
    }

    /// <summary>
    /// Gets sensor readings
    /// <param name="sensor">The port of the sensor, ranges from 1-4. Input as a string, so port 1 is "1". To get all values, pass "all" for this parameter</param>
    /// <returns>The raw sensor value in volts.</returns>
    /// </summary>
    public override object get(string sensor)
    {
        if (robot != null)
        {

            byte[] report = makePacket((byte)'G', (byte)51);
            byte[] readData = WriteBytesRead(report);

            double[] returnData = new double[4];

	    int START = 1; // Windows
	    if (OS == "Linux") 
		START = 2;

            for (int x = 0; x < 4; x++)
            {
                returnData[x] = readData[x+START];
            }
            if (sensor == "1")
            {
                return returnData[0];
            }
            else if (sensor == "2")
            {
                return returnData[1];
            }
            else if (sensor == "3")
            {
                return returnData[2];
            }
            else if (sensor == "4")
            {
                return returnData[3];
            }
            else if (sensor == "all")
            {
                return returnData;
            } else {
		throw new Exception(String.Format("get: invalid sensor name: '{0}'", sensor));

	    }
        }
        return null;
    }
    /// <summary>
    /// Gets sensor readings
    /// <param name="sensor">The port of the sensor, ranges from 1-4. Input as a string, so port 1 is "1".</param>
    /// <param name="value">The name of the sensor, either "temperature", "distance", "sound", "rotary", or "light".</param>
    /// <returns>The reading of the given sensor</returns>
    /// </summary>
    public override object get(string sensor, params object[] position)
    {
        if (robot != null)
        {
            string temp = "";
            try
            {
                temp = position[0].ToString();
            }
            catch 
            {
                Console.Error.WriteLine("Incorrect input");
            }
            double returnData = (double)get(sensor);
            if (temp == "temperature")
            {
                return (returnData - 127) / 2.4 + 25;
            }
            else if (temp == "distance" && returnData < 23)
            {
                return 80.0;
            }
            else if (temp == "distance")
            {
                double dist = 206.76903754529479 - 9.3402257299483011 * returnData;
                dist += 0.19133513242939543 * Math.Pow(returnData, 2);
                dist -= 0.0019720997497951645 * Math.Pow(returnData, 3);
                dist += 9.9382154479167215 * Math.Pow(10, -6) * Math.Pow(returnData, 4);
                dist -= 1.9442731496914311 * Math.Pow(10, -8) * Math.Pow(returnData, 5);
                return dist;
            }
            else
            {
                return returnData;
            }

        }
        return null;
    }

    /// <summary>
    /// Sets the angle of the servo. If idle is called immediatly after this, be sure to wait two seconds to allow the servo to adjust.
    /// <param name="id">the port of the servo, ranges from 1-4.</param>
    /// <param name="value">The angle to set the specified servo to, ranges from 0 - 180.</param>
    /// </summary>
    public override void servo(int id, int value)
    {
        if (robot != null && id >= 1 && id <= 4)
        {
            int angle = (value * 234 / 180) % 235;
            byte[] buffer = makePacket((byte)'S', (byte)(47 + id), (byte)angle );
            WriteBytes(buffer);
        }
    }
}

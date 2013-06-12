using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using HidSharp;

public class Hummingbird : Myro.Robot
{
    private HidStream stream = null;
    private HidDevice robot = null;
    private HidDeviceLoader loader = null;
    private Thread keepAliveThread = null;
    private string tri_color1 = "#000000";
    private byte changeByte = 1; //counter


    public Hummingbird()
    {
        open();
    }

    /// <summary>
    /// Open's connection to the Hummingbird.
    /// </summary>
    public void open()
    {
        try
        {
            loader = new HidDeviceLoader();
            robot = loader.GetDeviceOrDefault(0x2354, 0x2222);
            stream = robot.Open();
        }
        catch
        {
            Console.Error.WriteLine("Could not find the hummingbird");
        }
        if (robot != null && keepAliveThread == null)
        {
            keepAliveThread = new Thread(new ThreadStart(keepAlive)); // Start the thread that keeps Finch out of idle mode while program is running
            keepAliveThread.Start();
        }
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
        if (stream != null)
        {
            try
            {
                if (position == "t1")
                {
                    tri_color1 = (string)value;
                    int red = Int32.Parse(tri_color1.Substring(1, 2), System.Globalization.NumberStyles.HexNumber);
                    int green = Int32.Parse(tri_color1.Substring(3, 2), System.Globalization.NumberStyles.HexNumber);
                    int blue = Int32.Parse(tri_color1.Substring(5, 2), System.Globalization.NumberStyles.HexNumber);
                    byte[] report = { (byte)0, (byte)'O', (byte)48, (byte)red, (byte)green, (byte)blue };
                    stream.Write(report);
                }
                else if (position == "t2")
                {
                    string color = (string)value;
                    int red = Int32.Parse(color.Substring(1, 2), System.Globalization.NumberStyles.HexNumber);
                    int green = Int32.Parse(color.Substring(3, 2), System.Globalization.NumberStyles.HexNumber);
                    int blue = Int32.Parse(color.Substring(5, 2), System.Globalization.NumberStyles.HexNumber);
                    byte[] report = { (byte)0, (byte)'O', (byte)49, (byte)red, (byte)green, (byte)blue };
                    stream.Write(report);
                }
                else if (position == "s1")
                {
                    int intensity = (int)value;
                    byte[] report = { (byte)0, (byte)'L', (byte)48, (byte)intensity };
                    stream.Write(report);
                }
                else if (position == "s2")
                {
                    int intensity = (int)value;
                    byte[] report = { (byte)0, (byte)'L', (byte)49, (byte)intensity };
                    stream.Write(report);
                }
                else if (position == "s3")
                {
                    int intensity = (int)value;
                    byte[] report = { (byte)0, (byte)'L', (byte)50, (byte)intensity };
                    stream.Write(report);
                }
                else if (position == "s4")
                {
                    int intensity = (int)value;
                    byte[] report = { (byte)0, (byte)'L', (byte)51, (byte)intensity };
                    stream.Write(report);
                }
            }
            catch (Exception e)
            {
                Console.Write("Bad input");
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

        if (stream != null)
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
            byte[] report = { (byte)0, (byte)'M', (byte)48, (byte)dir_left, (byte)left1};
            stream.Write(report);
            byte[] report1 = { (byte)0, (byte)'M', (byte)49, (byte)dir_right, (byte)right1 };
            stream.Write(report1);
        }
    }

    private void keepAlive()
    {
        while (true)
        {
            if (stream != null)
            {
                setLED("t1", tri_color1); // Set the LED to the current values (doesn't change the LED color)
                wait(2000); // do this again in 2 seconds
            }
        }
    }
    /// <summary>
    /// Sets the intensity of a vibration motor
    /// </summary>
    /// <param name="volume">"v1" followed by the intensity for vibration port 1, "v2" followed by the intensity for vibration port 2
    /// Example: "v1255" (sets vibration port 1 to 255), "v20" (sets vibration port 2 to 0), "v225" (sets vibration port 2 to 25)</param>
    public override void setVolume(object volume)
    {
        try
        {
            string vib = (string)volume;
            int port = 48;
            if(vib.Substring(0,2) == "v2")
            {
                port++;
            }
            int intensity = Convert.ToInt32(vib.Substring(2, vib.Length - 2));
            Console.Write(intensity);
            byte[] report = { (byte)0, (byte)'V', (byte)port, (byte)intensity };
            stream.Write(report);
        }
        catch (Exception e)
        {
            Console.Write("Bad input");
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

            byte[] report = { (byte)0, (byte)'G', (byte)51, 0x00, 0x00, 0x00, 0x00, 0x00, changeByte };
            stream.Write(report);
            byte[] readData = stream.Read();
            // Keep reading until you get back to the report that matches to the one you wrote (hopefully this loop isn't triggered, but just in case
            while (readData[8] != changeByte)
            {
                stream.Write(report);
                readData = stream.Read();
            }
            changeByte++;
            double[] returnData = new double[4];
            for (int x = 0; x < 4; x++)
            {
                returnData[x] = readData[x+1];
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
        if (stream != null)
        {
            string temp = "";
            try
            {
                temp = (string)position[0];
            }
            catch (Exception e)
            {
                Console.Write("Incorrect input");
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
        if (stream != null && id >= 1 && id <= 4)
        {
            int angle = (value * 234 / 180) % 235;
            byte[] buffer = { (byte)0, (byte)'S', (byte)(47 + id), (byte)angle };
            stream.Write(buffer);
        }
    }

    /// <summary>
    /// Sets the robot into idle mode. Use this function rather than halt in most cases.
    /// </summary>
    public void idle()
    {
        if (stream != null)
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
        if (stream != null)
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
}
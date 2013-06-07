using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using HidSharp;

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
	}

        /// <summary>
        /// Open's connection to the Finch. Call this after making the robot and before calling any other functions.
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

                color = (string)value;
                red = Int32.Parse(color.Substring(1, 2), System.Globalization.NumberStyles.HexNumber);
                green = Int32.Parse(color.Substring(3, 2), System.Globalization.NumberStyles.HexNumber);
                blue = Int32.Parse(color.Substring(5, 2), System.Globalization.NumberStyles.HexNumber);
                byte[] report = { (byte)0, (byte)'O', (byte)red, (byte)green, (byte)blue };
                stream.Write(report);
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
        /// <returns>Two element array containing the the left and right light sensor values (0 to 255)</returns>
        public override object getLight(params object[] position)
        {
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
                return returnData;
            }
            return null;
        }

        /// <summary>
        /// Returns the value of the left light sensor
        /// </summary>
        /// <returns>Left light sensor value, 0(dark) to 255(bright)</returns>
        public int getLeftLightSensor()
        {
            int[] lights = (int[])getLight();

            if (lights != null)
                return lights[0];
            else
                return 0;
        }

        /// <summary>
        /// Returns the value of the right light sensor
        /// </summary>
        /// <returns>Right light sensor value, 0(dark) to 255(bright)</returns>
        public int getRightLightSensor()
        {
            int[] lights = (int[])getLight();

            if (lights != null)
                return lights[1];
            else
                return 0;
        }

        /// <summary>
        /// Returns the accelerations experienced by Finch's accelerometer. Values are -1.5g to 1.5g.
        /// </summary>
        /// <returns>An array of 3 doubles holding X, Y, and Z acceleration, null if the read failed.</returns>
        public double[] getAccelerations()
        {
            if (robot != null)
            {
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
                return returnData;
            }
            return null;
        }

        /// <summary>
        /// Returns the X (beak to tail) acceleration
        /// </summary>
        /// <returns>Acceleration in gees</returns>
        public double getXAcceleration()
        {
            double[] accels = getAccelerations();
            if (accels != null)
                return accels[0];
            else
                return 0;
        }

        /// <summary>
        /// Returns the Y (wheel to wheel) acceleration
        /// </summary>
        /// <returns>Acceleration in gees</returns>
        public double getYAcceleration()
        {
            double[] accels = getAccelerations();
            if (accels != null)
                return accels[1];
            else
                return 0;
        }

        /// <summary>
        /// Returns the Z (top to bottom) acceleration
        /// </summary>
        /// <returns>Acceleration in gees</returns>
        public double getZAcceleration()
        {
            double[] accels = getAccelerations();
            if (accels != null)
                return accels[2];
            else
                return 0;
        }

        /// <summary>
        /// Gets the temperature measured by the Finch's small temperature sensor.
        /// </summary>
        /// <returns>Ambient temperature in Celcius</returns>
        public override object getTemperature()
        {
            if (robot != null)
            {
                byte[] report = {(byte)0, (byte)'T', 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, changeByte };
                stream.Write(report);
                byte[] readData = stream.Read();
                // Keep reading until you get back to the report that matches to the one you wrote (hopefully this loop isn't triggered, but just in case
                while (readData[8] != changeByte)
                {
                    stream.Write(report);
                    readData = stream.Read();
                }
                changeByte++;
                // Converts data to Celcius
                double returnData = ((double)readData[1] - 127) / 2.4 + 25;
                return returnData;
            }
            return 0;
        }

        /// <summary>
        /// Gets a two element boolean array representing the left (element 0) and right (element 1) obstacle sensors. True if an obstacle is detected, false otherwise. 
        /// </summary>
        /// <returns>Array contain Finch obstacle data</returns>
        public override object getObstacle(params object[] position)
        {
            if (robot != null)
            {
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
                return returnData;
            }
            return null;
        }

        /// <summary>
        /// Checks if there's an obstacle on the left side
        /// </summary>
        /// <returns>True if there's an obstacle, false otherwise</returns>
        public bool isObstacleLeftSide()
        {
            bool[] obstacles = (bool[])getObstacle();
            if (obstacles != null)
            {
                if (obstacles[0])
                    return true;
                else
                    return false;
            }
            return false;
        }

        /// <summary>
        /// Checks if there's an obstacle on the right side
        /// </summary>
        /// <returns>True if there's an obstacle, false otherwise</returns>
        public bool isObstacleRightSide()
        {
            bool[] obstacles = (bool[])getObstacle();
            if (obstacles != null)
            {
                if (obstacles[1])
                    return true;
                else
                    return false;
            }
            return false;
        }


        private void keepAlive()
        {
            while (true)
            {
                if (robot != null)
                {
                    setLED("finch", color); // Set the LED to the current values (doesn't change the LED color)
                    wait(2000); // do this again in 2 seconds
                }
            }
        }

}


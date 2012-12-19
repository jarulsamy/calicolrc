using System;
using NxtNet;
using System.IO.Ports;
using System.Collections.Generic;
using Gtk;
using IronPython.Runtime;

/// <summary>
/// This class is responsible for bluetooth communtication with the NXT robot.
/// </summary>
public static class MyroNXT
{
	
	#region Infrastructure
	
	
	/// <summary>
	/// Creates an NXT object that will be used for issuing commands.
	/// </summary>
	private static Nxt robot;
	private static MotorPort left;
	private static MotorPort right;
	
	
	/// <summary>
	/// Wait for one milisecond, this is used before commands to prevent trsnmission errors.
	/// </summary>
	private static void wait ()
	{
		System.Threading.Thread.Sleep (1);
	}
	

	/// <summary>
	/// Initilizes the robot and resets sensor ports.
	/// </summary>
	/// <param name='PortName'>
	/// Port where the robot is bound.
	/// </param>
	public static void init (string PortName)
	{
		robot = new Nxt ();
		robot.Connect (PortName);
		resetSensors ();
	}
	
	
	/// <summary>
	/// Initializes the robot and sets the default motor and sensor ports based on the default robot chassis.
	/// </summary>
	/// <param name='PortName'>
	/// Port where the robot is bound.
	/// </param>
	public static void initEdu (string PortName)
	{
		init (PortName);
		SetSensors (1, "touch");
		left = MotorPort.PortC;
		right = MotorPort.PortB;
	}
		
	/// <summary>
	/// Disconnect the robot.
	/// </summary>
	public static void disconnect ()
	{
		robot.Disconnect ();
	}
	#endregion
	
	#region Movement 
	
	/// <summary>
	/// Move forward continuously at the specified power.
	/// </summary>
	/// <param name='power'>
	/// Power must be between 0 and 1.
	/// </param>
	public static void forward (float power)
	{
		
		wait ();
		power = power * 100;
		sbyte speed = Convert.ToSByte (power);
		robot.SetOutputState (left, speed, MotorModes.On, MotorRegulationMode.Speed, 0, MotorRunState.Running, 0);
		System.Threading.Thread.Sleep (1);
		robot.SetOutputState (right, speed, MotorModes.On, MotorRegulationMode.Speed, 0, MotorRunState.Running, 0);
		
	}
				
	
	/// <summary>
	/// Move forward at the specified power for the specified number of seconds.
	/// </summary>
	/// <param name='power'>
	/// Power must be between 0 and 1.
	/// </param>
	/// <param name='time'>
	/// Time is the number of seconds the robot should move for.
	/// </param>
	public static void forward (float power, int time)
	{
		
		forward (power);
		int secs = time * 1000;		
		System.Threading.Thread.Sleep (secs);
		stop ();

	}
	
	/// <summary>
	/// Move backward continuously at the specified power.
	/// </summary>
	/// <param name='power'>
	/// Power must be between 0 and 1.
	/// </param>
	public static void backward (float power)
	{
	
		wait ();
		power = power * -100;
		sbyte speed = Convert.ToSByte (power);
		robot.SetOutputState (left, speed, MotorModes.On, MotorRegulationMode.Speed, 0, MotorRunState.Running, 0);
		System.Threading.Thread.Sleep (1);
		robot.SetOutputState (right, speed, MotorModes.On, MotorRegulationMode.Speed, 0, MotorRunState.Running, 0);		
		
		
	}
	
	/// <summary>
	/// Move backward at the specified power for the specified number of seconds.
	/// </summary>
	/// <param name='power'>
	/// Power must be between 0 and 1
	/// </param>
	/// <param name='time'>
	/// Time is the number of seconds the robot should move for
	/// </param>
	public static void backward (float power, int time)
	{
		
		backward (power);
		int secs = time * 1000;		
		System.Threading.Thread.Sleep (secs);
		stop ();
		
	
	}
	
	
	/// <summary>
	/// Stops all robot movement.
	/// </summary>
	public static void stop ()
	{
		wait ();
		robot.SetOutputState (NxtNet.MotorPort.All, 0, NxtNet.MotorModes.Brake, NxtNet.MotorRegulationMode.Idle, 0, MotorRunState.Idle, 0);
	}
		
	
	/// <summary>
	/// Sets the specified motor port to be bound to the left or right motor.
	/// </summary>
	/// <param name='port'>
	/// Port must be A-C.
	/// </param>
	/// <param name='direction'>
	/// Direction must be left or right.
	/// </param>
	public static void SetMotorPort (string port, string direction)
	{
		wait ();
		MotorPort p = new MotorPort ();
		if (port.Equals ("A")) {
			p = MotorPort.PortA;
		} else if (port.Equals ("B")) {
			p = MotorPort.PortB;
		} else {
			p = MotorPort.PortC;
		}
		
		if (direction.Equals ("left")) {
				
			left = p;
			Console.Write ("Assigning port ");
			Console.Write (p);
			Console.Write (direction);
		} else if (direction.Equals ("right")) {
			right = p;
		} else {
			Console.Write ("Incorrect Motor Assignment");
		}
	}
	
	
	/// <summary>
	/// Turns the robot to the left continuously at the specified speed.
	/// </summary>
	/// <param name='speed'>
	/// Speed must be between 0 and 1.
	/// </param>
	public static void turnLeft (float speed)
	{
	
		wait ();
		speed = speed * 100;
		sbyte sp = Convert.ToSByte (speed);
		
		robot.SetOutputState (right, sp, NxtNet.MotorModes.On, NxtNet.MotorRegulationMode.Speed, 0, NxtNet.MotorRunState.Running, 0);

		
	}
		
	
	/// <summary>
	/// Turns the robot to the left at the specified speed for the specified number of seconds.
	/// </summary>
	/// <param name='speed'>
	/// Speed must be between 0 and 1.
	/// </param>
	/// <param name='time'>
	/// Time is the number of seconds the robot should move for.
	/// </param>
	public static void turnLeft (float speed, int time)
	{
		
		turnLeft (speed);
		System.Threading.Thread.Sleep (time * 1000);
		stop ();

	}
	
	
	/// <summary>
	/// Turns the robot to the right at the specified speed.
	/// </summary>
	/// <param name='speed'>
	/// Speed must be between 0 and 1.
	/// </param>
	public static void turnRight (float speed)
	{
	
		wait ();
		speed = speed * 100;
		sbyte sp = Convert.ToSByte (speed);
		
		robot.SetOutputState (left, sp, NxtNet.MotorModes.On, NxtNet.MotorRegulationMode.Speed, 0, NxtNet.MotorRunState.Running, 0);

		
	}
	
	
	/// <summary>
	/// Turns the robot to the right at the specified speed for the specified number of seconds.
	/// </summary>
	/// <param name='speed'>
	/// Speed must be between 0 and 1.
	/// </param>
	/// <param name='time'>
	/// Time is the number of seconds the robot should move fo.
	/// </param>
	public static void turnRight (float speed, int time)
	{
		turnRight (speed);
		System.Threading.Thread.Sleep (time * 1000);
		stop ();
		
	}
	
	
	/// <summary>
	/// Move forward or backward continuously.
	/// </summary>
	/// <param name='speed'>
	/// Speed must be between -1 and 1, -1 to 0 for backward movement and 0 to 1 for forward.
	/// </param>
	public static void translate (float speed)
	{
		
		wait ();
		speed = speed * 100;
		sbyte sp = Convert.ToSByte (speed);
		robot.SetOutputState (left, sp, MotorModes.On, MotorRegulationMode.Speed, 0, MotorRunState.Running, 0);
		wait ();
		robot.SetOutputState (right, sp, MotorModes.On, MotorRegulationMode.Speed, 0, MotorRunState.Running, 0);
			
	}
	
	
	/// <summary>
	/// Rotate continuously to the left or right at the specified speed.
	/// </summary>
	/// <param name='speed'>
	/// Speed must be between -1 and 1, -1 to 0 for right movement and 0 to 1 for left.
	/// </param>
	public static void rotate (float speed)
	{
		
		wait ();
		if (speed > 0) {
			speed = speed * 100;
			sbyte sp = Convert.ToSByte (speed);
			robot.SetOutputState (right, sp, MotorModes.On, MotorRegulationMode.Speed, 0, MotorRunState.Running, 0);
		} else {
			speed = speed * -100;
			sbyte sp = Convert.ToSByte (speed);
			robot.SetOutputState (left, sp, MotorModes.On, MotorRegulationMode.Speed, 0, MotorRunState.Running, 0);
		}
	}
	
	
	/// <summary>
	/// Rotate and translate at the same time, causing the robot to arc.
	/// </summary>
	/// <param name='translation'>
	/// Translation must be between -1 and 1, -1 to 0 for backward and 0 to 1 for forward.
	/// </param>
	/// <param name='rotation'>
	/// Rotation must be between -1 and 1, -1 to 0 for right and 0 to 1 for left.
	/// </param>
	public static void move (float translation, float rotation)
	{
		wait ();
		if (rotation != 0) {	
			if ((translation > 0) && (translation >= Math.Abs (rotation))) {
				translation = Math.Abs (rotation) - (float).1;
				translate (translation);
			} else if ((translation < 0) && (-translation >= Math.Abs (rotation))) {
				translation = Math.Abs (rotation) - (float).1;
				translate (-translation);
			} else
				translate (translation);
			System.Threading.Thread.Sleep (1);
			rotate (rotation);
		} else
			translate (translation);
	}
		
	/// <summary>
	/// Moves the motors continuously for the specified speeds, speed must be between -1 and 1.
	/// </summary>
	/// <param name='power1'>
	/// Power1 is the speed for port A.
	/// </param>
	/// <param name='power2'>
	/// Power2 is the speed for port B.
	/// </param>
	/// <param name='power3'>
	/// Power3 is the speed for port C.
	/// </param>
	public static void motors (double power1, double power2, double power3)
	{
		wait ();
		Console.Write ("new");
		motors ("A", power1, 0);
		motors ("B", power2, 0);
		motors ("C", power3, 0);
	}
	
	
	/// <summary>
	/// Motors the list.
	/// </summary>
	/// <param name='port'>
	/// Port.
	/// </param>
	/// <param name='power'>
	/// Power.
	/// </param>
	/// <param name='time'>
	/// Time.
	/// </param>
	public static void motorList (double[] port, double[] power, double[] time)
	{
		
		
		int size = port.Length;
		int p = 1;
		
		if ((size > 3) || (size < 1)) {	//error
			return;
		}
		while (size > 0) {
			motors (port [p], power [p], time [p]);
			p++;
			size--;
			
		}
	}	
				
	
	/// <summary>
	/// Moves the motor on the specified port at the specified power for the specified time.
	/// </summary>
	/// <param name='port'>
	/// Port must be between A-C.
	/// </param>
	/// <param name='power'>
	/// Power must be between -1 and 1.
	/// </param>
	/// <param name='time'>
	/// Time is a value in seconds.
	/// </param>
	public static void motors (string port, double power, int time)
	{
		wait ();
		power = power * 100;
		sbyte speed = Convert.ToSByte (power);
		
		MotorPort m = new MotorPort ();
		int secs = time * 1000;

		
		if (port.Equals ("A")) {
			m = MotorPort.PortA;
		} else if (port.Equals ("B")) {
			m = MotorPort.PortB;
		} else {
			m = MotorPort.PortC;
		}		
		if (time == 0) {
			robot.SetOutputState (m, speed, MotorModes.On, MotorRegulationMode.Speed, 0, MotorRunState.Running, 0);
		} else {
			robot.SetOutputState (m, speed, MotorModes.On, MotorRegulationMode.Speed, 0, MotorRunState.Running, 0);
			System.Threading.Thread.Sleep (secs);
			stop ();
		}
	}
	#endregion
	
	#region Sensors
	
	/// <summary>
	/// Initializes the given sensor port with the given type.
	/// </summary>
	/// <param name='port'>
	/// Port must be between 1-4.
	/// </param>
	/// <param name='type'>
	/// Type must be either touch, light, sound or ultrasonic.
	/// </param>
	public static void SetSensors (int port, string type)
	{
		wait ();
		SensorPort p = new SensorPort ();
		
		switch (port) {
		case 1 :
			p = SensorPort.Port1;
			break;
		case 2 :
			p = SensorPort.Port2;
			break;
		case 3 :
			p = SensorPort.Port3;
			break;
		case 4 :
			p = SensorPort.Port4;
			break;
		}
		
		switch (type) {
		case "touch" :
			robot.SetInputMode (p, SensorType.Switch, SensorMode.Boolean);
			break;
		case "sound" :
			robot.SetInputMode (p, SensorType.SoundDB, SensorMode.FullScale);
			break;
		case "light" :
			robot.SetInputMode (p, SensorType.LightInactive, SensorMode.FullScale);
			break;
		case "none" :
			robot.SetInputMode (p, SensorType.NoSensor, SensorMode.Raw);
			break;
		default :
			Console.Write ("Incorrect Sensor type");
			break;
		}
	}
	
	
	/// <summary>
	/// Gets the sensor attached to the specified port.
	/// </summary>
	/// <returns>
	/// The sensor state of the specified port.
	/// </returns>
	/// <param name='port'>
	/// Port must be between 1-4.
	/// </param>
	private static SensorState getSensor (int port)
	{
		wait ();
		SensorPort p = new SensorPort ();
		
		switch (port) {
		case 1 :
			p = SensorPort.Port1;
			break;
		case 2 :
			p = SensorPort.Port2;
			break;
		case 3 :
			p = SensorPort.Port3;
			break;
		case 4 :
			p = SensorPort.Port4; 
			break;
		default :
			Console.Write ("Please enter a port number 1-4");
			break;
		}
				
		SensorState ss = new SensorState ();
		ss = robot.GetInputValues (p);
		return  ss;
		
	}
	
	
	/// <summary>
	/// Gets the sensors states of each port and prints them to the console.
	/// </summary>
	public static void getSensors ()
	{
		wait ();
		for (int k = 1; k <= 4; k++) {
			string type;
			string val;
			SensorState ss = new SensorState ();
			ss = getSensor (k);
			switch (ss.Type) {
			case SensorType.Switch :
				type = "Touch";
				if (ss.ScaledValue == 1)
					val = "True";
				else
					val = "False";
				break;
			case SensorType.SoundDB :
				type = "Sound";
				val = (ss.NormalizedValue).ToString () + "DB";
				break;
			case SensorType.LightInactive:
				type = "Light";
				val = (ss.ScaledValue).ToString ();
				break;
			default:
				type = "None";
				val = "N/A";
				break;
					
			}
			
			
			
			if (type != "None") {
				Console.Write ("Port " + k + ": " + "\n" + type + "\n" + val + "\n" + "\n");
			}
			
				
		}
		
		
		
		
	}
				
			
	/// <summary>
	/// Gets the sound sensor attached to the specified port.
	/// </summary>
	/// <returns>
	/// The scaled value for the sound sensor on the specified port.
	/// </returns>
	/// <param name='port'>
	/// Port must be between 1-4.
	/// </param>
	public static short getSound (int port)
	{
		wait ();
		SensorState ss = new SensorState ();
		ss = getSensor (port);
		return ss.ScaledValue;
	}
	
	
	/// <summary>
	/// Gets the light sensor attached to the specified port.
	/// </summary>
	/// <returns>
	/// The scaled value for the light sensor attched to the specified port.
	/// </returns>
	/// <param name='port'>
	/// Port must be between 1-4.
	/// </param>
	public static short getLight (int port)
	{
		wait ();
		SensorState ss = new SensorState ();
		ss = getSensor (port);
		return ss.ScaledValue;
	}
	
	
	/// <summary>
	/// Gets the touch sensor attached to the specified port.
	/// </summary>
	/// <returns>
	/// The boolean value for the touch sensor attched to the specified port.
	/// </returns>
	/// <param name='port'>
	/// Port must be between 1-4.
	/// </param>
	public static bool getTouch (int port)
	{
		wait ();
		SensorState ss = new SensorState ();
		ss = getSensor (port);
	 
		if (ss.ScaledValue == 1)
			return true;
		else
			return false;
		
	}
	
	
	/// <summary>
	/// Resets the sensor ports.
	/// </summary>
	public static void resetSensors ()
	{
		wait ();
		for (int k = 1; k <=4; k++) {
			SetSensors (k, "none");
		}
	}
	
	
	/// <summary>
	/// Opens a GUI which displays the values of each initialized sensor.
	/// </summary>
	public static void senses ()
	{
		new Senses ();
	}

	class Senses
	{
		Gtk.Window win;
		public Label[] type = new Label[4];
		public Label[] val = new Label[4];
		
		public Senses ()
		{
			
			
			
			
			Application.Init ();
			win = new Gtk.Window ("Senses");
			win.DeleteEvent += delegate {
				Application.Quit ();
			};
			win.SetPosition (WindowPosition.Center);
			win.SetDefaultSize (400, 400);
			
			
			Table tab = new Table (9, 3, false);
			uint p_top = 0;
			uint p_bot = 2;
			uint t_top = 0;
			uint v_top = 1;
			SensorState ss;
			
			
			for (int i = 1; i <= 4; i++) {
				
				
				ss = getSensor (i);
				type [i - 1] = new Label ((ss.Type).ToString ());
				val [i - 1] = new Label ((ss.ScaledValue).ToString ());
				tab.Attach (new Label ("Port " + i + ": "), 0, 1, p_top, p_bot);
				tab.Attach (new Label ("Type: "), 1, 2, t_top, t_top + 1);
				tab.Attach (type [i - 1], 2, 3, t_top, t_top + 1);
				tab.Attach (val [i - 1], 2, 3, v_top, v_top + 1);
				tab.Attach (new Label ("Value: "), 1, 2, v_top, v_top + 1);
				p_top += 2;
				p_bot += 2;
				t_top = v_top + 1;
				v_top = v_top + 2;
			}
			
			Button refresh = new Button ("Refresh");
			refresh.Clicked += HandleRefreshClicked;
			tab.Attach (refresh, 0, 2, 8, 9);
			
			win.Add (tab);
			
			
			tab.Show ();
			win.ShowAll ();
			Application.Run ();
			

			
					
		}

		void HandleRefreshClicked (object sender, EventArgs e)
		{

			repopulateTable ();
		}
		
		private void repopulateTable ()
		{
		
			SensorState ss;
			
			
			for (int i = 1; i <= 4; i++) {
				
				
				ss = getSensor (i);
				type [i - 1].Text = (ss.Type).ToString ();
				val [i - 1].Text = (ss.ScaledValue).ToString ();			
			}
		
		}
		
	

	}

	
}

	
	#endregion
	
	
	
	
		



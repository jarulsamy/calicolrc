using System;
using NxtNet;
using System.IO.Ports;
using System.Collections.Generic;
using IronPython.Runtime;

/// <summary>
/// This class is responsible for bluetooth communtication with the NXT robot.
/// </summary>
public class NxtRobot: Myro.Robot {
	
	/// <summary>
	/// Creates an NXT object that will be used for issuing commands.
	/// </summary>

	private MotorPort left;
	private MotorPort right;
	private Nxt robot;

	public NxtRobot(string PortName)
	{
		robot = new Nxt ();
		robot.Connect (PortName);
		resetSensors ();
	}
	
	/// <summary>
	/// Wait for one milisecond, this is used before commands to prevent trsnmission errors.
	/// </summary>
	private void wait ()
	{
		System.Threading.Thread.Sleep (1);
	}
	
	/// <summary>
	/// Initializes the robot and sets the default motor and sensor ports based on the default robot chassis.
	/// </summary>
	/// <param name='PortName'>
	/// Port where the robot is bound.
	/// </param>
	public override void reinit(string PortName, int baud)
	{
	    robot.Disconnect();
		robot.Connect (PortName);
		resetSensors ();
	}
		
	/// <summary>
	/// Disconnect the robot.
	/// </summary>
	public void disconnect ()
	{
		robot.Disconnect ();
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
	public void SetMotorPort (string port, string direction)
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
	/// Rotate and translate at the same time, causing the robot to arc.
	/// </summary>
	/// <param name='translation'>
	/// Translation must be between -1 and 1, -1 to 0 for backward and 0 to 1 for forward.
	/// </param>
	/// <param name='rotation'>
	/// Rotation must be between -1 and 1, -1 to 0 for right and 0 to 1 for left.
	/// </param>
	public override void adjustSpeed ()
	{
	  double translation = _lastTranslate;
	  double rotation = _lastRotate;
	  if (rotation != 0) {	
		if ((translation > 0) && (translation >= Math.Abs (rotation))) {
		  translation = Math.Abs (rotation) - .1;
		  my_translate (translation);
		} else if ((translation < 0) && (-translation >= Math.Abs (rotation))) {
		  translation = Math.Abs (rotation) - .1;
		  my_translate (-translation);
		} else
		  my_translate (translation);
		System.Threading.Thread.Sleep (1);
		my_rotate (rotation);
	  } else
		my_translate (translation);
	}

	private void my_translate(double speed)
	{
	  wait();
	  speed = speed * 100;
	  sbyte sp = Convert.ToSByte(speed);
	  robot.SetOutputState(left,sp,MotorModes.On,MotorRegulationMode.Speed,0,MotorRunState.Running,0);
	  wait();
	  robot.SetOutputState(right,sp,MotorModes.On,MotorRegulationMode.Speed,0,MotorRunState.Running,0);
	}

	private void my_rotate(double speed)
	{
		wait();
		if(speed > 0) {
		  speed = speed * 100;
		  sbyte sp = Convert.ToSByte(speed);
		  robot.SetOutputState(right,sp,MotorModes.On,MotorRegulationMode.Speed,0,MotorRunState.Running,0);
		} else {
		  speed = speed * -100;
		  sbyte sp = Convert.ToSByte(speed);
		  robot.SetOutputState(left,sp,MotorModes.On,MotorRegulationMode.Speed,0,MotorRunState.Running,0);
		}
	}
		
	/// <summary>
	/// Initializes the given sensor port with the given type.
	/// </summary>
	/// <param name='port'>
	/// Port must be between 1-4.
	/// </param>
	/// <param name='type'>
	/// Type must be either touch, light, sound or ultrasonic.
	/// </param>
	public void SetSensors (int port, string type)
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
	private SensorState getSensor (int port)
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
	public void getSensors ()
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
	public short getSound (int port)
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
	public override object getLight (params object [] position)
	{
	  List list = new List();
	  foreach(int p in position) {
		list.append( getLight(p) );
	  }
	  return list;
	}

	public short getLight (int port)
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
	public bool getTouch (int port)
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
	public void resetSensors ()
	{
		wait ();
		for (int k = 1; k <=4; k++) {
			SetSensors (k, "none");
		}
	}
	
	
	/// <summary>
	/// Opens a GUI which displays the values of each initialized sensor.
	/// </summary>
	public void senses ()
	{
		new Senses (this);
	}

	class Senses
	{
		Gtk.Window win;
		public Gtk.Label[] type = new Gtk.Label[4];
		public Gtk.Label[] val = new Gtk.Label[4];
	    private NxtRobot nxtrobot;
		
		public Senses (NxtRobot nxtrobot)
		{
		  Myro.Invoke( delegate {
				this.nxtrobot = nxtrobot;
				win = new Gtk.Window ("Senses");
				win.DeleteEvent += delegate {
				  win.Destroy();
				};
				win.SetPosition (Gtk.WindowPosition.Center);
				win.SetDefaultSize (400, 400);
				Gtk.Table tab = new Gtk.Table (9, 3, false);
				uint p_top = 0;
				uint p_bot = 2;
				uint t_top = 0;
				uint v_top = 1;
				SensorState ss;
				for (int i = 1; i <= 4; i++) {
				  ss = nxtrobot.getSensor (i);
				  type [i - 1] = new Gtk.Label ((ss.Type).ToString ());
				  val [i - 1] = new Gtk.Label ((ss.ScaledValue).ToString ());
				  tab.Attach (new Gtk.Label ("Port " + i + ": "), 0, 1, p_top, p_bot);
				  tab.Attach (new Gtk.Label ("Type: "), 1, 2, t_top, t_top + 1);
				  tab.Attach (type [i - 1], 2, 3, t_top, t_top + 1);
				  tab.Attach (val [i - 1], 2, 3, v_top, v_top + 1);
				  tab.Attach (new Gtk.Label ("Value: "), 1, 2, v_top, v_top + 1);
				  p_top += 2;
				  p_bot += 2;
				  t_top = v_top + 1;
				  v_top = v_top + 2;
				}

				Gtk.Button refresh = new Gtk.Button ("Refresh");
				refresh.Clicked += HandleRefreshClicked;
				tab.Attach (refresh, 0, 2, 8, 9);
				win.Add (tab);
				tab.Show ();
				win.ShowAll ();
			  });
		}

		void HandleRefreshClicked (object sender, EventArgs e)
		{
			repopulateTable ();
		}
		
		private void repopulateTable ()
		{
		  Myro.Invoke( delegate {
				SensorState ss;
				for (int i = 1; i <= 4; i++) {
				  ss = nxtrobot.getSensor (i);
				  type [i - 1].Text = (ss.Type).ToString ();
				  val [i - 1].Text = (ss.ScaledValue).ToString ();			
				}
			  });
		}
	}
}

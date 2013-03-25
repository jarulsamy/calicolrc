using IronPython.Runtime;
using System.Collections;
using System.Collections.Generic;
using System.IO.Ports;
using System.Threading;
using System;
using Microsoft.Xna.Framework; // Vector2, Matrix

public class SimScribbler : Myro.Robot
	{
		public Graphics.Rectangle frame;
		public Myro.Simulation simulation;
		public double velocity = 0;
		public double rate = 8.0;
		public bool stall = false;
		public string name = "Scribby";
		public double battery = 7.6;
		public PythonDictionary sensors = new PythonDictionary ();
		public PythonDictionary readings = new PythonDictionary ();
		public List<Graphics.Shape> light_sensors = new List<Graphics.Shape> ();
		public bool show_sensors = false;

		public SimScribbler (Myro.Simulation simulation)
		{
			this.simulation = simulation;
			frame = new Graphics.Rectangle (new Graphics.Point (320 - 23, 240 - 23),
				     new Graphics.Point (320 + 23, 240 + 23));
			frame.pen.minDistance = 10; // minimum distance from last point
			// Draw a body:
			Graphics.Polygon body = new Graphics.Polygon ();

			double [] sx = new double[] {0.05, 0.05, 0.07, 0.07, 0.09, 0.09, 0.07, 
				   0.07, 0.05, 0.05, -0.05, -0.05, -0.07, 
				   -0.08, -0.09, -0.09, -0.08, -0.07, -0.05, 
				   -0.05};
			double [] sy = new double[] {0.06, 0.08, 0.07, 0.06, 0.06, -0.06, -0.06, 
				   -0.07, -0.08, -0.06, -0.06, -0.08, -0.07, 
				   -0.06, -0.05, 0.05, 0.06, 0.07, 0.08, 0.06};
			for (int i =0; i < sx.Length; i++) {
				body.append (new Graphics.Point (sx [i] * 250, sy [i] * 250));
			}
			body.fill = Myro.Color ("red");
			body.draw (frame);
			// Draw wheels:
			Graphics.Rectangle wheel1 = new Graphics.Rectangle (new Graphics.Point (-10, -23),
							 new Graphics.Point (10, -17));
			wheel1.color = Myro.Color ("black");
			wheel1.draw (frame);
			Graphics.Rectangle wheel2 = new Graphics.Rectangle (new Graphics.Point (-10, 23),
							 new Graphics.Point (10, 17));
			wheel2.color = Myro.Color ("black");
			wheel2.draw (frame);
      
			// Details
			Graphics.Circle hole = new Graphics.Circle (new Graphics.Point (0, 0), 3);
			hole.fill = Myro.Color ("black");
			hole.draw (frame);
      
			Graphics.Rectangle fluke = new Graphics.Rectangle (new Graphics.Point (15, -10),
							new Graphics.Point (19, 10));
			fluke.color = Myro.Color ("green");
			fluke.draw (frame);
      
			// light sensors
			Graphics.Circle light = new Graphics.Circle (new Graphics.Point (-18, -9), 
						  1);
			light.color = new Graphics.Color ("black");
			light.draw (frame);
			light.tag = "light-right";
			light_sensors.Add (light);

			light = new Graphics.Circle (new Graphics.Point (-18, 0), 
				  1);
			light.color = new Graphics.Color ("black");
			light.draw (frame);
			light.tag = "light-center";
			light_sensors.Add (light);

			light = new Graphics.Circle (new Graphics.Point (-18, 9), 
				  1);
			light.color = new Graphics.Color ("black");
			light.draw (frame);
			light.tag = "light-left";
			light_sensors.Add (light);

			// ray casting sensors:
			Microsoft.Xna.Framework.Vector2 v2;

			// sensors getObstacle("left")
			int ir_range = 25;
			v2 = Graphics.VectorRotate (Graphics.Vector (ir_range, 0), -45 * Math.PI / 180);
			Graphics.Line line = new Graphics.Line (new Graphics.Point (25, -12), 
					     new Graphics.Point (25 + v2.X, -12 + v2.Y));
			line.outline = new Graphics.Color (0, 0, 255, 64);
			line.visible = false;

			// Visualization of sensor
			Graphics.Pie pie = new Graphics.Pie (line.points [0],
					  ir_range, -30, 30);
			pie.rotate (45);
			pie.fill = new Graphics.Color (0, 0, 255, 64);
			// FIXME: outline can't be null
			pie.outline = new Graphics.Color (0, 0, 255, 64);
			pie.draw (line);

			line.draw (frame);
			sensors ["obstacle-left"] = line;

			// sensors getObstacle("center")
			line = new Graphics.Line (new Graphics.Point (25, 0), 
			       new Graphics.Point (25 + ir_range, 0));
			line.outline = new Graphics.Color (0, 0, 255, 64);
			line.visible = false;

			// Visualization of sensor
			pie = new Graphics.Pie (line.points [0],
			     ir_range, -30, 30);
			//pie.rotate(0);
			pie.fill = new Graphics.Color (0, 0, 255, 64);
			// FIXME: outline can't be null
			pie.outline = new Graphics.Color (0, 0, 255, 64);
			pie.draw (line);

			line.draw (frame);
			sensors ["obstacle-center"] = line;

			// sensors getObstacle("right")
			v2 = Graphics.VectorRotate (Graphics.Vector (ir_range, 0), 45 * Math.PI / 180);
			line = new Graphics.Line (new Graphics.Point (25, 12), 
			       new Graphics.Point (25 + v2.X, 12 + v2.Y));
			line.outline = new Graphics.Color (0, 0, 255, 64);
			line.visible = false;
			// Visualization of sensor
			pie = new Graphics.Pie (line.points [0],
			     ir_range, -30, 30);
			pie.rotate (-45);
			pie.fill = new Graphics.Color (0, 0, 255, 64);
			// FIXME: outline can't be null
			pie.outline = new Graphics.Color (0, 0, 255, 64);
			pie.draw (line);
			line.draw (frame);
			sensors ["obstacle-right"] = line;

			// sensors getIR("right")
			v2 = Graphics.VectorRotate (Graphics.Vector (ir_range, 0), -180 * Math.PI / 180);
			line = new Graphics.Line (new Graphics.Point (-25, -12), 
			       new Graphics.Point (-25 + v2.X, -12 + v2.Y));
			line.outline = new Graphics.Color (0, 0, 255, 64);
			line.visible = false;
			// Visualization of sensor
			pie = new Graphics.Pie (line.points [0],
			     ir_range, -30, 30);
			pie.rotate (180);
			pie.fill = new Graphics.Color (0, 0, 255, 64);
			// FIXME: outline can't be null
			pie.outline = new Graphics.Color (0, 0, 255, 64);
			pie.draw (line);
			line.draw (frame);
			sensors ["ir-right"] = line;

			// sensors getIR("left")
			v2 = Graphics.VectorRotate (Graphics.Vector (ir_range, 0), -180 * Math.PI / 180);
			line = new Graphics.Line (new Graphics.Point (-25, 12), 
			       new Graphics.Point (-25 + v2.X, 12 + v2.Y));
			line.outline = new Graphics.Color (0, 0, 255, 64);
			line.visible = false;
			// Visualization of sensor
			pie = new Graphics.Pie (line.points [0],
			     ir_range, -30, 30);
			pie.rotate (180);
			pie.fill = new Graphics.Color (0, 0, 255, 64);
			// FIXME: outline can't be null
			pie.outline = new Graphics.Color (0, 0, 255, 64);
			pie.draw (line);
			line.draw (frame);
			sensors ["ir-left"] = line;

			// Just the fill, to see outline of bounding box:
			frame.fill = null;
			// FIXME: something not closing correctly in render when :
			//frame.color = null;
			frame.outline = simulation.groundColor;
			// set collision
			frame.draw (simulation.window);
			frame.body.OnCollision += SetStall;

			this.simulation.robots.Add (this);
			setup();
		}

		bool SetStall (FarseerPhysics.Dynamics.Fixture fixture1,
		  FarseerPhysics.Dynamics.Fixture ficture2,
		  FarseerPhysics.Dynamics.Contacts.Contact contact)
		{
			stall = true;
			return true;
		}
	
		public override void adjustSpeed ()
		{
			lock (this) {
				velocity = _lastTranslate * rate;
				frame.body.AngularVelocity = (float)(-_lastRotate * rate);
			}
		}

	    public override void setPose(int x, int y, double theta) {
		  float MeterInPixels = 64.0f;
		  lock(frame.world) {
			frame.body.Position = new Vector2 (((float)x) / MeterInPixels, 
				((float)y) / MeterInPixels);
			frame.body.Rotation = (float)(theta * Math.PI/180.0);
		  }
		  simulation.window.refresh();
		}

		public override void setOption (string key, object value)
		{
			if (key == "show-sensors") {
				show_sensors = (bool)value;
				foreach (KeyValuePair<object,object> kvp in sensors) {
					Graphics.Shape shape = (Graphics.Shape)kvp.Value;
					shape.visible = show_sensors;
				}
			}
		}

		public override void flush ()
		{ 
			//
		}

		public override void penDown (string color)
		{ 
			frame.outline = new Graphics.Color (color);
			frame.penDown ();
		}

		public override Graphics.Line penUp ()
		{ 
			return frame.penUp ();
		}

		public override Graphics.Picture takePicture (string mode="jpeg")
		{ 
		    // simscribbler camera
		    Graphics.Picture picture = new Graphics.Picture (256, 192);
		    double view_angle = 60.0; // degrees
		    double max_distance = 20.0;
		    float MeterInPixels = 64.0f;
		    if (!simulation.window.isRealized())
			return picture;
		    lock (this) {
			double [] distance = new double[256];
			Graphics.Color [] colors = new Graphics.Color[256];
			Graphics.Point p1 = frame.getScreenPoint (new Graphics.Point (25, 0));
			for (int i = 0; i < 256; i++) {
			    var v = Graphics.VectorRotate (Graphics.Vector (max_distance * MeterInPixels, 0), 
							   (float)(((i / 256.0) * view_angle) - view_angle / 2.0) * Math.PI / 180.0);
			    Graphics.Point p2 = frame.getScreenPoint (new Graphics.Point (25 + v.X, v.Y));
			    simulation.window.canvas.world.RayCast ((fixture, v1, v2, hit) => {  
				    distance [i] = 1.0 - Math.Min (hit * max_distance, max_distance) / max_distance; /// 10 x car
				    if (fixture.UserData is Graphics.Shape)
					colors [i] = ((Graphics.Shape)fixture.UserData).fill;
				    return 1; 
				}, 
				Graphics.Vector ((float)(p1.x / MeterInPixels), (float)(p1.y / MeterInPixels)), 
				Graphics.Vector ((float)(p2.x / MeterInPixels), (float)(p2.y / MeterInPixels)));
			}
			Graphics.Color c;
			double g = 1.0;
			Graphics.Color sky = new Graphics.Color ("deepskyblue");
			ManualResetEvent ev = new ManualResetEvent(false);
			Myro.Invoke (delegate {
				for (int i = 0; i < 256; i++) {
				    if (distance [i] > 0) {
					if (colors [i] != null)
					    c = colors [i];
					else
					    c = new Graphics.Color ("black");
					g = distance [i];
				    } else {
					c = new Graphics.Color ("gray");
					g = 1.0;
				    }
				    for (int h = 0; h < 192; h++) {
					if (h >= (int)(192 / 2.0 - g * 192 / 2.0) && h <= (int)(192 / 2.0 + g * 192 / 2.0)) {
					    picture.setColor (i, h, new Graphics.Color (c.red * g, c.green * g, c.blue * g));
					} else if (h < (int)(192 / 2.0 - g * 192 / 2.0)) {
					    picture.setColor (i, h, sky);
					} else {
					    picture.setColor (i, h, simulation.groundColor);
					}
				    }
				}
				ev.Set();
			    });
			ev.WaitOne ();
		    }
		    //Graphics.Picture pic = makePicture("/home/dblank/Calico-dev/trunk/examples/images/pyramid.png");
		    //picture.setRegion(new Graphics.Point(10, 10), pic);
		    return picture;
		}
    
		public override void setup ()
		{
		    Console.WriteLine ("You are using:");
		    Console.WriteLine ("   Simulated Fluke, version 1.0.0");
		    Console.WriteLine ("   Simulated Scribbler 2, version 1.0.0");
		    Console.WriteLine ("Hello, my name is '{0}'!", getName ());
		}
    
		public override string getName ()
		{
			return name;
		}
    
		public override List getIRMessage ()
		{
			return null;
		}
    
		public override void setCommunicate ()
		{
		}
    
		public override void sendIRMessage (string data)
		{
		}
    
		public override List getBlob ()
		{
			return null;
		}
    
		public override object getData (params int [] position)
		{
			return null;
		}
    
		public override void setData (int position, int value)
		{
		}
    
		public override PythonDictionary getAll ()
		{
			return null;
		}
    
		public override PythonDictionary getInfo ()
		{
			return null;
		}
    
		public override object getObstacle (params object [] positions)
		{
			string key = null; 
			List retval = new List ();
			if (positions.Length == 0)
				positions = new object[3] {0, 1, 2};
			else if ((positions.Length == 1) && (positions [0] is string) && ((string)(positions [0]) == "all"))
				positions = new object[3] {0, 1, 2};
			lock (this) {
				foreach (object position in positions) {
					if (position is int) {
						if (((int)position) == 0) {
							key = "obstacle-left";
						} else if (((int)position) == 1) {
							key = "obstacle-center";
						} else if (((int)position) == 2) {
							key = "obstacle-right";
						} else {
							throw new Exception ("invalid position in getObstacle()");
						}
					} else if (position is double) {
						if (((double)position) == 0) {
							key = "obstacle-left";
						} else if (((double)position) == 1) {
							key = "obstacle-center";
						} else if (((double)position) == 2) {
							key = "obstacle-right";
						} else {
							throw new Exception ("invalid position in getObstacle()");
						}
					} else if (position is string) {
						if (((string)position) == "left") {
							key = "obstacle-left";
						} else if (((string)position) == "center") {
							key = "obstacle-center";
						} else if (((string)position) == "right") {
							key = "obstacle-right";
						} else {
							throw new Exception ("invalid position in getObstacle()");
						}
					} else {
						throw new Exception ("invalid position in getObstacle()");
					}
					if (readings.Contains (key)) {
						retval.append ((int)(((float)readings [key]) * 5000.0));
					} else {
						retval.append (0);
					}
				}
			}
			if (retval.Count == 1)
				return retval [0];
			else 
				return retval;
		}
    

		public override object getDistance (params object [] positions)
		{
			string key = null; 
			List retval = new List ();
			if (positions.Length == 0)
			  positions = new object[2] {0, 1};
			else if ((positions.Length == 1) && (positions [0] is string) && ((string)(positions [0]) == "all"))
				positions = new object[2] {0, 1};
			lock (this) {
				foreach (object position in positions) {
					if (position is int) {
						if (((int)position) == 0) {
							key = "distance-left";
						} else if (((int)position) == 1) {
							key = "distance-right";
						} else {
							throw new Exception ("invalid position in getDistance()");
						}
					} else if (position is double) {
						if (((double)position) == 0) {
							key = "distance-left";
						} else if (((double)position) == 1) {
							key = "distance-right";
						} else {
							throw new Exception ("invalid position in getDistance()");
						}
					} else if (position is string) {
						if (((string)position) == "left") {
							key = "distance-left";
						} else if (((string)position) == "right") {
							key = "distance-right";
						} else {
							throw new Exception ("invalid position in getDistance()");
						}
					} else {
						throw new Exception ("invalid position in getDistance()");
					}
					if (readings.Contains (key)) {
					  retval.append (readings [key]);
					} else {
					  retval.append (0);
					}
				}
			}
			if (retval.Count == 1)
				return retval [0];
			else 
				return retval;
		}
    
		public override object getLight (params object [] positions)
		{
			string key = null; 
			List retval = new List ();
			if (positions.Length == 0)
				positions = new object[3] {0, 1, 2};
			else if ((positions.Length == 1) && (positions [0] is string) && ((string)(positions [0]) == "all"))
				positions = new object[3] {0, 1, 2};
			lock (this) {
				foreach (object position in positions) {
					if (position is int) {
						if (((int)position) == 0) {
							key = "light-left";
						} else if (((int)position) == 1) {
							key = "light-center";
						} else if (((int)position) == 2) {
							key = "light-right";
						} else {
							throw new Exception ("invalid position in getLight()");
						}
					} else if (position is double) {
						if (((double)position) == 0) {
							key = "light-left";
						} else if (((double)position) == 1) {
							key = "light-center";
						} else if (((double)position) == 2) {
							key = "light-right";
						} else {
							throw new Exception ("invalid position in getLight()");
						}
					} else if (position is string) {
						if (((string)position) == "left") {
							key = "light-left";
						} else if (((string)position) == "center") {
							key = "light-center";
						} else if (((string)position) == "right") {
							key = "light-right";
						} else {
							throw new Exception ("invalid position in getLight()");
						}
					} else {
						throw new Exception ("invalid position in getLight()");
					}
					if (readings.Contains (key)) {
						retval.append (readings [key]);
					} else {
						retval.append (null);
					}
				}
			}
			if (retval.Count == 1)
				return retval [0];
			else 
				return retval;
		}
    
		public override object getIR (params object [] positions)
		{
			string key = null; 
			List retval = new List ();
			if (positions.Length == 0)
				positions = new object[2] {0, 1};
			else if ((positions.Length == 1) && (positions [0] is string) && ((string)(positions [0]) == "all"))
				positions = new object[2] {0, 1};
			lock (this) {
				foreach (object position in positions) {
					if (position is int) {
						if (((int)position) == 0) {
							key = "ir-left";
						} else if (((int)position) == 1) {
							key = "ir-right";
						} else {
							throw new Exception ("invalid position in getIR()");
						}
					} else if (position is double) {
						if (((double)position) == 0) {
							key = "ir-left";
						} else if (((double)position) == 1) {
							key = "ir-right";
						} else {
							throw new Exception ("invalid position in getIR()");
						}
					} else if (position is string) {
						if (((string)position) == "left") {
							key = "ir-left";
						} else if (((string)position) == "right") {
							key = "ir-right";
						} else {
							throw new Exception ("invalid position in getIR()");
						}
					} else {
						throw new Exception ("invalid position in getIR()");
					}
					if (readings.Contains (key)) {
						retval.append (0);
					} else {
						retval.append (1);
					}
				}
			}
			if (retval.Count == 1)
				return retval [0];
			else 
				return retval;
		}

		public override object getBright (string window)
		{
			if (window == "left")
				return ((List)getBright ()) [0];
			else if (window == "center")
				return ((List)getBright ()) [1];
			else if (window == "right")
				return ((List)getBright ()) [2];
			else if (window == "all")
				return getBright ();
			else
				throw new Exception ("invalid argument to getBright()");
		}

		public override object getBright ()
		{
			// simscribbler camera
			double view_angle = 60.0; // degrees
			double max_distance = 20.0;
			float MeterInPixels = 64.0f;
			List counts = Graphics.PyList (0, 0, 0);
			if (!simulation.window.isRealized())
				return counts;
			lock (this) {
				double [] distance = new double[256];
				Graphics.Color [] colors = new Graphics.Color[256];
				Graphics.Point p1 = frame.getScreenPoint (new Graphics.Point (25, 0));
				for (int i = 0; i < 256; i++) {
					var v = Graphics.VectorRotate (Graphics.Vector (max_distance * MeterInPixels, 0), 
				(float)(((i / 256.0) * view_angle) - view_angle / 2.0) * Math.PI / 180.0);
					Graphics.Point p2 = frame.getScreenPoint (new Graphics.Point (25 + v.X, v.Y));
					simulation.window.canvas.world.RayCast ((fixture, v1, v2, hit) => {  
						distance [i] = 1.0 - Math.Min (hit * max_distance, max_distance) / max_distance; /// 10 x car
						if (fixture.UserData is Graphics.Shape)
							colors [i] = ((Graphics.Shape)fixture.UserData).fill;
						return 1; 
					}, 
	    Graphics.Vector ((float)(p1.x / MeterInPixels), (float)(p1.y / MeterInPixels)), 
	    Graphics.Vector ((float)(p2.x / MeterInPixels), (float)(p2.y / MeterInPixels)));
				}
				Graphics.Color c;
				double g = 1.0;
				Graphics.Color sky = new Graphics.Color ("deepskyblue");
				for (int i = 0; i < 256; i++) {
					if (distance [i] > 0) {
						if (colors [i] != null)
							c = colors [i];
						else
							c = new Graphics.Color ("black");
						g = distance [i];
					} else {
						c = new Graphics.Color ("gray");
						g = 1.0;
					}
					double red, green, blue;
					for (int h = 0; h < 192; h++) {
						if (h >= (int)(192 / 2.0 - g * 192 / 2.0) && h <= (int)(192 / 2.0 + g * 192 / 2.0)) {
							red = c.red * g;
							green = c.green * g;
							blue = c.blue * g;
						} else if (h < (int)(192 / 2.0 - g * 192 / 2.0)) {
							red = sky.red;
							green = sky.green;
							blue = sky.blue;
						} else {
							red = simulation.groundColor.red;
							green = simulation.groundColor.green;
							blue = simulation.groundColor.blue;
						}
						if (Math.Max (Math.Max (red, green), blue) > 180)
							counts [Math.Min (i / (256 / 3), 2)] = ((int)counts [Math.Min (i / (256 / 3), 2)]) + 1;
					}
				}
			}
			return counts;
		}
    
		public override object getBright (int window)
		{
			return ((List)getBright ()) [window];
		}
    
		public override object getLine (params object [] position)
		{
			return Graphics.PyList (0, 0);
		}
    
		public override object get (string sensor="all")
		{
			return new List ();
		}
    
		public override object get (string sensor="all", params object [] position)
		{
			return new List ();
		}
    
		public override string getPassword ()
		{
			return "";
		}
    
		public override PythonDictionary getConfig ()
		{
			return new PythonDictionary ();
		}
    
		public override int getStall ()
		{
			return stall ? 1 : 0; 
		}
    
		public override double getBattery ()
		{
			return battery;
		}
    
		public override void setLED (string position, object value)
		{
		}
    
		public override void setLEDFront (object value)
		{
		}
    
		public override void setLEDBack (double value)
		{
		}
    
		public override void setEchoMode (int value)
		{
		}
    
		public override void setName (string name)
		{
			this.name = name;
		}
    
		public override void setIRPower (int power)
		{
		}
    
		public override void setWhiteBalance (object value)
		{
		}
    
		public override void setForwardness (object value)
		{
		}
    
		public override object getForwardness ()
		{
		  return "fluke-forward";
		}

		public override void setVolume (object volume)
		{
		}
    
		public override void setPassword (string password)
		{
		}

		public override void draw_simulation() {
		    float MeterInPixels = 64.0f;
		    lock (this) {
			this.stall = false;
			this.frame.body.LinearVelocity = Graphics.VectorRotate (
										 Graphics.Vector (this.velocity, 0), 
										 this.frame.body.Rotation);
			// Get sensor readings
			this.readings.clear ();
			if (!simulation.window.isRealized())
			    return;
			Graphics.Point p1 = null;
			Graphics.Point p2 = null;
			lock (simulation.window.canvas.shapes) {
			    foreach (KeyValuePair<object,object> kvp in this.sensors) {
				string key = (string)kvp.Key;
				Graphics.Line line = (Graphics.Line)kvp.Value;
				p1 = this.frame.getScreenPoint (line.getP1 ());
				p2 = this.frame.getScreenPoint (line.getP2 ());
				if (!simulation.window.isRealized())
				    return;
				simulation.window.canvas.world.RayCast ((fixture, v1, v2, hit) => {  
					this.readings [key] = hit;
					return 1; 
				    }, 
				    Graphics.Vector (((float)p1.x) / MeterInPixels, 
						     ((float)p1.y) / MeterInPixels), 
				    Graphics.Vector (((float)p2.x) / MeterInPixels, 
						     ((float)p2.y) / MeterInPixels));
			    }
			    foreach (Graphics.Shape light in simulation.lights) {
				foreach (Graphics.Shape light_sensor in this.light_sensors) {
				    Graphics.Point c = new Graphics.Point (light_sensor.center);
				    c.x -= 6; // hack to get outside of bounding box
				    p1 = this.frame.getScreenPoint (c);
				    p2 = light.center;
				    if (!simulation.window.isRealized())
					return;
				    simulation.window.canvas.world.RayCast ((fixture, v1, v2, hit) => {  
					    this.readings [light_sensor.tag] = hit;
					    return 1; 
					}, 
					Graphics.Vector (((float)p1.x) / MeterInPixels, 
							 ((float)p1.y) / MeterInPixels), 
					Graphics.Vector (((float)p2.x) / MeterInPixels, 
							 ((float)p2.y) / MeterInPixels));
				    if (this.readings.Contains (light_sensor.tag)) {
					this.readings [light_sensor.tag] = (float)5000.0; // blocked
				    } else {
					this.readings [light_sensor.tag] = ((float)Math.Min (Math.Pow (p1.distance (p2) / 10.0, 2), 5000));
				    }
				}
			    }
			}
			
			
		    }
		}
	} // SimScribbler

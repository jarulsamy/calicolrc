using IronPython.Runtime;
using System.Collections;
using System.Collections.Generic;
using System.IO.Ports;
using System.Threading;
using System;
using Microsoft.Xna.Framework; // Vector2, Matrix
//using System.Math.Cos;

public class SimScribblerRounded : Myro.Robot
	{
	    public Graphics.Polygon frame;
		//public Graphics.Circle frame;
		public Myro.Simulation simulation;
		public double velocity = 0;
        public float rvelocity = 0; //JH: Experimental
		public List<Action> queue = new List<Action>();
		public double rate = 8.0;
		public bool stall = false;
		public string name = "Scribby";
		public double battery = 7.6;
		public PythonDictionary sensors = new PythonDictionary ();
		public PythonDictionary readings = new PythonDictionary ();
		public List<Graphics.Shape> light_sensors = new List<Graphics.Shape> ();
		public bool show_sensors = false;
        public double [] blobRLE = { 0,254,51,136,190,254};
		string _forwardness = "fluke-forward";

		public SimScribblerRounded (Myro.Simulation simulation)
		{
			this.simulation = simulation;
			frame = new Graphics.Polygon (new Graphics.Point (320 - 8, 240 + 23),
							new Graphics.Point (320 + 8, 240 + 23),
							//new Graphics.Point (320 + 17, 240 + 12),
							new Graphics.Point (320 + 20, 240 + 10),
							new Graphics.Point (320 + 20, 240 - 10),
							new Graphics.Point (320 + 8, 240 - 23),
							new Graphics.Point (320 - 8, 240 - 23),
							new Graphics.Point (320 - 20, 240 - 10),
							//new Graphics.Point (320 - 20, 240 + 8),
							//new Graphics.Point (320 - 20, 240 + 12),
							new Graphics.Point (320 - 17, 240 + 10));

			//frame = new Graphics.Rectangle (new Graphics.Point (320 - 23, 240 - 23),
			//	     new Graphics.Point (320 + 23, 240 + 23));
			//frame = new Graphics.Circle (new Graphics.Point (320, 240), 23);
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
			body.fill = Myro.makeColor ("red");
			body.draw (frame);
			// Draw wheels:
			Graphics.Rectangle wheel1 = new Graphics.Rectangle (new Graphics.Point (-10, -23),
							 new Graphics.Point (10, -17));
			wheel1.color = Myro.makeColor ("black");
			wheel1.draw (frame);
			Graphics.Rectangle wheel2 = new Graphics.Rectangle (new Graphics.Point (-10, 23),
							 new Graphics.Point (10, 17));
			wheel2.color = Myro.makeColor ("black");
			wheel2.draw (frame);
      
			// Details
			Graphics.Circle hole = new Graphics.Circle (new Graphics.Point (0, 0), 3);
			hole.fill = Myro.makeColor ("black");
			hole.draw (frame);
      
			Graphics.Rectangle fluke = new Graphics.Rectangle (new Graphics.Point (15, -10),
							new Graphics.Point (19, 10));
			fluke.color = Myro.makeColor ("green");
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
			int ir_range = 300;
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

			ir_range = 50;
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

			ir_range = 5;
			// sensors getLine("right")
			v2 = Graphics.VectorRotate (Graphics.Vector (ir_range, 0), -180 * Math.PI / 180);
			line = new Graphics.Line (new Graphics.Point (-12, -5.00), 
			       new Graphics.Point (-12 + v2.X, -5.00 + v2.Y));
			line.outline = new Graphics.Color (0, 0, 255, 64);
			line.visible = false;
			line.border=3;
			// Visualization of sensor
			pie = new Graphics.Pie (line.points [0],
			     ir_range, -5, 5);
			pie.rotate (180);
			pie.fill = new Graphics.Color (0, 0, 255, 64);
			// FIXME: outline can't be null
			pie.outline = new Graphics.Color (0, 0, 255, 64);
			pie.draw (line);
			line.draw (frame);
			sensors ["line-right"] = line;

			// sensors getLine("left")
			v2 = Graphics.VectorRotate (Graphics.Vector (ir_range, 0), -180 * Math.PI / 180);
			line = new Graphics.Line (new Graphics.Point (-12, 5.00), 
			       new Graphics.Point (-12 + v2.X, 5.00 + v2.Y));
			line.outline = new Graphics.Color (0, 0, 255, 64);
			line.visible = false;
			line.border=3;
			// Visualization of sensor
			pie = new Graphics.Pie (line.points [0],
			     ir_range, -5, 5);
			pie.rotate (180);
			pie.fill = new Graphics.Color (0, 0, 255, 64);
			// FIXME: outline can't be null
			pie.outline = new Graphics.Color (0, 0, 255, 64);
			pie.draw (line);
			line.draw (frame);
			sensors ["line-left"] = line;


			// Just the fill, to see outline of bounding box:
			frame.fill = null;
			// FIXME: something not closing correctly in render when :
			//frame.color = null;
			frame.outline = simulation.groundColor;
			// set collision
			frame.draw (simulation.window);
			frame.tag = "robot";
			frame.body.OnCollision += SetStall;
			frame.body.IgnoreGravity = true;
			lock (this.simulation.robots) {
			    this.simulation.robots.Add (this);
			}
			setup();
		}

		public override void uninit() {
		    frame.undraw();
		}

		bool SetStall (FarseerPhysics.Dynamics.Fixture fixture1,
		  FarseerPhysics.Dynamics.Fixture ficture2,
		  FarseerPhysics.Dynamics.Contacts.Contact contact)
		{
			stall = true;
			return true;
		}

		double standard_range(double value) {
		    while (value < 0)
			value += 2 * Math.PI;
		    while (value > 2 * Math.PI)
			value -= 2 * Math.PI;
		    return value;
		}

		string getTurnDirection(double current, double target) {
		    current = standard_range(current) + 2.0 * Math.PI;
		    target = standard_range(target) + 2.0 * Math.PI; // 360 to 720
		    if (Math.Abs(current -target) < .01) { // same, 1 degree
			return "same";
		    } else if (current < target) { // to turn right
			if (target - current < Math.PI) {
			    return "right";
			} else {
			    return "left";
			}
		    } else { // turn to left
			if (current - target < Math.PI) {
			    return "left";
			} else {
			    return "right";
			}
		    }
		}

		bool isDone(string direction, double current, double target) {
		    //System.Console.WriteLine(String.Format("isDone: current={0}, target={1}, direction={2}, getTurn={3}", 
		    //						   current, target, direction,
		    //					   getTurnDirection(current, target)));
		    return getTurnDirection(current, target) != direction;
		}
		
		public override int getAngle() {
		    // get angle in degrees
		    return (int)frame.rotation;
		}

		public double getRotation() {
		    // get angle in radians
		    return frame.body.Rotation;
		}

		public void setAngle(double target) {
		    // set angle in degrees
		    frame.rotation = target;
		}

		public void setRotation(double target) {
		    // set angle in radians
		    frame.body.Rotation = (float)target;
		}

		public override void turnBy (int angle, string units = "deg")
		{
		    double target;
		    if (units == "deg") {
			target = -(Math.PI / 180.0) * angle; // degrees to rad
		    } else {
			target = -angle; // radians
		    }
		    double current_rotation = getRotation(); // convert from degrees
		    _turnTo((int)(standard_range(current_rotation + target) * 180/Math.PI - 360));
		}
		
		public override void turnTo (int angle, string units = "deg") 
		{
		    _turnTo(-angle, units);
		}

		public void _turnTo (int angle, string units = "deg")
		{
		    // uses internal idea of what direction it is pointing to (dead-reckoning)
		    ManualResetEvent ev = new ManualResetEvent(false);
		    // angle is positive (0 to 3.1415, or 0 to 360)
		    double target;
		    if (units == "deg") {
			target = (Math.PI / 180.0) * angle; // degrees to rad
		    } else {
			target = angle; // radians
		    }
		    double current_rotation = getRotation(); // convert from degrees
		    string direction = getTurnDirection(current_rotation, target);
		    if (direction == "same") {
			return;
		    } else if (direction == "right") {
			turnRight(.7); // right, increasing
		    } else {
			turnLeft(.7); // left, decreasing
		    }
		    continueTurning(direction, target, ev);
		    ev.WaitOne();
		}
		
		public void continueTurning(string direction, double target, ManualResetEvent ev) {
		    lock (queue) {
			queue.Add(delegate {
				if (frame.body.AngularVelocity != 0.0) { // still moving
				    if (isDone(direction, getRotation(), target)) {
					frame.body.AngularVelocity = 0.0f;
					setRotation(target); // target is in radians
					ev.Set();
				    } else {
					lock (queue) {
					    queue.Add(delegate {
						    continueTurning(direction, target, ev);
						});
					}
				    }
				} else {
				    // offically stop (must have stopped through another means)
				    stop();
				    ev.Set();
				}
			    });
		    }
		}
		
		public override void adjustSpeed () {
		    lock (queue) {
			queue.Add(delegate {
				  if (_forwardness == "fluke-forward") {
					velocity = _lastTranslate * rate;
				  } else {
					velocity = _lastTranslate * rate * -1;
				  }
                  rvelocity = (float)(-_lastRotate * rate); //JH: Experimental
				  frame.body.AngularVelocity = (float)(-_lastRotate * rate);
				  frame.body.LinearVelocity = Graphics.VectorRotate (
						    Graphics.Vector (velocity, 0), 
						    frame.body.Rotation);
			    });
		    }
		}

	    public override void setPose(int x, int y, double theta) {
		  lock(frame.world) {
		        frame.center.x = x;
		        frame.center.y = y;
			setAngle(theta); // theta is in degrees
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

		//public override void penDown (string color)
		 //{
		//	frame.outline = new Graphics.Color (color);
		//	frame.setPenColor(new Graphics.Color(color));
		//	frame.penDown ();
	  ///
		//}

		//public override void penDown (object color)
		 //{
		  // frame.outline = color;
		  // frame.setPenColor(color);
		  // frame.penDown ();
	         //
		//}

		public override Graphics.Line penUp ()
		{ 
			return penUp (null);
		}

		public override Graphics.Line penUp (string fillColor)
		{ 
			return frame.penUp (fillColor);
		}

		public override void update () {
		    // Go through all of the delegates that have built up, and run them
		    List<Action> copy = new List<Action>();
		    lock (queue) {
			foreach(Action function in queue) {
			    copy.Add(function);
			}
			queue.Clear();
		    }
		    foreach(Action function in copy) {
			function();
		    }
		}

		public override Graphics.Picture takePicture (string mode="jpeg") {
		    Graphics.Picture pic = null;
		    ManualResetEvent ev = new ManualResetEvent(false);
		    Exception exception = null;
		    // Lock the queue, as this method can fire at any time
		    lock (queue) {
			// Add a delegate to the queue, which will execute when appropriate
			queue.Add(delegate {
				try {
				    pic = _takePicture(mode);
				} catch (Exception e) {
				    exception = e;
				}
				ev.Set();
			    });
		    }
		    // Wait for delegate to fire
		    ev.WaitOne();
		    // And return picture
		    if (exception != null)
			throw exception;
		    return pic;
		}

		public class Hit {
		    public Graphics.Shape shape;
		    public double distance;
		    public Hit(Graphics.Shape shape, double distance) {
			this.shape = shape;
			this.distance = distance;
		    }
		}

		public Graphics.Picture _takePicture (string mode="jpeg")
		{ 
		    // simscribbler camera
		    int band = 8; // width of a vertical color, one band per ray
		    Graphics.Picture picture = new Graphics.Picture (256, 192);
		    double view_angle = 60.0; // degrees
		    double max_distance = 20.0;
		    float MeterInPixels = 64.0f;
		    if (!simulation.window.isRealized())
			return picture;
		    List<Hit> [] hits = new List<Hit>[256];
		    Graphics.Point p1 = frame.getScreenPoint (new Graphics.Point (25, 0));
		    for (int i = 0; i < 256; i += band) {
			var v = Graphics.VectorRotate (Graphics.Vector (max_distance * MeterInPixels, 0), 
						       (float)(((i / 256.0) * view_angle) - view_angle / 2.0) * Math.PI / 180.0);
			Graphics.Point p2 = frame.getScreenPoint (new Graphics.Point (25 + v.X, v.Y));
			simulation.window.canvas.world.RayCast ((fixture, v1, v2, hit) => {  
				if (fixture.UserData is Graphics.Shape) {
				    double distance = 1.0 - Math.Min (hit * max_distance, max_distance) / max_distance; /// 10 x car
				    // if not a list, make one
				    if (hits[i] == null) {
					hits[i] = new List<Hit>();
				    }
				    // add to list, with smallest (furthest) first
				    int pos = 0;
				    while (pos < hits[i].Count && distance > hits[i][pos].distance) {
					pos++;
				    }
				    hits[i].Insert(pos, new Hit((Graphics.Shape)fixture.UserData, distance));
				}
				return 1; 
			    }, 
			    Graphics.Vector ((float)(p1.x / MeterInPixels), (float)(p1.y / MeterInPixels)), 
			    Graphics.Vector ((float)(p2.x / MeterInPixels), (float)(p2.y / MeterInPixels)));
		    }
		    double center = 192 / 2.0;
		    int low = 0, high = 0, zero = 0;
		    Graphics.Color sky = new Graphics.Color ("deepskyblue");
		    ManualResetEvent ev = new ManualResetEvent(false);
		    Myro.InvokeBlocking (delegate {
			    Graphics.Color [] column = new Graphics.Color [192];
			    int h = 0;
			    // Compute colors
			    for (int i = 0; i < 256; i += band) { // width
				// background:
				for (h = 0; h < 192; h++) { // height
				    if (h < center) { // sky
					column[h] = sky;
				    } else { // ground
					column[h] = simulation.groundColor;
				    }
				}
				// layer shapes on background, furtherest to closest
				if (hits[i] != null) {
				    // draw half above, half below, normally
				    //low = (int)(center - g * center);  // position of top
				    // low is near top
				    // high is near bottom
				    foreach (Hit hit in hits[i]) {
					Graphics.Shape shape = hit.shape;
					double g = hit.distance;
					zero = (int)(center + g * center); // position of ground
					high = (int)(zero - (shape.z * 192 * g)); // distance above ground, 0 to 1
					low = (int)(high - (shape.zHeight * 192 * g)); // height of shape, 0 to 1
					for (h = low; h < high; h++) {
					  column[h] = new Graphics.Color (shape.color.red * g, 
									    shape.color.green * g, 
									    shape.color.blue * g);
					}
				    }
				}
				// now draw the column here:
				for (h = 0; h < 192; h++) {
				    for (int ii=0; ii<band; ii++) {
					picture.setColor (i + ii, h, column[h]);
				    }
				}
				ev.Set();
			    }
			});
		    ev.WaitOne ();
		    //Does a thresholdhing based on the value sin blobRLE
		    if(mode=="blob")
		      {
			for(int i =0;i<picture.width;i++)
			  {
			    for(int j=0;j<picture.height;j++)
			      {
				PythonTuple rgb = picture.getPixel (i, j).getRGB ();
				List yuv = Myro.rgb2yuv ((int)rgb [0], (int)rgb [1], (int)rgb [2]);
				if( (int)yuv[0] >= blobRLE[0] && (int)yuv[0] <= blobRLE[1] &&  
				    (int)yuv[1] >= blobRLE[2] && (int)yuv[1] <= blobRLE[3]  &&
				    (int)yuv[2] >= blobRLE[4] && (int)yuv[2]<= blobRLE[5])
				  {
				    picture.setRGB(i,j,0,0,0);				    
				  }
				else
				  picture.setRGB(i,j,255,255,255);
			      }
			  }
		      }
		    return picture;
	        }
    

	  public PythonTuple set_blob_yuv (Graphics.Picture picture, int x1, int y1, int x2, int y2)
	  {
	    int [] xs = new int[2]; //[x1,x2];
	    int [] ys = new int[2]; //[y1,y2];
	    xs [0] = Math.Min (x1, x2);
	    xs [1] = Math.Max (x1, x2);
	    ys [0] = Math.Min (y1, y2);
	    ys [1] = Math.Max (y1, y2);
	    
	    //set up variables to hold counts and accumulations:
	    double totalY = 0.0;
	    double totalU = 0.0;
	    double totalV = 0.0;
	    
	    List ySamples = new List ();
	    List uSamples = new List ();
	    List vSamples = new List ();
	    
	    for (int i=xs[0]; i < xs[1]; i++) {
	      for (int j=ys[0]; j < ys[1]; j++) {
                PythonTuple rgb = picture.getPixel (i, j).getRGB ();
                List yuv = Myro.rgb2yuv ((int)rgb [0], (int)rgb [1], (int)rgb [2]);
                totalY = totalY + (int)yuv [0];
                totalU = totalU + (int)yuv [1];
                totalV = totalV + (int)yuv [2];
                ySamples.append ((int)yuv [0]);
                uSamples.append ((int)yuv [1]);
                vSamples.append ((int)yuv [2]);
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
	      sY = sY + ((int)ySamples [i] - yMean) * ((int)ySamples [i] - yMean);
	      sU = sU + ((int)uSamples [i] - uMean) * ((int)uSamples [i] - uMean);
	      sV = sV + ((int)vSamples [i] - vMean) * ((int)vSamples [i] - vMean);
	    }
	    
	    sY = Math.Sqrt (sY / count);
	    sU = Math.Sqrt (sU / count);
	    sV = Math.Sqrt (sV / count);
	    
	    // Select the U/V bounding box based upon stdMod stdDev
	    // from the mean, with approripate
	    // min/max values to fit in an 8 bit register.
	    //
	    double stdMod = 3.0;
	    
	    int minU = (int)Math.Max (0, (uMean - sU * stdMod));
	    int maxU = (int)Math.Min (255, (uMean + sU * stdMod));
	    int minV = (int)Math.Max (0, (vMean - sV * stdMod));
	    int maxV = (int)Math.Min (255, (vMean + sV * stdMod));
	    
	    // Note that we use the default values for
	    // several parameters, most importantly the Y value
	    // defaults to a range of 0-254
	    conf_rle (0, 254, minU, maxU, minV, maxV);
	    
	    // Return a tupal of parameters suitable for the configureBlob
	    // function, to be shown to the user.
	    
	    return Graphics.PyTuple (0, 254, minU, maxU, minV, maxV);
	    
	  }
	  public override void configureBlob (int y_low, int y_high, int u_low, int u_high, int v_low, int v_high)
	  {
	    conf_rle(y_low, y_high, u_low, u_high, v_low, v_high);
	    //return Graphics.PyTuple(y_low,y_high,u_low,u_high,v_low,v_high);
	  }
	  
	  public void conf_rle (int y_low=0, int y_high=254,
				int u_low=51, int u_high=136,
				int v_low=190, int v_high=254, int delay = 90, int smooth_thresh = 4)
	  {
	    blobRLE[0]=y_low;
	    blobRLE[1]=y_high;
	    blobRLE[2]=u_low;
	    blobRLE[3]=u_high;
	    blobRLE[4]=v_low;
	    blobRLE[5]=v_high;
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
    
		public override object getObstacle (params object [] positions) {
		    object retval = null;
		    ManualResetEvent ev = new ManualResetEvent(false);
		    Exception exception = null;
		    // Lock the queue, as this method can fire at any time
		    lock (queue) {
			// Add a delegate to the queue, which will execute when appropriate
			queue.Add(delegate {
				try {
				    retval = _getObstacle(positions);
				} catch (Exception e) {
				    exception = e;
				}
				ev.Set();
			    });
		    }
		    // Wait for delegate to fire
		    ev.WaitOne();
		    if (exception != null)
			throw exception;
		    // And return picture
		    return retval;
	        }

		public object _getObstacle (params object [] positions)
		{
			string key = null; 
			List retval = new List ();
			if (positions.Length == 0)
				positions = new object[3] {0, 1, 2};
			else if ((positions.Length == 1) && (is_string(positions [0]) && (positions [0].ToString()) == "all"))
				positions = new object[3] {0, 1, 2};
			foreach (object position in positions) {
			    if (position is int) {
				if (System.Convert.ToInt32(position) == 0) {
				    key = "obstacle-left";
				} else if (System.Convert.ToInt32(position) == 1) {
				    key = "obstacle-center";
				} else if (System.Convert.ToInt32(position) == 2) {
				    key = "obstacle-right";
				} else {
				    throw new Exception ("invalid position in getObstacle()");
				}
			    } else if (position is double) {
				if (System.Convert.ToDouble(position) == 0) {
				    key = "obstacle-left";
				} else if ((System.Convert.ToDouble(position)) == 1) {
				    key = "obstacle-center";
				} else if ((System.Convert.ToDouble(position)) == 2) {
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
			    // ---- now get the data
			    if (readings.Contains (key)) {
				// distance is a percentage of length of line (300)
				float percent = (float)(readings [key]);
				double x_in = (percent * 300.0) / 50.0; // scribbler bodies
				// 6400: 0-3 scribbler bodies 1920: 3-5 scribbler bodies 0: >5 scribbler bodies.
				// body is 50 pixels

				// coefficients
				double a = 6.3999999999999927E+03;
				double b = -1.8133333333333314E+03;
				double c = 1.0666666666666652E+02;
				double value = Math.Max(a + b * x_in + c * Math.Pow(x_in, 2.0), 0);

				retval.append ((float)value);
			    } else {
				retval.append (0);
			    }
			}
			if (retval.Count == 1)
			    return retval [0];
			else 
			    return retval;
		}

		public override object getDistance (params object [] positions) {
		    object retval = null;
		    ManualResetEvent ev = new ManualResetEvent(false);
		    Exception exception = null;
		    // Lock the queue, as this method can fire at any time
		    lock (queue) {
			// Add a delegate to the queue, which will execute when appropriate
			queue.Add(delegate {
				try {
				    retval = _getDistance(positions);
				} catch (Exception e) {
				    exception = e;
				}
				ev.Set();
			    });
		    }
		    // Wait for delegate to fire
		    ev.WaitOne();
		    if (exception != null)
			throw exception;
		    // And return picture
		    return retval;
	        }

	  public object _getDistance (params object [] positions)
	  {
	    
	    
	    string key = null; 
	    List retval = new List ();
	    
	    if (positions.Length == 0)
	      positions = new object[2] {0, 1};
	    else if ((positions.Length == 1) && (positions [0] is string) && ((string)(positions [0]) == "all"))
	      positions = new object[2] {0, 1};
	    foreach (object position in positions) {
	      if (position is int) {
		if (System.Convert.ToInt32(position) == 0) {
		  //key = "distance-left";
		  key = "ir-left";
		} else if (System.Convert.ToInt32(position) == 1) {
		  //key = "distance-right";
		  key = "ir-right";
		} else {
		  throw new Exception ("invalid position in getDistance()");
		}
	      } else if (position is double) {
		if (System.Convert.ToDouble(position) == 0) {
		  //key = "distance-left";
		  key = "ir-left";
		} else if (System.Convert.ToDouble(position) == 1) {
		  //key = "distance-right";
		  key = "ir-right";
		} else {
		  throw new Exception ("invalid position in getDistance()");
		}
	      } else if (position is string) {
		if (((string)position) == "left") {
		  //key = "distance-left";
		  key = "ir-left";
		} else if (((string)position) == "right") {
		  //key = "distance-right";
		  key = "ir-right";
		} else {
		  throw new Exception ("invalid position in getDistance()");
		}
	      } else {
		throw new Exception ("invalid position in getDistance()");
	      }
	      //if (readings.Contains (key)) {
	      //	retval.append (readings [key]);
	      //    } else {
	      //retval.append (0);
	      // }
	      // ---- now get the data
	      if (readings.Contains (key)) {

		// distance is a percentage of length of line (300)
		float percent = (float)(readings [key]);

		//double x_in = (percent * 300.0) / 50.0; // scribbler bodies
		//// 6400: 0-3 scribbler bodies 1920: 3-5 scribbler bodies 0: >5 scribbler bodies.
		//// body is 50 pixels
		
		//// coefficients
		//double a = 6.3999999999999927E+03;
		//double b = -1.8133333333333314E+03;
		//double c = 1.0666666666666652E+02;
		//double value = Math.Max(a + b * x_in + c * Math.Pow(x_in, 2.0), 0);
		
		retval.append (100*percent);//(float)value);
	      } else {
		retval.append (99);
		
	      }
	    }
	    if (retval.Count == 1)
	      return retval [0];
	    else 
	      return retval;
	    	    
	  }

	  
		public override object getLight (params object [] positions) {
		    object retval = null;
		    ManualResetEvent ev = new ManualResetEvent(false);
		    Exception exception = null;
		    // Lock the queue, as this method can fire at any time
		    lock (queue) {
			// Add a delegate to the queue, which will execute when appropriate
			queue.Add(delegate {
				try {
				    retval = _getLight(positions);
				} catch (Exception e) {
				    exception = e;
				}
				ev.Set();
			    });
		    }
		    // Wait for delegate to fire
		    ev.WaitOne();
		    if (exception != null)
			throw exception;
		    // And return picture
		    return retval;
	        }

		public object _getLight (params object [] positions)
		{
			string key = null; 
			List retval = new List ();
			if (positions.Length == 0)
				positions = new object[3] {0, 1, 2};
			else if ((positions.Length == 1) && (positions [0] is string) && ((string)(positions [0]) == "all"))
				positions = new object[3] {0, 1, 2};
				foreach (object position in positions) {
					if (position is int) {
						if (System.Convert.ToInt32(position) == 0) {
							key = "light-left";
						} else if (System.Convert.ToInt32(position) == 1) {
							key = "light-center";
						} else if (System.Convert.ToInt32(position) == 2) {
							key = "light-right";
						} else {
							throw new Exception ("invalid position in getLight()");
						}
					} else if (position is double) {
						if (System.Convert.ToDouble(position) == 0) {
							key = "light-left";
						} else if (System.Convert.ToDouble(position) == 1) {
							key = "light-center";
						} else if (System.Convert.ToDouble(position) == 2) {
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
			if (retval.Count == 1)
				return retval [0];
			else 
				return retval;
		}
    
		public override object getIR (params object [] positions) {
		    object retval = null;
		    ManualResetEvent ev = new ManualResetEvent(false);
		    Exception exception = null;
		    // Lock the queue, as this method can fire at any time
		    lock (queue) {
			// Add a delegate to the queue, which will execute when appropriate
			queue.Add(delegate {
				try {
				    retval = _getIR(positions);
				} catch (Exception e) {
				    exception = e;
				}
				ev.Set();
			    });
		    }
		    // Wait for delegate to fire
		    ev.WaitOne();
		    if (exception != null)
			throw exception;
		    // And return picture
		    return retval;
	        }

	  public object _getIR (params object [] positions)
	  {
	    string key = null; 
	    List retval = new List ();
	    if (positions.Length == 0)
	      positions = new object[2] {0, 1};
	    else if ((positions.Length == 1) && (positions [0] is string) && ((string)(positions [0]) == "all"))
	      positions = new object[2] {0, 1};
	    foreach (object position in positions) {
	      if (position is int) {
		if (System.Convert.ToInt32(position) == 0) {
		  key = "ir-left";
		} else if (System.Convert.ToInt32(position) == 1) {
		  key = "ir-right";
		} else {
		  throw new Exception ("invalid position in getIR()");
		}
	      } else if (position is double) {
		if (System.Convert.ToDouble(position) == 0) {
		  key = "ir-left";
		} else if (System.Convert.ToDouble(position) == 1) {
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
	    if (retval.Count == 1)
	      return retval [0];
	    else 
	      return retval;
	  }
	  
		public override object getBright (string window) {
		    object retval = null;
		    ManualResetEvent ev = new ManualResetEvent(false);
		    Exception exception = null;
		    // Lock the queue, as this method can fire at any time
		    lock (queue) {
			// Add a delegate to the queue, which will execute when appropriate
			queue.Add(delegate {
				try {
				    retval = _getBright(window);
				} catch (Exception e) {
				    exception = e;
				}
				ev.Set();
			    });
		    }
		    // Wait for delegate to fire
		    ev.WaitOne();
		    if (exception != null) 
			throw exception;
		    // And return picture
		    return retval;
	        }

		public object _getBright (string window)
		{
			if (window == "left")
				return ((List)_getBright ()) [0];
			else if (window == "center")
				return ((List)_getBright ()) [1];
			else if (window == "right")
				return ((List)_getBright ()) [2];
			else if (window == "all")
				return _getBright ();
			else
				throw new Exception ("invalid argument to getBright()");
		}

		public override object getBright () {
		    object retval = null;
		    ManualResetEvent ev = new ManualResetEvent(false);
		    Exception exception = null;
		    // Lock the queue, as this method can fire at any time
		    lock (queue) {
			// Add a delegate to the queue, which will execute when appropriate
			queue.Add(delegate {
				try {
				    retval = _getBright();
				} catch (Exception e) {
				    exception = e;
				}
				ev.Set();
			    });
		    }
		    // Wait for delegate to fire
		    ev.WaitOne();
		    if (exception != null)
			throw exception;
		    // And return picture
		    return retval;
	        }

		public object _getBright ()
		{
			// simscribbler camera
			double view_angle = 60.0; // degrees
			double max_distance = 20.0;
			float MeterInPixels = 64.0f;
			List counts = Graphics.PyList (0, 0, 0);
			if (!simulation.window.isRealized())
				return counts;
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
			return counts;
		}
    
		public override object getBright (int window) {
		    object retval = null;
		    ManualResetEvent ev = new ManualResetEvent(false);
		    Exception exception = null;
		    // Lock the queue, as this method can fire at any time
		    lock (queue) {
			// Add a delegate to the queue, which will execute when appropriate
			queue.Add(delegate {
				try {
				    retval = _getBright(window);
				} catch (Exception e) {
				    exception = e;
				}
				ev.Set();
			    });
		    }
		    // Wait for delegate to fire
		    ev.WaitOne();
		    if (exception != null)
			throw exception;
		    // And return picture
		    return retval;
	        }

		public object _getBright (int window)
		{
			return ((List)_getBright ()) [window];
		}
    
		public override object getLine (params object [] positions) {
		    object retval = null;
		    ManualResetEvent ev = new ManualResetEvent(false);
		    Exception exception = null;
		    // Lock the queue, as this method can fire at any time
		    lock (queue) {
			// Add a delegate to the queue, which will execute when appropriate
			queue.Add(delegate {
				try {
				    retval = _getLine(positions);
				} catch (Exception e) {
				    exception = e;
				}
				ev.Set();
			    });
		    }
		    // Wait for delegate to fire
		    ev.WaitOne();
		    if (exception != null)
			throw exception;
		    // And return picture
		    return retval;
	        }

		public override object getLocation ()
		{
		    object retval = null;
		    ManualResetEvent ev = new ManualResetEvent(false);
		    Exception exception = null;
		    // Lock the queue, as this method can fire at any time
		    lock (queue) {
			// Add a delegate to the queue, which will execute when appropriate
			queue.Add(delegate {
				try {
				    retval = _getLocation();
				} catch (Exception e) {
				    exception = e;
				}
				ev.Set();
			    });
		    }
		    // Wait for delegate to fire
		    ev.WaitOne();
		    if (exception != null)
			throw exception;
		    // And return picture
		    return retval;

		}

		public object _getLine (params object [] positions)
		{
		    
		  string key = null; 
		  List retval = new List ();
		  if (positions.Length == 0)
		    positions = new object[2] {0, 1};
		  else if ((positions.Length == 1) && (positions [0] is string) && ((string)(positions [0]) == "all"))
		    positions = new object[2] {0, 1};
		  foreach (object position in positions) {
		    if (position is int) {
		      if (System.Convert.ToInt32(position) == 0) {
			key = "line-left";
		      } else if (System.Convert.ToInt32(position) == 1) {
			key = "line-right";
		      } else {
			throw new Exception ("invalid position in getLine()");
		      }
		    } else if (position is double) {
		      if (System.Convert.ToDouble(position) == 0) {
			key = "line-left";
		      } else if (System.Convert.ToDouble(position) == 1) {
			key = "line-right";
		      } else {
			throw new Exception ("invalid position in getLine()");
		      }
		    } else if (position is string) {
		      if (((string)position) == "left") {
			key = "line-left";
		      } else if (((string)position) == "right") {
			key = "line-right";
		      } else {
			throw new Exception ("invalid position in getLine()");
		      }
		    } else {
		      throw new Exception ("invalid position in getLine()");
		    }
		    if (readings.Contains (key)) {
		      retval.append (0);
		    } else {
		      retval.append (1);
		    }
		  }
		  if (retval.Count == 1)
		    return retval [0];
		  else 
		    return retval;
		}
		      	
	  //return Graphics.PyList(rightSensor,leftSensor);
	  
          
	  
		//}    


		  /*
		 	//Location of sensors in body coordinates with zero rotation
		  	//Zero rotation is the robot facing to the right
		  double [] IR_Loc1 = new double[2] {-12,2.25};//1.25};
		  double [] IR_Loc2 = new double[2] {-12,-2.25};//-1.25};
		  
		  	//current position and rotation of robot
			double [] loc = new double[2]{frame.center.x, frame.center.y};
		    
		    double theta = frame.body.Rotation;

		    //rotates and translates body coordinates to world coordinates
		double [] newIR_Loc1 = new double[2] { Math.Cos(theta)*IR_Loc1[0] - Math.Sin(theta)*IR_Loc1[1] + loc[0], 
			Math.Sin(theta)*IR_Loc1[0] + Math.Cos(theta)*IR_Loc1[1] + loc[1] };

		double [] newIR_Loc2 = new double[2] { Math.Cos(theta)*IR_Loc2[0] - Math.Sin(theta)*IR_Loc2[1] + loc[0], 
			Math.Sin(theta)*IR_Loc2[0] + Math.Cos(theta)*IR_Loc2[1] + loc[1] };

		  //newIR_Loc1[1]);
		//System.Console.WriteLine(newIR_Loc2[0],newIR_Loc2[1]);


		int leftSensor = 0;
		int rightSensor = 0;
		bool isRobot = false;
		PythonTuple values;
		Graphics.Picture p;
		foreach (Graphics.Shape s in simulation.window.canvas.shapes) 
		      {
			if(s==frame)
			  isRobot=true;
			if(!isRobot && s is Graphics.Picture)
			  {				    
			    p = (Graphics.Picture)s;
			    //System.Console.WriteLine(s.tag);			    
			    if(p.hit((double)newIR_Loc1[0],(double)newIR_Loc1[1]) && p.tag=="Line")			      
			      {

				values=p.getRGBA((int)newIR_Loc1[0],(int)newIR_Loc1[1]);
				//values=(Graphics.Picture)s.getRGBA((double)newIR_Loc1[0],(double)newIR_Loc1[1]);
				if((int)values [0]==0 && (int)values [1]==0 && (int)values [2]==0 && (int)values [3]==255)
				  leftSensor = 1;				  
			      }

			    if(p.hit((double)newIR_Loc2[0],(double)newIR_Loc2[1]) && p.tag=="Line")
			      {
				values=p.getRGBA((int)newIR_Loc2[0],(int)newIR_Loc2[1]);
				//System.Console.WriteLine(s.getRGBA((double)newIR_Loc1[0],(double)newIR_Loc1[1]));
				//values=s.getRGBA((double)newIR_Loc2[0],(double)newIR_Loc2[1]);
				if((int)values [0]==0 && (int)values [1]==0 && (int)values [2]==0 && (int)values [3]==255)
				  rightSensor=1;


			      }
			      
			    isRobot = false;

			  }
			  */

		public object _getLocation ()
		{
			return Graphics.PyList (frame.center.x, frame.center.y);
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
    
		public override void setLEDBack (object value)
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
		  if (value != null) {
			if (value.ToString() == "fluke-forward")
			  _forwardness = "fluke-forward";
			else if (value.ToString() == "scribbler-forward")
			  _forwardness = "scribbler-forward";
			else
			  throw new Exception("invalid forward direction; should be 'fluke-forward' or 'scribbler-forward'");
		  }
		}
    
		public override object getForwardness ()
		{
		  return _forwardness;
		}

		public override void setVolume (object volume)
		{
		}
    
		public override void setPassword (string password)
		{
		}

		public override void update_simulation() {
		    this.frame.body.LinearVelocity = Graphics.VectorRotate (
				       Graphics.Vector (this.velocity, 0), 
				       this.frame.body.Rotation);

            frame.body.AngularVelocity = this.rvelocity;
		    float MeterInPixels = 64.0f;
		    this.stall = false;
		    // Get sensor readings
		    this.readings.clear ();
		    Graphics.Point p1 = null;
		    Graphics.Point p2 = null;
		    lock (simulation.window.canvas.shapes) {
			foreach (KeyValuePair<object,object> kvp in this.sensors) {
			    string key = (string)kvp.Key;
			    Graphics.Line line = (Graphics.Line)kvp.Value;
			    p1 = this.frame.getScreenPoint (line.getP1 ());
			    p2 = this.frame.getScreenPoint (line.getP2 ());

			    if(key=="line-right" || key=="line-left")
			      {
				List< FarseerPhysics.Dynamics.Fixture > fixtureList =
				  simulation.window.canvas.world.TestPointAll (Graphics.Vector (((float)p1.x) / MeterInPixels,((float)p1.y) / MeterInPixels));

				if(fixtureList != null)
				  {
				    foreach (FarseerPhysics.Dynamics.Fixture fixture in fixtureList) 
				      {
					if(fixture.IsSensor && fixture.UserData is Graphics.Shape)
					  {
					    Graphics.Shape s=(Graphics.Shape)fixture.UserData;
					    if(s.tag=="line")
					      this.readings[key]=1;
					  }
				      }
				  }
			      }
			    else
			      {
				simulation.window.canvas.world.RayCast ((fixture, v1, v2, hit) => {  
				    this.readings [key] = hit;
				    return 1; 
				  }, 
				  Graphics.Vector (((float)p1.x) / MeterInPixels, 
						   ((float)p1.y) / MeterInPixels), 
				  Graphics.Vector (((float)p2.x) / MeterInPixels, 
						   ((float)p2.y) / MeterInPixels));
				
			      }
			}
			foreach (Graphics.Shape light in simulation.lights) {
			    foreach (Graphics.Shape light_sensor in this.light_sensors) {
				Graphics.Point c = new Graphics.Point (light_sensor.center);
				c.x -= 6; // hack to get outside of bounding box
				p1 = this.frame.getScreenPoint (c);
				p2 = light.center;
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
		} // draw_simulation
	} // SimScribbler

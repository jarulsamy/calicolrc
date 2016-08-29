using IronPython.Runtime;
using System.Collections;
using System.Collections.Generic;
using System.IO.Ports;
using System.Threading;
using System;

using Calico;

public class Bioloid: Myro.Robot
{
  static string REVISION = "$Revision: $";
  public SerialPort serial = null;
  double[] _fudge = new double[4];
  double[] _oldFudge = new double[4];
  public string dongle;
  public int volume;
  public string startsong;
  public byte[] color_header = null;
  public byte[] gray_header = null;
  public byte emitters = 0x1 | 0x2 | 0x4;
  private int imagewidth = 256; // defaults for fluke1
  private int imageheight = 192;  
  
  
  //Basic movement commands
  static byte FORWARD=1;
  static byte BACKWARD=2;  
  static byte STOP=3;
  static byte TURNLEFT=4;
  static byte TURNRIGHT=8;
  
  //Handshaking and communication bytes
  static byte HANDSHAKE1=5;
  static byte HANDSHAKE2=6;
  static byte HANDSHAKE3=7;
  
  
  static byte GET_SERVO=9;
  static byte GET_ALL=10;
  
  //Can invoke the micellaneous special moves
  //like Strike, Claws, etc
  static byte SPECIAL_MOVE=11;
  //Special move bytes
  static byte CLAW=1;
  static byte STRIKE=2;
  static byte ALERT=3;
  static byte PLAYDEAD=4;
  
  
  static byte SET_LED=12;
  static byte SET_BEEP=13;
  
  //Size of outgoing packet 
  //that encapsulates a single byte/value
  static byte OUT_PACKET_LENGTH = 6;
  //Size of incoming packet where 
  //each packet is an individual number  
  static byte IN_PACKET_LENGTH = 8;
  static byte PACKET_LENGTH = 8;



  
  public Bioloid (SerialPort serial)
  {
    setup ();
  }
  
  public override void reinit(string port, int baud) {
    bool need_port = true;
    SerialPort serial = (((Bioloid)(Myro.robot)).serial as SerialPort);
    if (port.StartsWith ("COM") || port.StartsWith ("com")) {
      port = "\\\\.\\" + port;             
    }
    if (serial != null)
      {            
	if (serial.IsOpen) {
	  if (port == null) 
	    need_port = false;
	  else if (serial.PortName.Equals (port) && serial.BaudRate == baud) {
	    //need_port = false;
	    serial.Close (); // and need_port
	  } else {
	    // It exists, but wrong port/baud, so close it:
	    serial.Close (); // and need_port
	  }
	} else { // already closed
	  if ((serial.PortName.Equals (port) || port == null) && serial.BaudRate == baud) {
	    need_port = false;
	    serial.Open ();
	  } else {
	    need_port = true;
	  }
	}
      }
    if (need_port) {
      if (port == null) {
	port = (string)Myro.ask ("Port");
	if (port.StartsWith ("COM") || port.StartsWith ("com")) {
	  port = "\\\\.\\" + port;             
	}
      }
      if (port != null) {
	Myro.robot = new Bioloid (port, baud);
      } else {
	Console.WriteLine ("init() cancelled");
      }
    } else {
      Myro.robot.setup();
    }
  }
  
  public override void uninit() {
    lock (serial) {
      serial.Close (); // and need_port
    }
  }

  public Bioloid (string port):  this(port, 57600)
  {
  }
  
  public Bioloid (string port, int baud)
  {
    if (port.StartsWith ("COM") || port.StartsWith ("com")) {
      port = "\\\\.\\" + port;             
    }
    serial = new SerialPort (port, baud);
    lock (serial) {
            serial.ReadTimeout = 1000; // milliseconds
            serial.WriteTimeout = 5000; // milliseconds
            try {
	      serial.Open ();
            } catch {
	      Console.Error.WriteLine (String.Format ("ERROR: unable to open port '{0}'", 
						      port));
	      serial = null;
            }
    }
    if (serial != null)
      setup ();
  }
  
  public override void setup ()
  {
    
      
    bool handshakeSuccess=false;
    try {
      //TODO: Refactor for Bioloid
      handshakeSuccess = initHandshake();
    } catch {
      Console.WriteLine ("ERROR: unable to talk to Bioloid");
    }
    if(handshakeSuccess)
      Console.WriteLine ("Able to talk to Bioloid.");
    else
      {
	Console.WriteLine ("Unable to talk to Bioloid. Disconnecting serial communication.");
	uninit();
      }
    
    if(isConnected())
      {
	flush ();
	
	// only ask to beep if there is a robot attached.
	stop ();
	setLED("foo",true);
	beep (.1, 1);
	beep (.1, 2);
	beep (.1, 3);
	beep (.1, 4);
	beep (.1, 5);
	Console.WriteLine ("Hello, my name is Scorponok.");
      }
  }
  
  
  public bool initHandshake()
  {
    
    int old = serial.ReadTimeout; // milliseconds
    List retval = new List ();
    int retInt=0;
    // serial.setTimeout(4)
    flush ();
    
    lock(serial) serial.ReadTimeout = 4000; // milliseconds
    // have to do this twice since sometime the first echo isn't
    // echoed correctly (spaces) from the scribbler
    lock (this) 
      {//locks the robot
	write_packet(Bioloid.HANDSHAKE1);
	lock (serial) 
	  {
	    try 
	      {
		//retval = ReadMessage();	      
		retInt=(int)ReadMessage()[0];
	      }
	    catch 
	      {
		Console.WriteLine ("ERROR: Unable to read value from robot.");
		return false;
	      }
	  }
	
	//#print "Got", retval
	Myro.wait (.1); 
	//time.sleep(.1)
	
	if((byte)retInt==Bioloid.HANDSHAKE2)
	  {
	    //Complete the handshake by telling the robot
	    //that we've received it's message
	    lock(this)
	      write_packet(Bioloid.HANDSHAKE3);
	    lock(serial) serial.ReadTimeout = old;
	    return true;
	  }
	else
	  {
	    Console.WriteLine ("ERROR: Wrong handshake value returned from robot.");
	    return false;
	  }
      }
  }
  
  
  // ------------------------------------------------------------
  // Data structures:
  public PythonDictionary dict (params object [] list)
  {
    // make a dictionary from a list
    PythonDictionary retval = new PythonDictionary ();
    for (int i = 0; i < list.Length; i += 2) {
      retval [list [i]] = list [i + 1];
    }
    return retval;
  }
  
  public List list (params object [] items)
  {
    // make a list from an array
    List retval = new List ();
    for (int i = 0; i < items.Length; i++) {
      retval.append (items [i]);
    }
    return retval;
  }
  // ------------------------------------------------------------
  
  List bytes2ints (byte [] bytes)
  {
    List ints = new List ();
    for (int i = 0; i < bytes.Length; i++) {
      ints.append ((int)bytes [i]);
    }
    return ints;
  }
  
  byte [] GetBytes (byte value, int bytes)
  {
    byte [] retval = null;
    lock (this) { // lock robot
      write_packet (value);
      read (Bioloid.PACKET_LENGTH); // read the echo
      retval = read (bytes);
    }
    return retval;
  }
  
  List GetInt (byte[] value, int bytes)
  {
    List retval = new List ();
    byte [] retvalBytes;
    lock (this) { // lock robot
      write_packet (value);
      read (Bioloid.PACKET_LENGTH); // read the echo
      retvalBytes = read (bytes);
    }
    for (int p = 0; p < retvalBytes.Length; p += 4) {
      byte [] bigEndian = new byte[4];
      for (int i = 0; i < 4; i += 1) {
	bigEndian [i] = retvalBytes [p + 3 - i];
      }
      retval.append (BitConverter.ToInt32 (bigEndian, 0));
    }
    return retval;
  }
  
  List GetWord (byte value, int bytes)
  {
    List retval = new List ();
    byte [] retvalBytes;
    lock (this) { // lock robot
      write_packet (value);
      read (Bioloid.PACKET_LENGTH); // read the echo
      retvalBytes = read (bytes);
    }
    for (int p = 0; p < retvalBytes.Length; p += 2) {
      retval.append (retvalBytes [p] << 8 | retvalBytes [p + 1]);
    }
    return retval;
  }
  
  
  
  List GetByte (byte[] value, int bytes)
  {
    List retval = new List ();
    byte [] retvalBytes;
    lock (this) { // lock robot
      write_packet (value);
      read (Bioloid.PACKET_LENGTH); // read the echo
      retvalBytes = read (bytes);
    }
    for (int p = 0; p < retvalBytes.Length; p += 1) {
      retval.append ((int)retvalBytes [p]);
    }
    return retval;
  }
  
  
  public override void adjustSpeed ()
  {
    if(_lastTranslate==0 && _lastRotate==0)
      {
	write_packet(Bioloid.STOP);
      }
      else if(_lastTranslate != 0)
	{
	  if(_lastTranslate>0)
	    write_packet(Bioloid.FORWARD);
	  else
	    write_packet(Bioloid.BACKWARD);
	  
	}
      else
	{
	  if(_lastRotate<0)
	    write_packet(Bioloid.TURNLEFT);
	  else
	    write_packet(Bioloid.TURNRIGHT);
	  
	}
  }
  
  /*
    public void set (byte value, string s)
    {
        byte [] buffer = new byte[s.Length + 1];
        buffer [0] = value;
        for (int i = 0; i < s.Length; i++) {
            buffer [i + 1] = (byte)s [i];
        }
        set (buffer);
    }

    public void set (byte value, byte [] bytes)
    {
        byte [] buffer = new byte[bytes.Length + 1];
        buffer [0] = value;
        for (int i = 0; i < bytes.Length; i++) {
            buffer [i + 1] = (byte)bytes [i];
        }
        set (buffer);
    }

    public void set (params byte [] values)
    {
        lock (this) { // lock robot
            write_packet (values);
            _lastSensors = ReadMessage(); // single bit sensors
        }
    }

    public void set (string item, object position, object value)
    {
        string sposition = position.ToString();
        if (item == "led") 
	  {
	    if (isTrue (value))
	      set (Scribbler.SET_LED,1);
	    else
	      set (Scribbler.SET_LED,0);
          }
	else if(item="special")
	  {
	    set(Bioloid.SPECIAL_MOVES,(byte)position);
	  }
	
         else 
	   {
             //throw new Exception (String.Format ("invalid sensor name: '{0}'",sensor));
	     throw new Exception (String.Format ("invalid set item name: '{0}'", item));
	   }
    }

*/
  
  public override void beep (double duration, double frequency)
  {
    int note=(int)frequency;
    int buzzerTime=(int)(duration/0.1);
    if(note>31)
      note=31;
    if (buzzerTime>31)
      buzzerTime=31;
    write_packet(Bioloid.SET_BEEP,(byte)buzzerTime,(byte)note);
    
    // 100% of the intended delay
    Myro.wait (duration);
  }
  
  
  public override void setLED (string position, object value)
  {
    if((bool)value)
      write_packet (Bioloid.SET_LED, 1);
    else
      write_packet (Bioloid.SET_LED, 0);
  }

  public override void setServo(int id, float degree)
  {

    if(id<0)
      id=0;
    if(id>=1 && id<19)
      {
	flush();
	float ratio=1024/(float)360;
	int value=(int)(ratio*degree);
	write_packet((byte)(id+13), (byte)(value & 31),(byte)((value>>5) & 31));
      }
  }

  public override void servo(int id, int value)
  {

    if(id<0)
      id=0;
    
    if(id>=1 && id<19)
      {
	flush();
	write_packet((byte)(id+13), (byte)(value & 31),(byte)((value>>5) & 31));
      }
  }

  
  public override void special(string type,int value)
  {
    type = type.ToLower ();
    
    if(type=="claw")
      write_packet(SPECIAL_MOVE,CLAW);
    else if(type=="strike")
      write_packet(SPECIAL_MOVE,STRIKE);
    else if(type=="alert")
      write_packet(SPECIAL_MOVE,ALERT);
    else if(type=="playdead")
      write_packet(SPECIAL_MOVE,PLAYDEAD);
    else
      System.Console.WriteLine("Incorrect special move", type);
    
  }
  
  public override object getDistance (params object [] position)
  {
    return get("distance");
  }
  
  public override object getButton (params object [] position)
  {
    return get("button");
  }

  public override List getServo(int id)
  {
    
    if(id<0)
      id=0;

    
    if(id>=1 && id<19)
      {
	flush();
	write_packet(Bioloid.GET_SERVO,(byte)(id+13));
	List values=ReadMessage();
	return values;
      }
    return null;
  }
  
  public override int getMicrophone()
  {
    return (int)get("mic");
  }
  public override object get (string sensor="all")
  {
    return get (sensor, null);
  }
  
  
  
  
  public override object get (string sensor="all", params object [] position)
  {
    object retval = null;
    sensor = sensor.ToLower ();
    
    write_packet(Bioloid.GET_ALL);
    List values=ReadMessage();
    if (sensor == "distance") 
      {
	return values[1];
      }
    else if (sensor == "mic")
      {
	return values[2];
	
      }
    else if (sensor == "button")
      {
	return values[3];
	
      }
    else if (sensor == "all") 
      {
	
	return dict ("distance", list ((int)values[1]),
		     "mic", list ((int)values[2]),
		     "button", list ((int)values[3]));
      }
    else
      {
	throw new Exception (String.Format ("invalid sensor name: '{0}'",sensor));
      }
  }
  
  

    public override PythonDictionary getAll ()
    {
        return (PythonDictionary)get ("all");
    }



  

  public int read_2byte ()
  {
    byte hbyte = read_byte ();
    byte lbyte = read_byte ();
    return (int)((hbyte << 8) | lbyte);
  }

  public int read_3byte ()
  {
    byte hbyte = read_byte ();
    byte mbyte = read_byte ();
    byte lbyte = read_byte ();
    return (int)((hbyte << 16) | (mbyte << 8) | lbyte);
  }

    public string ReadLine ()
    {
      // Replaces serial.ReadLine() as it doesn't stop on \n
      string retval = "";
      byte b = 0;
      int counter = 0;
      do
	{	  
	  try{		
	    b = read_byte ();
	    retval += (char)b;
	  } catch{
	    break;
	    //return "n";	    
	    //System.Console.Error.WriteLine("Serial ReadLine error");
	  }
	  counter++;
	} while (b != 10 && counter < 1024);  // '\n' newline	  
	return (retval + "\n");
    }


  public List ReadMessage()
  {
    List ints = new List ();
    string retStr = "";

    byte b = 0;
    int counter = 0;

    for(int i=0;i<Bioloid.IN_PACKET_LENGTH;i++)
      {
	for (int j=0; j < 5; j++) 
	  {
	    try
	      {		
		b = read_byte ();
		retStr += (char)b;
	      }
	    catch
	      {
		System.Console.Error.WriteLine("Serial ReadLine error");
		break;
	      }
	  }
	//Console.WriteLine("Test");
	//Console.WriteLine(b);
	
	//Console.WriteLine(retStr);
	ints.append(System.Convert.ToInt32(retStr));
      	retStr="";
      }
    flush();
    return ints;
  }

    public byte read_byte ()
    {
        // fixme  make sure serial.Read returns 1?
        byte [] bytes = new byte[1];
	lock (serial) serial.Read (bytes, 0, 1);
        return bytes [0];
    }

    public void read (int bytes, byte [] buffer, int start)
    {
        int tries = 0;
        int count = 0;
        while (count < bytes && tries < 4) { // 4 seconds
            byte [] retval = try_read (bytes - count);
            buffer_copy (retval, buffer, start, retval.Length);
            count += retval.Length;
            start += retval.Length;
            if (retval.Length == 0)
                tries++;
        }
    }

    public byte [] read (int bytes)
    {
        int count = 0;
        int tries = 0;
        byte [] buffer = new byte[bytes];
        while (count < bytes && tries < 4) { // 4 seconds
            byte [] retval = try_read (bytes - count);
            buffer_copy (retval, buffer, count, retval.Length);
            count += retval.Length;
            if (retval.Length == 0)
                tries++;
        }
        return buffer.Slice (0, count);
    }

    static void buffer_copy (byte [] from_buf, byte [] to_buf, int start, int length)
    {
        int count = 0;
        for (int i=start; i < start + length; i++) {
            to_buf [i] = from_buf [count];
            count++;
        }
    }

    static byte [] buffer_add (byte [] buf1, byte [] buf2)
    {
        byte[] buffer = new byte[buf1.Length + buf2.Length];
        buffer_copy (buf1, buffer, 0, buf1.Length);
        buffer_copy (buf2, buffer, buf1.Length, buf2.Length);
        return buffer;
    }

    public byte [] try_read (int bytes)
    {
        byte[] buffer = new byte[bytes];
        int len = 0;
        try {
            lock (serial)
                len = serial.Read (buffer, 0, bytes);
        } catch {
            Thread.Sleep (10); 
        }
        //if (len != bytes)
        //  Console.WriteLine("read: Wrong number of bytes read");
        if (dongle == null) {
            // HACK! THIS SEEMS TO NEED TO BE HERE!
            Thread.Sleep (10); 
        }
        return buffer.Slice (0, len);
    }
  /*
  public int read_mem (int page, int offset)
    {
        write (Scribbler.GET_SERIAL_MEM);
        write_2byte (page);
        write_2byte (offset);
        return read_byte ();
    }
    */
    public void write (byte b)
    {
        byte [] buffer = new byte[1];
        buffer [0] = b;
        lock (serial)
            serial.Write (buffer, 0, 1);
    }

    public void write (byte [] b)
    {
        lock (serial)
            serial.Write (b, 0, b.Length);
    }

    public void write_2byte (int value)
    {
        write ((byte)((value >> 8) & 0xFF));
        write ((byte)(value & 0xFF));
    }

    public void write_packet (params byte [] data)
    {
      int packetInt=0;
      int servoPosition=0;
      byte [] packetByte= new byte[Bioloid.OUT_PACKET_LENGTH]; 
      byte upper=0;
      byte lower=0;

      if(data.Length>0)
	{
	  packetInt=data[0];
	  //SET_SERVO1 - SER_SERVO16 are values 16-31
	  if(packetInt>15 && packetInt<32 && data.Length>=3)
	    {
	      //servoPosition=(int)(data[1]+data[2]);
	      //packetInt=packetInt+(servoPosition<<10);
	      packetInt=packetInt+(data[1]<<5);
	      packetInt=packetInt+(data[2]<<10);
	    }
	  else
	    {
	      if(data.Length>=2)
		packetInt=packetInt+(data[1]<<5);
	      if(data.Length>=3)
		packetInt=packetInt+(data[2]<<10);
	    }

	  lower=(byte)(packetInt & 0xFF);
	  upper= (byte)((packetInt >> 8) & 0xFF);
	  
	  packetByte[0]=(byte)(0xff);
	  packetByte[1]=(byte)(0x55);
	  packetByte[2]=lower;
	  packetByte[3]=(byte)(~lower);
	  packetByte[4]=upper;
	  packetByte[5]=(byte)(~upper);

	  lock (serial) 
	    {
	      try 
		{
		  serial.Write (packetByte, 0, Bioloid.OUT_PACKET_LENGTH);		  
		}
	      catch 
		{
		  Console.WriteLine ("ERROR: in write");		  
		}
            }
	}
	
    }

  //Simple test to see if I can send a properly formatted value to the bioloid
  public void sendValue(int value1)
  {

    byte lower1=(byte)(value1 & 0xFF);
    byte upper1= (byte)((value1 >> 8) & 0xFF);

    write_packet ((byte)(0xff),
		  (byte)(0x55),
		  lower1,
		  (byte)(~lower1),
		  upper1,
		  (byte)(~upper1));

    //Console.WriteLine ((byte)0xff);
    //Console.WriteLine ((byte)0x55);
    //Console.WriteLine (lower);
    //Console.WriteLine ((byte)(~lower));
    //Console.WriteLine (upper);
    //Console.WriteLine ((byte)(~upper));



  }

    public override bool isConnected ()
    {
        if (serial == null) 
            return false;
        return serial.IsOpen;
    }

    public override void flush ()
    { 
        if (! isConnected ()) 
            return;
        byte [] bytes = new byte[1];
        lock (serial) {
            // DiscardInBuffer() is causing trouble on Mac OSX - doesn't clear buffer
            //serial.DiscardInBuffer();
            //serial.DiscardOutBuffer();
            while (true) {
                try {
                    serial.Read (bytes, 0, 1);
                } catch {
                    // timeout, default is one second
                    // no data, so we're done
                    break;
                }
                //serial.DiscardInBuffer();
                //serial.DiscardOutBuffer();
            }
        }
    }

    
    public int read_uint32 ()
    {
        byte [] buf = read (4);
        return buf [0] + buf [1] * 256 + buf [2] * 65536 + buf [3] * 16777216;
    }

    public byte [] quadrupleSize (byte [] line, int width)
    {
        byte [] retval = new byte[line.Length * 4]; 
        int col = 0;
        int row = 0;
        for (int i=0; i< line.Length; i++) {
            byte c = line [i];
            retval [row * 2 * width + col] = c;
            retval [row * 2 * width + col + 1] = c;
            retval [(row + 1) * 2 * width + col] = c;
            retval [(row + 1) * 2 * width + col + 1] = c;
            col += 2;
            if (col == width * 2) {
                col = 0;
                row += 2;
            }
        }
        return retval;
    }

  /*
     public override void servo(int id, int value)
    {
        if (!flukeIsAtLeast("3.0.9")) return;
        write(Scribbler.SET_SERVO);
        write((byte)id);
        write((byte)value);
    }
    
    public override void enablePanNetworking()
    {
        if (!flukeIsAtLeast("3.0.9")) return;
        write(Scribbler.ENABLE_PAN);
        write_2byte(0x123);
    }
    
    public override string getFlukeLog()
    {
        if (!flukeIsAtLeast("3.0.9")) return "";
        write(Scribbler.GET_ERRORS);
        int size = read_2byte();
        String line = "";

        int counter = 0;
        while (counter < size) { 
            byte b = read_byte ();
            line += (char)b;
            counter++;
        }
        return (line);
    }


    public void setSingleData (int position, int value)
    {
        byte [] data = new byte[1]; 
        data [0] = (byte)position;
        data [1] = (byte)value;
        set (Scribbler.SET_SINGLE_DATA, data);
    }

    public void set_ir_power (int power)
    {
        write (Scribbler.SET_DONGLE_IR);
        write ((byte)power);
    }
    
    public override List getIRMessage ()
    {
        if (isFluke2()) return new List(); // not supported yet

        lock (this) { // lock robot
            write (Scribbler.GET_IR_MESSAGE);
            int size = read_2byte ();
            return bytes2ints (read (size));
        }
    }
    
    public override void sendIRMessage (string message)
    {
        if (isFluke2()) return; // not supported yet

        byte [] data = new byte[message.Length];
        for (int i = 0; i < message.Length; i ++) {
            data [i] = (byte)message [i];
        }
        write (Scribbler.SEND_IR_MESSAGE);
        write ((byte)data.Length);
        write (data);
    }
    
    public override void setCommunicate ()
    {
        write (Scribbler.SET_IR_EMITTERS);
        write (emitters);
    }
    */
}


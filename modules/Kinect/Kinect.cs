// Kinect Client

using System;
using System.Net.Sockets;

public static class Kinect {

  public static byte CMD_HELLO = 0;
  public static byte CMD_NUMDEVICES = 1;
  public static byte CMD_STARTKINECT = 2;
  public static byte CMD_STARTKINECTDEFAULT = 3;
  //
  public static byte CMD_INITRGB640X480 = 20;
  public static byte CMD_INITRGB1280X1024 = 21;
  public static byte CMD_GETRGBMODE = 22;
  public static byte CMD_INITDEPTH80X60 = 23;
  public static byte CMD_INITDEPTH320X240 = 24;
  public static byte CMD_INITDEPTH640X480 = 25;
  public static byte CMD_GETDEPTHMODE = 26;
  public static byte CMD_NORGB = 27;
  public static byte CMD_NODEPTH = 28;
  //
  public static byte CMD_READRGB = 30;
  public static byte CMD_READDEPTH = 31;
  public static byte CMD_READDEPTHXYZ = 32;
  public static byte CMD_READDEPTHRGB = 33;
  public static byte CMD_READDEPTHXYZRGB = 34;
  public static byte CMD_READSKELETON = 35;
  
  public class Client {
	public TcpClient client;
	public NetworkStream stream;
	public long timestamp = 0;
	static Random _random = new Random();

	public Client(string server, int port) {
	  client = new TcpClient(server, port);
	  stream = client.GetStream();
	}

	public int hello() {
	  write(CMD_HELLO);
	  return readByte();
	}
	
	public int readByte() {
	  return ((int)read(1)[0]);
	}
	
    public int initRGB(int resX) {
	  byte cmd;
	  if (resX == 640) cmd = CMD_INITRGB640X480;
	  else if (resX == 1280) cmd = CMD_INITRGB1280X1024;
	  else if (resX == 0) cmd = CMD_NORGB;
	  else return 0;
	  write(cmd);
	  return readByte();
    }

    public int initDepth(int resX){
	  byte cmd;
	  if (resX == 640) cmd = CMD_INITDEPTH640X480;
	  else if (resX == 320) cmd = CMD_INITDEPTH320X240;
	  else if (resX == 80) cmd = CMD_INITDEPTH80X60;
	  else if (resX == 0) cmd = CMD_NODEPTH;
	  else return(0);
	  write(cmd);
	  return readByte();
    }

    public int startKinect(bool deflt){
	  byte cmd = CMD_STARTKINECT;
	  if (deflt) cmd = CMD_STARTKINECTDEFAULT;
	  write(cmd);
	  int a = readByte();
	  return(a);
	}

	public byte[] readData(){
	  int l0,l1,l2,l3;
	  // determine length of stream
	  l0 = readByte();
	  l1 = readByte();
	  l2 = readByte();
	  l3 = readByte();
	  int length = l0 + (l1<<8) + (l2<<16) + (l3<<24);
	  length = length - 4;    // subtract timestamp
	  
	  // read timestamp
	  l0 = readByte();
	  if (l0<0) l0=256+l0;
	  l1 = readByte();
	  if (l1<0) l1=256+l1;
	  l2 = readByte();
	  if (l2<0) l2=256+l2;
	  l3 = readByte();
	  if (l3<0) l3=256+l3;
	  timestamp = l0 | (l1<<8) | (l2<<16) | (l3<<24);
	  
	  // read bytestream
	  byte [] dataB = new byte[length];
	  int totalRead = 0;
	  while (totalRead < length){
		totalRead  += stream.Read(dataB, totalRead, length - totalRead);
	  }
	  return dataB;
	}
	
	public int[,] readDepth(){
	  write(CMD_READDEPTH);
	  byte []buffer = readData();
	  // convert to color
	  int [,]depth = new int[buffer.Length/2,2];
	  for (int i=0; i<depth.Length; i++){
		depth[i,0] = buffer[2*i] +  ((buffer[2*i+1] & 0x1f)*256);
		depth[i,1] = (buffer[2*i+1] & 0xe0) >> 8;
	  }
	  return(depth);
	}

	public static int [] convertDepthToImage(int [,] depth){
	  // convert to color
	  int [] pixels = new int[depth.Length];
	  // color table for index (detected humans)
	  int [,] col = new int [6,3] {{200,0,0},{0,200,0},{0,0,200},{200,200,0},{0,200,200},{200,0,200}};

	  for (int i=0; i<pixels.Length; i++){
		int distance = depth[i,0];
		int red,green,blue;
		int index = depth[i,1];
		if (index>0){
		  red = Math.Min(col[index-1,0]+(int)(_random.NextDouble()*255),255);
		  green = Math.Min(col[index-1,1]+(int)(_random.NextDouble()*255),255);
		  blue = Math.Min(col[index-1,2]+(int)(_random.NextDouble()*255),255);
		} else {
		  blue = 255*(distance - 300) / (1500 - 300);
		  green = 255*(distance - 1500) / (2500 - 1500);
		  red = 255*(distance - 2500) / (6000 - 2500);
		  if (blue < 0) blue = 0;
		  if (blue > 512) blue = 0;
		  if (blue > 255) blue = 512-blue;
		  if (green < 0) green = 0;
		  if (green > 512) green = 0;
		  if (green > 255) green = 512-green;
		  if (red < 0) red = 0;
		  if (red > 512) red = 0;
		  if (red > 255) red = 512 - red;
		}
		pixels[i] = (0xff<<24 | red<<16 | green << 8 | blue);
	  }
	  return (pixels);
    }
	
	public void write(string message) {
	  // Translate the passed message into ASCII and store it as a Byte array.
	  Byte[] data = System.Text.Encoding.ASCII.GetBytes(message);         
	  // Send the message to the connected TcpServer. 
	  stream.Write(data, 0, data.Length);
	  Console.WriteLine("Sent: {0}", message);         
	}
	
	public void write(params Byte[] data) {
	  stream.Write(data, 0, data.Length);
	  Console.WriteLine("Sent: {0}", data.Length);         
	}
	
	public string readString() {
	  // Buffer to store the response bytes.
	  Byte [] data = new Byte[256];
	  // String to store the response ASCII representation.
	  String responseData = String.Empty;
	  
	  // Read the first batch of the TcpServer response bytes.
	  int bytes = stream.Read(data, 0, data.Length);
	  responseData = System.Text.Encoding.ASCII.GetString(data, 0, bytes);
	  Console.WriteLine("Received: {0}", responseData);         
	  return responseData;
	}
	
	public Byte[] read(int count) {
	  // Buffer to store the response bytes.
	  Byte[] data = new Byte[count];
	  stream.Read(data, 0, count);
	  return data;
	}
	
	public void close() {
	  stream.Close();         
	  client.Close();         
	}
  } 
}

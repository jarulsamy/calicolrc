/*
Calico - Scripting Environment
Kinect Client for Rolf Lakaemper's Kinect Server

Copyright (c) 2011, Doug Blank <dblank@cs.brynmawr.edu>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

$Id: $
*/

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
	  stream.ReadTimeout = 100;
	  hello();
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
	  byte [] buffer = readData(); // 200k more so far
	  // convert to color
	  int [,] depth = new int[buffer.Length/2,2];
	  for (int i=0; i<depth.Length/2; i++){
		depth[i,0] = buffer[2*i] +  ((buffer[2*i+1] & 0x1f)*256);
		depth[i,1] = (buffer[2*i+1] & 0xe0) >> 8;
	  }
	  return(depth);
	}

    public byte [] readRGB() {
	  write(CMD_READRGB);
	  byte [] data = readData();
	  return data;
    }

    public int[] readRGBImageArray() {
	  write(CMD_READRGB);
	  byte [] data = readData();
	  int [] rgb = new int[data.Length/4];
	  int count = 0;
	  for (int i=0;i<data.Length; i+=4){
		int b = data[i];
		int g = data[i+1];
		int r = data[i+2];
		if (r<0) r=256+r;
		if (g<0) g=256+g;
		if (b<0) b=256+b;
		rgb[count++] = (0xff<<24 | r<< 16 | g<< 8 | b);
	  }
	  return (rgb);
    }

    public int[] readDepthXYZ(){
	  write(CMD_READDEPTHXYZ);
	  byte [] data = readData();
	  int [] depthXYZ = new int[data.Length/2];
	  int indexS = 0;
	  int indexT = 0;
	  while(indexS < data.Length){
		int low, high;
		low = data[indexS++];
		high = data[indexS++];
		depthXYZ[indexT++]=low + (high<<8) - 32768;
	  }
	  return (depthXYZ);
    }

    public byte [] readSkeleton(){
	  write(CMD_READSKELETON);
	  byte [] data = readData();
	  return data;
    }

    public static int[,] getJointPositions(byte []skel, int skelIndex){
	  if (skel[0]<skelIndex)
		return(null);
	  int dataIndex = 17 + 149 * (skelIndex-1)+9;
	  int [,] joints = new int[20,4]; // tracked x y z
	  for (int jIndex = 0; jIndex < 20; jIndex++){
		joints[jIndex,0] = (int)skel[dataIndex++];
		joints[jIndex,1] = (int)((skel[dataIndex] | skel[dataIndex+1]*256)-32768);
		dataIndex+=2;
		joints[jIndex,2] = (int)((skel[dataIndex] | skel[dataIndex+1]*256)-32768);
		dataIndex+=2;
		joints[jIndex,3] = (int)((skel[dataIndex] | skel[dataIndex+1]*256)-32768);
		dataIndex+=2;
	  }
	  return(joints);
    }

	public static int [,] getJointSegments(byte [] data, int skelIndex) {
	  // returns x1,y1,x2,y2,state for each joint
	  int [,] joints = getJointPositions(data, skelIndex);
	  int [,] segments = new int[19,5];
	  // head to shoulder center
	  segments[0,0] = (int)joints[3,1];
	  segments[0,1] = (int)joints[3,2];
	  segments[0,2] = (int)joints[2,1];
	  segments[0,3] = (int)joints[2,2];
	  segments[0,4] = (int)(joints[3,0]*joints[2,0]);
	  
	  //shoulder center to shoulder right
	  segments[1,0] = (int)joints[8,1];
	  segments[1,1] = (int)joints[8,2];
	  segments[1,2] = (int)joints[2,1];
	  segments[1,3] = (int)joints[2,2];
	  segments[1,4] = (int)(joints[8,0]*joints[2,0]);
	  
	  //shoulder right to elbow right
	  segments[2,0] = (int)joints[8,1];
	  segments[2,1] = (int)joints[8,2];
	  segments[2,2] = (int)joints[9,1];
	  segments[2,3] = (int)joints[9,2];
	  segments[2,4] = (int)(joints[8,0]*joints[9,0]);
	  
	  //elbow right to wrist right
	  segments[3,0] = (int)joints[10,1];
	  segments[3,1] = (int)joints[10,2];
	  segments[3,2] = (int)joints[9,1];
	  segments[3,3] = (int)joints[9,2];
	  segments[3,4] = (int)(joints[10,0]*joints[9,0]);
	  
	  //wrist right to hand right
	  segments[4,0] = (int)joints[10,1];
	  segments[4,1] = (int)joints[10,2];
	  segments[4,2] = (int)joints[11,1];
	  segments[4,3] = (int)joints[11,2];
	  segments[4,4] = (int)(joints[10,0]*joints[11,0]);
	  
	  //
	  //shoulder center to shoulder left
	  segments[5,0] = (int)joints[4,1];
	  segments[5,1] = (int)joints[4,2];
	  segments[5,2] = (int)joints[2,1];
	  segments[5,3] = (int)joints[2,2];
	  segments[5,4] = (int)(joints[2,0]*joints[4,0]);
	  
	  //shoulder left to elbow left
	  segments[6,0] = (int)joints[4,1];
	  segments[6,1] = (int)joints[4,2];
	  segments[6,2] = (int)joints[5,1];
	  segments[6,3] = (int)joints[5,2];
	  segments[6,4] = (int)(joints[4,0]*joints[5,0]);
	  
	  //elbow left to wrist left
	  segments[7,0] = (int)joints[5,1];
	  segments[7,1] = (int)joints[5,2];
	  segments[7,2] = (int)joints[6,1];
	  segments[7,3] = (int)joints[6,2];
	  segments[7,4] = (int)(joints[6,0]*joints[5,0]);
	  
	  //wrist left to hand left
	  segments[8,0] = (int)joints[6,1];
	  segments[8,1] = (int)joints[6,2];
	  segments[8,2] = (int)joints[7,1];
	  segments[8,3] = (int)joints[7,2];
	  segments[8,4] = (int)(joints[7,0]*joints[6,0]);
	  
	  // spine
	  segments[9,0] = (int)joints[2,1];
	  segments[9,1] = (int)joints[2,2];
	  segments[9,2] = (int)joints[1,1];
	  segments[9,3] = (int)joints[1,2];
	  segments[9,4] = (int)(joints[1,0]*joints[2,0]);
	  
	  segments[10,0] = (int)joints[1,1];
	  segments[10,1] = (int)joints[1,2];
	  segments[10,2] = (int)joints[0,1];
	  segments[10,3] = (int)joints[0,2];
	  segments[10,4] = (int)(joints[0,0]*joints[1,0]);
	  
	  //right leg
	  segments[11,0] = (int)joints[16,1];
	  segments[11,1] = (int)joints[16,2];
	  segments[11,2] = (int)joints[0,1];
	  segments[11,3] = (int)joints[0,2];
	  segments[11,4] = (int)(joints[0,0]*joints[16,0]);
	  
	  segments[12,0] = (int)joints[16,1];
	  segments[12,1] = (int)joints[16,2];
	  segments[12,2] = (int)joints[17,1];
	  segments[12,3] = (int)joints[17,2];
	  segments[12,4] = (int)(joints[17,0]*joints[16,0]);
	  
	  segments[13,0] = (int)joints[17,1];
	  segments[13,1] = (int)joints[17,2];
	  segments[13,2] = (int)joints[18,1];
	  segments[13,3] = (int)joints[18,2];
	  segments[13,4] = (int)(joints[18,0]*joints[17,0]);
	  
	  segments[14,0] = (int)joints[18,1];
	  segments[14,1] = (int)joints[18,2];
	  segments[14,2] = (int)joints[19,1];
	  segments[14,3] = (int)joints[19,2];
	  segments[14,4] = (int)(joints[19,0]*joints[18,0]);
	  
	  //left leg
	  segments[15,0] = (int)joints[12,1];
	  segments[15,1] = (int)joints[12,2];
	  segments[15,2] = (int)joints[0,1];
	  segments[15,3] = (int)joints[0,2];
	  segments[15,4] = (int)(joints[0,0]*joints[12,0]);
	  
	  segments[16,0] = (int)joints[12,1];
	  segments[16,1] = (int)joints[12,2];
	  segments[16,2] = (int)joints[13,1];
	  segments[16,3] = (int)joints[13,2];
	  segments[16,4] = (int)(joints[13,0]*joints[12,0]);
	  
	  segments[17,0] = (int)joints[13,1];
	  segments[17,1] = (int)joints[13,2];
	  segments[17,2] = (int)joints[14,1];
	  segments[17,3] = (int)joints[14,2];
	  segments[17,4] = (int)(joints[14,0]*joints[13,0]);
	  
	  segments[18,0] = (int)joints[14,1];
	  segments[18,1] = (int)joints[14,2];
	  segments[18,2] = (int)joints[15,1];
	  segments[18,3] = (int)joints[15,2];
	  segments[18,4] = (int)(joints[15,0]*joints[14,0]);
	  return segments;
	}

	public static int [] convertDepthToImageArray(int [,] depth){
	  // convert to color
	  int [] pixels = new int[depth.GetUpperBound(0) + 1];
	  // color table for index (detected humans)
	  int [,] col = new int [6,3] {{200,0,0},
								   {0,200,0},
								   {0,0,200},
								   {200,200,0},
								   {0,200,200},
								   {200,0,200}};

	  for (int i=0; i<=depth.GetUpperBound(0); i++){
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
	
	public void write(params Byte[] data) {
	  try {
		stream.Write(data, 0, data.Length);
	  } catch {
		Console.WriteLine("Kinect.Client: write failed!");		
	  }
	}
	
	public Byte[] read(int count) {
	  // Buffer to store the response bytes.
	  Byte[] data = new Byte[count];
	  try {
		stream.Read(data, 0, count);
	  } catch {
		Console.WriteLine("Kinect.Client: read failed!");
		return null;
	  }
	  return data;
	}
	
	public void close() {
	  stream.Close();         
	  client.Close();         
	}
  } 
}

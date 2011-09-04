// Kinect Client

using System;
using System.Net.Sockets;

public class Kinect {
  public TcpClient client;
  public NetworkStream stream;
  
  public Kinect(string server, int port) {
	client = new TcpClient(server, port);
	stream = client.GetStream();
  }
  
  public void write(string message) {
	// Translate the passed message into ASCII and store it as a Byte array.
	Byte[] data = System.Text.Encoding.ASCII.GetBytes(message);         
	// Send the message to the connected TcpServer. 
	stream.Write(data, 0, data.Length);
	Console.WriteLine("Sent: {0}", message);         
  }
  
  public void write(Byte[] data) {
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

using System;
using System.Text;
using ZeroMQ;

public static class ZMQ {

    public class Session(string filename) {
	

	ZmqContext context = ZmqContext.Create();
	ZmqSocket shell_socket = context.CreateSocket(SocketType.DEALER);
	server.Bind("tcp://*:5555");

	public static void loop() {
	    // Wait for next request from client
	    string message = server.Receive(Encoding.Unicode);
	    Console.WriteLine("Received request: {0}", message);
	    
	    // Do Some 'work'
	    Thread.Sleep(1000);
	    
	    // Send reply back to client
	    server.Send("World", Encoding.Unicode);
	}


    }

    static void Main(string[] args) {
	ZMQ zmq = ZMQ();
    }
}

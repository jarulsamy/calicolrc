using System;
using System.Diagnostics;
using org.ros;
using org.ros.node;
using org.ros.@namespace;
using org.ros.@internal.loader;
using org.ros.node.topic;
using org.ros.message;
using geometry_msgs;
using turtlesim;

public static class MyROS
{
  
  public static Graphics.Picture rosRawRGBToPicture(org.jboss.netty.buffer.ChannelBuffer cbuffer,
						    int width,
						    int height)
  {
    byte[] buffer = new byte[cbuffer.capacity()];
    cbuffer.getBytes(0, buffer, 0, cbuffer.capacity());
    return new Graphics.Picture (width, height, buffer);
  }

  public static Graphics.Picture rosRGBToPicture(org.jboss.netty.buffer.ChannelBuffer cbuffer,
						 int width,
						 int height)
  {
    byte[] buffer = new byte[cbuffer.capacity()];
    cbuffer.getBytes(0, buffer, 0, cbuffer.capacity());
    System.IO.MemoryStream ms = new System.IO.MemoryStream (buffer);
    System.Drawing.Bitmap bitmap = (System.Drawing.Bitmap)System.Drawing.Bitmap.FromStream (ms);    
    return new Graphics.Picture (bitmap, width, height, false);
  }
  
  public class CSharpTurtleTest: NodeMain
  {
    Publisher publisher;
    Subscriber subscriber;
    
    public GraphName getDefaultNodeName() {
      return GraphName.of("calico_node");
    }  

    private class MessageHandler: MessageListener
    {
      public void onNewMessage(object o)
      {
	turtlesim.Color c = (turtlesim.Color)o;
	Console.WriteLine(c);
      }
    }
    
    public void onStart(ConnectedNode node) {
      Console.WriteLine("Hello!");
      publisher = node.newPublisher("/turtle1/command_velocity", 
				    turtlesim.Velocity._TYPE);
      subscriber = node.newSubscriber("/turtle1/color_sensor", 
				      turtlesim.Color._TYPE);
      subscriber.addMessageListener(new MessageHandler());
    }

    
    public void move()
    {
      turtlesim.Velocity velocity = (turtlesim.Velocity)publisher.newMessage();      
      float linearVelocity = 0.5f;
      velocity.setLinear(linearVelocity);
      publisher.publish(velocity);	  
    }
    
    public void onShutdown(Node node) {
    }

    public void onShutdownComplete(Node node) {
    }
    
    public void onError(Node node, System.Exception throwable) {
    }
    
    public void start()
    {
      /*      CommandLineLoader loader = new CommandLineLoader(Lists.newArrayList("CSharpTurtleTest"));
  
      string nodeClassName = loader.getNodeClassName();
      Console.WriteLine(nodeClassName);
      NodeConfiguration nodeConfiguration = loader.build();
      NodeMain nodeMain = this;
      Console.WriteLine("nodeMain:" + nodeMain);
      Preconditions.checkState(nodeMain != null);*/     

      NodeMainExecutor nodeMainExecutor = DefaultNodeMainExecutor.newDefault();
      nodeMainExecutor.execute(this, NodeConfiguration.newPrivate());
    }
  }

    public static void ROSCore() {
	// It is ok to try to start roscore more than once
	Process myProcess = new Process ();
	myProcess.StartInfo.UseShellExecute = false;
	myProcess.StartInfo.FileName = "roscore";
	myProcess.StartInfo.CreateNoWindow = true;
	myProcess.StartInfo.WindowStyle = ProcessWindowStyle.Hidden;
	try {
	    myProcess.Start();
	} catch {
	    System.Console.Error.WriteLine("ROSCore: unable to start roscore");
	}
    }
    
    public static void ROSRun(string package, string executable, params string [] arguments) {
	// Only run if not already running
	ROSCore();
	if (!ROSRunning(executable)) {
	    Process myProcess = new Process ();
	    myProcess.StartInfo.UseShellExecute = false;
	    myProcess.StartInfo.FileName = "rosrun";
	    myProcess.StartInfo.Arguments = (package + " " + executable + " " + String.Join(" ", arguments));
	    myProcess.StartInfo.CreateNoWindow = true;
	    myProcess.StartInfo.WindowStyle = ProcessWindowStyle.Hidden;
	    try {
		myProcess.Start();
	    } catch {
		System.Console.Error.WriteLine("ROSRun: unable to start 'rosrun {0} {1}...'", package, executable);
	    }
	}
    }
    
    public static bool ROSRunning(string package) {
	// look for a running process
	foreach (System.Diagnostics.Process process in System.Diagnostics.Process.GetProcesses()) {
	    // System.Console.WriteLine(process.ProcessName);
	    try {
		if (process.ProcessName == package) {
		    return true;
		}
	    } catch {
		// process no longer around
	    }
	}
	return false;
    }
}

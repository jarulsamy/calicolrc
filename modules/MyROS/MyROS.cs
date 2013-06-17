using System;
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
  
  public static Graphics.Picture rosRawRGBToPicture(org.jboss.netty.buffer.SlicedChannelBuffer cbuffer,
						    int width,
						    int height)
  {
    byte[] buffer = new byte[cbuffer.capacity()];
    cbuffer.getBytes(0, buffer, 0, cbuffer.capacity());
    return new Graphics.Picture(width, height, buffer);
  }
  
  public class CSharpTurtleTest: NodeMain
  {
    Publisher publisher;
    Subscriber subscriber;
    
    public GraphName getDefaultNodeName() {
      return GraphName.of("my_node");
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
}
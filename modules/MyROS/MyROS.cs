using System;
using org.ros;
using org.ros.node;
using org.ros.@namespace;
using org.ros.@internal.loader;
using org.ros.node.topic;
using geometry_msgs;
using turtlesim;

public static class ROS
{
  public class CSharpTurtleTest: NodeMain
  {
    Publisher publisher;
    
    public GraphName getDefaultNodeName() {
      return GraphName.of("my_node");
    }  
    
    public void onStart(ConnectedNode node) {
      Console.WriteLine("Hello!");
      publisher = node.newPublisher("/turtle1/command_velocity", 
				    turtlesim.Velocity._TYPE);
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
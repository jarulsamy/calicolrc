using System;
using org.ros;
using org.ros.node;
using org.ros.@namespace;
using org.ros.@internal.loader;
using org.ros.node.topic;
using org.ros.message;
using geometry_msgs;
using turtlesim;

public class ROSRobot: Myro.Robot, NodeMain
{
  Publisher vel_publisher;
  
  public ROSRobot ()
  {
    NodeMainExecutor nodeMainExecutor = DefaultNodeMainExecutor.newDefault();
      nodeMainExecutor.execute(this, NodeConfiguration.newPrivate());
  }
  
  public virtual GraphName getDefaultNodeName() 
  {
    return GraphName.of("calico_ros_robot");
  }  
  
  public virtual void onStart(ConnectedNode node) 
  {
    vel_publisher = node.newPublisher("/cmd_vel", Twist._TYPE);
  }
      
  public virtual void onShutdown(Node node) {
  }
  
  public virtual void onShutdownComplete(Node node) {
  }
  
  public virtual void onError(Node node, System.Exception throwable) {
  }

  public override void adjustSpeed ()
  {
    // set motors based on_lastTranslate and _lastRotate
    Twist velocity = (Twist)vel_publisher.newMessage();      
    velocity.getLinear().setX(_lastTranslate);
    velocity.getAngular().setZ(_lastRotate);
    vel_publisher.publish(velocity);	  
  }    

  public override void up (double speed)
  {
    Twist velocity = (Twist)vel_publisher.newMessage();      
    velocity.getLinear().setZ(speed);
    vel_publisher.publish(velocity);	  
  }    

  public override void down (double speed)
  {
    Twist velocity = (Twist)vel_publisher.newMessage();      
    velocity.getLinear().setZ(-speed);
    vel_publisher.publish(velocity);	  
  }    

  public override void left (double speed)
  {
    Twist velocity = (Twist)vel_publisher.newMessage();      
    velocity.getLinear().setY(speed);
    vel_publisher.publish(velocity);	  
  }    

  public override void right (double speed)
  {
    Twist velocity = (Twist)vel_publisher.newMessage();      
    velocity.getLinear().setY(-speed);
    vel_publisher.publish(velocity);	  
  }
}

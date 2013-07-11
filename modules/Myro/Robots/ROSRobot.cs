using System;
using org.ros;
using org.ros.node;
using org.ros.@namespace;
using org.ros.@internal.loader;
using org.ros.node.topic;
using org.ros.message;
using geometry_msgs;
using sensor_msgs;
using java.net;

public class ROSRobot: Myro.Robot, NodeMain
{
  Publisher vel_publisher;
  public Image lastImg;
  private Subscriber imgSub;
  
  public ROSRobot ()
  {
    NodeMainExecutor nodeMainExecutor = DefaultNodeMainExecutor.newDefault();
      nodeMainExecutor.execute(this, NodeConfiguration.newPrivate());
  }

  public ROSRobot (String ourIP, String ROSIP)
  {    
    NodeMainExecutor nodeMainExecutor = DefaultNodeMainExecutor.newDefault();
    NodeConfiguration nodeConfig = NodeConfiguration.newPublic(ourIP);
    nodeConfig.setMasterUri(new URI("http://" + ROSIP + ":11311"));    
    nodeMainExecutor.execute(this, nodeConfig);
  }
  
  public virtual GraphName getDefaultNodeName() 
  {
    return GraphName.of("calico_ros_robot");
  }  
  
  public virtual void onStart(ConnectedNode node) 
  {
    vel_publisher = node.newPublisher("/cmd_vel", Twist._TYPE);

    imgSub = node.newSubscriber("/usb_cam/image_raw", Image._TYPE);
    imgSub.addMessageListener(new ImageHandler(this), 1);    

  }
      
  public virtual void onShutdown(Node node) {
  }
  
  public virtual void onShutdownComplete(Node node) {
  }
  
  public virtual void onError(Node node, System.Exception throwable) {
  }

  private class ImageHandler: MessageListener
  {
    private ROSRobot outer;
    public ImageHandler(ROSRobot ar): base()
    {
      outer = ar;   
    }

    public void onNewMessage(object o)
    {
      Image p = (Image)o;
      outer.lastImg = p;
    }
  }

  public override void adjustSpeed ()
  {
    // set motors based on_lastTranslate and _lastRotate
    Twist velocity = (Twist)vel_publisher.newMessage();      
    velocity.getLinear().setX(_lastTranslate);
    velocity.getAngular().setZ(_lastRotate);
    vel_publisher.publish(velocity);	  
  }    
    
  public override void waitForMove (double interval)
  {
    double start = Myro.currentTime ();
    while (Myro.currentTime() - start < interval)
      {	
	Myro.wait (.1);
	adjustSpeed();
      }
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
  public override Graphics.Picture takePicture (string mode="jpeg")
  {
    return MyROS.rosRawRGBToPicture(lastImg.getData(), lastImg.getWidth(), lastImg.getHeight());
  }
  
}

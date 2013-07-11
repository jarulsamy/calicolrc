using System;
using org.ros;
using org.ros.node;
using org.ros.@namespace;
using org.ros.@internal.loader;
using org.ros.node.topic;
using org.ros.message;
using geometry_msgs;
using sensor_msgs;
using std_msgs;
using std_srvs;
using ardrone_autonomy;
using org.ros.node.service;
using org.ros.exception;


public class ROSARDrone: ROSRobot
{
  public new Image lastImg;
  public Navdata lastNav;
  private Subscriber imgSub, navSub;
  Publisher takeoffPub, landPub, resetPub;
  private int msginterval = 5;
  private ConnectedNode node;
  
  public ROSARDrone ():base(){}
  
  public override GraphName getDefaultNodeName() 
  {
    return GraphName.of("calico_ros_ardrone");
  }  

  private class ImageHandler: MessageListener
  {
    private ROSARDrone outer;
    public ImageHandler(ROSARDrone ar): base()
    {
      outer = ar;      
    }

    public void onNewMessage(object o)
    {
      Image p = (Image)o;
      outer.lastImg = p;
    }
  }

  private class NavdataHandler: MessageListener
  {
    private ROSARDrone outer;
    public NavdataHandler(ROSARDrone ar): base()
    {
      outer = ar;      
    }

    public void onNewMessage(object o)
    {
      Navdata p = (Navdata)o;
      outer.lastNav = p;
    }
  }

  public override double getBattery ()
  {
    if (lastNav != null) return lastNav.getBatteryPercent();
    else return -1;
  }

  public override object getTemperature ()
  {
    if (lastNav != null) return lastNav.getTemp();
    else return -1;
  }

  public void flattrim()
  {
    emptyService("/ardrone/flattrim");    
  }

  public void togglecam()
  {
    emptyService("/ardrone/togglecam");    
  }

  /*
  def getState(self):
    states = ["Unknown", "Initied", "Landed", "Flying", "Hovering", "Test", "Taking Off",
              "Flying", "Landing", "Looping"]
    if self.lastNav:
        return states[self.lastNav.getState()]

  def getTilt(self):
    if self.lastNav:
        return [self.lastNav.getRotX(), self.lastNav.getRotY(), self.lastNav.getRotZ()]

  def getMag(self):
    if self.lastNav:
        return [self.lastNav.getRotX(), self.lastNav.getMagY(), self.lastNav.getMagZ()]

  def getVelocity(self):
    if self.lastNav:
        return [self.lastNav.getVx(), self.lastNav.getVy(), self.lastNav.getVz()]

  def getAcceleration(self):
    if self.lastNav:
        return [self.lastNav.getAx(), self.lastNav.getAy(), self.lastNav.getAz()]

  def getTemperature(self):
    if self.lastNav:
        return self.lastNav.getTemp()

	def getPressure(self):
    if self.lastNav:
        return self.lastNav.getPressure()

  def getWindSpeed(self):
    if self.lastNav:
        return self.lastNav.getWindSpeed()

	*/

  public void msgCmd(Publisher pub)
  {
    double start = Myro.currentTime ();
    while (Myro.currentTime() - start < msginterval) {
      pub.publish((std_msgs.Empty)pub.newMessage());
      Myro.wait (.1);
    }  
  }

  public override void reset()
  {
    msgCmd(resetPub);
    flattrim();
  }

  public override void takeoff()
  {
    msgCmd(takeoffPub);
  }

  public override void land()
  {
    msgCmd(landPub);
  }

  private class EmptyService: ServiceResponseListener {
    public void onSuccess(object response) {
    }
    public void onFailure(RemoteException e) {
      throw new RosRuntimeException(e);
    }
  }

  public void emptyService(String name)
  {
    ServiceClient serviceClient;
    try {
      serviceClient = node.newServiceClient(name, std_srvs.Empty._TYPE);
    } catch (ServiceNotFoundException e) {
      throw new RosRuntimeException(e);
    }
    EmptyRequest request = (EmptyRequest)serviceClient.newMessage();
    serviceClient.call(request, new EmptyService());
  }

  public override void onStart(ConnectedNode node) 
  {
    this.node = node;
    base.onStart(node);
    takeoffPub = node.newPublisher("/ardrone/takeoff", std_msgs.Empty._TYPE);   
    landPub = node.newPublisher("/ardrone/land", std_msgs.Empty._TYPE);   
    resetPub = node.newPublisher("/ardrone/reset", std_msgs.Empty._TYPE);   

    imgSub = node.newSubscriber("/ardrone/image_raw", Image._TYPE);
    imgSub.addMessageListener(new ImageHandler(this), 1);    

    navSub = node.newSubscriber("/ardrone/navdata", Navdata._TYPE);
    navSub.addMessageListener(new NavdataHandler(this), 1);    
  }

  public override Graphics.Picture takePicture (string mode="jpeg")
  {
    return MyROS.rosRawRGBToPicture(lastImg.getData(), lastImg.getWidth(), lastImg.getHeight());
  }
}

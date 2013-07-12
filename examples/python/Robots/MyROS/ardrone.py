from org.ros.namespace import GraphName
from org.ros.node import *
from org.ros.node.service import *
from org.ros.address import *
from org.ros.message import MessageListener
from std_msgs import *
import sensor_msgs
from geometry_msgs import Twist
from ardrone_autonomy import Navdata
from Myro import *
import MyROS

class ARDrone(NodeMain):

  def takePicture(self):
    image = self.lastP
    return MyROS.rosRawRGBToPicture(image.getData(), image.getWidth(), image.getHeight())

  def gamepad(self):
    buttons = getGamepadNow("button")
    while buttons[7] == 0:
        vy, vx = getGamepadNow("axis")
        buttons = getGamepadNow("button")
        down = buttons[0]
        up = buttons[3]
        tl = buttons[1]
        tr = buttons[2]
        if tr:
            vpan = 1
        elif tr:
            vpan = -1
        else:
            vpan = 0
        if up:
            vz = 1
        elif down:
            vz = -1
        else:
            vz = 0
        self.move(vx/-7, vy/-7, vz/7, vpan/-7)

        if buttons[4] == 1:
            show(self.takePicture())

    self.move()
    self.land()

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

  def getBattery(self):
    if self.lastNav:
        return self.lastNav.getBatteryPercent()

  def getPressure(self):
    if self.lastNav:
        return self.lastNav.getPressure()

  def getWindSpeed(self):
    if self.lastNav:
        return self.lastNav.getWindSpeed()

  def move(self, vx=0, vy=0, vz=0, vpan=0):
    print(vx, vy, vz, vpan)
    twist = self.vel_pub.newMessage()
    twist.getLinear().setX(vx)
    twist.getLinear().setY(vy)
    twist.getLinear().setZ(vz)
    twist.getAngular().setZ(vpan)
    self.vel_pub.publish(twist)

  def move_timed(self, time, vx=0, vy=0, vz=0, vpan=0):
    self.move(vx, vy, vz, vpan)
    if time != None:
        wait(time)
        self.move()

  def forward(self, speed, time=None):
    self.move_timed(time, speed)

  def backward(self, speed, time=None):
    self.move_timed(time, -speed)

  def left(self, speed, time=None):
    self.move_timed(time, 0, speed)

  def right(self, speed, time=None):
    self.moved_time(time, 0, -speed)

  def up(self, speed, time=None):
    self.move_timed(time, 0, 0, speed)

  def down(self, speed, time=None):
    self.move_timed(time, 0, 0, -speed)

  def turnLeft(self, speed, time=None):
    self.move_timed(time, 0, 0, 0, speed)

  def turnRight(self, speed, time=None):
    self.turn_timed(time, 0, 0, 0, -speed)

  def emptyService(self, str):
    resetF = self.node.newServiceClient(str, Empty._TYPE)
    class response(ServiceResponseListener):
        def onSuccess(self, response):
            print(response)
        def onFailure(self, e):
            print (e)
    resetF.call(resetF.newMessage(), response())

  def flatTrim(self):
    self.emptyService("/ardrone/flattrim")

  def toggleCam(self):
    self.emptyService("/ardrone/togglecam")

  def takeoff(self):
    for i in timer(5):
        self.to_pub.publish(self.to_pub.newMessage())
        wait(.05)

  def land(self):
    for i in timer(5):
        self.l_pub.publish(self.l_pub.newMessage())
        wait(.05)

  def reset(self):
    for i in timer(5):
        self.reset_pub.publish(self.reset_pub.newMessage())
        wait(.05)

  def getDefaultNodeName(self):
    return GraphName.of("my_node")

  def onStart(self, node):
    self.node = node
    self.vel_pub =   node.newPublisher("cmd_vel", Twist._TYPE)
    self.to_pub  =   node.newPublisher("/ardrone/takeoff", Empty._TYPE)
    self.l_pub   =   node.newPublisher("/ardrone/land", Empty._TYPE)
    self.reset_pub = node.newPublisher("/ardrone/reset", Empty._TYPE)

    outer = self
    class OnImage(MessageListener):
        def onNewMessage(self, image):
          outer.lastP = image

    self.subscriber_img = node.newSubscriber("/ardrone/image_raw", sensor_msgs.Image._TYPE)
    queue_size=1
    self.subscriber_img.addMessageListener(OnImage(), queue_size)

    class OnNavdata(MessageListener):
        def onNewMessage(self, msg):
            outer.lastNav = msg
    self.subscriber_nav = node.newSubscriber("/ardrone/navdata", Navdata._TYPE)
    self.subscriber_nav.addMessageListener(OnNavdata(), queue_size)

  def onStop(self, node):
    pass

  def onShutdown(self, node):
    pass

  def onError(self, node, throwable):
    pass

### Running ROSCore in this process
#from org.ros import RosCore
#import time
#roscore = RosCore.newPublic()
#roscore.start()
#time.sleep(5)

MyROS.ROSRun("ardrone_autonomy", "ardrone_driver")

drone = ARDrone()
nodeMainExecutor = DefaultNodeMainExecutor.newDefault()
nodeConfig = NodeConfiguration.newPrivate()
nodeMainExecutor.execute(drone, nodeConfig);


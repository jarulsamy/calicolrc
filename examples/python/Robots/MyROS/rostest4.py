from org.ros.namespace import GraphName
from org.ros.node import *
from org.ros.node.service import *
from org.ros.address import *
from org.ros.message import MessageListener
from turtlesim import *
from std_srvs import *


class ROSTurtle(NodeMain):

  def __init__(self):
    self.publisher = None
    self.resetF = None

  def reset(self):
    class response(ServiceResponseListener):
        def onSuccess(self, response):
            print(response)
        def onFailure(self, e):
            print (e)
    self.resetF.call(self.resetF.newMessage(), response())

  def move(self, dx=0.1, da=0):
    velocity = self.publisher.newMessage()
    velocity.setLinear(dx)
    velocity.setAngular(da)
    self.publisher.publish(velocity)

  def getDefaultNodeName(self):
    return GraphName.of("my_node")

  def onStart(self, node):
    self.publisher = node.newPublisher("/turtle1/command_velocity", Velocity._TYPE)

    class myListener(MessageListener):
        def onNewMessage(self, msg):
            print("r:%d\tg:%d\tb:%d" % (msg.getR(), msg.getG(), msg.getB()))
    self.subscriber = node.newSubscriber("/turtle1/color_sensor", Color._TYPE)
    self.subscriber.addMessageListener(myListener())
    self.resetF = node.newServiceClient("reset", Empty._TYPE)


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

turtle = ROSTurtle()
nodeMainExecutor = DefaultNodeMainExecutor.newDefault()
nodeConfig = NodeConfiguration.newPublic("192.168.1.8") #our IP

#### To use a ros core on another machine set
#### the Master URI in the following way:
import clr
clr.AddReference("IKVM.OpenJDK.Core")
from java.net import URI
nodeConfig.setMasterUri(URI("http://192.168.1.7:11311")) #ros IP

nodeMainExecutor.execute(turtle, nodeConfig);

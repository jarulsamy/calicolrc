from org.ros.namespace import GraphName
from org.ros.node import *
from org.ros.internal.loader import CommandLineLoader
from org.ros.node.service import *
from org.ros.address import *
from org.ros.message import MessageListener
from turtlesim import *
from std_srvs import *
from org.ros import RosCore
import time

class MyNode(NodeMain):

  def __init__(self):
    self.publisher = None
    self.resetF = None

  def reset(self):
    class response(ServiceResponseListener):
        def onSuccess(response):
            print(response)
        def onFailure(e):
            print (e)
    self.resetF.call(self.resetF.newMessage(), response())

  def move(self, dx=0, da=0):
    print("moving")
    velocity = self.publisher.newMessage()
    velocity.setLinear(dx)
    velocity.setAngular(da)
    self.publisher.publish(velocity)

  def getDefaultNodeName(self):
    return GraphName.of("my_node")

  def onStart(self, node):
    self.publisher = node.newPublisher("/turtle1/command_velocity", Velocity._TYPE)
    class myListener(MessageListener):
        def onNewMessage(msg):
            print(msg)
    self.subscriber = node.newSubscriber("/turtle1/color_sensor", Color._TYPE)
    self.subscriber.addMessageListener(myListener())
    print("all set")

    try:
        #self.resetF = node.newServiceClient("reset", Empty._TYPE)
        pass
    except Exception as e:
        print ("trouble connecting to resetF", e)
    print ("hello")

  def onStop(self, node):
    pass

  def onShutdown(self, node):
    pass

  def onError(self, node, throwable):
    pass

#roscore = RosCore.newPublic()
#roscore.start()
#time.sleep(5)

nodeMain = MyNode()
nodeMainExecutor = DefaultNodeMainExecutor.newDefault()
nodeConfig = NodeConfiguration.newPublic("127.0.0.1")
nodeMainExecutor.execute(nodeMain, nodeConfig);

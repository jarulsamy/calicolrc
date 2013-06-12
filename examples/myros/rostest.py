import clr
clr.AddReference("ros.dll")

from org.ros.namespace import GraphName
from org.ros.node import *
from org.ros.internal.loader import CommandLineLoader
from turtlesim import *
from org.ros import RosCore
import time

class MyNode(NodeMain):

  def __init__(self):
    self.publisher = None

  def move(self):
    print("moving")
    velocity = self.publisher.newMessage();
    velocity.setLinear(0.5);
    self.publisher.publish(velocity);

  def getDefaultNodeName(self):
    return GraphName.of("my_node")

  def onStart(self, node):
    self.publisher = node.newPublisher("/turtle1/command_velocity", Velocity._TYPE);

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
nodeMainExecutor.execute(nodeMain, NodeConfiguration.newPrivate());

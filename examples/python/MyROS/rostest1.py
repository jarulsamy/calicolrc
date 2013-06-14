from org.ros.namespace import GraphName
from org.ros.node import *
from turtlesim import *
import time

class ROSTurtle(NodeMain):

  def __init__(self):
    self.publisher = None

  def move(self, dx=0.1, da=0):
    velocity = self.publisher.newMessage();
    velocity.setLinear(dx);
    velocity.setAngular(da);
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

#from org.ros import RosCore
#roscore = RosCore.newPublic()
#roscore.start()
#time.sleep(5)

turtle = ROSTurtle()
nodeMainExecutor = DefaultNodeMainExecutor.newDefault()
nodeMainExecutor.execute(turtle, NodeConfiguration.newPrivate());

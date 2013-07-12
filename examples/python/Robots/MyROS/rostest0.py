from org.ros.namespace import GraphName
from org.ros.node import *
from turtlesim import *

class ROSTurtle(NodeMain):

  def move(self, dx=0.1, da=0):
    velocity = self.publisher.newMessage();
    velocity.setLinear(dx);
    velocity.setAngular(da);
    self.publisher.publish(velocity);

  def getDefaultNodeName(self):
    return GraphName.of("my_node")

  def onStart(self, node):
    self.publisher = node.newPublisher("/turtle1/command_velocity", Velocity._TYPE);

calico.ROSCore()
calico.ROSRun("turtlesim", "turtlesim_node")

turtle = ROSTurtle()
nodeMainExecutor = DefaultNodeMainExecutor.newDefault()
nodeMainExecutor.execute(turtle, NodeConfiguration.newPrivate());

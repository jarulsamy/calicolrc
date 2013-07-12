from org.ros.namespace import GraphName
from org.ros.node import *
from org.ros.message import MessageListener
import sensor_msgs
import MyROS
from Myro import *

class ImageViewer(NodeMain):

  def __init__(self):
    self.lastP = None

  def getDefaultNodeName(self):
    return GraphName.of("calico_image_viewer")

  def onStart(self, node):
    class myListener(MessageListener):
        def onNewMessage(self, image):
            p = MyROS.rosRGBToPicture(image.getData(), 320, 240)
            show(p)

    self.subscriber = node.newSubscriber("/usb_cam/image_raw/compressed", sensor_msgs.CompressedImage._TYPE)

    queue_size=1
    self.subscriber.addMessageListener(myListener(), queue_size)

MyROS.ROSRun("usb_cam", "usb_cam_node", "_pixel_format:=yuyv", "_image_width:=320", "_image_height:=240")

iv = ImageViewer()
nodeMainExecutor = DefaultNodeMainExecutor.newDefault()
nodeConfig = NodeConfiguration.newPrivate()
nodeMainExecutor.execute(iv, nodeConfig);

#iv.subscriber.shutdown()

# assuming this ros service is running:
# rosrun usb_cam usb_cam_node _pixel_format:=yuyv _image_width:=320 _image_height:=240 

from org.ros.namespace import GraphName
from org.ros.node import *
from org.ros.message import MessageListener
import sensor_msgs
import MyROS
from Myro import show

class ImageViewer(NodeMain):

  def getDefaultNodeName(self):
    return GraphName.of("calico_image_viewer")


  def onStart(self, node):
    class myListener(MessageListener):
        def onNewMessage(self, image):
            p = MyROS.rosRawRGBToPicture(image.getData(), image.getWidth(), image.getHeight())
            show(p)

    self.subscriber = node.newSubscriber("/usb_cam/image_raw", sensor_msgs.Image._TYPE)

    queue_size=1
    self.subscriber.addMessageListener(myListener(), queue_size)


MyROS.ROSRun("usb_cam", "usb_cam_node", "_pixel_format:=yuyv", "_image_width:=320", "_image_height:=240")

iv = ImageViewer()
nodeMainExecutor = DefaultNodeMainExecutor.newDefault()
nodeConfig = NodeConfiguration.newPrivate()
nodeMainExecutor.execute(iv, nodeConfig);

#iv.subscriber.shutdown()

# assumes usb_cam is running:
# rosrun usb_cam usb_cam_node _pixel_format:=yuyv _image_width:=320 _image_height:=240

from Myro import *
#import MyROS
#MyROS.ROSRun("usb_cam", "usb_cam_node", "_pixel_format:=yuyv", "_image_width:=320", "_image_height:=240")

#makeRobot("ROSRobot")
#makeRobot("ROSROBOT", "myip", "ROSip")
makeRobot("ROSRobot", "192.168.1.2", "192.168.1.5")

#to access remote ROS process

wait(5)
for t in timer(10):
  show(takePicture())



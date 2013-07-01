from Myro import *
makeRobot("ROSARDrone")
wait(4)
from Myro import robot

reset()

show(takePicture())
takeoff()
lst = []
for t in timer(10):
    p = takePicture()
    lst.append(p)

land()

savePicture(lst[4], "calicros1.jpg")
savePicture(lst, "calicoros1.gif")
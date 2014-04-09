from Myro import *

#makeRobot("ROSARDrone", "myip", "ROSip")

speak("Please prepare for liftoff")
for i in range(10, 0, -1):
    speak(str(i))

reset()
show(takePicture())
takeoff()

lst = []
for t in timer(10):
    p = takePicture()
    lst.append(p)
    show(p)
land()

savePicture(lst, "119animation.gif")

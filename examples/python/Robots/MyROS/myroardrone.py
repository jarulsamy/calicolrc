from Myro import *

calico.ROSCore()
calico.ROSRun("ardrone_autonomy", "ardrone_driver")

makeRobot("ROSARDrone")
speak("Please prepare for liftoff")
for i in range(5, 0, -1):
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

savePicture(lst, "animation.gif")

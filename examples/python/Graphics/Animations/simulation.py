from Myro import *

init("sim")

## for i in range(4):
##     forward(1, 1.7)
##     turnLeft(1, 1)
##     print("stall", getStall())
##     print("IRs", getIR())
##     print("Obstacles", getObstacle())
##     show(takePicture())

## backward(1, 1)
turnLeft(.3)
pics = []
for i in range(20):
    pics.append(takePicture())
    show(pics[-1])
    wait(1)
stop()
savePicture(pics, "animated.gif")

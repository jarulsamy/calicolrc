from Myro import *

init("sim")

senses()
show(takePicture())
for i in range(4):
    forward(1, 1.7)
    turnLeft(1, 1)
    print("stall", getStall())
    print("IRs", getIR())
    print("Obstacles", getObstacle())
    show(takePicture(), init=False)

backward(1, 1)
turnLeft(.3)
while True:
    show(takePicture(), init=False)
    wait(.1)


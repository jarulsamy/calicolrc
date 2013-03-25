from Myro import *

init("sim")

senses()
show(takePicture(), init=True)
for i in range(4):
    forward(1, 1.7)
    turnLeft(1, 1)
    print("stall", getStall())
    print("IRs", getIR())
    print("Obstacles", getObstacle())
    show(takePicture())

backward(1, 1)
turnLeft(.3)
while True:
    show(takePicture())
    wait(.1)


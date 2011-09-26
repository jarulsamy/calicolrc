from Myro import *

init("sim")

for i in range(4):
    forward(1, 1)
    turnLeft(1, .7)
    print("stall", getRobot().stall)
    show(takePicture())

turnLeft(.1)
while True:
    show(takePicture())
    wait(.1)

#forward(1)
#while getSimulation().window.state == "run":
#    if getRobot().stall:
#        print("stall", getRobot().stall)

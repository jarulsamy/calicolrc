from Myro import makeRobot, Simulation, Color, yesno
from ai.conx import *

network = Network()
network.addLayers(2, 5, 1)

width, height = 500, 400
sim = Simulation("Learning Simulation", width, height, Color("lightgrey"))
sim.addWall((0, 0), (width, 10), Color("black"))
sim.addWall((0, 0), (10, height), Color("black"))
sim.addWall((width - 10, 0), (width, height), Color("black"))
sim.addWall((0, height- 10), (width, height), Color("black"))

robot = makeRobot("SimPioneer", sim)
sim.setOption(0, "show-sensors", True)

def trainOne():
    count = 0
    error = [0] * 100
    while count < 100 or max(error) > .1:
        sim.setPose(0, width/4, height/2, 180)
        robot.backward(1)
        while True:
            distances = [float(x) for x in robot.getDistance()] # between 0 and 1 (1 = no obstacle in site)
            targets = [float(distances[0])]
            robot.backward(targets[0]/.5 - 1)
            feedback = network.step(input=distances, output=targets)
            print(distances, targets, "Error:", "%.2f" % feedback[0])
            error[count % 100] = feedback[0]
            count += 1
            sim.step()
            if distances[0] <= .51:
                break
        # check TSS error, and stop at some point
    robot.stop()

def netDrive(x=width/4, y=height/2, minDistance=.51):
    sim.setPose(0, x, y, 180)
    while not yesno("Ready?"): pass
    output = [1, 1]
    while output[0] > minDistance:
        distances = [float(x) for x in robot.getDistance()] # between 0 and 1 (1 = no obstacle in site)
        output = network.propagate(input=distances)
        print(distances, output[0])
        robot.backward(float(output[0])/.5 - 1)
        sim.step()

trainOne()
netDrive()
netDrive(width/10 * 9, minDistance=0.0)
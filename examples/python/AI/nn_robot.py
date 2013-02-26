from Myro import makeRobot, Simulation, Color
from ai.conx import *

network = Network()
network.addLayers(2, 2, 1)

network.setInputs([[0, 0], [0, 1], [1, 0], [1, 1]])
network.setTargets([[0], [1], [1], [0]])

network.setTolerance(.2)
##network.train()

width, height = 500, 400
sim = Simulation("Learning Simulation", width, height, Color("lightgrey"))
sim.addWall((0, 0), (width, 10), Color("black"))
sim.addWall((0, 0), (10, height), Color("black"))
sim.addWall((width - 10, 0), (width, height), Color("black"))
sim.addWall((0, height- 10), (width, height), Color("black"))

robot = makeRobot("SimPioneer", sim)
sim.setPose(0, width/4, height/2, 180)
sim.setOption(0, "show-sensors", True)

robot.getDistance() # between 0 and 1 (1 = no obstacle in site)
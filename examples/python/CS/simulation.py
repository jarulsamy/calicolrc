import Myro
from Myro import *
from Graphics import *

sim = Simulation("My World", 500, 400, Color("lightgrey"))
# Add lights first:
sim.addLight((200, 200), 25, Color("orange"))
# Add walls and other objects:
sim.addWall((10, 10), (20, 20), Color("black"))
sim.addWall((100, 100), (120, 120))
# Is movable:
circle = Circle((100, 200), 20)
sim.addShape(circle)
# Start simulation loop:
sim.setup()

# Make a robot (save return, or use Myro.robot)
makeRobot("SimScribbler", sim)

# Control it somehow:
#senses()
#joystick()

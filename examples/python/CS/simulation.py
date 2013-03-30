import Myro
from Myro import *
from Graphics import *

width, height = 500, 400,
sim = Simulation("My World", width, height, Color("lightgrey"))
# Add lights first:
sim.addLight((200, 200), 25, Color("orange"))
# Add walls and other objects:
sim.addWall((10, 10), (20, 20), Color("black"))
sim.addWall((100, 100), (120, 120))
# Add boundaries
sim.addWall((0, 0), (width, 10), Color("blue"))
sim.addWall((0, 0), (10, height), Color("lightblue"))
sim.addWall((width - 10, 0), (width, height), Color("blue"))
sim.addWall((0, height - 10), (width, height), Color("lightblue"))

# Is movable:
circle = Circle((100, 200), 20)
sim.addShape(circle)
# Start simulation loop:
sim.setup()

# Make a robot (save return, or use Myro.robot)
makeRobot("SimScribbler", sim)

# Control it somehow:
#senses()
joystick()

while True:
    show(takePicture())
    
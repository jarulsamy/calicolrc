from Myro import Simulation, makeRobot
from Graphics import *

outdoor = Simulation(("Forest World"), 500,400, Color("green"))

circle = Circle((50,50),20)
circle.fill = Color("sienna")
outdoor.addShape(circle)

circle = Circle((100,75),20)
circle.fill = Color("sienna")
outdoor.addShape(circle)

circle = Circle((350,45),20)
circle.fill = Color("sienna")
outdoor.addShape(circle)

circle = Circle((200,300),20)
circle.fill = Color("sienna")
outdoor.addShape(circle)

circle = Circle((100,375),20)
circle.fill = Color("sienna")
outdoor.addShape(circle)

circle = Circle((400,245),20)
circle.fill = Color("sienna")
outdoor.addShape(circle)

circle = Circle((200,215),20)
circle.fill = Color("sienna")
outdoor.addShape(circle)

circle = Circle((35,200),20)
circle.fill = Color("sienna")
outdoor.addShape(circle)

outdoor.setup()
makeRobot("SimScribbler", outdoor)

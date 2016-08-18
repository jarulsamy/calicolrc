from Myro import Simulation, makeRobot
from Graphics import *

indoor = Simulation(("Indoor World"), 500,400, Color("grey"))

indoor.addWall((0,0), (10,400), Color("red"))
indoor.addWall((0,0), (500,10), Color("red"))
indoor.addWall((0,390), (500,400), Color("red"))
indoor.addWall((490,0), (500,400), Color("red"))

circle = Circle((130,200),40)
circle.fill = Color("hotpink")
indoor.addShape(circle)

square = Rectangle((200,300), (280,380))
square.fill = Color("green")
indoor.addShape(square)

square = Rectangle((400,70), (480,150))
square.fill = Color("aqua")
indoor.addShape(square)

indoor.addLight((100,100),40, Color("yellow"))

indoor.setup() # starts simulator thread
makeRobot("SimScribbler", indoor)

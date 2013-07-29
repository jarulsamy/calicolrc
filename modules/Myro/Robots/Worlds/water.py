from Myro import Simulation, makeRobot
from Graphics import *

water = Simulation(("Water World"), 500,400, Color("dodgerblue"))

water.addWall((0,0), (10,400), Color("white"))
water.addWall((0,0), (500,10), Color("white"))
water.addWall((0,390), (500,400), Color("white"))
water.addWall((490,0), (500,400), Color("white"))

circle = Circle((350,50),40)
circle.fill = Color("crimson")
water.addShape(circle)

square = Rectangle((30,100), (100,170))
square.fill = Color("gold")
water.addShape(square)

circle2 = Circle((200,300),45)
circle2.fill = Color("chartreuse")
water.addShape(circle2)

water.setup() # starts simulator thread
r = makeRobot("SimScribbler", water)

from Processing import *
from Myro import *
import Processing

makeRobot("Hummingbird")
window(800, 200)
smooth()
background(0)
noStroke()

x = 0
size = 3

def draw(o, e):
    global x
    #pos = get("2","distance")
    pos = get("4","light")
    y = Processing.map(pos, 255, 0, 0, height())
    ellipse(x, y, size, size)
    x = x + 1
    if x > width():
        x = 0
        background(0)

frameRate(120)
onLoop += draw
loop()
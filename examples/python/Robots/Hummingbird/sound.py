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

def draw():
    global x
    y3 = Processing.map(get("1","sound"), 80, 0, 0, height())

    fill(255, 196, 0)
    ellipse(x, y3, size, size)

    x = x + 1
    if x > width():
        x = 0
        background(0)

frameRate(120)
onLoop += draw
loop()

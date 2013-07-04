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
    y1 = Processing.map(get("2","distance"), 80, 0, 0, height())
    y2 = Processing.map(get("4","light"), 255, 0, 0, height())
    y3 = Processing.map(get("1","knob"), 255, 0, 0, height())

    fill(255, 196, 0)
    ellipse(x, y1, size, size)
    fill(0, 255, 196)
    ellipse(x, y2, size, size)
    fill(255, 0, 196)
    ellipse(x, y3, size, size)

    x = x + 1
    if x > width():
        x = 0
        background(0)

frameRate(120)
onLoop += draw
loop()
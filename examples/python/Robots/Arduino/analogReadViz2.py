from Processing import *
from Myro import *
import Processing

makeRobot("Arduino", "/dev/ttyUSB0")
window(800, 200)
smooth()
background(0)
noStroke()

x = 0
for t in timer(30):
    v = analogRead(0)
    y = Processing.map(v, 1024, 0, 0, height())

    fill(255, 0, 196)
    ellipse(x, y, 3, 3)

    x = x + 1
    if x > width():
        x = 0
        background(0)

    delay(1)
    redraw()
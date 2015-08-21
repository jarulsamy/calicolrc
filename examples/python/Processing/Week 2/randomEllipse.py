from Processing import *
from Graphics import *

window(300, 300)
smooth()
frameRate(32)

def draw():
    fill(random(255), random(255), random(255))
    ellipse(mouseX(), mouseY(), 30, 30)

onLoop += draw
loop()

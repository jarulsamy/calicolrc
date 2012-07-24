from Processing import *

window(300, 300)
smooth()
frameRate(1)

def draw(o, e):
    fill(random(255), random(255), random(255))
    ellipse(mouseX(), mouseY(), 30, 30)

onLoop += draw
loop()

from Processing import *
import string
from random import choice, uniform
window(1680, 1050)
window(displayWidth(), displayHeight())
fullscreen()
smooth()
frameRate(60)
noFill()
textFont("Courier")
textSize(24)
stroke(0, 255, 22)

def keyPressed():
    if (key() == "space"):
        stroke(random(255), random(255), random(255))

onKeyPressed += keyPressed

choices = string.ascii_lowercase + string.uppercase + string.punctuation
msg = []
for i in range(2**13):
    msg = msg + [choice(choices)]

def draw():
    background(0)
    for i in range(150):
        idx = int(uniform(0, len(msg)))
        msg[idx] = choice(choices)
    text("".join(msg), 0, 0, width(), height())



onLoop += draw
loop()

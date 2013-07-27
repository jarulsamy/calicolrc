from Processing import *
import string
import random

window(500, 500)
window(displayWidth(), displayHeight())
fullscreen()
smooth()
noFill()
textFont("Courier")
textSize(64)
frameRate(40)
stroke(0, 255, 24)

def draw(o, e):
    background(0)
    msg = ""
    choices = string.ascii_lowercase + string.uppercase + string.punctuation
    for i in range(2**11):
        msg = msg + random.choice(choices)
    text(msg, 0, 0, width(), height())


onLoop += draw
loop()
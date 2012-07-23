# This is a variation on 001.py
# This version un/hooks loop event handler on mouse clicks
# instead of stopping/starting timer
from Processing import *

window(600, 400)
background(255, 200, 200)

# Sets internal timer elapsed time
frameRate(1)

# Function that will handle internal timer elapsed event
def draw(o, e):
    line( random(600), random(400), random(600), random(400))
    #println(frameCount())
    #println(focused())

def doMousePressed(o, e):
    global onLoop
    onLoop -= draw  # Unhook internal timer elapsed event handler

def doMouseReleased(o, e):
    global onLoop
    onLoop += draw  # Hook internal timer elapsed event handler

# Handle events
onMousePressed += doMousePressed
onMouseReleased += doMouseReleased
onLoop += draw

# Start internal timer
loop()

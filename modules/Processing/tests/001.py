from Processing import *

window(600, 400)
background(255, 200, 200)

# Sets internal timer elapsed time
frameRate(1)

# Function that will handle internal timer elapsed event
def draw(o, e):
    line( random(600), random(400), random(600), random(400))

def doMousePressed(o, e):
    noLoop()        # Stop internal timer

def doMouseReleased(o, e):
    loop()          # Stop internal timer again

# Handle events
onMousePressed += doMousePressed
onMouseReleased += doMouseReleased
onLoop += draw

# Start internal timer
loop()

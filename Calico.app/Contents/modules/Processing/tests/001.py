from Processing import *

window(600, 400)
background(255, 200, 200)

# Sets internal timer elapsed time
frameRate(1)

# Function that will handle internal timer elapsed event
def draw():
    line( random(600), random(400), random(600), random(400))

def doMousePressed():
    noLoop()        # Stop internal timer

def doMouseReleased():
    loop()          # Stop internal timer again

# Handle events
onMousePressed += doMousePressed
onMouseReleased += doMouseReleased
onLoop += draw

# Start internal timer
loop()

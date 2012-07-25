# The Walker

# Copyright  2012 Mark F. Russo
# Bryn Mawr College, Department of Computer Science
# Permission is granted to copy and distribute provided 
# this entire copyright notice is reproduced in all copies.

# Use the arrow keys to move the Walker around the screen

from Processing import *

walkPose = False   # Current walk pose
speed = 5.0          # Max walking speed in any direction
cX = 100.0           # Current walker location
cY = 100.0

window(500, 500)
smooth()
frameRate(20)

def doLoop(o, e) :
    global walkPose, speed, cX, cY
    background(255)
    fill(200)
    stroke(0)

    # Draw the walker
    # Space legs based on current walk step
    line(cX, cY, cX, cY+20)                 # body
    ellipse(cX, cY, 10, 10)                 # head
    
    if walkPose == True:
        line(cX-10, cY+10, cX+10, cY+10)    # arms at pose 1
        line(cX, cY+20, cX-10, cY+30)       # legs at pose 1
        line(cX, cY+20, cX+10, cY+30)

    else:
        line(cX-10, cY+5, cX+10, cY+15)     # arms at pose 2
        line(cX, cY+20, cX-5, cY+30)        # legs at pose 2
        line(cX, cY+20, cX+5, cY+30)

def doKeyPressed(o, e):
    global walkPose, speed, cX, cY
    if keyCode() == 65362:
        walkPose = not walkPose
        cY -= speed
    elif keyCode() == 65364:
        walkPose = not walkPose
        cY += speed
    elif keyCode() == 65361:
        walkPose = not walkPose
        cX -= speed
    elif keyCode() == 65363:
        walkPose = not walkPose
        cX += speed

onLoop += doLoop
onKeyPressed += doKeyPressed

loop()

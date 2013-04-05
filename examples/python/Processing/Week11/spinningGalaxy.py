## For more information on how this math works, check out:
## http:##blog.marmakoide.org/?p=1

from Processing import *
import math

window(400, 400)
noStroke()
fill(252, 252, 205)

## The number of points
N = 240

## The initial angle change between succesive points
## Roughly 360 * (1 - 1/phi) degrees
angleChange = 137.5

## The radius of the circle
maxRadius = 200

def draw(o, e):
    global angleChange
    background(26, 26, 26)
    
    for i in range(N):
        radius = maxRadius * math.sqrt(i / N)
        theta = angleChange * i

        x = 200 + radius * math.cos(theta)
        y = 200 + radius * math.sin(theta)
        ellipse(x, y, 5, 5)
    
    angleChange += 0.0001
    doEvents()

# Set up looping
onLoop += draw
frameRate(60)
loop()

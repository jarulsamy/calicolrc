# bounce.py

# Copyright  2012 Mark F. Russo
# Bryn Mawr College, Department of Computer Science
# Permission is granted to copy and distribute provided
# this entire copyright notice is reproduced in all copies.

from Processing import *

sx = 0.0    # x position
sy = 0.0    # y position
vx = 1.0    # x velocity
vy = 1.0    # y velocity
ay = 0.2    # y acceleration (gravity)

window(500, 500)
fill(255, 0, 0)
smooth()
ellipseMode(CENTER)

def draw(o, e):
    global sx, sy, vx, vy, ay

    # Equations of Motion
    sx = sx + vx
    sy = sy + vy
    vy = vy + ay

    # Bounce off walls
    if sx <= 0.0 or sx >= width(): vx = -vx

    # Bounce off floor and
    # lose some velocity due to friction
    if sy >= (height()-10.0):
        sy = (height()-10.0)
        vy = -0.9*vy

    # Draw at current location
    background(255)
    ellipse(sx, sy, 20, 20)

frameRate(100)
onLoop += draw
loop()

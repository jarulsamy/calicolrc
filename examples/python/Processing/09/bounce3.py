# bounce3
# Demonstrating the use of lists
# Expand program to bounce a variable number of balls

from Processing import *

nBalls = 20

# Open window
window(500, 500)
fill(255, 0, 0)
smooth()
ellipseMode(CENTER)

w = width()
h = height()

# Init all variables
ay = 0.2                    # y acceleration (gravity)
sx = []                     # x positions
sy = []                     # y positions
vx = []                     # x velocities
vy = []                     # y velocities

# Initialize lists
for i in range(nBalls):
    sx.append( random(0.0, w) )
    sy.append( random(0.0, 10.0) )
    vx.append( random(-3.0, 3.0) )
    vy.append( random(0.0, 5.0) )

# Redraw all balls
def draw(o, e):
    global w, h, ay, sx, sy, vx, vy
    background(255)

    # Move balls
    for i in range(nBalls):
        sx[i] += vx[i]
        sy[i] += vy[i]
        vy[i] += ay

        # Bounce off walls and floor
        if sx[i] <= 10.0 or sx[i] >= (w-10.0): vx[i] = -vx[i]
        if sy[i] >= (h-10.0) and vy[i] > 0.0: vy[i] = -0.9*vy[i]

        # Draw ball
        ellipse( sx[i], sy[i], 20, 20)

# Set up looping
onLoop += draw
frameRate(25)
loop()

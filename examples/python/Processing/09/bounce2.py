# bounce2

from Processing import *

# Open window
window(500, 500)
fill(255, 0, 0)
smooth()
ellipseMode(CENTER)

w = width()
h = height()

# Init all variables
ay = 0.2                    # y acceleration (gravity)
sx = random(0.0, w)         # x position
sy = random(0.0, 10.0)      # y position
vx = random(-3.0, 3.0)      # x velocity
vy = random(0.0, 5.0)       # y velocity

sx2 = random(0.0, width())  # x position
sy2 = random(0.0, 10.0)     # y position
vx2 = random(-3.0, 3.0)     # x velocity
vy2 = random(0.0, 5.0)      # y velocity

def draw(o, e):
    global w, h, ay, sx, sy, vx, vy, sx2, sy2, vx2, vy2
    background(255)

    # Move ball
    sx += vx
    sy += vy
    vy += ay

    sx2 += vx2
    sy2 += vy2
    vy2 += ay

    # Bounce off walls and floor
    if sx <= 10.0 or sx >= (w-10.0): vx = -vx
    if sy >= (h-10.0) and vy > 0.0: vy = -0.9*vy

    if sx2 <= 10.0 or sx2 >= (w-10.0): vx2 = -vx2
    if sy2 >= (h-10.0) and vy2 > 0.0: vy2 = -0.9*vy2

    # Draw ball
    ellipse( sx, sy, 20, 20)
    ellipse( sx2, sy2, 20, 20)

# Set up looping
onLoop += draw
frameRate(25)
loop()

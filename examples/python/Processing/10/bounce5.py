# bounce5
# Demonstrating the use of objects

from Processing import *

# Declare a Ball class that encapsulates all attributes and methods of a Ball
class Ball:
    ay = 0.2        # y acceleration (gravity)
    sx = 0.0        # x position
    sy = 0.0        # y position
    vx = 0.0        # x velocity
    vy = 0.0        # y velocity
    diameter = 50   # diameter and color
    clr = color(255, 0, 0)

    # The connstructir chooses a random starting location and velocity
    # and sets the give diameter and color
    def __init__(self, diam, clr):
        self.sx = random(0.0, width())
        self.sy = random(0.0, 10.0)
        self.vx = random(-3.0, 3.0)
        self.vy = random(0.0, 5.0)
        self.diameter = diam
        self.clr = clr

    # Update the position and velocity of this Ball instance
    def update(self):
        # Move ball
        self.sx += self.vx
        self.sy += self.vy
        self.vy += self.ay

        # Bounce off walls and floor
        if self.sx <= (0.5*self.diameter) or self.sx >= (width()-0.5*self.diameter):
            self.vx = -self.vx
        if self.sy >= (height()-0.5*self.diameter) and self.vy > 0.0:
            self.vy = -0.9*self.vy

    def draw(self):
        fill( self.clr )
        ellipse( self.sx, self.sy, self.diameter, self.diameter )

# Open a window and set some global parameters
window(500, 500)
smooth()
ellipseMode(CENTER)

# Set the number of Balls in this instance and init the list that holds all instances
nBalls = 10 # 100
balls = []

# Create all Ball instances and add to balls list
for i in range(nBalls):
    diam = random(10, 30)
    clr = color( random(255), random(255), random(255) )
    ball = Ball( diam, clr )
    balls.append( ball )

# Function that updates and draws balls
def draw(o, e):
    background(255)

    # Update and redraw all Ball instances
    for i in range(nBalls):
        balls[i].update()
        balls[i].draw()

    # Update sketch now
    redraw()

# Handle loop event and start looping
frameRate(30)
immediateMode(False)    # Only update scketch when requested
onLoop += draw
startLoop()

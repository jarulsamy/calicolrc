# Based on "Processing: A programming Handbook for
# Visual Designers and Artists"
# p.489

from Processing import *

class Spring2D:
    def __init__(self, x, y, mass, gravity):
        self.x = x
        self.y = y
        self.mass = mass
        self.gravity = gravity
        self.radius = 10
        self.stiffness = 0.2
        self.damping = 0.7
        self.vx, self.vy = 0, 0 # velocities

    def update(self, x, y):
        forceX = (x - self.x) * self.stiffness
        ax = forceX / self.mass
        self.vx = self.damping * (self.vx + ax)
        self.x += self.vx

        forceY = (y - self.y) * self.stiffness + self.gravity
        ay = forceY / self.mass
        self.vy = self.damping * (self.vy + ay)
        self.y += self.vy

    def display(self, x, y):
        noStroke()
        ellipse(self.x, self.y, self.radius * 2, self.radius * 2)
        stroke(255)
        line(self.x, self.y, x, y)

gravity = 5.0
mass = 2.0
s1, s2 = None, None

def setup():
    global s1, s2
    window(400, 400)
    smooth()
    fill(0)
    s1 = Spring2D(0, width()/2, mass, gravity)
    s2 = Spring2D(0, width()/2, mass, gravity)

def draw():
    global s1, s2
    background(204)
    s1.update(mouseX(), mouseY())
    s1.display(mouseX(), mouseY())
    s2.update(s1.x, s1.y)
    s2.display(s1.x, s1.y)

setup()
onLoop += draw
loop()

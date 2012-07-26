# Mouse orbiter
from Processing import *
import math

window(500, 300)
background(255)
immediateMode(False)

angle = 0                  # Orbit angle
while True:
    background(255)
    fill(0, 0, 255)
    angle = angle + 0.3         # Increment angle
    dX = 30.0*math.cos(angle)  # Mouse position offset
    dY = 30.0*math.sin(angle)  # Draw two orbiting shapes
    ellipse(mouseX() + dX, mouseY() + dY, 5, 5)
    ellipse(mouseX() - dX, mouseY() - dY, 5, 5)

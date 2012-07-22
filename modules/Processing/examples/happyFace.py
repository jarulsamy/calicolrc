# happyFace.pde

from Processing import *
import math

window(500, 500)
background(0)
smooth()

# Draw happy face
def happyFace( x, y, diam ):
    # Face
    fill(255, 255, 0)
    stroke(0)
    strokeWeight(2)
    ellipseMode(CENTER)
    ellipse(x, y, diam, diam )

    # Smile
    startAng = 0.1*math.pi
    endAng = 0.9*math.pi
    smileDiam = 0.6*diam
    arc(x, y, smileDiam, smileDiam, startAng, endAng)

    # Eyes
    offset = 0.2*diam
    eyeDiam = 0.1*diam
    fill(0)
    ellipse(x-offset, y-offset, eyeDiam, eyeDiam)
    ellipse(x+offset, y-offset, eyeDiam, eyeDiam)

def onPressed(o, e):
    diam = random(30, 60)
    happyFace( mouseX(), mouseY(), diam )

onMousePressed += onPressed

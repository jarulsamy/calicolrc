# Build a 2D normal or uniform distribution
# by overlapping semitransparent circles

from Processing import *
import math

size(500, 500)
background(0)
fill(255,255,255,32)
noStroke()
smooth()

def draw(o, e):
    # A uniform distribution
    #x = random(0,width())
    #y = random(0,height())

    # A normal distribution
    x = map( normal(), -3.0, 3.0, 0, width() )
    y = map( normal(), -3.0, 3.0, 0, width() )
    ellipse(x, y, 10, 10)

# Normal distribution sampler
def normal():
    # Polar form of Box-Muller transformation
    # Converts uniform distribution in [-1,1]
    # to a normal distribution w/ mean=0.0, sd=1.0
    
    while (True):
        x1 = random(-1.0, 1.0)
        x2 = random(-1.0, 1.0)
        w = x1 * x1 + x2 * x2
        if w < 1.0: break    # Try again if w >= 1.0

    # Generate two samples
    w = math.sqrt( (-2.0 * math.log( w ) ) / w )
    y1 = x1 * w
    y2 = x2 * w
    
    return y1

frameRate(1)
onLoop += draw
loop()

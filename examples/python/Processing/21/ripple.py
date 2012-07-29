# ripple.py

from Processing import *
import math

window(400, 400)

# Get pixel colors from window
loadPixels()

for i in range( 400 ):
    for j in range( 400 ):
        # Compute distance to center of sketch
        d = dist( i, j, 200, 200 )

        # Compute the sine of the distance and scale
        b = math.sin(d/5.0)
        b = ((200.0-d)/200.0)*map(b, -1.0, 1.0, 0, 255);

        # Constrain distance to byte range
        b = constrain(b, 0, 255)

        # Set pixel color
        c = color( b )
        setPixel(i, j, c)

# Repaint
updatePixels()

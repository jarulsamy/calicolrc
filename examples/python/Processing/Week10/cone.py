# cone.py

from Processing import *

window(400, 400)

# Get pixel colors from window
loadPixels()

for i in range( 400 ):
    for j in range( 400 ):
        # Compute distance to center of sketch
        # Constrain distance to byte range
        b = dist( i, j, 200, 200 )
        b = constrain(b, 0, 255)

        # Set pixel color
        c = color( b )
        setPixel(i, j, c)

# Repaint
updatePixels()

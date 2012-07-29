# pointlillism.py

from Processing import *

# Load the picture, create a window of the same size, and draw the picture
img = loadImage("images/bmc3.jpg")
window( img.width(), img.height() )
image(img, 0, 0)

# Load pixels so all colors are available to read
loadPixels()

# Sample 20000 colors from random locations
# and paint a small circle of same color at same location
noStroke()
#while True:
for i in xrange(20000):
    x = random(width())
    y = random(height())
    c = getPixel(x, y)
    fill(c)
    ellipse(x, y, 7, 7)
    delay(1)

    # Break out of loop when mouse is pressed
    if isMousePressed(): break

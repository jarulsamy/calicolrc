# threshold.py
# Demonstrating the threshold function

from Processing import *

# Load the image to process
img = loadImage("obama.jpg")
#img = loadImage("colony.jpg")

# Create a window of the same size
w = int( img.width() )
h = int( img.height() )
window( w, h )

# Draw the image
image( img, 0, 0)

# Perform the threshold function
def threshold(o, e):

    # Redraw the original image
    image( img, 0, 0)

    # Get the cutoff as the y mouse position
    cutoff = mouseY()
    print( "cutoff =", cutoff )

    # Load pixels in preparation for processing
    loadPixels()

    # Loop over all pixels
    for i in range(w):
        for j in range(h):

            # Get the color
            c = getPixel(i, j)

            # Convert the color to grayscale
            gray = 0.2989 * red(c) + 0.5870 * green(c) + 0.1140 * blue(c)

            # Compute threshold: white if above cutoff, and black if below cutoff
            if gray >= cutoff:
                gray = 255
            else:
                gray = 0

            # Reset color to threshold value
            setPixel(i, j, color(gray))

    # Update pixels in image
    updatePixels()

# When the mouse is pressed, perform the threshold function
onMousePressed += threshold


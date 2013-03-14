# imageExample3

# Fade an image on each mouse click by fading each pixel individually.
# The color components of each pixel are extracted, multiplied by a fraction, and replaced.

from Processing import *

window(500, 400)

# Load and display the image of interest
img = loadImage("natura-morta.jpg")
image(img, 50, 40)

# Handle mouse pressed events
def doMousePressed(o, e):
    global img

    # Define the fade factor
    fade = 0.95 #1.05

    # Load all pixels for manipulation
    img.loadPixels()

    # Loop over all columns
    for i in range(img.width()):

        # For each column, loop over all rows
        for j in range(img.height()):

            # Get the pixel color
            p = img.getPixel(i, j)

            # Build a new, faded color
            r = constrain(fade*red(p), 0, 255)
            g = constrain(fade*green(p), 0, 255)
            b = constrain(fade*blue(p), 0, 255)
            newColor = color( r, g, b )

            # Replace the pixel color
            img.setPixel(i, j, newColor)

    # Update the color data in the image
    img.updatePixels()

    # Redraw the image
    image(img, 50, 40)

# Handle the mouse pressed event
onMousePressed += doMousePressed

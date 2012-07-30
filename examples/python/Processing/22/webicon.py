from Processing import *

# Define colors
darkBlue = color(0, 51, 76)
reddish = color(217, 26, 33)
lightBlue = color(112, 150, 158)
yellow = color(252, 227, 166)

# Load image
img = loadImage("obama.jpg")
w = toInt( img.width() )
h = toInt( img.height() )

# Open a window and draw the initial image
window( w, h )
image(img, 0, 0)

# Load pixels so they can be manipulated
loadPixels()

# Loop over all pixels in the images
for i in range(w):
    for j in range(h):

        # Get pixel color
        c = getPixel(i, j)

        # Sum up all color components
        total = red(c)+green(c)+blue(c)

        # Remap to new color depending upon total
        if total < 182:
            newColor = darkBlue

        elif total < 364:
            newColor = reddish

        elif total < 440:
            newColor = lightBlue

        else:
            newColor = yellow

        # Update to new color
        setPixel(i, j, newColor)

# Paint back on the window
updatePixels()

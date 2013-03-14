from Processing import *

window(255, 255)

# Load the pixels
loadPixels()

# Set pixel grayscale value to row index
for i in range(255):
    for j in range(255):
        setPixel(i, j, j)

# Update pixels
updatePixels()

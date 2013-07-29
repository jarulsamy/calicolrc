from Processing import *

window(500, 500)

# Create a new image
im = createImage(100, 100, ARGB)
#background(255, 0, 0)
# Modify its pixels
im.loadPixels()
#background(0,  255, 0)
for i in range(100):
    im.setPixel(i, i, 255, 0, 0)
    im.setPixel(100-i, i, 0, 255, 0)
im.updatePixels()
#background(0, 0, 255)

# Draw it to the window
image(im, 100, 100)
print ("There!")
# Save it to a file - only "png" and "jpeg" are supported
im.save("test.jpeg", True)

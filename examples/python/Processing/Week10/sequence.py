# sequence.py

from Processing import *

# Load all images
images = []
for i in range(1, 16):
    fileName = "images/horse" + str(i) + ".gif"
    img = loadImage( fileName )
    images.append( img )

# Open a window
window( 184, 135 )

# Draw all images to window in sequence
while True:
    # Repeatedly display all images in list
    for i in range(15):
        img = images[i]
        image( img, 0, 0)
        delay(60)

    # Hold mouse down to stop animation
    if isMousePressed() == True:
        break

# Exit the window and stop the program
exit()

# sequence2.py

from Processing import *

# Load all images
images = []
for i in range(1, 16):
    fileName = "images/horse" + str(i) + ".gif"
    img = loadImage( fileName )
    images.append( img )

# Open a window
window( 800, 135 )

x = 0

# Draw all images to window in sequence
while True:

    # Repeatedly display all images in list
    for i in range(15):
        # Clear background
        background(230);

        # Draw image
        img = images[i]
        image( img, x, 0)
        x = (x + 20) % width()
        delay(60)

    # Hold mouse down to stop animation
    if isMousePressed() == True:
        break

# Exit the window and stop the program
exit()

from Processing import *
import math

# A letter "B" on the ski slope

window(700, 400)
textSize(24)

# Redraw the sine function
def drawSlope():
    # Clear the background
    background(240, 240, 255)

    # Initialize the first x and y coordinate, mapping y from [-1, 1] to the screen
    x1 = 0.0
    y1 = map(0.0, -1.0, 1.0, height(), 100.0)

    # Continue to loop until the right edge of the window is reached
    while x1 < width():

        # Increment x in screen coordinates, and then map it to [0, 2PI]
        x2 = x1 + 2.0
        tx2 = map(x2, 0.0, width(), 0.0, TWO_PI)

        # Compute teh corresponding y coordinate, and then map to the screen
        ty2 = math.sin(tx2)
        y2 = map(ty2, -1.0, 1.0, height(), 100.0)

        # Draw a line from the previous coordinates to the new coordinates
        line(x1, y1, x2, y2)

        # Reassign the new point to the old point in preparate for repeating the loop
        x1 = x2
        y1 = y2

# Initialize the x coordinate
x = 0.0

# Define the function that will get called repeatedly
def draw(o, e):
    global x

    # First clear the screen and draw the empty ski slope
    drawSlope()

    # Increment the x coordinate and map to the range [0, 2PI]
    x = (x + 2.0) % width()
    tx = map(x, 0.0, width(), 0.0, TWO_PI)

    # Compute the corresponding y coordiate and map to the screen
    ty = math.sin(tx)
    y = map(ty, -1.0, 1.0, height(), 100.0)

    # Save the current transformation matrix
    pushMatrix()

    # Compute the slope of the sine curve at x - recall d/dx(sin(x)) = cos(x), but y is inverted, so -cos(x)
    a = -math.cos(tx)

    # Translate and rotate the coordiate system
    translate(x, y)
    rotate(a)

    # Draw the B
    text("B", 0.0, 0.0)

    # Restore the transformation matrix
    popMatrix()
    redraw()

# Run draw every 10-ish milliseconds
onLoop += draw
frameRate(10)
loop()

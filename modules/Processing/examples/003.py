from Processing import *

window(600, 400)    # Create a window
background(200)     # Color background
ellipseMode("CENTER")

def draw(o,e):
    background(200)
    ellipse(mouseX(), mouseY(), 50, 50)

def doExit(o, e):
    exit()

# Handle a events
onMouseMoved += draw        # Call draw when the mouse moves
onMousePressed += doExit    # Exit the program when the mouse is clicked

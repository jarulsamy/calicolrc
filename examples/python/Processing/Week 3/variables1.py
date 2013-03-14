# Variables that store the last mouse pressed position.
from Processing import *
window(500, 300)

# Draw a line from the last mouse position to the current position.
lastX = 0
lastY = 0

def mousePressed(o, e):
    global lastX, lastY
    line(lastX, lastY, mouseX(), mouseY())
    lastX = mouseX()
    lastY = mouseY()

onMousePressed += mousePressed

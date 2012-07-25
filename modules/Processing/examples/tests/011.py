from Processing import *

# Testing text functions

window(500, 400)

yoffset = 20
for p in range(12, 36, 2):
    global yoffset
    textSize(p)
    text("blah", 100, yoffset)
    yoffset += (p + 3)

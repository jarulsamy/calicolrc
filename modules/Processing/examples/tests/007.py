from Processing import *

window(500, 500)
background(255)
im = loadImage("blueslug.png")
#im = loadImage("natura-morta.jpg")
#im = loadImage("butterfly.gif")

# Draw actual size with corner in upper left
imageMode(CORNER)
image(im, 0, 0)

# Draw with defined width and height
image(im, 50, 50, 50, 400)

# Draw in a give rectangle by specifying corners
imageMode(CORNERS)
image(im, 250, 250, 500, 500)

# Draw backwards and upside down by setting corners
image(im, 400, 100, 300, 0)

# Draw at given center point with defined width and height
imageMode(CENTER)
translate(250, 250)
rotate( radians(45) )
image(im, 0, 0, 100, 100)

from Processing import *

window(400, 400)

loadPixels()

for i in range( 400 ):
    for j in range( 400 ):
        b = dist( i, j, 200, 200 )
        b = int(b)
        setPixel(i, j, b)

updatePixels()

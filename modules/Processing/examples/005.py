from Processing import *

window(500, 300)
background(0, 0, 200)

def noise():
    background(0, 0, 200)
    loadPixels()
    for i in range(1000):
        x = random(500)
        y = random(300)
        setPixel(x, y, 255, 255, 255, 255)
    updatePixels()
    delay(1)

for i in range(1000):
    noise()

from Processing import *

window(500, 300)
background(0)

# Allow all pixel manipulation without updating the screen
immediateMode(False)

def noise():
    background(0)
    loadPixels()
    for i in range(5000):
        x = random(500)
        y = random(300)
        setPixel(x, y, 255, 255, 255, 255)
    updatePixels()

    # Trigger a redraw and a delay to smooth animation
    redraw()
    delay(1)

for i in range(1000):
    noise()

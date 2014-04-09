from Processing import *
from random import choice

window(1200, 800)
smooth()
textSize(8)
fonts = listFonts()

x = 0
y = 0
for f in fonts:
    textFont(f)
    if y + 8 > height():
        y = 0
        x += 150
    if x > width():
        break
    text(f, x, y)
    y = y + 8
    delay(1)
    redraw()


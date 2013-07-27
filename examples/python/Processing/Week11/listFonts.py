from Processing import *
from random import choice

window(1500, 800)
smooth()
textSize(8)
fonts = listFonts()

x = 0
y = 0
for f in sorted(fonts):
    textFont(f)
    if y + textHeight(f) > height():
        y = 0
        x += 150
    if x > width():
        break
    text(f, x, y)
    y = y + textHeight(f)

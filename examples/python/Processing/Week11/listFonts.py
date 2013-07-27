from Processing import *
from random import choice

window(400, 800)
smooth()
textSize(12)
fonts = listFonts()

y = 0
while y < height():
    f = choice(fonts)
    textFont(f)
    text(f, 0, y)
    y = y + textHeight(f)



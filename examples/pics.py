file = "/home/test/Desktop/Pyjama/images/blankenship.jpg"
from Graphics import *
init()
pic = Picture(file)
win = Window("Laura")
pic.draw(win)
pic.move(150, 150)

#for pixel in pic.getPixels():
#    r, g, b = pixel.getRGB()
#    pixel.setRGB(255 - r, 255 - g, 255 - b)

pixels = list(pic.getPixels())

import random
random.shuffle(pixels)
for p in pixels:
    p.setRGB(255 - p.getRed(), 255 - p.getGreen(), 255 - p.getBlue())

pixels.sort(key=lambda p: p.x + p.y)
for p in pixels:
    p.setRGB(255 - p.getRed(), 255 - p.getGreen(), 255 - p.getBlue())

#for x in range(pic.width):
#   for y in range(pic.height):
#       r = pic.getRed(x, y)
#       g = pic.getGreen(x, y)
#       b = pic.getBlue(x, y)
#       pic.setRGB(x, y, 255 - r, 255 - g, 255 - b)
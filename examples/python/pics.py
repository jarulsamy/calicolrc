from Graphics import *
import random

file = "examples/images/blankenship.jpg"
win = Window("Laura")
pic = Picture(file)
pic.draw(win)
pic.move(150, 150)
pixels = list(pic.getPixels())


def reverse():
    win.mode = 'auto'
    for p in pixels:
        p.setRGB(255 - p.getRed(), 255 - p.getGreen(), 255 - p.getBlue())

def spin():
    win.mode = 'manual'
    for times in range(3):
        for degrees in range(0, 360, 10):
            pic.rotate(10)
            win.step()

def sortem():
    pixels.sort(key=lambda p: p.x + p.y)

def shuffle():
    random.shuffle(pixels)
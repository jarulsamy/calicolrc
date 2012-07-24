from Graphics import *
from Myro import random, getColorNames, pickOne, wait

win = Window()
win.mode = "bitmap"

while True:
    x1, y1, x2, y2 = [random() * 300 for i in range(4)]
    line = Line((x1, y1), (x2, y2))
    line.color = Color(pickOne(getColorNames()))
    line.draw(win)
    wait(.01)
#print(len(win.canvas.shapes))


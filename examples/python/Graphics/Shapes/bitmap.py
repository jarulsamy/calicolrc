from Graphics import *
from Myro import random, getColorNames, pickOne

win = Window()
win.mode = "bitmap"

while win.isRealized():
    x1, y1, x2, y2 = [random() * 300 for i in range(4)]
    line = Line((x1, y1), (x2, y2))
    line.color = Color(pickOne(getColorNames()))
    line.draw(win)


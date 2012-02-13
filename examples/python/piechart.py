from Graphics import *
from Myro import pickOne
import math

win = Window()

data = {"Food": 20, "Car": 40, "School": 30, "Entertainment": 15, "Books": 15}

start = 0
for key in data:
    datum = data[key]
    p = datum/sum(data.values()) * 360
    pie = Pie((win.width/2, win.height/2), win.width/3, start, start + p)
    pie.fill = Color(pickOne(getColorNames()))
    pie.draw(win)
    angle = (start + p/2) * math.pi/180
    x = win.width/3/2 * math.cos(angle)
    y = win.width/3/2 * math.sin(angle)
    text = Text((x + win.width/2, y + win.height/2), key)
    text.fontSize = 12
    text.draw(win)
    start += p
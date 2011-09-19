from Graphics import *
from Myro import random

size = 250
win = Window(size, size)

circle = Circle((size/2, size/2), size/3)
circle.fill = Color("white")
circle.draw(win)

circle2 = Circle((size/2, size/2), size/2.2)
circle2.fill = None
circle2.draw(win)

def average(p1, p2, p3):
    return Point((p1.x + p2.x + p3.x)/3, (p1.y + p2.y + p3.y)/3)

def connect(a1, a2, no_label):
    frame1 = Frame((size/2, size/2))
    frame1.draw(win)
    if no_label:
        t = ""
    else:
        t = " %sMb" % int(random() * 30)
    text1 = Text((0, size/3), t)
    text1.color = Color("black")
    text1.xJustification = "left"
    text1.fontSize = 6
    text1.draw(frame1)
    text1.rotate(-90)
    frame1.rotate(a1)

    frame2 = Frame((size/2, size/2))
    frame2.draw(win)
    text2 = Text((0, size/3), t)
    text2.color = Color("black")
    text2.xJustification = "left"
    text2.fontSize = 6
    text2.draw(frame2)
    text2.rotate(-90)
    frame2.rotate(a2)

    p1 = frame1.getScreenPoint(text1.getCenter())
    p2 = frame2.getScreenPoint(text2.getCenter())

    control = average(p1, Point(size/2, size/2), p2)

    curve = Curve((p1.x, p1.y), (control.x, control.y),
                  (control.x, control.y), (p2.x, p2.y))
    curve.outline = Color("red") if random() < .9 else Color("blue")
    curve.setWidth(1)
    curve.draw(win)

already = {}
for count in range(0, 200, 5):
    no_label = False
    r = int(random() * 36 * 2) * 5
    if count in already or r in already:
        no_label = True
    connect(count, r, no_label)
    already[count] = 1
    already[r] = 1

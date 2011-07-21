from Graphics import *
import random

win = Window()
win.setBackground(Color("white"))

offset = 50
side = 300
size = side - 2 * offset
width, height = side, side

def makeP():
    r = random.randint(0, 3)
    if r == 0:
        p1 = Point(offset + random.random() * size, offset)
    elif r == 1:
        p1 = Point(width - offset, offset + random.random() * size)
    elif r == 2:
        p1 = Point(offset + random.random() * size, height - offset)
    elif r == 3:
        p1 = Point(offset, offset + random.random() * size)
    return p1

for i in range(200):
    P1 = makeP()
    P2 = makeP()
    intensity = random.randint(100, 255)
    if max(P1.x, P1.y) < 150:
        control = Point(125, 125)
        color = Color(intensity, 0, 0, random.random() * 255)
    elif max(P2.x, P2.y) < 150:
        control = Point(125, 125)
        color = Color(intensity, intensity, 0, random.random() * 255)
    elif min(P2.x, P2.y) > 150:
        control = Point(125, 125)
        color = Color(0, intensity, 0, random.random() * 255)
    else:
        control = Point(175, 175)
        color = Color(0, 0, intensity, random.random() * 255)
    c = Curve(P1, control, control, P2)
    c.draw(win)
    c.outline = color

r = Rectangle((offset, offset), (width - offset, height - offset))
r.fill = None
r.draw(win)


from Graphics import *
import time

win = Window("Clock")
win.mode = "manual"

face = Circle((150, 150), 125)
face.fill = None
face.draw(win)

s = Frame(150, 150)
line = Line((0, 0), (100, 0))
line.color = Color("red")
line.draw(s)

m = Frame(150, 150)
line = Line((0, 0), (75, 0))
line.color = Color("blue")
line.border = 2
line.draw(m)

h = Frame(150, 150)
line = Line((0, 0), (50, 0))
line.color = Color("black")
line.border = 3
line.draw(h)

s.draw(win)
m.draw(win)
h.draw(win)

def main():
    while True:
        t = time.localtime()
        s.rotateTo(t[5]/60 * 360 - 90)
        m.rotateTo(t[4]/60 * 360 - 90)
        h.rotateTo(t[3]/12 * 360 - 90)
        win.step(1)

win.run(main)

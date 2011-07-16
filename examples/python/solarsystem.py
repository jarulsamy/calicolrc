from Graphics import *

win = Window()
win.setBackground(Color("black"))

sun = Circle((150, 150), 50)
sun.fill = Color("yellow")
sun.draw(win)

earth = Circle((70, 70), 20)
earth.fill = Color("green")
earth.draw(sun)

moon = Circle((20, 20), 5)
moon.fill = Color("grey")
moon.draw(earth)

pen = Pen(Color("white"), True)
pen.draw(win)
pen.stackOnBottom()

def main():
    win.mode = "manual"
    while True:
        sun.rotate(1)
        earth.rotate(5)
        win.step(.1)
        pen.appendPath(Point(moon.gx, moon.gy))

getMouse()
win.run(main)

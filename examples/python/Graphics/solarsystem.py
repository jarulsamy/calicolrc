from Graphics import *

win = Window()
win.setBackground(Color("darkgray"))

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

#pics = []
def main():
    win.mode = "manual"
    for s in range(360):
        sun.rotate(1)
        earth.rotate(5)
        win.step(.05)
        pen.appendPath(Point(moon.gx, moon.gy))
        #pics.append(makePicture(win))

t = Text((150, 150), "Click to begin...")
t.draw(win)
getMouse()
t.undraw()
win.run(main)

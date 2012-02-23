from Graphics import *
from Myro import pickOne, randomNumber, Random

win = Window("Pong", 640, 480)
win.mode = "physics"
win.gravity = Vector(0,0)

top = Rectangle((0, 0), (640, 20))
top.bodyType = "static"
top.color = Color("green")
top.draw(win)

bottom = Rectangle((0, 460), (640, 480))
bottom.bodyType = "static"
bottom.color = Color("green")
bottom.draw(win)

left = Rectangle((20, 50), (40, 130))
left.bodyType = "static"
left.color = Color("black")
left.draw(win)

right = Rectangle((640 - 40, 50), (640 - 20, 130))
right.bodyType = "static"
right.color = Color("black")
right.draw(win)

def handleMouseClick(win, event):
    text.undraw()
    x, y = 100, 100 # event.X, event.Y
    c = Circle((x, y), 10)
    c.fill = Color("blue")
    c.bounce = 1.0
    c.draw(win)
    c.body.ApplyForce( Vector(50, 50))

onMouseDown(handleMouseClick)

text = Text(Point(200, 400), "Click in window to begin")
text.draw(win)

def main():
    while True:
        key = getKeyPressed()
        if key == "q":
            left.move(0, -5)
        elif key == "a":
            left.move(0, 5)
        if key == "p":
            right.move(0, -5)
        elif key == "l":
            right.move(0, 5)
        win.step(.01)

win.run(main)

from Graphics import *
from Myro import pickOne, randomNumber
import math

win = Window("Angry Blocks", 800, 300)
win.mode = "physics"

for x in range(5):
    for y in range(5 - x):
        #if y % 2 == 1:
        c = Rectangle((600 + y * 25 + x *10, 260 - x * 20),
                      (600 + y * 25 + 20 + x * 10, 280 - x * 20))
        #else:
        #    c = Pie((600 + y * 25 + x *10, 260 - x * 20), 10, 0, 360)
        c.fill = Color(pickOne(getColorNames()))
        c.draw(win)
        c.bounce = 0
radius = 10
ball = Circle((50, 270), radius)
ball.draw(win)
#ball.mass = .5
ball.bounce = 0.5
ball.friction = 0.05
#ball.wrap = True

ground = Rectangle((0, 281), (800, 310))
ground.bodyType = "static"
ground.color = Color("green")
ground.draw(win)
ground.friction = 10.0
#ground.bounce = .4

wall = Rectangle((780, 100), (800, 280))
wall.bodyType = "static"
wall.color = Color("blue")
wall.draw(win)

arrow = Line((0, 0), (0, 0))
head = Arrow((0, 0))

dragging = False

def mouseMove(obj, event):
    if dragging:
        head.moveTo(event.x, event.y)
        vec = Vector(event.x - ball.x, ball.y - event.y)
        arrow.set_points(Point(ball.x, ball.y), Point(event.x, event.y))
        arrow.draw(win)
        head.draw(win)
        dx = event.x - ball.x
        dy = event.y - ball.y
        if dx == 0:
            head.rotation = 90 if dy > 0 else 270
        else:
            head.rotation = math.atan(dy/dx) * 180/math.pi + (180 if dx < 0 else 0)

def mouseUp(obj, event):
    global dragging
    if dragging:
        arrow.undraw()
        head.undraw()
        dragging = False
        vec = Vector(event.x - ball.x, ball.y - event.y)
        ball.body.ApplyForce(vec)

def mouseDown(obj, event):
    global dragging
    if Point(event.x, event.y).distance(Point(ball.x, ball.y)) < radius:
        dragging = True
        mouseMove(obj, event)

def resetCallback(obj, event):
    ball.body.ResetDynamics()
    ball.moveTo(50, 270)

win.onMouseDown(mouseDown)
win.onMouseUp(mouseUp)
win.onMouseMovement(mouseMove)
win.onKeyPress(resetCallback)

#win.run()

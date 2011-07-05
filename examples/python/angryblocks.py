from Graphics import *
from Myro import pickOne, randomNumber
from Microsoft.Xna.Framework import Vector2

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

ball = Circle((50, 280), 10)
ball.draw(win)
#ball.mass = .5
ball.bounce = 0.5
ball.friction = 0.05

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

for x in range(1000):
    win.step(.01)
    if x == 100:
        ball.body.ApplyForce( Vector2(80,-42))

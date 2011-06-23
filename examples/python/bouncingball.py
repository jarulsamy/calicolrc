from Graphics import *
from Myro import pickOne

win = Window("Bouncing Shapes", 300, 600)
win.mode = "physics"
for i in range(5):
    c = Rectangle((130 - i, 130 - i), (130 - i + 20, 130 - i + 20))
    c.fill = Color(pickOne(["red", "blue", "pink"]))
    c.draw(win)
    c.bounce = .1

for i in range(5):
    c = Circle((130 - i, 150 - i), 10)
    c.fill = Color(pickOne(["red", "blue", "pink"]))
    c.draw(win)

ground = Rectangle((0, 580), (200, 610))
ground.draw(win)
ground.bodyType = "static"
ground.color = Color("green")

wall = Rectangle((0, 500), (10, 580))
wall.draw(win)
wall.bodyType = "static"

wall2 = Rectangle((180, 500), (190, 580))
wall2.draw(win)

wall3 = Rectangle((0, 400), (10, 500))
wall3.draw(win)

for x in range(600):
    win.step(.01)

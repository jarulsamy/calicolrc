from Graphics import *
from Myro import pickOne

win = Window("Bouncing Ball", 300, 600)
win.mode = "physics"
circles = []
for i in range(5):
    c = Rectangle((130 - i, 130 - i), (130 - i + 20, 130 - i + 20))
    c.draw(win)
    c.fill = Color(pickOne(["red", "blue", "pink"]))
    c.border = 1
    c.item.Restitution = .1
    circles.append(c)

for i in range(10):
    c = Circle((130 - i, 150 - i), 10)
    c.draw(win)
    c.fill = Color(pickOne(["red", "blue", "pink"]))
    c.border = 1
    circles.append(c)

ground = Rectangle((0, 580), (200, 610))
ground.draw(win)
ground.bodyType = "static"
ground.color = Color("green")
ground.border = 1

wall = Rectangle((0, 500), (10, 580))
wall.draw(win)
wall.bodyType = "static"
wall2 = Rectangle((180, 500), (190, 580))
wall2.draw(win)
wall2.bodyType = "dynamic"
wall3 = Rectangle((0, 400), (10, 500))
wall3.draw(win)
wall3.bodyType = "dynamic"

for x in range(750):
    win.step(.01)
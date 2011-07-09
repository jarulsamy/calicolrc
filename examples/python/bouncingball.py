from Graphics import *
from Myro import pickOne, randomNumber

win = Window("Bouncing Shapes", 300, 600)
win.mode = "physics"

p = Polygon((100, 100), (130, 100), (130, 150))
p.fill=Color("gold")
p.bounce = randomNumber() * .3
p.draw(win)

for i in range(5):
    c = Rectangle((130 - i, 130 - i), (130 - i + 20, 130 - i + 20))
    c.fill = Color(pickOne(getColorNames()))
    c.bounce = .1
    c.draw(win)

for i in range(5):
    c = Pie((130 - i, 150 - i), 10, 0, 360)
    c.fill = Color(pickOne(getColorNames()))
    c.draw(win)

ground = Rectangle((0, 580), (200, 610))
ground.bodyType = "static"
ground.color = Color("green")
ground.draw(win)

wall = Rectangle((0, 500), (10, 580))
wall.bodyType = "dynamic"
wall.draw(win)

wall2 = Rectangle((180, 500), (190, 580))
wall2.draw(win)

wall3 = Rectangle((0, 400), (10, 500))
wall3.draw(win)

def main():
    while True:
        if getMouseState() == "down":
            x, y = getMouseNow()
            c = Oval((x, y), 10, 20)
            c.fill = Color("gray")
            c.draw(win)
        win.step(.01)

# win.run() or
win.run(main)

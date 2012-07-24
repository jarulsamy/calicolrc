from Graphics import *
from Myro import wait

win = Window("Circles", 480, 120)
circle = Circle(getMouseNow(), 80)
circle.draw(win)
while True:
    circle.center.x, circle.center.y = getMouseNow()
    if getMouseState() == "down":
        circle.fill = Color("black")
    else:
        circle.fill = Color("white")
    wait(.1)

from Graphics import *

win = Window("Circles", 480, 120)
circle = Circle(getMouseNow(), 80)
circle.draw(win)

while win.IsRealized:
    circle.center.x, circle.center.y = getMouseNow()
    if getMouseState() == "down":
        circle.fill = Color("black")
    else:
        circle.fill = Color("white")

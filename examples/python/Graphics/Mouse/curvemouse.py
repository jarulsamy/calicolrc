from Graphics import *
win = Window("Shapes", 200, 200)
curve = None

while win.IsRealized:
    mouse = getMouseNow()
    if curve:
        curve.undraw()
    curve = Curve((10, 10), mouse, mouse, (190, 190))
    curve.border = 3
    curve.draw(win)
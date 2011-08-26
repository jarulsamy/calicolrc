from Graphics import *
win = Window("Shapes", 200, 200)
curve = None

while True:
    mouse = Point(*getMouseNow())
    if curve:
        curve.undraw()
    curve = Curve((10, 10), (mouse.x, mouse.y), (mouse.x, mouse.y), (190, 190))
    curve.border = 3
    curve.draw(win)
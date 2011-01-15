from Graphics import *

win = Window("USFlag", 700, 400)

def make_star(x, y, segment):
    arrow = Arrow(Point(x, y), 1)
    arrow.draw(win)
    arrow.pen_down()
    for i in range(5):
        arrow.forward(segment)
        arrow.rotate(72)
        arrow.forward(segment)
        arrow.rotate(-72)
        arrow.rotate(-72)
    polygon = arrow.pen_up()
    polygon.draw(win)
    polygon.color = "white"
    polygon.fill_color = "white"
    arrow.undraw()
    return polygon

for row in range(13):
    band = Rectangle(Point(0,row * 400/13), Point(700, row * 400/13 + 400/13))
    band.draw(win)
    if row % 2 == 1: # odd, white
        band.fill_color = "white"
        band.color = "white"
    else:
        band.fill_color = "red"
        band.color = "red"

blue = Rectangle(Point(0,0), Point(300, 214))
blue.fill_color = "blue"
blue.color = "blue"
blue.draw(win)
for col in range(6):
    for row in range(9):
        if row % 2 == 1: # odd row
            if col == 5:
                continue
            x = col * 50 + 25
        else:
            x = col * 50
        y = row * 22
        star = make_star(x + 10, y + 13, 5)


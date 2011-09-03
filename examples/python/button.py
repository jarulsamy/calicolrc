from Graphics import *
win = Window()
button = Button(Point(150, 150), "Press me!")
button.draw(win)

def printit(o, e):
    print(e)

button.connect("click", printit)

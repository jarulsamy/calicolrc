from Graphics import *
win = Window()
button = Button((120, 140), "Press me!")
button.draw(win)

def printit(o, e):
    print(e)

button.connect("click", printit)

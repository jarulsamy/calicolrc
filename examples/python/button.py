from Graphics import *
win = Window()
button = Button("Press me!")
button.draw(win, (120, 140))

def printit(o, e):
    print(e)

button.connect("click", printit)

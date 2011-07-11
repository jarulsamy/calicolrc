from Graphics import *
win = Window()

def handle(o, e):
    print(e)

win.onMouseDown(handle)

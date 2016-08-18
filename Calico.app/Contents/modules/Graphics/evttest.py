from Graphics import *
import Events

def goPrint(o, e):
    print("go was broadcast")

def pressPrint(o, e):
    print("circle pressed")

Events.init()
win = Window(500, 500)
c = Circle((250, 250), 50)
c.draw(win)

goID = c.subscribe("go", goPrint)
pressID = c.subscribe("mouse-press", pressPrint)

print("goID: " + str(goID))
print("pressID: " + str(pressID))

Events.publish("go")
from Myro import *
from Graphics import *
import Myro

x1, y1, x2, y2 = None, None, None, None
w = None
r = None
p = None

def handleMouseUp(obj, event):
    global r,x1,y1,x2,y2,p
    x2,y2= event.x, event.y
    if r:
        r.undraw()
    configureBlob(p, x1, y1, x2, y2)
    show(takePicture('blob'), "blob image")
    x1, y1, x2, y2 = None, None, None, None

def handleMouseMovement(obj, event):
    global r,w,x1,y1,x2,y2
    if r:
        r.undraw()
    if x1:
        r = Rectangle((x1, y1), (event.x, event.y))
        r.outline = None
        r.color = Color(255, 255, 0, 64)
        r.draw(w)


def handleMouseDown(obj, event):
    global x1,y1
    x1,y1= event.x, event.y

def main():
    global p, w
    p = takePicture()
    w = Window(getWidth(p), getHeight(p))
    p.draw(w)
    w.onMouseMovement(handleMouseMovement)
    w.onMouseUp(handleMouseUp)
    w.onMouseDown(handleMouseDown)

from Myro import *
from Graphics import *
import Myro

x1, y1, x2, y2 = None, None, None, None
w = None
r = None
p = None

show()

def handleMouseUp(obj, event):
    global x1,y1,x2,y2,p
    x2,y2= event.x, event.y
    print (Myro.robot.set_blob_yuv(p, x1, y1, x2, y2))
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

def configureBlob():
    global p, w
    p = takePicture()
    w = Window(256, 192)
    p.draw(w)
    w.onMouseMovement(handleMouseMovement)
    w.onMouseUp(handleMouseUp)
    w.onMouseDown(handleMouseDown)

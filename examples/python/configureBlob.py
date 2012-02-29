from Myro import *
from Graphics import *
import Myro

def configureBlob():
    p = takePicture()
    w = Window(256, 192)
    p.draw(w)
    p1 = getMouse()

    while getMouseState() == "down":
        p2 = getMouseNow()

    r = Rectangle(p1, p2)
    r.outline = None
    r.color = Color(255, 255, 0, 64)
    r.draw(w)
    
    print(p1, p2)

    print (Myro.robot.set_blob_yuv(p, p1[0], p1[1], p2[0], p2[1]))
    show(takePicture('blob'), "blob image")

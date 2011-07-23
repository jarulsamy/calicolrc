from Graphics import *
from Myro import wait

def handle_event(o, e):
    print(e)

w = Window("Coords")
w.onKeyPress( handle_event )
w.onKeyRelease( handle_event )
w.onMouseUp( handle_event )
w.onMouseDown( handle_event )
w.onMouseMovement( handle_event )

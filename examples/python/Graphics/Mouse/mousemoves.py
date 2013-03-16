from Graphics import *

last_time = []

def print_coords(o, e):
    print(e)
    last_time.append(e)

w = Window("Coords")
w.onMouseMovement( print_coords )

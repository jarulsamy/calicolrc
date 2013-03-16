# Turtle Graphics Example for Calico Python
# After http://en.wikipedia.org/wiki/File:Turtle-Graphics_Polyspiral.svg
# Doug Blank <dblank@cs.brynmawr.edu>

from Graphics import *

size = 600
win = Window("Turtle Graphics", size, size)
turtle = Arrow((size/2, size/2), 0)
turtle.draw(win)
turtle.penDown()

def f(dist, angle, incr, segs):
  for i in range(segs):
    turtle.forward(dist * (size * .35))
    turtle.rotate(-angle)
    dist += incr

f(.01, 89.5, .01, 184)

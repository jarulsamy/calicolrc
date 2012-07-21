# alphaChannels.py

from Processing import *
import math

window(500, 500)
smooth()

w = width()
h = height()
ww = 2*math.sqrt(w/4*w/4+h/4*h/4)
noStroke()
fill(255, 0, 0, 255)
ellipse(w/4, h/4, ww, ww)
fill(0, 255, 0, 127)
ellipse(3*w/4, 3*h/4, ww, ww)
fill(0, 0, 255, 127)
ellipse(3*w/4, h/4, ww, ww)
fill(255, 255, 0, 127)
ellipse(w/4, 3*h/4, ww, ww)

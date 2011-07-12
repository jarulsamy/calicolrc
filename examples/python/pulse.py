# Pulse Example
# Indicator that the computer is busy
# Doug Blank <dblank@cs.brynmawr.edu>

from Graphics import *
from Myro import wait
import math

win = Window()

alphas = list(reversed([x/10 * 255 for x in range(10)]))

ovals = []
for i in range(0, 360, 36):
    oval = Oval((150, 150), 50, 20)
    oval.rotate(-i)
    oval.color = Color("purple")
    position = int(abs(oval.rotation/(2 * math.pi) * 10))
    oval.color.alpha = alphas[9 - position]
    oval.draw(win)
    oval.forward(60)
    ovals.append(oval)

alphas.append(alphas.pop(0))

while True:
    for oval in ovals:
        position = int(abs(oval.rotation/(2 * math.pi) * 10))
        oval.color.alpha = alphas[9 - position]
        win.step(.0075)
    alphas.append(alphas.pop(0))


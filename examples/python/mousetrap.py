# Ping Pong Ball Model for Calico Python
# Doug Blank <dblank@cs.brynmawr.edu>

from Graphics import *
from Myro import askQuestion
import random

plot = Plot("Ping Pong Balls in the Air", 600, 300)
plot.xLabel.text = "time"
plot.yLabel.text = "ping pong balls"

coverage = Plot("Coverage", 600, 300)
coverage.xLabel.text = "time"
coverage.yLabel.text = "mousetraps"

size = 300
win = Window("Mousetrap Model", size, size)

picture = Picture(size, size, Color("blue"))
picture.draw(win)

def pick(x, y):
    return (x + random.gauss(0, 1) * 5,
            y + random.gauss(0, 1) * 5)

text = Text((150, 150), "Click to drop ping pong ball")
text.fill = Color("white")
text.draw(win)
traps = [getMouse()]
text.undraw()

steps = 0
sum = 0
while traps:
    steps += 1
    plot.append(len(traps))
    sum += len(traps)
    coverage.append(sum)
    next = []
    for x,y in traps:
        setColor(getPixel(picture,x,y), Color("red"))
        for i in range(2):
            x,y = pick(x, y)
            if getRed(getPixel(picture, x, y)) != 255:
                next.append((x,y))
    traps = next
print("Took", steps, "steps")
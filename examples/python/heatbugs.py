# Heatbugs Diffusion Model for Calico Python
# Doug Blank <dblank@cs.brynmawr.edu>

from Graphics import *
from random import randint
import math

size = 300
win = Window("Heatbug Model", size, size)

picture = Picture(size, size)
picture.draw(win)

bugs = []
for i in range(50):
    x, y = randint(0, size - 1 - 3), randint(0, size - 1 - 3)
    bug = Rectangle((x,y), (x+3, y+3))
    bug.color = Color("green")
    bug.draw(win)
    bugs.append(bug)

def distance(p1, p2):
    return math.sqrt( (p1.x - p2.x) ** 2 +
                      (p1.y - p2.y) ** 2)

maxdist = distance(Point(0,0), Point(10, 10))

def display(picture, bugs):
    newPic = Picture(size, size, Color("black"))
    for bug in bugs:
        for x in range(-10, 10):
            for y in range(-10, 10):
                if 0 <= x + bug.center.x < size and 0 <= y + bug.center.y < size:
                    pixel = getPixel(newPic, x + bug.center.x, y + bug.center.y)
                    prev = getRed(pixel)
                    heat = int((maxdist - distance(bug.center, Point(x + bug.center.x,y + bug.center.y)))/maxdist * 255)
                    heat = min(prev + heat, 255)
                    setColor(pixel, Color(heat, 0, 0))
    picture.setPixels(newPic)

def moveBugs(bugs):
    for bug in bugs:
        left = getColor(picture, bug.center.x - 1, bug.center.y).red
        right = getColor(picture, bug.center.x + 1, bug.center.y).red
        up = getColor(picture, bug.center.x, bug.center.y - 1).red
        down = getColor(picture, bug.center.x, bug.center.y + 1).red
        if left == max(left, right, up, down):
            bug.move(-1, 0)
        elif right == max(left, right, up, down):
            bug.move(1, 0)
        elif up == max(left, right, up, down):
            bug.move(0, -1)
        elif down == max(left, right, up, down):
            bug.move(0, 1)

for i in range(100):
    display(picture, bugs)
    moveBugs(bugs)
from Graphics import *
import random

win = Window("Turtles!")

turtles = []
for t in range(1000):
    width, height = win.DefaultWidth, win.DefaultHeight
    turtle = Arrow(Point(random.random() * win.DefaultWidth, 
                         random.random() * win.DefaultHeight), 
                   random.random() * 360)
    turtle.draw(win)
    turtle.color = random.choice(["red", "green", "blue", "yellow"])
    turtles.append(turtle)

win.ShowAll()

for step in range(10):
    for t in turtles:
        t.move(random.random() * 6 - 3, 
               random.random() * 6 - 3)

for t in range(1000):
    turtles[t].color = random.choice(["red", "green", "blue", "black"])

"""
for steps in range(100): 
    for t in range(1000):
        turtles[t].forward(1)

win.mode = 'manual'
for steps in range(100): 
    for t in range(1000):
        turtles[t].forward(1)
    win.update()

for t in range(1000):
    turtles[t].rotate(180)

"""

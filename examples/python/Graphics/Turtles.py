#
# Calico - Scripting Environment
#
# Copyright (c) 2011, Doug Blank <dblank@cs.brynmawr.edu>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# $Id: $

from Graphics import *
import random

win = Window("Turtles!")
win.mode = "manual"

turtles = []
for t in range(1000):
    width, height = win.DefaultWidth, win.DefaultHeight
    turtle = Arrow(Point(random.random() * win.DefaultWidth, 
                         random.random() * win.DefaultHeight), 
                   random.random() * 360)
    turtle.draw(win)
    turtle.color = makeColor(random.choice(["red", "green", "blue", "yellow"]))
    turtles.append(turtle)

win.ShowAll()

for step in range(100):
    for t in turtles:
        t.move(random.random() * 6 - 3, 
               random.random() * 6 - 3)
    win.step()

for t in range(1000):
    turtles[t].color = makeColor(random.choice(["red", "green", "blue", "black"]))
win.update()

"""
for steps in range(100): 
    for t in range(1000):
        turtles[t].forward(1)

win.mode = 'manual'
for steps in range(100): 
    for t in range(1000):
        turtles[t].forward(1)

for t in range(1000):
    turtles[t].rotate(180)

"""

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

win = Window("Octagons!")

turtle1 = Arrow(Point(30, 35))
turtle1.color = makeColor("red")

turtle1.draw(win)
win.mode = "manual"

hexes = []
count = 0
for rows in range(10):
    col = 6 if count % 2 == 0 else 5
    for cols in range(col):
        turtle1.penDown()
        for x in range(8):
            turtle1.rotate(45)
            turtle1.forward(10)
            win.step(.01)
        line = turtle1.penUp(True)
        s = Polygon(line)
        hexes.append(s)
        s.fill = makeColor(random.choice(["red", "black", "green", "white"]))
        s.outline = makeColor("black")
        s.draw(win)
        turtle1.move(35 + 17, 0)
    if count % 2 == 0:
        turtle1.moveTo(30 + 27, turtle1.center.y + 27)
    else:
        turtle1.moveTo(30, turtle1.center.y + 27)
    count += 1

#hexes = list(hexes)
for x in range(100):
    random.shuffle(hexes)
    for s in hexes:
        s.color.alpha = min(max(s.color.alpha + random.random() * 128 - 64, 0), 255)
    win.step(.1)

"""
win.mode = 'auto'
for s in hexes:
    s.color.alpha = 255
group = Group(*hexes)
group.rotate(20)
"""

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
turtle1.color = "red"

turtle1.draw(win)
win.mode = "manual"
win.step_time = 100

hexes = []
count = 0
for rows in range(10):
    col = 6 if count % 2 == 0 else 5
    for cols in range(col):
        turtle1.pen_down()
        for x in range(8):
            turtle1.rotate(45)
            turtle1.forward(10)
            win.step()
        s = turtle1.pen_up()
        hexes.append(s)
        s.fill_color = random.choice(["red", "black", "green", "white"])
        s.outline_color = "black"
        s.draw(win)
        turtle1.move(35 + 17, 0)
    if count % 2 == 0:
        turtle1.move_to(30 + 27, turtle1.center.y + 27)
    else:
        turtle1.move_to(30, turtle1.center.y + 27)
    count += 1

win.step_time = 5
#hexes = list(hexes)
for x in range(100):
    random.shuffle(hexes)
    for s in hexes:
        s.alpha = min(max(s.alpha + random.random() * .5 - .25, 0), 1)
    win.step()

"""
win.mode = 'auto'
for s in hexes:
    s.alpha = 1
win.update()
group = Group(*hexes)
group.rotate(20)
"""

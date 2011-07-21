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

win = Window("Group Test")

turtles = []
for x in range(4):
    turtle = Arrow(Point(random.random() * win.DefaultWidth, 
                         random.random() * win.DefaultHeight))
    turtle.color = makeColor(random.choice(["red", "blue", "green"]))
    turtle.draw(win)
    turtles.append(turtle)

#turtle
poly = Polygon(Point(10, 10), Point(50, 10), Point(50, 60), Point(10, 60))
poly.color = makeColor("red")
poly.draw(win)

group = Group(*turtles)
group.rotate(10)

win.mode = "manual"
for x in range(10):
    poly.rotate(36)
    win.step(.1)

# FIXME BUG:
#for x in range(360):
#    group.rotate(1)
#    win.step(.1)

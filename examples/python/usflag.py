#
# Pyjama - Scripting Environment
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

win = Window("USFlag", 700, 400)

def make_star(x, y, segment):
    arrow = Arrow(Point(x, y), 1)
    arrow.draw(win)
    arrow.pen_down()
    for i in range(5):
        arrow.forward(segment)
        arrow.rotate(72)
        arrow.forward(segment)
        arrow.rotate(-72)
        arrow.rotate(-72)
    polygon = arrow.pen_up()
    polygon.draw(win)
    polygon.color = "white"
    polygon.fill_color = "white"
    arrow.undraw()
    return polygon

for row in range(13):
    band = Rectangle(Point(0,row * 400/13), Point(700, row * 400/13 + 400/13))
    band.draw(win)
    if row % 2 == 1: # odd, white
        band.fill_color = "white"
        band.color = "white"
    else:
        band.fill_color = "red"
        band.color = "red"

blue = Rectangle(Point(0,0), Point(300, 214))
blue.fill_color = "blue"
blue.color = "blue"
blue.draw(win)
stars = []
for col in range(6):
    for row in range(9):
        if row % 2 == 1: # odd row
            if col == 5:
                continue
            x = col * 50 + 25
        else:
            x = col * 50
        y = row * 22
        star = make_star(x + 10, y + 13, 5)
        stars.append(star)

def animate():
    g = Group(*stars)
    win.mode = "manual"
    win.step_time = 1000
    for i in range(20):
        g.rotate(10)
        win.step()

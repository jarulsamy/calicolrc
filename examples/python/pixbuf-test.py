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
from Myro import wait
win = Window()
pic = Picture(pickAFile())
pic.draw(win)

arrow = Arrow(Point(10, 10), 0)
arrow.draw(win)

win.mode = "auto" # the default
for x in range(36):
    pic.rotate(10)
    wait(.1)

win.mode = "manual"
for x in range(36):
    pic.rotate(10)
    win.step(100)

def negative(pic):
    for pixel in getPixels(pic):
        setRed(pixel, 255 - getRed(pixel))

negative(pic)

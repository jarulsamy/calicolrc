#
# Pyjama - Educational Scripting Environment
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

file = "examples/images/blankenship.jpg"
win = Window("Laura")
pic = Picture(file)
pic.draw(win)
pic.move(150, 150)
pixels = list(pic.getPixels())


def reverse():
    win.mode = 'auto'
    for p in pixels:
        p.setRGB(255 - p.getRed(), 255 - p.getGreen(), 255 - p.getBlue())

def spin():
    win.mode = 'manual'
    for times in range(3):
        for degrees in range(0, 360, 10):
            pic.rotate(10)
            win.step()

def sortem():
    pixels.sort(key=lambda p: p.x + p.y)

def shuffle():
    random.shuffle(pixels)

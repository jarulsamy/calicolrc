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
win = Window("Mandelbrot", 300, 300)
win.mode = "manual"
pic = Picture(win.width, win.height)
pic.draw(win)

xa = -2.0
xb = 1.0
ya = -1.5
yb = 1.5
maxIt = 255 # max iterations allowed

for y in range(pic.height):
    zy = y * (yb - ya) / (pic.height - 1)  + ya
    for x in range(pic.width):
        zx = x * (xb - xa) / (pic.width - 1)  + xa
        z = zx + zy * 1j
        c = z
        for i in range(maxIt):
            if abs(z) > 2.0: break 
            z = z * z + c
        if i == maxIt - 1:
            i = 0
        pic.setRGB(x, y, i % 4 * 64, i % 16 * 16, i % 8 * 32)
    win.step()

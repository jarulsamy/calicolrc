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

import clr
clr.AddReference("gtk-sharp")
clr.AddReference("gdk-sharp")
clr.AddReference("Mono.Cairo")
import Gtk
import Gdk
import Cairo

filename = "examples/images/blankenship.jpg"
_pixbuf = Gdk.Pixbuf(filename)
_pixbuf.HasAlpha
format = Cairo.Format.Rgb24

#surface = Cairo.Surface.CreateForImage(format, _pixbuf.Width, _pixbuf.Height)

surface = Cairo.ImageSurface(format, _pixbuf.Width, _pixbuf.Height)


from Graphics import *
init()
#win = Window()

class MyWindow(Gtk.Window):
    def __init__(self, title):
        super(MyWindow, self).__init__()
        self.Add(Gtk.DrawingArea())
        Gtk.Application.Invoke(self.update)

    def update(self, obj, event):
        self.ShowAll()

class MyCanvas(Gtk.DrawingArea):
    #def __init__(self):
    #    self.ExposeEvent += self.render
    #    pass
    #def OnExposeEvent(self, event):
        #Gtk.Application.Invoke(self.update)
    #    print event

    def update(self, obj, args):
        print("expose!", obj, args)


win = MyWindow("")

#
# Calico - Scripting Environment
#
# Copyright (c) 2011-2012, Doug Blank <dblank@cs.brynmawr.edu>
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

from __future__ import print_function
import sys
sys.path.append("../../modules")
import clr
clr.AddReference("Calico")
import Calico
import System
clr.AddReference("Graphics.dll")
import Graphics
import os
import Gtk

class MyEngine(Calico.Engine):
    pass

class MyDocument(Calico.Document):
    def __init__(self, calico, filename, language):
        self.canvas = Graphics.Canvas("auto", 
                                      self.widget.Hadjustment, 
                                      self.widget.Vadjustment)
        self.focus_widget = self.canvas
        if filename:
            pic = Graphics.Picture(filename)
            pic.draw(self.canvas)
        self.widget.AddWithViewport(self.canvas)
        self.widget.ScrollChild += lambda o, args: print("here")
        self.widget.ShowAll()

    def GetAuthors(self):
        return System.Array[str]([
            "Douglas S. Blank <dblank@cs.brynmawr.edu>",
        ])

class MyLanguage(Calico.Language):
    def __init__(self):
        self.name = "images"
        self.proper_name = "Images"
        self.extensions = System.Array[str](["png", "jpg", "gif"])
        self.mimetype = "image/png"
        
    def MakeEngine(self, manager):
        self.engine = MyEngine(manager)

    def MakeDocument(self, calico, filename):
        return MyDocument(calico, filename, "images")

    def getExamplesPath(self, root_path):
    	return os.path.join(root_path, "../examples/images")

def MakeLanguage():
    return MyLanguage()

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

from __future__ import print_function
import sys
sys.path.append("../../modules")
import clr
clr.AddReference("Graphics.dll")
import os
import Gtk
import Graphics
from utils import Language
from document import Document, MyScrolledWindow
from engine import Engine

class ImagesDocument(Document):
    def make_widget(self):
        self.widget = MyScrolledWindow()
        self.canvas = Graphics.Canvas("auto", 
                                      self.widget.Hadjustment, 
                                      self.widget.Vadjustment)
        #self.widget.AddWithViewport(self.canvas)
        self.widget.Add(self.canvas)
        self.widget.document = self
        self.widget.ShowAll()

    def modify_font(self, font):
        pass

    def get_dirty(self):
        return True

    def configure(self):
        pass

    def open(self):
        self.picture = Graphics.Picture(self.filename)
        self.picture.draw(self.canvas)
        self.canvas.SetSize(self.picture.width, self.picture.height)

    def get_text(self):
        return ""
 
    def save(self):
        return True

    def save_as(self):
        return True

class Images(Language):
    def get_engine_class(self):
        return None

    def get_document_type(self):
        return "Image"

    def get_document_class(self):
        return ImagesDocument

def register_language():
    return Images("images", ["gif", "jpg"])

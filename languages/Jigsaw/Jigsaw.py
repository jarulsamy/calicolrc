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
import clr
import os
import sys
sys.path.append(os.path.abspath("languages/Jigsaw/Jigsaw/bin/Debug"))
clr.AddReference('gtk-sharp')
clr.AddReference('Jigsaw.exe')
import JigsawWidget
import Gtk
from utils import Language
from document import Document, MyScrolledWindow
from engine import Engine

class JigsawDocument(Document):
    def make_widget(self):
        self.widget = JigsawWidget()
        self.widget.document = self
        self.widget.ShowAll()

    def modify_font(self, font):
        pass

    def get_dirty(self):
        return True

    def configure(self):
        pass

    def open(self):
        pass

    def get_text(self):
        return "ok"
 
    def save(self):
        return True

    def save_as(self):
        return True

class JigsawEngine(Engine):
    def __init__(self, manager):
        super(JigsawEngine, self).__init__(manager, "jigsaw")
        self.text_based = False

    def setup(self):
        super(JigsawEngine, self).setup()
        #for file in glob.glob("modules/*.dll"):
        #    #path, dll_name = os.path.split(file)
        #    full_path = os.path.abspath(file)
        #    clr.AddReference(full_path)

    def execute_file(self, filename):
        print("Run filename '%s'!" % filename)

class Jigsaw(Language):
    def get_engine_class(self):
        return JigsawEngine

    def get_document_class(self):
        return JigsawDocument

def register_language():
    return Jigsaw("jigsaw", "xml")

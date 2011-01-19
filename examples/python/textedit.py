#! .pyjama --exec --nogui textedit.py

import sys
sys.path.append("bin")
import clr
clr.AddReference("gtk-sharp")
clr.AddReference("Mono.TextEditor")

import Gtk
Gtk.Application.Init()

from Mono.TextEditor import TextEditor

win = Gtk.Window("Text Editor Test")
te = TextEditor()
win.Add(te)

win.ShowAll()

Gtk.Application.Run()

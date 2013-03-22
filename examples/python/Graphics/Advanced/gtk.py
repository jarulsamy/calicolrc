import Gtk
invoke = Gtk.Application.Invoke

def block(obj, e):
    window = Gtk.Window("Title")
    vbox = Gtk.VBox()
    entry = Gtk.Entry()
    vbox.PackStart(entry)
    window.Add(vbox)

invoke(block)

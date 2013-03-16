import Gtk
invoke = Gtk.Application.Invoke
window = Gtk.Window("Title")
vbox = Gtk.VBox()
entry = Gtk.Entry()
vbox.PackStart(entry)
window.Add(vbox)
invoke(lambda obj, event: window.ShowAll())

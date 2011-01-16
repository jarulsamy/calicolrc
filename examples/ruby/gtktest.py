include Gtk

win = Gtk::Window.new("Testing 1 2 3")
button = Gtk::Button.new("Click me!")
button.click do
    print "Clicked!"
end
win.destroy do
    print "Destroy!" 
    Gtk.Quit
end
win.Add(button)
win.ShowAll
Gtk.Main
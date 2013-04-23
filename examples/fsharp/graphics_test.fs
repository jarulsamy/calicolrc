module MyModule

Gtk.Application.Init()

let win = new Graphics.WindowClass("Hello")
let line = new Graphics.Line(new Graphics.Point(0, 0),
                             new Graphics.Point(100, 100))
line.draw(win)
Gtk.Application.Run()
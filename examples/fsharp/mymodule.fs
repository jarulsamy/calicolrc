module MyModule

Graphics.Init()
let win = Graphics.Window("Hello")
let line = new Graphics.Line(new Graphics.Point(0, 0),
                             new Graphics.Point(100, 100))
line.draw(win)

# Ruby Graphics Example
win = Graphics::WindowClass.new("Hello")
line = Graphics::Line.new(Graphics::Point.new(0, 0), 
                          Graphics::Point.new(100, 100))
line.draw(win)
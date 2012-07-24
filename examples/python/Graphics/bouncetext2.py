from Graphics import *

win = Window("Hello World!", 640, 360)
win.mode = "physics"

line = 0
for text in ["Hello", "World!", "Welcome", "to", "Calico"]:
    col = 0
    for letter in text:
        t = Text((320 + col, 50 + line), letter)
        t.draw(win)
        t.wrap = True
        col += 10
    line += 16

ground = Rectangle((100, 340), (400, 360))
ground.draw(win)
ground.bodyType = "static"
ground.color = Color("green")

for i in range(20):
    win.step()
getMouse()
win.run()
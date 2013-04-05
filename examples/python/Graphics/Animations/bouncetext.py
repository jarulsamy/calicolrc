from Graphics import *

win = Window("Hello World!", 300, 600)
win.mode = "physics"

for text in ["Hello", "World!",
             "Welcome", "to", "Calico"]:
    t = Text((130, 100), text)
    t.draw(win)
    t.wrap = True

ground = Rectangle((0, 580), (200, 610))
ground.draw(win)
ground.bodyType = "static"
ground.color = Color("green")

win.run()

from Graphics import *

win = Window("Two Bouncing Balls", 300, 600)
win.mode = "physics"

c1 = Circle((130, 150), 10)
c1.draw(win)
c1.fill = Color("red")
c1.mass = 1.0

c2 = Circle((130, 150 - 20), 10)
c2.draw(win)
c2.fill = Color("blue")
c2.mass = 2.0

ground = Rectangle((0, 580), (200, 610))
ground.draw(win)
ground.bodyType = "static"
ground.color = Color("green")

win.run()
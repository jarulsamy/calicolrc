from Graphics import *
win = Window("Bouncing Ball", 300, 600)
win.mode = "physics"
c = Circle((150, 150), 45)
c.draw(win)
for x in range(100):
    win.step(.005)
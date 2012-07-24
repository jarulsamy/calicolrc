from Graphics import *

win = Window("Gravity Test", 300, 600)
win.mode = "physics"

ball = Circle((150, 100), 10)
ball.draw(win)

base = Rectangle((0, 500), (300, 510))
base.bodyType = "static"
base.draw(win)

win.run()
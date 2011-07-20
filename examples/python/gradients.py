from Graphics import *

win = Window()

sq = Rectangle((100, 100), (200, 200))
sq.gradient = Gradient("linear", (-50, 0), Color("red"), (50, 0), Color("blue"))
sq.draw(win)

c = Circle((250, 250), 50)
c.gradient = Gradient("radial", (0, 0), 10, Color("red"),
                      (0, 0), 30, Color(0, 0, 0, 0))
c.outline = None
c.draw(win)

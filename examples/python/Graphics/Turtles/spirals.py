from Graphics import *
win = Window()
def spiral(x, y, d, loops, color):
   arrow = Arrow((150, 150))
   arrow.pen.color = color
   arrow.draw(win)
   arrow.moveTo(x, y)
   arrow.rotateTo(d)
   arrow.penDown()
   i = 0.0
   while i < .180 * loops:
       arrow.rotate(-2)
       arrow.forward(.1 + i)
       i = i + .001

spiral(150, 150, 0, 18, Color("black"))
spiral(148, 158, 180, 18, Color("red"))
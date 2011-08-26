from Graphics import *
win = Window()

arrow1 = Arrow((150, 150))
arrow1.pen.color = Color("blue")
arrow1.draw(win)
arrow1.rotateTo(0)
arrow1.pen.setWidth(4)
arrow1.penDown()

arrow2 = Arrow((148, 158))
arrow2.pen.color = Color("red")
arrow2.draw(win)
arrow2.rotateTo(180)
arrow2.pen.setWidth(4)
arrow2.penDown()

def spirals(loops):
   i = 0.0
   while i < .180 * loops:
       arrow1.rotate(-2)
       arrow1.forward(.1 + i)
       arrow2.rotate(-2)
       arrow2.forward(.1 + i)
       i = i + .001

spirals(18)

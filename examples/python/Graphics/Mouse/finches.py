# Keith O'Hara <kohara@bard.edu>
# January 2012
# Calico python program based on processing sketch:
# http://studio.sketchpad.cc/sp/pad/view/ro.9sh2m5U9kyct-/rev.0

from Graphics import *
w = Window(800, 800)
w.mode = "bitmapmanual"
w.updateInterval = 1/30.0
w.setBackground(Color(20, 20, 20))

i = 32
x,y = 50, 50

while w.isRealized():

   r = Rectangle((0, 0), (w.width, w.height))
   r.fill = Color(0, 0, 0, 2)
   r.outline = None
   r.draw(w)
   if i < 32:
        i = i + .75
   else:
        i = 0

   mx, my = w.getMouseNow()
   vx = mx - x
   vy = my - y
   x += .015 * vx
   y += .015 * vy

   color = Color(255, 128, i*6, 16)

   c = Circle((x, y), i)
   c.outline =  None
   c.fill = color
   c.draw(w)

   w.step(1/60.0)

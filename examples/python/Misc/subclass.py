from Graphics import *
win = Window()

class MyCircle(Circle):
    def render(self, g):
        g.MoveTo(10, 10)
        g.LineTo(50, 50)
        g.Stroke()

mc = MyCircle((150, 150), 20)
mc.draw(win)

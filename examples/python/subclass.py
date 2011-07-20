from Graphics import *
win = Window()

class MyCircle(Circle):
    def render(self, g):
        pass

mc = MyCircle((150, 150), 20)
mc.draw(win)

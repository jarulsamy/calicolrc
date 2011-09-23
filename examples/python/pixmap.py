from Graphics import *

win = Window()
pic = Picture(100, 100, Color("white"))

pixmap = Pixmap(pic)

line = Line((10, 10), (20, 20))
pixmap.drawFromShape(line)
pixmap.draw(pic)
pic.draw(win)
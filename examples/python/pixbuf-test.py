from Graphics import *
win = Window()
pic = Picture("/home/dblank/Desktop/blankenship.jpg")
pic.center.x = win.width/2
pic.center.y = win.height/2
pic.draw(win)

arrow = Arrow(Point(10, 10), 0)
arrow.draw(win)

win.update()

win.mode = "auto"
for x in range(36):
    pic.rotate(10)
    #win.step()

def negative(pic):
    for pixel in getPixels(pic):
        setRed(pixel, 255 - getRed(pixel))

from Graphics import *

left = Picture(640, 480, Color("blue"))
#left.fill = Color("blue")

right = Picture(640, 480, Color("red"))
#right.fill =

win = Window("Histograph", 640, 520)
image = copyPicture(right)
image.center = Point(320, 240)
image.draw(win)

slider = HSlider((0, 480), 640)
slider.draw(win)

last = 0
region = None
def redisplay(o, e):
    global last, region
    new = int(e.value)
    if new != last:
        if region:
            region.undraw()
        if new > 0:
            region = left.getRegion((last/100 * left.width,0), new/100 * left.width, left.height, 0)
            region.draw(win)
        last = new

slider.connect("change-value", redisplay)

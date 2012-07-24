from Graphics import *

left = Picture("../../images/statue-2.png")
right = Picture("../../images/statue-orig.png")

win = Window("Histograph", left.width, left.height + 40)
image = copyPicture(right)
image.center = Point(left.width/2, left.height/2)
image.draw(win)

slider = HSlider((0, left.height), left.width)
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
            region = left.getRegion((0,0), new/100 * left.width, left.height)
            region.draw(win)
        last = new

slider.connect("change-value", redisplay)

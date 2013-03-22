from Graphics import *
from Myro import getFilenames

files = getFilenames(calico.relativePath("../examples/images/brain/*.jpg"))

pics = []

for file in files:
    pics.append(makePicture(file))

window = Window("Laura's Brain", pics[0].width, pics[1].height + 50)

last = 0
pics[last].draw(window)
pics[last].moveTo(pics[0].width/2, pics[0].height/2)

#slider = HSlider((0,pics[0].height), pics[0].width)
#slider.draw(window)

def showimage(obj, event):
    global last
    v = event.value
    pos = int(v/101 * len(pics) )
    if pos != last:
        window.undraw(pics[last])
        pics[pos].draw(window)
        pics[pos].moveTo(pics[0].width/2, pics[0].height/2)
        last = pos

#slider.connect("change-value", showimage)
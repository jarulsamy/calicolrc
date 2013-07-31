# Code to make a directory of sprite frames

from Graphics import *
import Myro
import os

def splitRow(filename, x, y, width, height, count):
    """
    Split a sprite sheet into a list of images.
      x,y = starting corner, upper-right hand position
      width - width of each image
      height - height of each image
      count - how many images on this row
    """
    retval = []
    pic = Picture(filename)
    for i in range(count):
        p = pic.getRegion((x + i * width, y), width, height)
        retval.append(p)
    return retval

def show(list, repeat=False, pause=.1):
    """
    Show the list as an animation.
    """
    if repeat:
        while True:
            showOnce(list, pause)
    else:
        showOnce(list, pause)

def showOnce(list, pause=.1):
    """
    Show the list as an animation just once.
    """
    for pic in list:
        Myro.show(pic)
        wait(pause)

def save(path, list):
    """
    Save the list of pictures in the given folder,
    numbered 0 through N.
    """
    try:
        os.mkdir(path)
    except:
        pass
    count = 0
    for p in list:
        filename = path + ("/%02d.png" % count)
        print("saving", filename)
        p.savePicture(filename)
        count += 1

def load(path):
    """
    Load a list of pictures in the given folder.
    """
    count = 0
    retval = []
    while True:
        filename = path + ("/%02d.png" % count)
        if os.path.exists(filename):
            print("loading", filename)
            p = Picture(filename)
            p.border = 0
            retval.append(p)
            count += 1
        else:
            break
    return retval


def animate(list):
    win = Window(800, list[0].height + 20)
    win.setBackground(Color(0,128,248))
    x, y = 0, 10
    count = 0
    previous = None
    while True:
        pic = list[count] ##walk_right[count]
        ##pic = Picture("bear/walk-right/%02d.png" % count)
        pic.moveTo(x - pic.width/2, y + pic.height/2)
        pic.draw(win)
        if previous:
            previous.undraw()
        previous = pic
        wait(.05)
        x += pic.width/16
        count += 1
        if count % len(list) == 0:
            count = 0
        if x > 800:
            x = 0

## Now, let's use the above in a specific way:

def main():
    walk_right = splitRow("source/bear_child.png", 0, 0, 60, 80, 14)
    walk_left = list(reversed(splitRow("source/bear_child.png", 0, 80, 60, 80, 14)))
    walk_right_with_spear = splitRow("source/bear_child.png", 0, 160, 104, 80, 14)
    walk_left_with_spear = list(reversed(splitRow("source/bear_child.png", 0, 240, 104, 80, 14)))

    p = Picture("source/walking-woman.gif")

    woman_right = splitRow("source/walking-woman.gif", 0, 0, 200, 400, 10)
    woman_right.extend(splitRow("source/walking-woman.gif", 0, 400, 200, 400, 10))
    for p in woman_right:
        p.setTransparent(Color(0, 128, 248))
    #
    walking = Myro.loadPictures("source/walkside.gif")
    new_walking = []
    for p in walking:
        new_walking.append(p.getRegion((20, 9), 128 - 20, 188 - 9))
        new_walking[-1].setTransparent(Color(0,0,0))

    save("sprites/bear/walk-right", walk_right)
    save("sprites/bear/walk-left", walk_left)
    save("sprites/bear-with-spear/walk-right", walk_right_with_spear)
    save("sprites/bear-with-spear/walk-left", walk_left_with_spear)
    save("sprites/stickman/walk-right", new_walking)
    save("sprites/woman/walk-right", woman_right)

if __name__ == "__main__":
    main()

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
        filename = path + ("/%d.gif" % count)
        print("saving", filename)
        p.savePicture(filename)
        walk_right = splitRow("bear_child.png", 0, 0, 60, 80, 14)
        count += 1

## Now, let's use the above in a specific way:

def main():
    walk_right = splitRow("bear_child.png", 0, 0, 60, 80, 14)
    walk_left = list(reversed(splitRow("bear_child.png", 0, 80, 60, 80, 14)))
    walk_right_with_spear = splitRow("bear_child.png", 0, 160, 104, 80, 14)
    walk_left_with_spear = list(reversed(splitRow("bear_child.png", 0, 240, 104, 80, 14)))
    
    save("bear/walk-right", walk_right)
    save("bear/walk-left", walk_left)
    save("bear-with-spear/walk-right", walk_right_with_spear)
    save("bear-with-spear/walk-left", walk_left_with_spear)

main()

from Graphics import *
import gis

fp = open("irene.dat")
dots = []
for line in fp:
    data = line.strip().split(",")
    x, y = (float(data[1]), float(data[2]))
    dots.append((x, y))

def drawIrene(win):
    line = Line()
    for (x,y) in dots:
        x, y = gis.ll2xy(x, y)
        c = Circle(Point(x, y), (1.8 ** gis.zoom))
        line.append(Point(x,y))
        c.fill = Color(255, 255, 0, 62)
        c.draw(win)
    line.draw(win)
    line.outline = Color(0, 0, 0, 62)
    line.border = 1.8 ** gis.zoom

def main():
    win = Window("Calico GIS", gis.width, gis.height)
    gis.drawStates(win)
    gis.drawCapitals(win)
    gis.displayControls(win)
    drawIrene(win)

    while win.IsRealized:
        x, y = getMouse()
        if x < 50:
            gis.zoom = max(0, gis.zoom - 1)
        elif x > gis.width - 50:
            gis.zoom = min(30, gis.zoom + 1)
        else:
            gis.center_ll = gis.xy2ll(x, y)
        print("Center at (%s North, %s West) at zoom %s" % (gis.center_ll[0], gis.center_ll[1], gis.zoom))
        gis.center = gis.ll2px(gis.center_ll[0], gis.center_ll[1], gis.zoom) # center of map, in global XY
        win.clear()
        gis.drawStates(win)
        gis.drawCapitals(win)
        gis.displayControls(win)
        drawIrene(win)

if __name__ == "<module>":
    main()